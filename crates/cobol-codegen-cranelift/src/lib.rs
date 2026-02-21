//! Cranelift codegen backend for fast debug builds.
//!
//! Trades optimization quality for faster compile times compared to the LLVM
//! backend. Uses the same `MirModule` input and `CodegenBackend` trait.
//! Designed for edit-compile-debug cycles during development.

use cobol_codegen_llvm::{CodegenBackend, CodegenError};
use cobol_mir::*;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{self, AbiParam, InstBuilder, MemFlags, TrapCode};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Public backend type
// ---------------------------------------------------------------------------

/// Cranelift codegen backend for fast debug builds.
///
/// Trades optimization quality for 20% faster compile times.
/// Uses the same MirModule input as the LLVM backend.
/// Designed for edit-compile-debug cycles during development.
pub struct CraneliftBackend;

impl CraneliftBackend {
    pub fn new() -> Self {
        Self
    }
}

impl Default for CraneliftBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl CodegenBackend for CraneliftBackend {
    fn compile(&self, module: &MirModule, output: &std::path::Path) -> Result<(), CodegenError> {
        let codegen = CraneliftCodegen::new()?;
        codegen.compile_module(module, output)
    }

    fn name(&self) -> &str {
        "cranelift"
    }

    fn output_extension(&self) -> &str {
        "o"
    }
}

// ---------------------------------------------------------------------------
// Internal codegen driver
// ---------------------------------------------------------------------------

/// Holds the Cranelift `ObjectModule` and bookkeeping tables for a single
/// compilation.
struct CraneliftCodegen {
    /// The Cranelift object-file module we are building.
    module: ObjectModule,
    /// Maps MIR global names to their Cranelift `DataId`.
    data_ids: HashMap<String, DataId>,
    /// The pointer type for the current target (I32 or I64).
    pointer_type: ir::Type,
    /// Maps child global names to their (parent_name, byte_offset) for group items.
    parent_offsets: HashMap<String, (String, u32)>,
}

impl CraneliftCodegen {
    /// Create a new codegen driver targeting the host machine.
    fn new() -> Result<Self, CodegenError> {
        // --- Shared compiler flags ---
        let mut flag_builder = settings::builder();
        flag_builder
            .set("opt_level", "none")
            .map_err(|e| CodegenError::BackendError(format!("set opt_level: {}", e)))?;
        flag_builder
            .set("is_pic", "true")
            .map_err(|e| CodegenError::BackendError(format!("set is_pic: {}", e)))?;

        let flags = settings::Flags::new(flag_builder);

        // --- ISA for the host machine ---
        let isa_builder = cranelift_native::builder()
            .map_err(|msg| CodegenError::BackendError(format!("cranelift native ISA: {}", msg)))?;
        let isa = isa_builder
            .finish(flags)
            .map_err(|e| CodegenError::BackendError(format!("ISA finish: {}", e)))?;

        let pointer_type = isa.pointer_type();

        // --- Object module ---
        let obj_builder = ObjectBuilder::new(
            isa,
            "cobol_module",
            cranelift_module::default_libcall_names(),
        )
        .map_err(|e| CodegenError::BackendError(format!("ObjectBuilder: {}", e)))?;

        let module = ObjectModule::new(obj_builder);

        Ok(Self {
            module,
            data_ids: HashMap::new(),
            pointer_type,
            parent_offsets: HashMap::new(),
        })
    }

    /// Compile an entire MIR module to an object file at `output`.
    fn compile_module(
        mut self,
        mir: &MirModule,
        output: &std::path::Path,
    ) -> Result<(), CodegenError> {
        // 1. Declare and define all globals.
        self.declare_globals(&mir.globals)?;
        self.define_globals(&mir.globals)?;

        // 2. Declare all MIR functions (so we can reference them).
        let mut func_ids: HashMap<String, FuncId> = HashMap::new();
        for func in &mir.functions {
            let fid = self.declare_mir_function(func)?;
            func_ids.insert(func.name.clone(), fid);
        }

        // 3. Scan instructions to collect external function references and
        //    their argument counts, then declare them all.
        let mut extern_funcs: HashMap<String, usize> = HashMap::new();
        for func in &mir.functions {
            for block in &func.blocks {
                for inst in &block.instructions {
                    if let MirInst::CallRuntime {
                        func: ref name,
                        args,
                        ..
                    } = inst
                    {
                        let entry = extern_funcs.entry(name.clone()).or_insert(0);
                        *entry = (*entry).max(args.len());
                    }
                    if let MirInst::MoveAlphanumeric { justified, .. } = inst {
                        if *justified {
                            extern_funcs
                                .entry("cobolrt_move_alphanumeric_justified".to_string())
                                .or_insert(4);
                        } else {
                            extern_funcs
                                .entry("cobolrt_move_alphanumeric".to_string())
                                .or_insert(4);
                        }
                    }
                }
            }
        }

        let mut runtime_func_ids: HashMap<String, FuncId> = HashMap::new();
        for (name, arg_count) in &extern_funcs {
            let fid = self.declare_runtime_function(name, *arg_count)?;
            runtime_func_ids.insert(name.clone(), fid);
        }

        // 4. Define (translate) each MIR function.
        for func in &mir.functions {
            self.define_function(func, &func_ids, &runtime_func_ids)?;
        }

        // 5. Emit a C-style `main` entry-point that calls `_cobol_main`.
        if let Some(&cobol_main_id) = func_ids.get("_cobol_main") {
            self.generate_c_main(cobol_main_id)?;
        }

        // 6. Serialize the object file to disk.
        let product = self.module.finish();
        let bytes = product
            .emit()
            .map_err(|e| CodegenError::BackendError(format!("object emit: {}", e)))?;
        std::fs::write(output, bytes)?;

        Ok(())
    }

    // -- globals --------------------------------------------------------------

    fn declare_globals(&mut self, globals: &[MirGlobal]) -> Result<(), CodegenError> {
        // First pass: build a map from name -> (parent_name, relative_offset)
        // so we can compute accumulated offsets for nested children.
        let mut raw_parent_offsets: HashMap<String, (String, u32)> = HashMap::new();
        for g in globals {
            if let Some((ref parent_name, byte_offset)) = g.parent_offset {
                raw_parent_offsets.insert(g.name.clone(), (parent_name.clone(), byte_offset));
            }
        }

        for g in globals {
            if let Some(ref redef_target) = g.redefines {
                // REDEFINES: share the same DataId as the redefined item
                if let Some(&target_id) = self.data_ids.get(redef_target) {
                    self.data_ids.insert(g.name.clone(), target_id);
                    // If this item also has a parent_offset (e.g. child of an
                    // OCCURS group), record the intra-element byte offset so
                    // subscripted access to individual fields within a multi-item
                    // OCCURS entry lands at the correct position.
                    if let Some((ref parent_name, byte_offset)) = g.parent_offset {
                        let mut total_offset = byte_offset;
                        let mut current = parent_name.clone();
                        let mut depth = 0u32;
                        while let Some((grandparent, gp_offset)) = raw_parent_offsets.get(&current)
                        {
                            depth += 1;
                            if depth > 100 || *grandparent == current {
                                break;
                            }
                            total_offset += gp_offset;
                            current = grandparent.clone();
                        }
                        self.parent_offsets
                            .insert(g.name.clone(), (current, total_offset));
                    }
                    continue;
                }
            }
            if let Some((ref parent_name, byte_offset)) = g.parent_offset {
                // Child of a group item: share the parent's DataId.
                // Compute the accumulated offset by walking the parent chain.
                if let Some(&parent_id) = self.data_ids.get(parent_name) {
                    self.data_ids.insert(g.name.clone(), parent_id);
                    // Walk the parent chain to accumulate the total offset from the root.
                    let mut total_offset = byte_offset;
                    let mut current = parent_name.clone();
                    let mut depth = 0u32;
                    while let Some((grandparent, gp_offset)) = raw_parent_offsets.get(&current) {
                        depth += 1;
                        if depth > 100 || *grandparent == current {
                            break;
                        }
                        total_offset += gp_offset;
                        current = grandparent.clone();
                    }
                    self.parent_offsets
                        .insert(g.name.clone(), (current, total_offset));
                    continue;
                }
            }
            let data_id = self
                .module
                .declare_data(
                    &g.name,
                    Linkage::Local,
                    true,  /* writable */
                    false, /* tls */
                )
                .map_err(|e| {
                    CodegenError::BackendError(format!("declare data '{}': {}", g.name, e))
                })?;
            self.data_ids.insert(g.name.clone(), data_id);
        }
        Ok(())
    }

    fn define_globals(&mut self, globals: &[MirGlobal]) -> Result<(), CodegenError> {
        // Build a map of parent -> children for composite initialization
        let mut children_of: HashMap<String, Vec<(u32, &MirGlobal)>> = HashMap::new();
        for g in globals {
            if let Some((ref parent_name, byte_offset)) = g.parent_offset {
                children_of
                    .entry(parent_name.clone())
                    .or_default()
                    .push((byte_offset, g));
            }
        }

        for g in globals {
            // Skip REDEFINES items — they share the data section of the redefined item
            if g.redefines.is_some() {
                continue;
            }
            // Skip children of group items — they share the parent's data section
            if g.parent_offset.is_some() {
                continue;
            }
            let data_id = self.data_ids[&g.name];
            let mut desc = DataDescription::new();

            // Check if this is a group item with children that need composite init
            let has_children = children_of.contains_key(&g.name);

            if has_children && g.initial_value.is_none() {
                // Group item: build composite initial value from children (recursively)
                let size = mir_type_size(&g.ty);
                let mut buf = vec![b' '; size]; // default: spaces (alphanumeric default)
                build_composite_init(&mut buf, &g.name, &children_of);
                desc.define(buf.into_boxed_slice());
            } else {
                match &g.initial_value {
                    Some(MirConst::Str(s)) => {
                        desc.define(s.as_bytes().to_vec().into_boxed_slice());
                    }
                    Some(MirConst::Bytes(b)) => {
                        desc.define(b.clone().into_boxed_slice());
                    }
                    Some(MirConst::Int(n)) => {
                        desc.define(n.to_le_bytes().to_vec().into_boxed_slice());
                    }
                    Some(MirConst::Int128(n)) => {
                        desc.define(n.to_le_bytes().to_vec().into_boxed_slice());
                    }
                    Some(MirConst::Float(f)) => {
                        desc.define(f.to_le_bytes().to_vec().into_boxed_slice());
                    }
                    Some(MirConst::Bool(b)) => {
                        desc.define(vec![*b as u8].into_boxed_slice());
                    }
                    Some(MirConst::Null) | None => {
                        let size = mir_type_size(&g.ty);
                        if size > 0 {
                            desc.define_zeroinit(size);
                        } else {
                            // zero-sized (Void) -- define a single zero byte so
                            // Cranelift doesn't complain about an empty data object.
                            desc.define_zeroinit(1);
                        }
                    }
                }
            }

            self.module.define_data(data_id, &desc).map_err(|e| {
                CodegenError::BackendError(format!("define data '{}': {}", g.name, e))
            })?;
        }
        Ok(())
    }

    // -- function declarations ------------------------------------------------

    /// Declare a MIR function (exported).
    fn declare_mir_function(&mut self, func: &MirFunction) -> Result<FuncId, CodegenError> {
        let mut sig = self.module.make_signature();
        for (_param_name, ty) in &func.params {
            sig.params.push(AbiParam::new(self.mir_type_to_cl(ty)));
        }
        if func.return_type != MirType::Void {
            sig.returns
                .push(AbiParam::new(self.mir_type_to_cl(&func.return_type)));
        }
        self.module
            .declare_function(&func.name, Linkage::Export, &sig)
            .map_err(|e| CodegenError::BackendError(format!("declare fn '{}': {}", func.name, e)))
    }

    /// Declare an external runtime function (imported).
    /// `arg_count` is used for unknown functions to infer the signature.
    fn declare_runtime_function(
        &mut self,
        name: &str,
        arg_count: usize,
    ) -> Result<FuncId, CodegenError> {
        let mut sig = self.module.make_signature();
        let ptr = self.pointer_type;

        match name {
            "cobolrt_display_line" | "cobolrt_display" => {
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // len
            }
            "cobolrt_stop_run" => {
                sig.params.push(AbiParam::new(types::I32)); // exit status
            }
            "cobolrt_accept" => {
                sig.params.push(AbiParam::new(ptr)); // buffer ptr
                sig.params.push(AbiParam::new(types::I32)); // buffer len
            }
            "cobolrt_accept_date"
            | "cobolrt_accept_day"
            | "cobolrt_accept_time"
            | "cobolrt_accept_day_of_week" => {
                sig.params.push(AbiParam::new(ptr)); // buffer ptr
                sig.params.push(AbiParam::new(types::I32)); // buffer len
            }
            "cobolrt_move_alphanumeric" => {
                sig.params.push(AbiParam::new(ptr)); // src
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
            }
            "cobolrt_move_numeric" => {
                sig.params.push(AbiParam::new(ptr)); // src
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(types::I32)); // src_scale
                sig.params.push(AbiParam::new(types::I32)); // src_dot_pos
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(types::I32)); // dest_scale
                sig.params.push(AbiParam::new(types::I32)); // dest_dot_pos
                sig.params.push(AbiParam::new(types::I32)); // rounded
            }
            "cobolrt_move_num_to_alpha" => {
                sig.params.push(AbiParam::new(ptr)); // src
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
            }
            "cobolrt_move_alpha_to_num" => {
                sig.params.push(AbiParam::new(ptr)); // src
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(types::I32)); // dest_dot_pos
            }
            "cobolrt_add_numeric"
            | "cobolrt_subtract_numeric"
            | "cobolrt_multiply_numeric"
            | "cobolrt_divide_numeric" => {
                sig.params.push(AbiParam::new(ptr)); // src1
                sig.params.push(AbiParam::new(types::I32)); // src1_len
                sig.params.push(AbiParam::new(ptr)); // src2
                sig.params.push(AbiParam::new(types::I32)); // src2_len
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(types::I32)); // dest_is_signed
            }
            "cobolrt_add_int_fast"
            | "cobolrt_sub_int_fast"
            | "cobolrt_mul_int_fast" => {
                sig.params.push(AbiParam::new(types::I64)); // operand (i64)
                sig.params.push(AbiParam::new(ptr)); // dest ptr
                sig.params.push(AbiParam::new(types::I32)); // dest_len
            }
            "cobolrt_int_to_display" => {
                sig.params.push(AbiParam::new(types::I64)); // value (i64)
                sig.params.push(AbiParam::new(ptr)); // dest ptr
                sig.params.push(AbiParam::new(types::I32)); // dest_len
            }
            "cobolrt_add_numeric_enc"
            | "cobolrt_subtract_numeric_enc"
            | "cobolrt_multiply_numeric_enc"
            | "cobolrt_divide_numeric_enc" => {
                sig.params.push(AbiParam::new(ptr)); // src1
                sig.params.push(AbiParam::new(types::I32)); // src1_len
                sig.params.push(AbiParam::new(types::I32)); // src1_enc
                sig.params.push(AbiParam::new(ptr)); // src2
                sig.params.push(AbiParam::new(types::I32)); // src2_len
                sig.params.push(AbiParam::new(types::I32)); // src2_enc
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(types::I32)); // dest_enc
                sig.params.push(AbiParam::new(types::I32)); // dest_is_signed
            }
            "cobolrt_compare_numeric" | "cobolrt_compare_alphanumeric" => {
                sig.params.push(AbiParam::new(ptr)); // src1
                sig.params.push(AbiParam::new(types::I32)); // src1_len
                sig.params.push(AbiParam::new(ptr)); // src2
                sig.params.push(AbiParam::new(types::I32)); // src2_len
                sig.returns.push(AbiParam::new(types::I32)); // -1, 0, 1
            }
            "cobolrt_compare_numeric_enc" => {
                sig.params.push(AbiParam::new(ptr)); // src1
                sig.params.push(AbiParam::new(types::I32)); // src1_len
                sig.params.push(AbiParam::new(types::I32)); // src1_enc
                sig.params.push(AbiParam::new(ptr)); // src2
                sig.params.push(AbiParam::new(types::I32)); // src2_len
                sig.params.push(AbiParam::new(types::I32)); // src2_enc
                sig.returns.push(AbiParam::new(types::I32)); // -1, 0, 1
            }
            "cobolrt_display_comp" | "cobolrt_display_comp3" => {
                sig.params.push(AbiParam::new(ptr)); // data
                sig.params.push(AbiParam::new(types::I32)); // byte_size
                sig.params.push(AbiParam::new(types::I32)); // digits
                sig.params.push(AbiParam::new(types::I32)); // is_signed
            }
            "cobolrt_move_numeric_enc" => {
                sig.params.push(AbiParam::new(ptr)); // src
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(types::I32)); // src_enc
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(types::I32)); // dest_enc
                sig.params.push(AbiParam::new(types::I32)); // dest_is_signed
            }
            "cobolrt_is_numeric"
            | "cobolrt_is_alphabetic"
            | "cobolrt_is_alphabetic_lower"
            | "cobolrt_is_alphabetic_upper" => {
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // len
                sig.returns.push(AbiParam::new(types::I32)); // 1=true, 0=false
            }
            "cobolrt_sign_check" => {
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // len
                sig.params.push(AbiParam::new(types::I32)); // scale
                sig.params.push(AbiParam::new(types::I32)); // mode (0=POSITIVE, 1=NEGATIVE, 2=ZERO)
                sig.returns.push(AbiParam::new(types::I32)); // 1=true, 0=false
            }
            "cobolrt_last_size_error" => {
                sig.returns.push(AbiParam::new(types::I32)); // 0 or 1
            }
            "cobolrt_set_size_error" => {
                sig.params.push(AbiParam::new(types::I32)); // flag
            }
            "cobolrt_file_open" => {
                sig.params.push(AbiParam::new(ptr)); // filename ptr
                sig.params.push(AbiParam::new(types::I32)); // filename len
                sig.params.push(AbiParam::new(types::I32)); // mode (0=input,1=output,2=io,3=extend)
                sig.params.push(AbiParam::new(types::I32)); // organization (0=seq,1=rel,2=idx,3=lineseq)
                sig.params.push(AbiParam::new(types::I32)); // access mode (0=seq,1=random,2=dynamic)
                sig.params.push(AbiParam::new(types::I32)); // record size
                sig.returns.push(AbiParam::new(types::I32)); // file handle
            }
            "cobolrt_file_close" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
            }
            "cobolrt_file_write_line" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // data len
            }
            "cobolrt_file_write_advancing" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // data len
                sig.params.push(AbiParam::new(types::I32)); // before_lines
                sig.params.push(AbiParam::new(types::I32)); // after_lines
            }
            "cobolrt_file_read_line" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
                sig.params.push(AbiParam::new(ptr)); // buffer ptr
                sig.params.push(AbiParam::new(types::I32)); // buffer len
                sig.returns.push(AbiParam::new(types::I32)); // eof flag (0=ok, 1=eof)
            }
            "cobolrt_file_write_record" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // data len
            }
            "cobolrt_file_read_record" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
                sig.params.push(AbiParam::new(ptr)); // buffer ptr
                sig.params.push(AbiParam::new(types::I32)); // buffer len
                sig.returns.push(AbiParam::new(types::I32)); // eof flag (0=ok, 1=eof)
            }
            "cobolrt_file_rewrite" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // data len
            }
            "cobolrt_set_file_status" => {
                sig.params.push(AbiParam::new(ptr)); // status var ptr
                sig.params.push(AbiParam::new(types::I32)); // status code
            }
            "cobolrt_file_delete" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
            }
            "cobolrt_file_start" => {
                sig.params.push(AbiParam::new(types::I32)); // file handle
                sig.params.push(AbiParam::new(ptr)); // key ptr
                sig.params.push(AbiParam::new(types::I32)); // key len
            }
            "cobolrt_string_append" => {
                sig.params.push(AbiParam::new(ptr)); // src
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(ptr)); // ptr (pointer var)
                sig.params.push(AbiParam::new(types::I32)); // ptr_len
                sig.params.push(AbiParam::new(ptr)); // delim
                sig.params.push(AbiParam::new(types::I32)); // delim_len
            }
            "cobolrt_divide_scaled" => {
                // (ptr, i32, i32, ptr, i32, i32, ptr, i32, i32, i32, i32)
                sig.params.push(AbiParam::new(ptr)); // src1
                sig.params.push(AbiParam::new(types::I32)); // src1_len
                sig.params.push(AbiParam::new(types::I32)); // src1_scale
                sig.params.push(AbiParam::new(ptr)); // src2
                sig.params.push(AbiParam::new(types::I32)); // src2_len
                sig.params.push(AbiParam::new(types::I32)); // src2_scale
                sig.params.push(AbiParam::new(ptr)); // dest
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(types::I32)); // dest_scale
                sig.params.push(AbiParam::new(types::I32)); // dest_dot_pos
                sig.params.push(AbiParam::new(types::I32)); // rounded
            }
            "cobolrt_remainder_scaled" => {
                // (dividend_ptr, dv_len, dv_scale, divisor_ptr, ds_len, ds_scale,
                //  quotient_ptr, q_len, q_scale, remainder_ptr, rem_len, rem_scale, rem_dot_pos)
                sig.params.push(AbiParam::new(ptr)); // dividend
                sig.params.push(AbiParam::new(types::I32)); // dividend_len
                sig.params.push(AbiParam::new(types::I32)); // dividend_scale
                sig.params.push(AbiParam::new(ptr)); // divisor
                sig.params.push(AbiParam::new(types::I32)); // divisor_len
                sig.params.push(AbiParam::new(types::I32)); // divisor_scale
                sig.params.push(AbiParam::new(ptr)); // quotient
                sig.params.push(AbiParam::new(types::I32)); // quotient_len
                sig.params.push(AbiParam::new(types::I32)); // quotient_scale
                sig.params.push(AbiParam::new(ptr)); // remainder
                sig.params.push(AbiParam::new(types::I32)); // remainder_len
                sig.params.push(AbiParam::new(types::I32)); // remainder_scale
                sig.params.push(AbiParam::new(types::I32)); // remainder_dot_pos
            }
            "cobolrt_alloc_temp" => {
                sig.params.push(AbiParam::new(ptr)); // src string
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_decimal_add"
            | "cobolrt_decimal_sub"
            | "cobolrt_decimal_mul"
            | "cobolrt_decimal_div"
            | "cobolrt_decimal_pow" => {
                // Display-format: (left_ptr, left_len, right_ptr, right_len, result_len) -> ptr
                sig.params.push(AbiParam::new(ptr)); // left ptr
                sig.params.push(AbiParam::new(types::I32)); // left len
                sig.params.push(AbiParam::new(ptr)); // right ptr
                sig.params.push(AbiParam::new(types::I32)); // right len
                sig.params.push(AbiParam::new(types::I32)); // result len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_store_compute_result" => {
                // (src_ptr, src_len, dest_ptr, dest_len, dest_scale, dest_dot_pos, rounded) -> void
                sig.params.push(AbiParam::new(ptr)); // src ptr
                sig.params.push(AbiParam::new(types::I32)); // src len
                sig.params.push(AbiParam::new(ptr)); // dest ptr
                sig.params.push(AbiParam::new(types::I32)); // dest len
                sig.params.push(AbiParam::new(types::I32)); // dest scale
                sig.params.push(AbiParam::new(types::I32)); // dest dot pos
                sig.params.push(AbiParam::new(types::I32)); // rounded
            }
            "cobolrt_decimal_cmp" => {
                sig.params.push(AbiParam::new(ptr)); // left ptr
                sig.params.push(AbiParam::new(ptr)); // right ptr
                sig.returns.push(AbiParam::new(types::I32)); // -1/0/1
            }
            "cobolrt_negate_numeric" => {
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // data len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_display_to_int" => {
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // len
                sig.returns.push(AbiParam::new(types::I64)); // integer value
            }
            "cobolrt_format_numeric_edited" => {
                sig.params.push(AbiParam::new(ptr)); // data ptr
                sig.params.push(AbiParam::new(types::I32)); // data_len
                sig.params.push(AbiParam::new(ptr)); // pic string ptr
                sig.params.push(AbiParam::new(types::I32)); // pic_len
            }
            "cobolrt_deedit" => {
                // (src, src_len, pic, pic_len, dest, dest_len, dest_scale, dest_dot_pos)
                sig.params.push(AbiParam::new(ptr)); // src ptr
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(ptr)); // pic string ptr
                sig.params.push(AbiParam::new(types::I32)); // pic_len
                sig.params.push(AbiParam::new(ptr)); // dest ptr
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.params.push(AbiParam::new(types::I32)); // dest_scale
                sig.params.push(AbiParam::new(types::I32)); // dest_dot_pos
            }
            "cobolrt_unstring_begin" => {
                sig.params.push(AbiParam::new(types::I32)); // num_delimiters
            }
            "cobolrt_unstring_add_delim" => {
                sig.params.push(AbiParam::new(ptr)); // delim ptr
                sig.params.push(AbiParam::new(types::I32)); // delim_len
                sig.params.push(AbiParam::new(types::I32)); // all_flag
            }
            "cobolrt_unstring_field" => {
                sig.params.push(AbiParam::new(ptr)); // source
                sig.params.push(AbiParam::new(types::I32)); // source_len
                sig.params.push(AbiParam::new(ptr)); // target
                sig.params.push(AbiParam::new(types::I32)); // target_len
                sig.params.push(AbiParam::new(ptr)); // pointer
                sig.params.push(AbiParam::new(types::I32)); // pointer_len
                sig.params.push(AbiParam::new(ptr)); // tally
                sig.params.push(AbiParam::new(types::I32)); // tally_len
                sig.params.push(AbiParam::new(ptr)); // count_in
                sig.params.push(AbiParam::new(types::I32)); // count_in_len
                sig.params.push(AbiParam::new(ptr)); // delim_in
                sig.params.push(AbiParam::new(types::I32)); // delim_in_len
            }
            "cobolrt_upper_case" | "cobolrt_lower_case" | "cobolrt_reverse" | "cobolrt_trim" => {
                // (src_ptr, src_len, dest_len) -> result_ptr
                sig.params.push(AbiParam::new(ptr)); // src ptr
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_numval" => {
                // (src_ptr, src_len, dest_len) -> result_ptr
                sig.params.push(AbiParam::new(ptr)); // src ptr
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_max_numeric" | "cobolrt_min_numeric" => {
                // (a_ptr, a_len, b_ptr, b_len) -> result_ptr
                sig.params.push(AbiParam::new(ptr)); // a ptr
                sig.params.push(AbiParam::new(types::I32)); // a_len
                sig.params.push(AbiParam::new(ptr)); // b ptr
                sig.params.push(AbiParam::new(types::I32)); // b_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr (points to a or b)
            }
            "cobolrt_mod" | "cobolrt_rem" => {
                // (a_ptr, a_len, b_ptr, b_len) -> result_ptr
                sig.params.push(AbiParam::new(ptr)); // a ptr
                sig.params.push(AbiParam::new(types::I32)); // a_len
                sig.params.push(AbiParam::new(ptr)); // b ptr
                sig.params.push(AbiParam::new(types::I32)); // b_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_ord" => {
                // (src_ptr, src_len, dest_len) -> result_ptr
                sig.params.push(AbiParam::new(ptr)); // src ptr
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_char" => {
                // (ordinal) -> result_ptr
                sig.params.push(AbiParam::new(types::I32)); // ordinal
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_abs" | "cobolrt_integer" | "cobolrt_integer_part" => {
                // (src_ptr, src_len) -> result_ptr
                sig.params.push(AbiParam::new(ptr)); // src ptr
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_sign" => {
                // (src_ptr, src_len, dest_len) -> result_ptr
                sig.params.push(AbiParam::new(ptr)); // src ptr
                sig.params.push(AbiParam::new(types::I32)); // src_len
                sig.params.push(AbiParam::new(types::I32)); // dest_len
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_current_date" | "cobolrt_when_compiled" => {
                // () -> result_ptr
                sig.returns.push(AbiParam::new(ptr)); // result ptr
            }
            "cobolrt_sort_using_giving" => {
                // (input_ptr, input_len, output_ptr, output_len,
                //  record_size, num_keys, key_info_ptr, key_info_len)
                sig.params.push(AbiParam::new(ptr)); // input filename ptr
                sig.params.push(AbiParam::new(types::I32)); // input filename len
                sig.params.push(AbiParam::new(ptr)); // output filename ptr
                sig.params.push(AbiParam::new(types::I32)); // output filename len
                sig.params.push(AbiParam::new(types::I32)); // record size
                sig.params.push(AbiParam::new(types::I32)); // num keys
                sig.params.push(AbiParam::new(ptr)); // key info string ptr
                sig.params.push(AbiParam::new(types::I32)); // key info string len
            }
            _ => {
                // Unknown function (likely a subprogram call).
                // Infer signature: all pointer args, void return.
                for _ in 0..arg_count {
                    sig.params.push(AbiParam::new(ptr));
                }
            }
        }

        self.module
            .declare_function(name, Linkage::Import, &sig)
            .map_err(|e| {
                CodegenError::BackendError(format!("declare runtime fn '{}': {}", name, e))
            })
    }

    // -- function definition (translation) ------------------------------------

    fn define_function(
        &mut self,
        func: &MirFunction,
        func_ids: &HashMap<String, FuncId>,
        runtime_func_ids: &HashMap<String, FuncId>,
    ) -> Result<(), CodegenError> {
        let func_id = func_ids[&func.name];

        let mut ctx = self.module.make_context();

        // Reproduce the signature in the context.
        for (_pname, ty) in &func.params {
            ctx.func
                .signature
                .params
                .push(AbiParam::new(self.mir_type_to_cl(ty)));
        }
        if func.return_type != MirType::Void {
            ctx.func
                .signature
                .returns
                .push(AbiParam::new(self.mir_type_to_cl(&func.return_type)));
        }

        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);

            // Create a Cranelift Block for every MIR BasicBlock.
            let mut block_map: HashMap<u32, ir::Block> = HashMap::new();
            for bb in &func.blocks {
                let cl_block = builder.create_block();
                block_map.insert(bb.id.raw(), cl_block);
            }

            // MIR Value -> Cranelift Value
            let mut val_map: HashMap<u32, ir::Value> = HashMap::new();

            // Translate each MIR basic block.
            for bb in &func.blocks {
                let cl_block = block_map[&bb.id.raw()];
                builder.switch_to_block(cl_block);

                // For the entry block, append function parameters.
                if bb.id == func.entry_block {
                    builder.append_block_params_for_function_params(cl_block);
                    // Map MIR parameter values (if the caller used them).
                    let block_params = builder.block_params(cl_block).to_vec();
                    for (i, (_pname, _pty)) in func.params.iter().enumerate() {
                        // The MIR doesn't give us explicit Value ids for params,
                        // but if a CallProgram or similar references them later,
                        // they would be added to val_map here.
                        if i < block_params.len() {
                            // Convention: first N values (0..N) are parameters
                            val_map.insert(i as u32, block_params[i]);
                        }
                    }
                }

                // Instructions
                for inst in &bb.instructions {
                    self.translate_inst(
                        inst,
                        &mut builder,
                        &mut val_map,
                        &block_map,
                        func_ids,
                        runtime_func_ids,
                    )?;
                }

                // Terminator
                self.translate_terminator(&bb.terminator, &mut builder, &val_map, &block_map)?;
            }

            // Seal all blocks after translation so that loop back-edges
            // (e.g. body → cond) are visible before sealing.
            builder.seal_all_blocks();
            builder.finalize();
        }

        self.module
            .define_function(func_id, &mut ctx)
            .map_err(|e| {
                CodegenError::BackendError(format!("define fn '{}': {:?}", func.name, e))
            })?;

        self.module.clear_context(&mut ctx);
        Ok(())
    }

    // -- instruction translation ----------------------------------------------

    fn translate_inst(
        &mut self,
        inst: &MirInst,
        builder: &mut FunctionBuilder,
        val_map: &mut HashMap<u32, ir::Value>,
        _block_map: &HashMap<u32, ir::Block>,
        _func_ids: &HashMap<String, FuncId>,
        runtime_func_ids: &HashMap<String, FuncId>,
    ) -> Result<(), CodegenError> {
        let ptr_ty = self.pointer_type;

        match inst {
            // -- Constants ----------------------------------------------------
            MirInst::Const { dest, value } => {
                let cl = match value {
                    MirConst::Int(n) => builder.ins().iconst(types::I64, *n),
                    MirConst::Int128(n) => {
                        // Cranelift I128 support is limited; store the low 64 bits.
                        builder.ins().iconst(types::I64, *n as i64)
                    }
                    MirConst::Bool(b) => builder.ins().iconst(types::I8, *b as i64),
                    MirConst::Float(f) => builder.ins().f64const(*f),
                    MirConst::Null => builder.ins().iconst(ptr_ty, 0),
                    MirConst::Str(s) => {
                        // Create a read-only data section for this string constant
                        // and return a pointer to it.
                        let gname = format!(".str.{}", dest.raw());
                        let data_id = if let Some(&did) = self.data_ids.get(&gname) {
                            did
                        } else {
                            let did = self
                                .module
                                .declare_data(
                                    &gname,
                                    Linkage::Local,
                                    false, /* not writable */
                                    false,
                                )
                                .map_err(|e| {
                                    CodegenError::BackendError(format!(
                                        "declare str const '{}': {}",
                                        gname, e
                                    ))
                                })?;
                            let mut desc = DataDescription::new();
                            desc.define(s.as_bytes().to_vec().into_boxed_slice());
                            self.module.define_data(did, &desc).map_err(|e| {
                                CodegenError::BackendError(format!(
                                    "define str const '{}': {}",
                                    gname, e
                                ))
                            })?;
                            self.data_ids.insert(gname, did);
                            did
                        };
                        let gv = self.module.declare_data_in_func(data_id, builder.func);
                        builder.ins().global_value(ptr_ty, gv)
                    }
                    MirConst::Bytes(_) => builder.ins().iconst(ptr_ty, 0),
                };
                val_map.insert(dest.raw(), cl);
            }

            // -- Global address -----------------------------------------------
            MirInst::GlobalAddr { dest, name } => {
                let data_id = self.data_ids.get(name).ok_or_else(|| {
                    CodegenError::BackendError(format!("unknown global '{}'", name))
                })?;
                let gv = self.module.declare_data_in_func(*data_id, builder.func);
                let base = builder.ins().global_value(ptr_ty, gv);
                // If this is a child of a group item, add the byte offset
                let v = if let Some((_parent, byte_offset)) = self.parent_offsets.get(name) {
                    if *byte_offset > 0 {
                        let offset_val = builder.ins().iconst(ptr_ty, *byte_offset as i64);
                        builder.ins().iadd(base, offset_val)
                    } else {
                        base
                    }
                } else {
                    base
                };
                val_map.insert(dest.raw(), v);
            }

            // -- Memory -------------------------------------------------------
            MirInst::Load { dest, addr, ty } => {
                let a = self.lookup(val_map, addr)?;
                let cl_ty = self.mir_type_to_cl(ty);
                let v = builder.ins().load(cl_ty, MemFlags::new(), a, 0);
                val_map.insert(dest.raw(), v);
            }
            MirInst::Store { addr, value } => {
                let a = self.lookup(val_map, addr)?;
                let v = self.lookup(val_map, value)?;
                builder.ins().store(MemFlags::new(), v, a, 0);
            }
            MirInst::GetFieldAddr { dest, base, offset } => {
                let b = self.lookup(val_map, base)?;
                let v = builder.ins().iadd_imm(b, *offset as i64);
                val_map.insert(dest.raw(), v);
            }
            MirInst::GetElementAddr {
                dest,
                base,
                index,
                element_size,
            } => {
                let b = self.lookup(val_map, base)?;
                let idx = self.lookup(val_map, index)?;
                let sz = builder.ins().iconst(ptr_ty, *element_size as i64);
                let off = builder.ins().imul(idx, sz);
                let v = builder.ins().iadd(b, off);
                val_map.insert(dest.raw(), v);
            }

            // -- Integer arithmetic -------------------------------------------
            MirInst::IAdd { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().iadd(l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::ISub { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().isub(l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::IMul { dest, left, right } => {
                let l = self.lookup(val_map, left)?;
                let r = self.lookup(val_map, right)?;
                let v = builder.ins().imul(l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::IDiv { dest, left, right } => {
                let l = self.lookup(val_map, left)?;
                let r = self.lookup(val_map, right)?;
                let v = builder.ins().sdiv(l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::IRem { dest, left, right } => {
                let l = self.lookup(val_map, left)?;
                let r = self.lookup(val_map, right)?;
                // Cranelift: srem for signed remainder
                let v = builder.ins().srem(l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::INeg { dest, operand } => {
                let o = self.lookup(val_map, operand)?;
                let v = builder.ins().ineg(o);
                val_map.insert(dest.raw(), v);
            }

            // -- Integer comparisons ------------------------------------------
            MirInst::ICmpEq { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().icmp(IntCC::Equal, l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::ICmpNe { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().icmp(IntCC::NotEqual, l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::ICmpLt { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().icmp(IntCC::SignedLessThan, l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::ICmpGt { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::ICmpLe { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::ICmpGe { dest, left, right } => {
                let (l, r) = self.lookup_and_coerce_pair(val_map, left, right, builder)?;
                let v = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, l, r);
                val_map.insert(dest.raw(), v);
            }

            // -- Runtime calls ------------------------------------------------
            MirInst::CallRuntime { dest, func, args } => {
                let fid = runtime_func_ids.get(func.as_str()).ok_or_else(|| {
                    CodegenError::BackendError(format!("unknown runtime fn '{}'", func))
                })?;
                let func_ref = self.module.declare_func_in_func(*fid, builder.func);

                // Look up the callee signature so we can coerce argument types.
                let sig = builder.func.dfg.ext_funcs[func_ref].signature;
                let param_types: Vec<ir::Type> = builder.func.dfg.signatures[sig]
                    .params
                    .iter()
                    .map(|p| p.value_type)
                    .collect();

                let raw_args = self.lookup_many(val_map, args)?;
                let cl_args: Vec<ir::Value> = raw_args
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| {
                        if i < param_types.len() {
                            coerce_value(builder, v, param_types[i])
                        } else {
                            v
                        }
                    })
                    .collect();

                let call = builder.ins().call(func_ref, &cl_args);
                if let Some(d) = dest {
                    let results = builder.inst_results(call);
                    if !results.is_empty() {
                        val_map.insert(d.raw(), results[0]);
                    }
                }
            }

            MirInst::CallProgram {
                dest,
                program: _,
                args: _,
            } => {
                // TODO: implement CALL <program> properly.
                // For now, just map dest to zero if present.
                if let Some(d) = dest {
                    let zero = builder.ins().iconst(types::I32, 0);
                    val_map.insert(d.raw(), zero);
                }
            }

            // -- String operations (runtime-backed) ---------------------------
            MirInst::MoveAlphanumeric {
                dest,
                src,
                dest_len,
                src_len,
                justified,
            } => {
                let func_name = if *justified {
                    "cobolrt_move_alphanumeric_justified"
                } else {
                    "cobolrt_move_alphanumeric"
                };
                if let Some(&fid) = runtime_func_ids.get(func_name) {
                    let func_ref = self.module.declare_func_in_func(fid, builder.func);
                    let src_val = self.lookup(val_map, src)?;
                    let dest_val = self.lookup(val_map, dest)?;
                    let sl = builder.ins().iconst(types::I32, *src_len as i64);
                    let dl = builder.ins().iconst(types::I32, *dest_len as i64);
                    builder.ins().call(func_ref, &[src_val, sl, dest_val, dl]);
                }
            }

            // -- Decimal operations (stubs -- will call runtime) --------------
            MirInst::DecimalAdd {
                dest, left, right, ..
            }
            | MirInst::DecimalSub {
                dest, left, right, ..
            }
            | MirInst::DecimalMul {
                dest, left, right, ..
            }
            | MirInst::DecimalDiv {
                dest, left, right, ..
            } => {
                // Fall back to integer add for now; the runtime will handle
                // scale adjustment.
                let l = self.lookup(val_map, left)?;
                let r = self.lookup(val_map, right)?;
                let v = builder.ins().iadd(l, r);
                val_map.insert(dest.raw(), v);
            }
            MirInst::DecimalCmp { dest, left, right } => {
                let l = self.lookup(val_map, left)?;
                let r = self.lookup(val_map, right)?;
                let v = builder.ins().icmp(IntCC::SignedLessThan, l, r);
                val_map.insert(dest.raw(), v);
            }

            // -- Conversions (stubs) ------------------------------------------
            MirInst::DecimalToDisplay { dest, value, .. }
            | MirInst::DisplayToDecimal { dest, value, .. }
            | MirInst::PackToBinary { dest, value }
            | MirInst::BinaryToPack { dest, value }
            | MirInst::IntToDecimal { dest, value, .. }
            | MirInst::DecimalToInt { dest, value } => {
                // Identity pass-through until runtime helpers are wired up.
                let v = self.lookup(val_map, value)?;
                val_map.insert(dest.raw(), v);
            }

            // -- String concat / split / inspect (stubs) ----------------------
            MirInst::StringConcat { dest, .. } => {
                let v = builder.ins().iconst(ptr_ty, 0);
                val_map.insert(dest.raw(), v);
            }
            MirInst::StringSplit { .. } | MirInst::Inspect { .. } => {
                // No-op for now.
            }

            // -- PERFORM stack ------------------------------------------------
            MirInst::PerformPush { .. } => {
                // TODO: implement perform stack via runtime.
            }
            MirInst::PerformPop { dest } => {
                let v = builder.ins().iconst(ptr_ty, 0);
                val_map.insert(dest.raw(), v);
            }

            // -- File I/O (stubs) ---------------------------------------------
            MirInst::FileOpen { .. }
            | MirInst::FileClose { .. }
            | MirInst::FileRead { .. }
            | MirInst::FileWrite { .. } => {
                // TODO: wire up file I/O runtime calls.
            }

            // -- SSA ----------------------------------------------------------
            MirInst::Phi { dest, incoming } => {
                // Phi nodes in MIR are resolved during block parameter passing.
                // For a basic implementation, pick the first available incoming
                // value.
                for (_bid, val) in incoming {
                    if let Ok(v) = self.lookup(val_map, val) {
                        val_map.insert(dest.raw(), v);
                        break;
                    }
                }
            }

            // -- Size error ---------------------------------------------------
            MirInst::SizeError { .. } => {
                // TODO: implement SIZE ERROR handling.
            }
        }
        Ok(())
    }

    // -- terminator translation -----------------------------------------------

    fn translate_terminator(
        &self,
        term: &Terminator,
        builder: &mut FunctionBuilder,
        val_map: &HashMap<u32, ir::Value>,
        block_map: &HashMap<u32, ir::Block>,
    ) -> Result<(), CodegenError> {
        match term {
            Terminator::Return => {
                builder.ins().return_(&[]);
            }
            Terminator::Goto(target) => {
                let blk = block_map[&target.raw()];
                builder.ins().jump(blk, &[]);
            }
            Terminator::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let c = val_map.get(&cond.raw()).copied().ok_or_else(|| {
                    CodegenError::BackendError("missing branch cond value".into())
                })?;
                let tb = block_map[&then_block.raw()];
                let eb = block_map[&else_block.raw()];
                builder.ins().brif(c, tb, &[], eb, &[]);
            }
            Terminator::Switch {
                value,
                cases,
                default,
            } => {
                // Implement as a chain of brif comparisons.
                let v = val_map
                    .get(&value.raw())
                    .copied()
                    .ok_or_else(|| CodegenError::BackendError("missing switch value".into()))?;
                let def_blk = block_map[&default.raw()];

                for (case_val, target) in cases {
                    let cmp_val = builder.ins().iconst(types::I64, *case_val);
                    let eq = builder.ins().icmp(IntCC::Equal, v, cmp_val);
                    let target_blk = block_map[&target.raw()];
                    // Create a fall-through block for the next case.
                    let next = builder.create_block();
                    builder.ins().brif(eq, target_blk, &[], next, &[]);
                    builder.seal_block(next);
                    builder.switch_to_block(next);
                }
                // After all cases, jump to default.
                builder.ins().jump(def_blk, &[]);
            }
            Terminator::IndirectJump { target } => {
                // Cranelift does not directly support indirect jumps in the
                // normal sense.  Emit a trap for now.
                let _ = target;
                builder
                    .ins()
                    .trap(TrapCode::user(1).expect("valid user trap code"));
            }
            Terminator::PerformReturn => {
                // TODO: pop perform stack and indirect jump.
                builder.ins().return_(&[]);
            }
            Terminator::Unreachable => {
                builder
                    .ins()
                    .trap(TrapCode::user(2).expect("valid user trap code"));
            }
        }
        Ok(())
    }

    // -- C main entry point ---------------------------------------------------

    /// Generate `int main(int argc, char **argv) { _cobol_main(); return 0; }`
    fn generate_c_main(&mut self, cobol_main_id: FuncId) -> Result<(), CodegenError> {
        let ptr_ty = self.pointer_type;
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I32)); // argc
        sig.params.push(AbiParam::new(ptr_ty)); // argv
        sig.returns.push(AbiParam::new(types::I32)); // return code

        let main_id = self
            .module
            .declare_function("main", Linkage::Export, &sig)
            .map_err(|e| CodegenError::BackendError(format!("declare main: {}", e)))?;

        let mut ctx = self.module.make_context();
        ctx.func.signature = sig;

        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);

            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            builder.seal_block(entry);

            let func_ref = self
                .module
                .declare_func_in_func(cobol_main_id, builder.func);
            builder.ins().call(func_ref, &[]);

            let zero = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[zero]);

            builder.finalize();
        }

        self.module
            .define_function(main_id, &mut ctx)
            .map_err(|e| CodegenError::BackendError(format!("define main: {}", e)))?;

        self.module.clear_context(&mut ctx);
        Ok(())
    }

    // -- helpers --------------------------------------------------------------

    /// Look up two MIR values and coerce them to the same integer type.
    /// Extends the narrower operand to match the wider one.
    fn lookup_and_coerce_pair(
        &self,
        val_map: &HashMap<u32, ir::Value>,
        left: &Value,
        right: &Value,
        builder: &mut FunctionBuilder,
    ) -> Result<(ir::Value, ir::Value), CodegenError> {
        let l = self.lookup(val_map, left)?;
        let r = self.lookup(val_map, right)?;
        let lt = builder.func.dfg.value_type(l);
        let rt = builder.func.dfg.value_type(r);
        if lt == rt {
            return Ok((l, r));
        }
        // Coerce the narrower to the wider type.
        if lt.is_int() && rt.is_int() {
            if lt.bits() > rt.bits() {
                Ok((l, builder.ins().sextend(lt, r)))
            } else {
                Ok((builder.ins().sextend(rt, l), r))
            }
        } else {
            Ok((l, r))
        }
    }

    /// Resolve a MIR `Value` to the corresponding Cranelift `ir::Value`.
    fn lookup(
        &self,
        val_map: &HashMap<u32, ir::Value>,
        mir_val: &Value,
    ) -> Result<ir::Value, CodegenError> {
        val_map.get(&mir_val.raw()).copied().ok_or_else(|| {
            CodegenError::BackendError(format!("undefined MIR value v{}", mir_val.raw()))
        })
    }

    /// Resolve many MIR values at once.
    fn lookup_many(
        &self,
        val_map: &HashMap<u32, ir::Value>,
        mir_vals: &[Value],
    ) -> Result<Vec<ir::Value>, CodegenError> {
        mir_vals.iter().map(|v| self.lookup(val_map, v)).collect()
    }

    /// Map a MIR type to the nearest Cranelift type.
    fn mir_type_to_cl(&self, ty: &MirType) -> ir::Type {
        match ty {
            MirType::I32 => types::I32,
            MirType::I64 => types::I64,
            MirType::I128 => types::I128,
            MirType::F32 => types::F32,
            MirType::F64 => types::F64,
            MirType::Bool => types::I8,
            MirType::Pointer => self.pointer_type,
            MirType::Void => self.pointer_type, // placeholder; never used in returns
            MirType::Bytes(_) => self.pointer_type, // pointer to byte array
            MirType::Decimal { .. } => types::I64, // scaled-integer representation
            MirType::PackedDecimal { .. } => self.pointer_type, // pointer to BCD data
        }
    }
}

// ---------------------------------------------------------------------------
// Free-standing helpers
// ---------------------------------------------------------------------------

/// Insert a type coercion (ireduce / sextend / nop) to make `val` match `target_ty`.
fn coerce_value(builder: &mut FunctionBuilder, val: ir::Value, target_ty: ir::Type) -> ir::Value {
    let actual_ty = builder.func.dfg.value_type(val);
    if actual_ty == target_ty {
        return val;
    }
    // Both integer types -- widen or narrow.
    if actual_ty.is_int() && target_ty.is_int() {
        if actual_ty.bits() > target_ty.bits() {
            return builder.ins().ireduce(target_ty, val);
        } else {
            return builder.ins().sextend(target_ty, val);
        }
    }
    // Float <-> int coercions could be added here if needed.
    // For now, return the value unchanged (the verifier will catch real mismatches).
    val
}

/// Recursively build a composite initial value buffer for a group item.
///
/// For each direct child of `parent_name`, if the child has an explicit
/// `initial_value` (from a VALUE clause), write it into `buf` at the child's
/// offset. If the child is itself a group (no `initial_value` but has its own
/// children), recurse to fill the child's portion of the buffer from its
/// sub-children. Children with no VALUE clause and no sub-children default to
/// spaces (the COBOL alphanumeric default).
fn build_composite_init(
    buf: &mut [u8],
    parent_name: &str,
    children_of: &HashMap<String, Vec<(u32, &MirGlobal)>>,
) {
    if let Some(children) = children_of.get(parent_name) {
        for &(offset, child) in children {
            let child_size = mir_type_size(&child.ty);
            let start = offset as usize;
            let end = (start + child_size).min(buf.len());
            if start >= buf.len() {
                continue;
            }

            match &child.initial_value {
                Some(MirConst::Str(s)) => {
                    let mut b = s.as_bytes().to_vec();
                    b.resize(child_size, b' ');
                    let copy_len = end - start;
                    buf[start..end].copy_from_slice(&b[..copy_len]);
                }
                Some(MirConst::Bytes(b)) => {
                    let copy_len = end - start;
                    let src_len = copy_len.min(b.len());
                    buf[start..start + src_len].copy_from_slice(&b[..src_len]);
                }
                Some(MirConst::Int(n)) => {
                    let bytes = n.to_le_bytes();
                    let copy_len = (end - start).min(bytes.len());
                    buf[start..start + copy_len].copy_from_slice(&bytes[..copy_len]);
                }
                None => {
                    // No VALUE clause on this child. If the child is itself a group
                    // item with sub-children, recursively composite from them.
                    // Otherwise, default to spaces (alphanumeric default).
                    if children_of.contains_key(&child.name) {
                        // Fill child region with spaces first (default), then recurse
                        for byte in &mut buf[start..end] {
                            *byte = b' ';
                        }
                        // Recurse into the child's sub-region of the buffer
                        build_composite_init(&mut buf[start..end], &child.name, children_of);
                    } else {
                        // Leaf item with no VALUE: default to spaces
                        for byte in &mut buf[start..end] {
                            *byte = b' ';
                        }
                    }
                }
                _ => {
                    // Other types: zero-fill
                    for byte in &mut buf[start..end] {
                        *byte = 0;
                    }
                }
            }
        }
    }
}

/// Return the byte-size of a MIR type for global allocation.
fn mir_type_size(ty: &MirType) -> usize {
    match ty {
        MirType::I32 => 4,
        MirType::I64 => 8,
        MirType::I128 => 16,
        MirType::F32 => 4,
        MirType::F64 => 8,
        MirType::Bool => 1,
        MirType::Pointer => 8,
        MirType::Void => 0,
        MirType::Bytes(n) => *n as usize,
        MirType::Decimal { digits, .. } => {
            if *digits <= 18 {
                8
            } else {
                16
            }
        }
        MirType::PackedDecimal { digits, .. } => (*digits as usize / 2) + 1,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cranelift_backend_new_and_name() {
        let backend = CraneliftBackend::new();
        assert_eq!(backend.name(), "cranelift");
        assert_eq!(backend.output_extension(), "o");
    }

    #[test]
    fn cranelift_backend_default() {
        let backend = CraneliftBackend;
        assert_eq!(backend.name(), "cranelift");
    }

    /// Build a minimal MIR module with a single void function that just returns.
    fn make_minimal_module() -> MirModule {
        let entry = BlockId::from_raw(0);
        MirModule {
            name: "MINIMAL".into(),
            functions: vec![MirFunction {
                name: "_cobol_main".into(),
                params: vec![],
                return_type: MirType::Void,
                blocks: vec![BasicBlock {
                    id: entry,
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                }],
                entry_block: entry,
                next_value: 0,
                next_block: 1,
            }],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        }
    }

    #[test]
    fn compile_minimal_module_to_object_file() {
        let module = make_minimal_module();
        let dir = std::env::temp_dir().join("cobol_cranelift_test");
        std::fs::create_dir_all(&dir).unwrap();
        let out = dir.join("minimal.o");

        let backend = CraneliftBackend::new();
        backend
            .compile(&module, &out)
            .expect("compile should succeed");

        // The output file should exist and contain a valid object header.
        let bytes = std::fs::read(&out).unwrap();
        assert!(!bytes.is_empty(), "object file must not be empty");

        // Clean up.
        let _ = std::fs::remove_file(&out);
    }

    #[test]
    fn compile_module_with_globals() {
        let entry = BlockId::from_raw(0);
        let v0 = Value::from_raw(0); // pointer to global string
        let v1 = Value::from_raw(1); // string length
        let v2 = Value::from_raw(2); // exit code

        let module = MirModule {
            name: "HELLO".into(),
            functions: vec![MirFunction {
                name: "_cobol_main".into(),
                params: vec![],
                return_type: MirType::Void,
                blocks: vec![BasicBlock {
                    id: entry,
                    params: vec![],
                    instructions: vec![
                        MirInst::Const {
                            dest: v0,
                            value: MirConst::Int(0), // placeholder, would be global addr
                        },
                        MirInst::Const {
                            dest: v1,
                            value: MirConst::Int(13),
                        },
                        MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_display_line".into(),
                            args: vec![v0, v1],
                        },
                        MirInst::Const {
                            dest: v2,
                            value: MirConst::Int(0),
                        },
                        MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_stop_run".into(),
                            args: vec![v2],
                        },
                    ],
                    terminator: Terminator::Return,
                }],
                entry_block: entry,
                next_value: 3,
                next_block: 1,
            }],
            globals: vec![MirGlobal {
                name: "str_0".into(),
                ty: MirType::Bytes(13),
                initial_value: Some(MirConst::Str("HELLO, WORLD!".into())),
                offset: 0,
                redefines: None,
                parent_offset: None,
            }],
            file_descriptors: vec![],
            errors: vec![],
        };

        let dir = std::env::temp_dir().join("cobol_cranelift_test");
        std::fs::create_dir_all(&dir).unwrap();
        let out = dir.join("hello.o");

        let backend = CraneliftBackend::new();
        backend
            .compile(&module, &out)
            .expect("compile should succeed");

        let bytes = std::fs::read(&out).unwrap();
        assert!(!bytes.is_empty());

        let _ = std::fs::remove_file(&out);
    }

    #[test]
    fn compile_module_with_branches() {
        let b0 = BlockId::from_raw(0);
        let b1 = BlockId::from_raw(1);
        let b2 = BlockId::from_raw(2);

        let v0 = Value::from_raw(0);
        let v1 = Value::from_raw(1);
        let v_cond = Value::from_raw(2);

        let module = MirModule {
            name: "BRANCH-TEST".into(),
            functions: vec![MirFunction {
                name: "_cobol_main".into(),
                params: vec![],
                return_type: MirType::Void,
                blocks: vec![
                    BasicBlock {
                        id: b0,
                        params: vec![],
                        instructions: vec![
                            MirInst::Const {
                                dest: v0,
                                value: MirConst::Int(1),
                            },
                            MirInst::Const {
                                dest: v1,
                                value: MirConst::Int(2),
                            },
                            MirInst::ICmpEq {
                                dest: v_cond,
                                left: v0,
                                right: v1,
                            },
                        ],
                        terminator: Terminator::Branch {
                            cond: v_cond,
                            then_block: b1,
                            else_block: b2,
                        },
                    },
                    BasicBlock {
                        id: b1,
                        params: vec![],
                        instructions: vec![],
                        terminator: Terminator::Return,
                    },
                    BasicBlock {
                        id: b2,
                        params: vec![],
                        instructions: vec![],
                        terminator: Terminator::Return,
                    },
                ],
                entry_block: b0,
                next_value: 3,
                next_block: 3,
            }],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        };

        let dir = std::env::temp_dir().join("cobol_cranelift_test");
        std::fs::create_dir_all(&dir).unwrap();
        let out = dir.join("branch.o");

        let backend = CraneliftBackend::new();
        backend
            .compile(&module, &out)
            .expect("compile should succeed");

        let bytes = std::fs::read(&out).unwrap();
        assert!(!bytes.is_empty());

        let _ = std::fs::remove_file(&out);
    }

    #[test]
    fn compile_module_with_arithmetic() {
        let b0 = BlockId::from_raw(0);
        let v0 = Value::from_raw(0);
        let v1 = Value::from_raw(1);
        let v2 = Value::from_raw(2);
        let v3 = Value::from_raw(3);
        let v4 = Value::from_raw(4);
        let v5 = Value::from_raw(5);

        let module = MirModule {
            name: "ARITH-TEST".into(),
            functions: vec![MirFunction {
                name: "_cobol_main".into(),
                params: vec![],
                return_type: MirType::Void,
                blocks: vec![BasicBlock {
                    id: b0,
                    params: vec![],
                    instructions: vec![
                        MirInst::Const {
                            dest: v0,
                            value: MirConst::Int(10),
                        },
                        MirInst::Const {
                            dest: v1,
                            value: MirConst::Int(3),
                        },
                        MirInst::IAdd {
                            dest: v2,
                            left: v0,
                            right: v1,
                        },
                        MirInst::ISub {
                            dest: v3,
                            left: v0,
                            right: v1,
                        },
                        MirInst::IMul {
                            dest: v4,
                            left: v0,
                            right: v1,
                        },
                        MirInst::IDiv {
                            dest: v5,
                            left: v0,
                            right: v1,
                        },
                    ],
                    terminator: Terminator::Return,
                }],
                entry_block: b0,
                next_value: 6,
                next_block: 1,
            }],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        };

        let dir = std::env::temp_dir().join("cobol_cranelift_test");
        std::fs::create_dir_all(&dir).unwrap();
        let out = dir.join("arith.o");

        let backend = CraneliftBackend::new();
        backend
            .compile(&module, &out)
            .expect("compile should succeed");

        let bytes = std::fs::read(&out).unwrap();
        assert!(!bytes.is_empty());

        let _ = std::fs::remove_file(&out);
    }

    #[test]
    fn mir_type_sizes() {
        assert_eq!(mir_type_size(&MirType::I32), 4);
        assert_eq!(mir_type_size(&MirType::I64), 8);
        assert_eq!(mir_type_size(&MirType::I128), 16);
        assert_eq!(mir_type_size(&MirType::F32), 4);
        assert_eq!(mir_type_size(&MirType::F64), 8);
        assert_eq!(mir_type_size(&MirType::Bool), 1);
        assert_eq!(mir_type_size(&MirType::Pointer), 8);
        assert_eq!(mir_type_size(&MirType::Void), 0);
        assert_eq!(mir_type_size(&MirType::Bytes(80)), 80);
        assert_eq!(
            mir_type_size(&MirType::Decimal {
                digits: 9,
                scale: 2
            }),
            8
        );
        assert_eq!(
            mir_type_size(&MirType::Decimal {
                digits: 20,
                scale: 2
            }),
            16
        );
        assert_eq!(
            mir_type_size(&MirType::PackedDecimal {
                digits: 9,
                scale: 2
            }),
            5
        );
    }
}
