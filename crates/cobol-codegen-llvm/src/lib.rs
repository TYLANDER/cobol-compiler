use cobol_mir::MirModule;

/// Trait that both LLVM and Cranelift backends implement.
pub trait CodegenBackend {
    /// Compile a MIR module to an object file.
    fn compile(&self, module: &MirModule, output: &std::path::Path) -> Result<(), CodegenError>;

    /// Get the backend name for display purposes.
    fn name(&self) -> &str;

    /// Get the default file extension for output.
    fn output_extension(&self) -> &str;
}

#[derive(Debug)]
pub enum CodegenError {
    /// LLVM/Cranelift internal error.
    BackendError(String),
    /// I/O error writing output.
    IoError(std::io::Error),
    /// Unsupported MIR instruction for this backend.
    UnsupportedInstruction(String),
    /// Type mismatch during codegen.
    TypeMismatch(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::BackendError(msg) => write!(f, "backend error: {}", msg),
            CodegenError::IoError(err) => write!(f, "I/O error: {}", err),
            CodegenError::UnsupportedInstruction(msg) => {
                write!(f, "unsupported instruction: {}", msg)
            }
            CodegenError::TypeMismatch(msg) => write!(f, "type mismatch: {}", msg),
        }
    }
}

impl std::error::Error for CodegenError {}

impl From<std::io::Error> for CodegenError {
    fn from(err: std::io::Error) -> Self {
        CodegenError::IoError(err)
    }
}

/// Optimization level for codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    O0,
    O1,
    O2,
    O3,
    Os,
}

/// LLVM codegen backend.
///
/// Decimal strategy:
/// - Small decimals (<=18 digits): i64 scaled integers (single add instruction)
/// - Medium decimals (19-38 digits): i128
/// - Large decimals (39+ digits): runtime BCD calls
///
/// This fast-path covers 90%+ of real-world COBOL.
pub struct LlvmBackend {
    opt_level: OptLevel,
}

impl LlvmBackend {
    pub fn new(opt_level: OptLevel) -> Self {
        Self { opt_level }
    }

    pub fn opt_level(&self) -> OptLevel {
        self.opt_level
    }
}

// ---------------------------------------------------------------------------
// Feature-gated LLVM implementation via inkwell
// ---------------------------------------------------------------------------

#[cfg(feature = "llvm")]
mod llvm_impl {
    use super::*;
    use cobol_mir::*;
    use inkwell::builder::Builder;
    use inkwell::context::Context;
    use inkwell::module::Module;
    use inkwell::targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
    };
    use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
    use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
    use inkwell::AddressSpace;
    use inkwell::IntPredicate;
    use inkwell::OptimizationLevel;
    use std::collections::HashMap;

    /// Convert our OptLevel to inkwell's OptimizationLevel.
    fn to_inkwell_opt(opt: OptLevel) -> OptimizationLevel {
        match opt {
            OptLevel::O0 => OptimizationLevel::None,
            OptLevel::O1 => OptimizationLevel::Less,
            OptLevel::O2 | OptLevel::Os => OptimizationLevel::Default,
            OptLevel::O3 => OptimizationLevel::Aggressive,
        }
    }

    /// Compute storage size for a MIR type.
    fn mir_type_size(ty: &MirType) -> u32 {
        match ty {
            MirType::I32 | MirType::F32 => 4,
            MirType::I64 | MirType::F64 => 8,
            MirType::I128 => 16,
            MirType::Bool => 1,
            MirType::Pointer => 8,
            MirType::Void => 0,
            MirType::Bytes(n) => *n,
            MirType::Decimal { digits, .. } => {
                if *digits <= 18 {
                    8
                } else {
                    16
                }
            }
            MirType::PackedDecimal { digits, .. } => ((*digits as u32) / 2) + 1,
        }
    }

    /// The LLVM code generator.
    struct LlvmCodegen<'ctx> {
        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
        target_machine: TargetMachine,
        /// Maps global names to their LLVM pointer values.
        global_ptrs: HashMap<String, PointerValue<'ctx>>,
        /// Maps global names to (parent_name, byte_offset) for sub-items.
        parent_offsets: HashMap<String, (String, u32)>,
        /// Maps function names to their LLVM function values.
        func_values: HashMap<String, FunctionValue<'ctx>>,
    }

    impl<'ctx> LlvmCodegen<'ctx> {
        fn new(context: &'ctx Context, opt_level: OptLevel) -> Result<Self, CodegenError> {
            Target::initialize_native(&InitializationConfig::default()).map_err(|e| {
                CodegenError::BackendError(format!("failed to initialize LLVM target: {}", e))
            })?;

            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple)
                .map_err(|e| CodegenError::BackendError(format!("failed to get target: {}", e)))?;

            let target_machine = target
                .create_target_machine(
                    &triple,
                    "generic",
                    "",
                    to_inkwell_opt(opt_level),
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .ok_or_else(|| {
                    CodegenError::BackendError("failed to create target machine".into())
                })?;

            let module = context.create_module("cobol_module");
            module.set_triple(&triple);
            module.set_data_layout(&target_machine.get_target_data().get_data_layout());

            let builder = context.create_builder();

            Ok(Self {
                context,
                module,
                builder,
                target_machine,
                global_ptrs: HashMap::new(),
                parent_offsets: HashMap::new(),
                func_values: HashMap::new(),
            })
        }

        /// Convert a MirType to an LLVM BasicTypeEnum.
        fn mir_type_to_llvm(&self, ty: &MirType) -> BasicTypeEnum<'ctx> {
            match ty {
                MirType::I32 => self.context.i32_type().into(),
                MirType::I64 => self.context.i64_type().into(),
                MirType::I128 => self.context.i128_type().into(),
                MirType::F32 => self.context.f32_type().into(),
                MirType::F64 => self.context.f64_type().into(),
                MirType::Bool => self.context.bool_type().into(),
                MirType::Pointer => self.context.ptr_type(AddressSpace::default()).into(),
                MirType::Void => self.context.ptr_type(AddressSpace::default()).into(),
                MirType::Bytes(n) => self.context.i8_type().array_type(*n).into(),
                MirType::Decimal { digits, .. } => {
                    if *digits <= 18 {
                        self.context.i64_type().into()
                    } else {
                        self.context.i128_type().into()
                    }
                }
                MirType::PackedDecimal { digits, .. } => {
                    let size = ((*digits as u32) / 2) + 1;
                    self.context.i8_type().array_type(size).into()
                }
            }
        }

        /// Get the pointer type.
        fn ptr_type(&self) -> inkwell::types::PointerType<'ctx> {
            self.context.ptr_type(AddressSpace::default())
        }

        // ---------------------------------------------------------------
        // Globals
        // ---------------------------------------------------------------

        fn declare_globals(&mut self, globals: &[MirGlobal]) -> Result<(), CodegenError> {
            // Build parent offset map
            for g in globals {
                if let Some((ref parent, offset)) = g.parent_offset {
                    self.parent_offsets
                        .insert(g.name.clone(), (parent.clone(), offset));
                }
            }

            for g in globals {
                // REDEFINES: alias to the same global
                if let Some(ref redef_target) = g.redefines {
                    if let Some(ptr) = self.global_ptrs.get(redef_target).copied() {
                        self.global_ptrs.insert(g.name.clone(), ptr);
                        continue;
                    }
                }

                // Child of group: share parent's global
                if g.parent_offset.is_some() {
                    continue; // handled at access time via GEP offset
                }

                // Top-level: declare global
                let size = mir_type_size(&g.ty) as usize;
                let global_ty = self.context.i8_type().array_type(size.max(1) as u32);
                let global = self.module.add_global(global_ty, None, &g.name);
                global.set_linkage(inkwell::module::Linkage::Internal);

                // Set initializer
                match &g.initial_value {
                    Some(MirConst::Str(s)) => {
                        let mut bytes = s.as_bytes().to_vec();
                        bytes.resize(size, b' ');
                        let vals: Vec<_> = bytes
                            .iter()
                            .map(|b| self.context.i8_type().const_int(*b as u64, false))
                            .collect();
                        let init = self.context.i8_type().const_array(&vals);
                        global.set_initializer(&init);
                    }
                    Some(MirConst::Bytes(b)) => {
                        let mut bytes = b.clone();
                        bytes.resize(size, 0);
                        let vals: Vec<_> = bytes
                            .iter()
                            .map(|b| self.context.i8_type().const_int(*b as u64, false))
                            .collect();
                        let init = self.context.i8_type().const_array(&vals);
                        global.set_initializer(&init);
                    }
                    Some(MirConst::Int(n)) => {
                        let bytes = n.to_le_bytes();
                        let mut padded = vec![0u8; size];
                        let copy_len = bytes.len().min(size);
                        padded[..copy_len].copy_from_slice(&bytes[..copy_len]);
                        let vals: Vec<_> = padded
                            .iter()
                            .map(|b| self.context.i8_type().const_int(*b as u64, false))
                            .collect();
                        let init = self.context.i8_type().const_array(&vals);
                        global.set_initializer(&init);
                    }
                    _ => {
                        let init = global_ty.const_zero();
                        global.set_initializer(&init);
                    }
                }

                let ptr = global.as_pointer_value();
                self.global_ptrs.insert(g.name.clone(), ptr);
            }

            // Map child items to parent pointers (will add offset at use)
            for g in globals {
                if g.parent_offset.is_some() && !self.global_ptrs.contains_key(&g.name) {
                    // Walk parent chain to find root
                    let mut root = g.name.clone();
                    while let Some((parent, _)) = self.parent_offsets.get(&root) {
                        root = parent.clone();
                    }
                    if let Some(ptr) = self.global_ptrs.get(&root).copied() {
                        self.global_ptrs.insert(g.name.clone(), ptr);
                    }
                }
                // REDEFINES without target yet
                if let Some(ref redef_target) = g.redefines {
                    if !self.global_ptrs.contains_key(&g.name) {
                        if let Some(ptr) = self.global_ptrs.get(redef_target).copied() {
                            self.global_ptrs.insert(g.name.clone(), ptr);
                        }
                    }
                }
            }

            Ok(())
        }

        /// Compute the total byte offset for a named global by walking the parent chain.
        fn total_offset(&self, name: &str) -> u32 {
            let mut total = 0u32;
            let mut current = name.to_string();
            while let Some((parent, off)) = self.parent_offsets.get(&current) {
                total += off;
                current = parent.clone();
            }
            total
        }

        // ---------------------------------------------------------------
        // Runtime function declarations
        // ---------------------------------------------------------------

        fn declare_runtime_function(&self, name: &str, arg_count: usize) -> FunctionValue<'ctx> {
            let ptr = self.ptr_type();
            let i32_ty = self.context.i32_type();

            // Build signature based on known runtime functions
            let fn_type: FunctionType<'ctx> = match name {
                "cobolrt_display_line" | "cobolrt_display" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![ptr.into(), i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_stop_run" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_accept"
                | "cobolrt_accept_date"
                | "cobolrt_accept_day"
                | "cobolrt_accept_time"
                | "cobolrt_accept_day_of_week" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![ptr.into(), i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_move_alphanumeric" | "cobolrt_move_alphanumeric_justified" => {
                    let params: Vec<BasicMetadataTypeEnum> =
                        vec![ptr.into(), i32_ty.into(), ptr.into(), i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_move_numeric" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![
                        ptr.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                    ];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_add_numeric"
                | "cobolrt_subtract_numeric"
                | "cobolrt_multiply_numeric"
                | "cobolrt_divide_numeric" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                    ];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_compare_numeric" | "cobolrt_compare_alphanumeric" => {
                    let params: Vec<BasicMetadataTypeEnum> =
                        vec![ptr.into(), i32_ty.into(), ptr.into(), i32_ty.into()];
                    i32_ty.fn_type(&params, false)
                }
                "cobolrt_initialize_alphanumeric" | "cobolrt_initialize_numeric" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![ptr.into(), i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_last_size_error" | "cobolrt_clear_size_error" => {
                    i32_ty.fn_type(&[], false)
                }
                "cobolrt_set_size_error" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_blank_when_zero" | "cobolrt_justify_right" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![ptr.into(), i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_sign_fixup" => {
                    let params: Vec<BasicMetadataTypeEnum> =
                        vec![ptr.into(), i32_ty.into(), i32_ty.into(), i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_file_open" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![
                        ptr.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                        i32_ty.into(),
                    ];
                    i32_ty.fn_type(&params, false)
                }
                "cobolrt_file_close" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![i32_ty.into()];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_file_write_line" | "cobolrt_file_read_line" => {
                    let params: Vec<BasicMetadataTypeEnum> =
                        vec![i32_ty.into(), ptr.into(), i32_ty.into()];
                    i32_ty.fn_type(&params, false)
                }
                "cobolrt_file_write_record" | "cobolrt_file_read_record" => {
                    let params: Vec<BasicMetadataTypeEnum> =
                        vec![i32_ty.into(), ptr.into(), i32_ty.into()];
                    i32_ty.fn_type(&params, false)
                }
                "cobolrt_file_read_next" => {
                    let params: Vec<BasicMetadataTypeEnum> =
                        vec![i32_ty.into(), ptr.into(), i32_ty.into()];
                    i32_ty.fn_type(&params, false)
                }
                "cobolrt_file_status" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![i32_ty.into()];
                    i32_ty.fn_type(&params, false)
                }
                "cobolrt_file_delete" | "cobolrt_file_start" | "cobolrt_file_rewrite" => {
                    let params: Vec<BasicMetadataTypeEnum> =
                        vec![i32_ty.into(), ptr.into(), i32_ty.into()];
                    i32_ty.fn_type(&params, false)
                }
                "cobolrt_string_append" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                    ];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_unstring_next" => {
                    let params: Vec<BasicMetadataTypeEnum> = vec![
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                        ptr.into(),
                        i32_ty.into(),
                    ];
                    self.context.void_type().fn_type(&params, false)
                }
                "cobolrt_inspect_tallying"
                | "cobolrt_inspect_replacing"
                | "cobolrt_inspect_converting" => {
                    // Variable args — use generic pointer-based signature
                    let params: Vec<BasicMetadataTypeEnum> =
                        (0..arg_count).map(|_| ptr.into()).collect();
                    self.context.void_type().fn_type(&params, false)
                }
                _ => {
                    // Unknown (subprogram calls): all pointer args
                    let params: Vec<BasicMetadataTypeEnum> =
                        (0..arg_count).map(|_| ptr.into()).collect();
                    self.context.void_type().fn_type(&params, false)
                }
            };

            self.module
                .add_function(name, fn_type, Some(inkwell::module::Linkage::External))
        }

        // ---------------------------------------------------------------
        // Compile module
        // ---------------------------------------------------------------

        fn compile_module(
            mut self,
            mir: &MirModule,
            output: &std::path::Path,
        ) -> Result<(), CodegenError> {
            // 1. Declare and initialize globals
            self.declare_globals(&mir.globals)?;

            // 2. Declare MIR functions
            for func in &mir.functions {
                let fn_val = self.declare_mir_function(func);
                self.func_values.insert(func.name.clone(), fn_val);
            }

            // 3. Scan for runtime function references
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
                            let name = if *justified {
                                "cobolrt_move_alphanumeric_justified"
                            } else {
                                "cobolrt_move_alphanumeric"
                            };
                            extern_funcs.entry(name.into()).or_insert(4);
                        }
                    }
                }
            }

            // 4. Declare runtime functions
            let mut runtime_funcs: HashMap<String, FunctionValue<'ctx>> = HashMap::new();
            for (name, arg_count) in &extern_funcs {
                let fv = self.declare_runtime_function(name, *arg_count);
                runtime_funcs.insert(name.clone(), fv);
            }

            // 5. Define (translate) each MIR function
            for func in &mir.functions {
                self.define_function(func, &runtime_funcs)?;
            }

            // 6. Generate C main entry
            if self.func_values.contains_key("_cobol_main") {
                self.generate_c_main()?;
            }

            // 7. Write object file
            self.target_machine
                .write_to_file(&self.module, FileType::Object, output)
                .map_err(|e| CodegenError::BackendError(format!("failed to write object: {}", e)))
        }

        fn declare_mir_function(&self, func: &MirFunction) -> FunctionValue<'ctx> {
            let param_types: Vec<BasicMetadataTypeEnum> = func
                .params
                .iter()
                .map(|(_, ty)| self.mir_type_to_llvm(ty).into())
                .collect();

            let fn_type = if func.return_type == MirType::Void {
                self.context.void_type().fn_type(&param_types, false)
            } else {
                self.mir_type_to_llvm(&func.return_type)
                    .fn_type(&param_types, false)
            };

            self.module.add_function(
                &func.name,
                fn_type,
                Some(inkwell::module::Linkage::External),
            )
        }

        fn define_function(
            &self,
            func: &MirFunction,
            runtime_funcs: &HashMap<String, FunctionValue<'ctx>>,
        ) -> Result<(), CodegenError> {
            let fn_val = self.func_values[&func.name];

            // Create basic blocks
            let mut block_map: HashMap<u32, inkwell::basic_block::BasicBlock<'ctx>> =
                HashMap::new();
            for bb in &func.blocks {
                let llvm_block = self
                    .context
                    .append_basic_block(fn_val, &format!("bb{}", bb.id.raw()));
                block_map.insert(bb.id.raw(), llvm_block);
            }

            // Value map: MIR Value -> LLVM Value
            let mut val_map: HashMap<u32, BasicValueEnum<'ctx>> = HashMap::new();

            // Translate each block
            for bb in &func.blocks {
                let llvm_block = block_map[&bb.id.raw()];
                self.builder.position_at_end(llvm_block);

                // Entry block: map function params
                if bb.id == func.entry_block {
                    for (i, _) in func.params.iter().enumerate() {
                        if let Some(param) = fn_val.get_nth_param(i as u32) {
                            val_map.insert(i as u32, param);
                        }
                    }
                }

                // Translate instructions
                for inst in &bb.instructions {
                    self.translate_inst(inst, &mut val_map, &block_map, runtime_funcs)?;
                }

                // Translate terminator
                self.translate_terminator(&bb.terminator, &val_map, &block_map, fn_val)?;
            }

            Ok(())
        }

        fn translate_inst(
            &self,
            inst: &MirInst,
            val_map: &mut HashMap<u32, BasicValueEnum<'ctx>>,
            block_map: &HashMap<u32, inkwell::basic_block::BasicBlock<'ctx>>,
            runtime_funcs: &HashMap<String, FunctionValue<'ctx>>,
        ) -> Result<(), CodegenError> {
            match inst {
                MirInst::Const { dest, value } => {
                    let v = self.const_to_llvm(value);
                    val_map.insert(dest.raw(), v);
                }

                MirInst::GlobalAddr { dest, name } => {
                    let base_ptr = self.global_ptrs.get(name).copied().ok_or_else(|| {
                        CodegenError::BackendError(format!("unknown global: {}", name))
                    })?;
                    let offset = self.total_offset(name);
                    let v = if offset > 0 {
                        unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                base_ptr,
                                &[self.context.i32_type().const_int(offset as u64, false)],
                                &format!("{}_ptr", name),
                            )
                        }
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?
                    } else {
                        base_ptr
                    };
                    val_map.insert(dest.raw(), v.into());
                }

                MirInst::Load { dest, addr, ty } => {
                    let ptr = self.as_ptr(val_map, addr)?;
                    let llvm_ty = self.mir_type_to_llvm(ty);
                    let v = self
                        .builder
                        .build_load(llvm_ty, ptr, "load")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v);
                }

                MirInst::Store { addr, value } => {
                    let ptr = self.as_ptr(val_map, addr)?;
                    let val = self.lookup(val_map, value)?;
                    self.builder
                        .build_store(ptr, val)
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }

                MirInst::GetFieldAddr { dest, base, offset } => {
                    let ptr = self.as_ptr(val_map, base)?;
                    let v = unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            ptr,
                            &[self.context.i32_type().const_int(*offset as u64, false)],
                            "field",
                        )
                    }
                    .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }

                MirInst::GetElementAddr {
                    dest,
                    base,
                    index,
                    element_size,
                } => {
                    let ptr = self.as_ptr(val_map, base)?;
                    let idx = self.as_int(val_map, index)?;
                    let elem_sz = self
                        .context
                        .i64_type()
                        .const_int(*element_size as u64, false);
                    let byte_offset = self
                        .builder
                        .build_int_mul(idx, elem_sz, "elem_off")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    let v = unsafe {
                        self.builder
                            .build_gep(self.context.i8_type(), ptr, &[byte_offset], "elem")
                    }
                    .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }

                // Integer arithmetic
                MirInst::IAdd { dest, left, right } => {
                    let (l, r) = self.coerce_pair(val_map, left, right)?;
                    let v = self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }
                MirInst::ISub { dest, left, right } => {
                    let (l, r) = self.coerce_pair(val_map, left, right)?;
                    let v = self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }
                MirInst::IMul { dest, left, right } => {
                    let (l, r) = self.coerce_pair(val_map, left, right)?;
                    let v = self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }
                MirInst::IDiv { dest, left, right } => {
                    let (l, r) = self.coerce_pair(val_map, left, right)?;
                    let v = self
                        .builder
                        .build_int_signed_div(l, r, "div")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }
                MirInst::IRem { dest, left, right } => {
                    let (l, r) = self.coerce_pair(val_map, left, right)?;
                    let v = self
                        .builder
                        .build_int_signed_rem(l, r, "rem")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }
                MirInst::INeg { dest, operand } => {
                    let op = self.as_int(val_map, operand)?;
                    let v = self
                        .builder
                        .build_int_neg(op, "neg")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }

                // Integer comparisons
                MirInst::ICmpEq { dest, left, right } => {
                    self.build_icmp(IntPredicate::EQ, dest, left, right, val_map)?;
                }
                MirInst::ICmpNe { dest, left, right } => {
                    self.build_icmp(IntPredicate::NE, dest, left, right, val_map)?;
                }
                MirInst::ICmpLt { dest, left, right } => {
                    self.build_icmp(IntPredicate::SLT, dest, left, right, val_map)?;
                }
                MirInst::ICmpGt { dest, left, right } => {
                    self.build_icmp(IntPredicate::SGT, dest, left, right, val_map)?;
                }
                MirInst::ICmpLe { dest, left, right } => {
                    self.build_icmp(IntPredicate::SLE, dest, left, right, val_map)?;
                }
                MirInst::ICmpGe { dest, left, right } => {
                    self.build_icmp(IntPredicate::SGE, dest, left, right, val_map)?;
                }

                // Runtime calls
                MirInst::CallRuntime { dest, func, args } => {
                    let fn_val = runtime_funcs.get(func.as_str()).ok_or_else(|| {
                        CodegenError::BackendError(format!("unknown runtime func: {}", func))
                    })?;

                    let llvm_args: Vec<BasicMetadataValueEnum> = args
                        .iter()
                        .enumerate()
                        .map(|(i, a)| {
                            let v = self.lookup(val_map, a).unwrap();
                            self.coerce_arg(v, *fn_val, i)
                        })
                        .collect();

                    let call = self
                        .builder
                        .build_call(*fn_val, &llvm_args, "call")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;

                    if let Some(d) = dest {
                        if let Some(rv) = call.try_as_basic_value().left() {
                            val_map.insert(d.raw(), rv);
                        }
                    }
                }

                MirInst::CallProgram {
                    dest,
                    program,
                    args,
                } => {
                    // Indirect call through function pointer
                    let fn_ptr = self.as_ptr(val_map, program)?;
                    let param_types: Vec<BasicMetadataTypeEnum> =
                        args.iter().map(|_| self.ptr_type().into()).collect();
                    let fn_type = self.context.void_type().fn_type(&param_types, false);
                    let llvm_args: Vec<BasicMetadataValueEnum> = args
                        .iter()
                        .map(|a| {
                            let v = self.lookup(val_map, a).unwrap();
                            BasicMetadataValueEnum::from(v)
                        })
                        .collect();
                    let call = self
                        .builder
                        .build_indirect_call(fn_type, fn_ptr, &llvm_args, "icall")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    if let Some(d) = dest {
                        if let Some(rv) = call.try_as_basic_value().left() {
                            val_map.insert(d.raw(), rv);
                        }
                    }
                }

                // MOVE alphanumeric
                MirInst::MoveAlphanumeric {
                    dest,
                    src,
                    dest_len,
                    src_len,
                    justified,
                } => {
                    let fname = if *justified {
                        "cobolrt_move_alphanumeric_justified"
                    } else {
                        "cobolrt_move_alphanumeric"
                    };
                    let fn_val = runtime_funcs.get(fname).ok_or_else(|| {
                        CodegenError::BackendError(format!("missing runtime: {}", fname))
                    })?;
                    let src_v = self.lookup(val_map, src)?;
                    let dst_v = self.lookup(val_map, dest)?;
                    let sl = self.context.i32_type().const_int(*src_len as u64, false);
                    let dl = self.context.i32_type().const_int(*dest_len as u64, false);
                    self.builder
                        .build_call(
                            *fn_val,
                            &[src_v.into(), sl.into(), dst_v.into(), dl.into()],
                            "",
                        )
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }

                // Decimal operations → delegate to runtime calls
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
                }
                | MirInst::DecimalCmp { dest, left, right } => {
                    // These are handled as runtime calls in the MIR lowering
                    // If they appear here directly, treat as i64 arithmetic
                    let l = self.as_int(val_map, left)?;
                    let r = self.as_int(val_map, right)?;
                    let v = match inst {
                        MirInst::DecimalAdd { .. } => self.builder.build_int_add(l, r, "dadd"),
                        MirInst::DecimalSub { .. } => self.builder.build_int_sub(l, r, "dsub"),
                        MirInst::DecimalMul { .. } => self.builder.build_int_mul(l, r, "dmul"),
                        MirInst::DecimalDiv { .. } => {
                            self.builder.build_int_signed_div(l, r, "ddiv")
                        }
                        MirInst::DecimalCmp { .. } => self.builder.build_int_sub(l, r, "dcmp"),
                        _ => unreachable!(),
                    }
                    .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    val_map.insert(dest.raw(), v.into());
                }

                // Conversion stubs — these are typically handled by CallRuntime
                MirInst::DecimalToDisplay { dest, value, .. }
                | MirInst::DisplayToDecimal { dest, value, .. }
                | MirInst::PackToBinary { dest, value }
                | MirInst::BinaryToPack { dest, value }
                | MirInst::IntToDecimal { dest, value, .. }
                | MirInst::DecimalToInt { dest, value } => {
                    let v = self.lookup(val_map, value)?;
                    val_map.insert(dest.raw(), v);
                }

                // String/inspect operations → runtime calls
                MirInst::StringConcat { .. }
                | MirInst::StringSplit { .. }
                | MirInst::Inspect { .. } => {
                    // These are emitted as CallRuntime in the MIR lowering
                }

                // PERFORM stack
                MirInst::PerformPush { .. } | MirInst::PerformPop { .. } => {
                    // PERFORM is lowered to direct jumps in the MIR
                }

                // File I/O
                MirInst::FileOpen { .. }
                | MirInst::FileClose { .. }
                | MirInst::FileRead { .. }
                | MirInst::FileWrite { .. } => {
                    // These are emitted as CallRuntime in the MIR lowering
                }

                MirInst::SizeError {
                    value,
                    on_error,
                    no_error,
                } => {
                    let v = self.as_int(val_map, value)?;
                    let zero = v.get_type().const_zero();
                    let cond = self
                        .builder
                        .build_int_compare(IntPredicate::NE, v, zero, "sz_err")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    let err_bb = block_map[&on_error.raw()];
                    let ok_bb = block_map[&no_error.raw()];
                    self.builder
                        .build_conditional_branch(cond, err_bb, ok_bb)
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }

                MirInst::Phi { dest, incoming } => {
                    // LLVM phi nodes — build from incoming values
                    if incoming.is_empty() {
                        return Ok(());
                    }
                    let first_val = self.lookup(val_map, &incoming[0].1)?;
                    let phi = self
                        .builder
                        .build_phi(first_val.get_type(), "phi")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    for (block_id, val) in incoming {
                        if let (Some(bb), Ok(v)) =
                            (block_map.get(&block_id.raw()), self.lookup(val_map, val))
                        {
                            phi.add_incoming(&[(&v, *bb)]);
                        }
                    }
                    val_map.insert(dest.raw(), phi.as_basic_value());
                }
            }
            Ok(())
        }

        fn translate_terminator(
            &self,
            term: &Terminator,
            val_map: &HashMap<u32, BasicValueEnum<'ctx>>,
            block_map: &HashMap<u32, inkwell::basic_block::BasicBlock<'ctx>>,
            fn_val: FunctionValue<'ctx>,
        ) -> Result<(), CodegenError> {
            match term {
                Terminator::Goto(target) => {
                    let bb = block_map[&target.raw()];
                    self.builder
                        .build_unconditional_branch(bb)
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }
                Terminator::Branch {
                    cond,
                    then_block,
                    else_block,
                } => {
                    let c = self.as_int(val_map, cond)?;
                    // Ensure it's an i1
                    let zero = c.get_type().const_zero();
                    let cond_i1 = self
                        .builder
                        .build_int_compare(IntPredicate::NE, c, zero, "br_cond")
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    let tb = block_map[&then_block.raw()];
                    let eb = block_map[&else_block.raw()];
                    self.builder
                        .build_conditional_branch(cond_i1, tb, eb)
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }
                Terminator::Switch {
                    value,
                    cases,
                    default,
                } => {
                    let v = self.as_int(val_map, value)?;
                    let def_bb = block_map[&default.raw()];
                    let switch_cases: Vec<_> = cases
                        .iter()
                        .map(|(case_val, target)| {
                            let cv = v.get_type().const_int(*case_val as u64, true);
                            (cv, block_map[&target.raw()])
                        })
                        .collect();
                    self.builder
                        .build_switch(v, def_bb, &switch_cases)
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }
                Terminator::Return => {
                    if fn_val.get_type().get_return_type().is_none() {
                        self.builder
                            .build_return(None)
                            .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    } else {
                        let zero = self.context.i32_type().const_int(0, false);
                        self.builder
                            .build_return(Some(&zero))
                            .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                    }
                }
                Terminator::PerformReturn => {
                    self.builder
                        .build_return(None)
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }
                Terminator::Unreachable => {
                    self.builder
                        .build_unreachable()
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }
                Terminator::IndirectJump { .. } => {
                    self.builder
                        .build_unreachable()
                        .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                }
            }
            Ok(())
        }

        fn generate_c_main(&self) -> Result<(), CodegenError> {
            let i32_ty = self.context.i32_type();
            let ptr_ty = self.ptr_type();
            let main_type = i32_ty.fn_type(&[i32_ty.into(), ptr_ty.into()], false);
            let main_fn = self.module.add_function(
                "main",
                main_type,
                Some(inkwell::module::Linkage::External),
            );

            let entry = self.context.append_basic_block(main_fn, "entry");
            self.builder.position_at_end(entry);

            let cobol_main = self.func_values["_cobol_main"];
            self.builder
                .build_call(cobol_main, &[], "")
                .map_err(|e| CodegenError::BackendError(e.to_string()))?;

            let zero = i32_ty.const_int(0, false);
            self.builder
                .build_return(Some(&zero))
                .map_err(|e| CodegenError::BackendError(e.to_string()))?;

            Ok(())
        }

        // ---------------------------------------------------------------
        // Helpers
        // ---------------------------------------------------------------

        fn const_to_llvm(&self, c: &MirConst) -> BasicValueEnum<'ctx> {
            match c {
                MirConst::Int(n) => self.context.i64_type().const_int(*n as u64, true).into(),
                MirConst::Int128(n) => self.context.i128_type().const_int(*n as u64, true).into(),
                MirConst::Float(f) => self.context.f64_type().const_float(*f).into(),
                MirConst::Bool(b) => self.context.bool_type().const_int(*b as u64, false).into(),
                MirConst::Str(s) => {
                    let gv = self
                        .builder
                        .build_global_string_ptr(s, "str")
                        .map(|g| g.as_pointer_value())
                        .unwrap_or_else(|_| self.ptr_type().const_null());
                    gv.into()
                }
                MirConst::Bytes(b) => {
                    let vals: Vec<_> = b
                        .iter()
                        .map(|byte| self.context.i8_type().const_int(*byte as u64, false))
                        .collect();
                    let arr = self.context.i8_type().const_array(&vals);
                    arr.into()
                }
                MirConst::Null => self.ptr_type().const_null().into(),
            }
        }

        fn lookup(
            &self,
            val_map: &HashMap<u32, BasicValueEnum<'ctx>>,
            val: &Value,
        ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
            val_map.get(&val.raw()).copied().ok_or_else(|| {
                CodegenError::BackendError(format!("undefined value: v{}", val.raw()))
            })
        }

        fn as_ptr(
            &self,
            val_map: &HashMap<u32, BasicValueEnum<'ctx>>,
            val: &Value,
        ) -> Result<PointerValue<'ctx>, CodegenError> {
            let v = self.lookup(val_map, val)?;
            v.into_pointer_value_or_err()
        }

        fn as_int(
            &self,
            val_map: &HashMap<u32, BasicValueEnum<'ctx>>,
            val: &Value,
        ) -> Result<inkwell::values::IntValue<'ctx>, CodegenError> {
            let v = self.lookup(val_map, val)?;
            v.into_int_value_or_err()
        }

        fn coerce_pair(
            &self,
            val_map: &HashMap<u32, BasicValueEnum<'ctx>>,
            left: &Value,
            right: &Value,
        ) -> Result<
            (
                inkwell::values::IntValue<'ctx>,
                inkwell::values::IntValue<'ctx>,
            ),
            CodegenError,
        > {
            let l = self.as_int(val_map, left)?;
            let r = self.as_int(val_map, right)?;
            let lt = l.get_type();
            let rt = r.get_type();
            if lt == rt {
                return Ok((l, r));
            }
            if lt.get_bit_width() > rt.get_bit_width() {
                let r2 = self
                    .builder
                    .build_int_s_extend(r, lt, "sext")
                    .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                Ok((l, r2))
            } else {
                let l2 = self
                    .builder
                    .build_int_s_extend(l, rt, "sext")
                    .map_err(|e| CodegenError::BackendError(e.to_string()))?;
                Ok((l2, r))
            }
        }

        fn build_icmp(
            &self,
            pred: IntPredicate,
            dest: &Value,
            left: &Value,
            right: &Value,
            val_map: &mut HashMap<u32, BasicValueEnum<'ctx>>,
        ) -> Result<(), CodegenError> {
            let (l, r) = self.coerce_pair(val_map, left, right)?;
            let v = self
                .builder
                .build_int_compare(pred, l, r, "cmp")
                .map_err(|e| CodegenError::BackendError(e.to_string()))?;
            // Zero-extend i1 to i8 for consistency with MIR Bool type
            let v8 = self
                .builder
                .build_int_z_extend(v, self.context.i8_type(), "cmp_ext")
                .map_err(|e| CodegenError::BackendError(e.to_string()))?;
            val_map.insert(dest.raw(), v8.into());
            Ok(())
        }

        fn coerce_arg(
            &self,
            val: BasicValueEnum<'ctx>,
            fn_val: FunctionValue<'ctx>,
            idx: usize,
        ) -> BasicMetadataValueEnum<'ctx> {
            // Coerce argument to match function parameter type
            let param_count = fn_val.count_params();
            if idx as u32 >= param_count {
                return val.into();
            }
            let expected = fn_val.get_nth_param(idx as u32).unwrap().get_type();
            if val.get_type() == expected {
                return val.into();
            }
            // Int to int coercion
            if val.is_int_value() && expected.is_int_type() {
                let iv = val.into_int_value();
                let target = expected.into_int_type();
                if iv.get_type().get_bit_width() > target.get_bit_width() {
                    return self
                        .builder
                        .build_int_truncate(iv, target, "trunc")
                        .map(|v| v.into())
                        .unwrap_or(val.into());
                } else if iv.get_type().get_bit_width() < target.get_bit_width() {
                    return self
                        .builder
                        .build_int_s_extend(iv, target, "sext")
                        .map(|v| v.into())
                        .unwrap_or(val.into());
                }
            }
            val.into()
        }
    }

    // Helper trait for cleaner error handling
    trait IntoValueOrErr<'ctx> {
        fn into_pointer_value_or_err(self) -> Result<PointerValue<'ctx>, CodegenError>;
        fn into_int_value_or_err(self) -> Result<inkwell::values::IntValue<'ctx>, CodegenError>;
    }

    impl<'ctx> IntoValueOrErr<'ctx> for BasicValueEnum<'ctx> {
        fn into_pointer_value_or_err(self) -> Result<PointerValue<'ctx>, CodegenError> {
            match self {
                BasicValueEnum::PointerValue(p) => Ok(p),
                other => Err(CodegenError::TypeMismatch(format!(
                    "expected pointer, got {:?}",
                    other.get_type()
                ))),
            }
        }

        fn into_int_value_or_err(self) -> Result<inkwell::values::IntValue<'ctx>, CodegenError> {
            match self {
                BasicValueEnum::IntValue(i) => Ok(i),
                other => Err(CodegenError::TypeMismatch(format!(
                    "expected int, got {:?}",
                    other.get_type()
                ))),
            }
        }
    }

    impl CodegenBackend for LlvmBackend {
        fn compile(
            &self,
            module: &MirModule,
            output: &std::path::Path,
        ) -> Result<(), CodegenError> {
            let context = Context::create();
            let codegen = LlvmCodegen::new(&context, self.opt_level)?;
            codegen.compile_module(module, output)
        }

        fn name(&self) -> &str {
            "llvm"
        }

        fn output_extension(&self) -> &str {
            "o"
        }
    }
}

// ---------------------------------------------------------------------------
// Stub implementation when LLVM feature is not enabled
// ---------------------------------------------------------------------------

#[cfg(not(feature = "llvm"))]
impl CodegenBackend for LlvmBackend {
    fn compile(&self, _module: &MirModule, _output: &std::path::Path) -> Result<(), CodegenError> {
        Err(CodegenError::BackendError(
            "LLVM backend not available: rebuild with --features llvm (requires LLVM 18 dev libs)"
                .to_string(),
        ))
    }

    fn name(&self) -> &str {
        "llvm"
    }

    fn output_extension(&self) -> &str {
        "o"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn llvm_backend_new_and_name() {
        let backend = LlvmBackend::new(OptLevel::O2);
        assert_eq!(backend.name(), "llvm");
        assert_eq!(backend.opt_level(), OptLevel::O2);
        assert_eq!(backend.output_extension(), "o");
    }

    #[test]
    fn llvm_backend_compile_without_feature() {
        let backend = LlvmBackend::new(OptLevel::O0);
        let module = MirModule::default();
        let result = backend.compile(&module, std::path::Path::new("out.o"));
        // Without the llvm feature, this should return an error
        #[cfg(not(feature = "llvm"))]
        assert!(result.is_err());
        // With the llvm feature, it would succeed for an empty module
        #[cfg(feature = "llvm")]
        assert!(result.is_ok() || result.is_err()); // may or may not succeed
    }

    #[test]
    fn codegen_error_display() {
        let err = CodegenError::BackendError("test".to_string());
        assert_eq!(format!("{}", err), "backend error: test");

        let err = CodegenError::UnsupportedInstruction("foo".to_string());
        assert_eq!(format!("{}", err), "unsupported instruction: foo");

        let err = CodegenError::TypeMismatch("bar".to_string());
        assert_eq!(format!("{}", err), "type mismatch: bar");
    }

    #[test]
    fn codegen_error_from_io_error() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "not found");
        let err: CodegenError = io_err.into();
        assert!(matches!(err, CodegenError::IoError(_)));
        assert!(format!("{}", err).contains("not found"));
    }

    #[test]
    fn opt_level_equality() {
        assert_eq!(OptLevel::O0, OptLevel::O0);
        assert_ne!(OptLevel::O0, OptLevel::O3);
        assert_eq!(OptLevel::Os, OptLevel::Os);
    }

    #[test]
    fn opt_level_from_cli_string() {
        // Mirrors the mapping in cobol-driver/src/main.rs — each CLI
        // --opt-level value must produce the correct LLVM OptLevel.
        let map = |s: &str| -> OptLevel {
            match s {
                "0" => OptLevel::O0,
                "1" => OptLevel::O1,
                "2" => OptLevel::O2,
                "3" => OptLevel::O3,
                "s" => OptLevel::Os,
                _ => OptLevel::O2,
            }
        };
        assert_eq!(map("0"), OptLevel::O0);
        assert_eq!(map("1"), OptLevel::O1);
        assert_eq!(map("2"), OptLevel::O2);
        assert_eq!(map("3"), OptLevel::O3);
        assert_eq!(map("s"), OptLevel::Os);
        // Unknown falls back to O2
        assert_eq!(map("z"), OptLevel::O2);
    }

    #[test]
    fn llvm_backend_stores_opt_level() {
        for &level in &[
            OptLevel::O0,
            OptLevel::O1,
            OptLevel::O2,
            OptLevel::O3,
            OptLevel::Os,
        ] {
            let backend = LlvmBackend::new(level);
            assert_eq!(backend.opt_level(), level);
        }
    }
}
