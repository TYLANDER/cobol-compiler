//! SSA-based mid-level IR with COBOL-specific operations.
//!
//! MIR sits between HIR and the codegen backends. It represents a COBOL
//! program as a collection of functions, each containing a control-flow graph
//! of basic blocks. Instructions use SSA form (each value is defined exactly
//! once) and include COBOL-specific operations for decimal arithmetic, packed
//! decimal conversions, and PERFORM stack management.

pub mod passes;

use std::hash::Hash;

// ---------------------------------------------------------------------------
// Core value and block identifiers
// ---------------------------------------------------------------------------

/// An SSA value handle. Each `Value` is defined by exactly one instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Value(u32);

impl Value {
    /// Creates a new `Value` from a raw index.
    pub fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    /// Returns the underlying raw index.
    pub fn raw(self) -> u32 {
        self.0
    }
}

/// A basic block identifier within a function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(u32);

impl BlockId {
    /// Creates a new `BlockId` from a raw index.
    pub fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    /// Returns the underlying raw index.
    pub fn raw(self) -> u32 {
        self.0
    }
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// MIR-level type system, mapping COBOL storage concepts to machine types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirType {
    I32,
    I64,
    I128,
    F32,
    F64,
    /// Fixed-point decimal stored in binary (COMP-3 unpacked to integer).
    Decimal {
        digits: u8,
        scale: i8,
    },
    /// Packed decimal (BCD) stored in raw bytes.
    PackedDecimal {
        digits: u8,
        scale: i8,
    },
    /// Raw byte array of a given length.
    Bytes(u32),
    Pointer,
    Bool,
    Void,
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// A compile-time constant value.
#[derive(Debug, Clone, PartialEq)]
pub enum MirConst {
    Int(i64),
    Int128(i128),
    Float(f64),
    Str(String),
    Bytes(Vec<u8>),
    Bool(bool),
    Null,
}

// ---------------------------------------------------------------------------
// Decimal and file helpers
// ---------------------------------------------------------------------------

/// Format descriptor for DISPLAY <-> decimal conversions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DecimalFormat {
    /// Total number of digit positions.
    pub digits: u8,
    /// Decimal scale (digits after the implied decimal point).
    pub scale: i8,
    /// Whether the item is signed.
    pub signed: bool,
}

/// Mode for the INSPECT statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InspectMode {
    Tallying,
    Replacing,
    Converting,
}

/// Mode for the OPEN statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileOpenMode {
    Input,
    Output,
    IoMode,
    Extend,
}

// ---------------------------------------------------------------------------
// Instructions
// ---------------------------------------------------------------------------

/// A single MIR instruction. Most instructions produce a `dest` value and
/// consume zero or more operand values.
#[derive(Debug, Clone, PartialEq)]
pub enum MirInst {
    // -- Constants and memory --
    Const {
        dest: Value,
        value: MirConst,
    },
    /// Get the address of a named global variable.
    GlobalAddr {
        dest: Value,
        name: String,
    },
    Load {
        dest: Value,
        addr: Value,
        ty: MirType,
    },
    Store {
        addr: Value,
        value: Value,
    },
    GetFieldAddr {
        dest: Value,
        base: Value,
        offset: u32,
    },
    GetElementAddr {
        dest: Value,
        base: Value,
        index: Value,
        element_size: u32,
    },

    // -- Integer arithmetic --
    IAdd {
        dest: Value,
        left: Value,
        right: Value,
    },
    ISub {
        dest: Value,
        left: Value,
        right: Value,
    },
    IMul {
        dest: Value,
        left: Value,
        right: Value,
    },
    IDiv {
        dest: Value,
        left: Value,
        right: Value,
    },
    IRem {
        dest: Value,
        left: Value,
        right: Value,
    },
    INeg {
        dest: Value,
        operand: Value,
    },

    // -- COBOL-specific decimal operations --
    DecimalAdd {
        dest: Value,
        left: Value,
        right: Value,
        result_scale: i8,
        rounded: bool,
    },
    DecimalSub {
        dest: Value,
        left: Value,
        right: Value,
        result_scale: i8,
        rounded: bool,
    },
    DecimalMul {
        dest: Value,
        left: Value,
        right: Value,
        result_scale: i8,
        rounded: bool,
    },
    DecimalDiv {
        dest: Value,
        left: Value,
        right: Value,
        result_scale: i8,
        rounded: bool,
    },
    DecimalCmp {
        dest: Value,
        left: Value,
        right: Value,
    },

    // -- Conversions --
    DecimalToDisplay {
        dest: Value,
        value: Value,
        pic: DecimalFormat,
    },
    DisplayToDecimal {
        dest: Value,
        value: Value,
        pic: DecimalFormat,
    },
    PackToBinary {
        dest: Value,
        value: Value,
    },
    BinaryToPack {
        dest: Value,
        value: Value,
    },
    IntToDecimal {
        dest: Value,
        value: Value,
        scale: i8,
    },
    DecimalToInt {
        dest: Value,
        value: Value,
    },

    // -- String operations --
    MoveAlphanumeric {
        dest: Value,
        src: Value,
        dest_len: u32,
        src_len: u32,
        justified: bool,
    },
    StringConcat {
        dest: Value,
        parts: Vec<Value>,
        delimiter: Option<Value>,
        pointer: Option<Value>,
    },
    StringSplit {
        src: Value,
        delimiters: Vec<Value>,
        targets: Vec<Value>,
    },
    Inspect {
        target: Value,
        mode: InspectMode,
    },

    // -- Comparisons --
    ICmpEq {
        dest: Value,
        left: Value,
        right: Value,
    },
    ICmpNe {
        dest: Value,
        left: Value,
        right: Value,
    },
    ICmpLt {
        dest: Value,
        left: Value,
        right: Value,
    },
    ICmpGt {
        dest: Value,
        left: Value,
        right: Value,
    },
    ICmpLe {
        dest: Value,
        left: Value,
        right: Value,
    },
    ICmpGe {
        dest: Value,
        left: Value,
        right: Value,
    },

    // -- PERFORM stack management --
    PerformPush {
        return_block: BlockId,
    },
    PerformPop {
        dest: Value,
    },

    // -- File I/O --
    FileOpen {
        file_handle: Value,
        mode: FileOpenMode,
    },
    FileClose {
        file_handle: Value,
    },
    FileRead {
        file_handle: Value,
        into: Value,
        at_end: BlockId,
        not_at_end: BlockId,
    },
    FileWrite {
        file_handle: Value,
        from: Value,
    },

    // -- Runtime calls --
    CallRuntime {
        dest: Option<Value>,
        func: String,
        args: Vec<Value>,
    },
    CallProgram {
        dest: Option<Value>,
        program: Value,
        args: Vec<Value>,
    },

    // -- SSA --
    Phi {
        dest: Value,
        incoming: Vec<(BlockId, Value)>,
    },

    // -- Size error --
    SizeError {
        value: Value,
        on_error: BlockId,
        no_error: BlockId,
    },
}

// ---------------------------------------------------------------------------
// Terminators
// ---------------------------------------------------------------------------

/// The terminating instruction of a basic block, controlling where execution
/// continues.
#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    /// Unconditional jump.
    Goto(BlockId),
    /// Conditional branch.
    Branch {
        cond: Value,
        then_block: BlockId,
        else_block: BlockId,
    },
    /// Multi-way switch.
    Switch {
        value: Value,
        cases: Vec<(i64, BlockId)>,
        default: BlockId,
    },
    /// Return from the current function.
    Return,
    /// Indirect jump (used for ALTER'd GO TOs).
    IndirectJump { target: Value },
    /// Pop the perform stack and jump to the saved return address.
    PerformReturn,
    /// Marks unreachable code.
    Unreachable,
}

// ---------------------------------------------------------------------------
// Basic blocks, functions, and modules
// ---------------------------------------------------------------------------

/// A basic block: a straight-line sequence of instructions ending with a
/// terminator.
#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    /// This block's identifier.
    pub id: BlockId,
    /// Block parameters (SSA phi-elimination targets).
    pub params: Vec<(Value, MirType)>,
    /// The instructions in this block.
    pub instructions: Vec<MirInst>,
    /// The terminating control flow instruction.
    pub terminator: Terminator,
}

/// A function in the MIR module. Each COBOL paragraph or section may lower
/// to one or more MIR functions.
#[derive(Debug, Clone, PartialEq)]
pub struct MirFunction {
    /// Function name (e.g. the paragraph or section name).
    pub name: String,
    /// Formal parameters with their types.
    pub params: Vec<(String, MirType)>,
    /// Return type.
    pub return_type: MirType,
    /// All basic blocks in this function.
    pub blocks: Vec<BasicBlock>,
    /// The entry block of this function.
    pub entry_block: BlockId,
    /// Counter for generating fresh `Value` ids.
    pub next_value: u32,
    /// Counter for generating fresh `BlockId` ids.
    pub next_block: u32,
}

impl MirFunction {
    /// Allocates a fresh [`Value`] in this function.
    pub fn new_value(&mut self) -> Value {
        let v = Value(self.next_value);
        self.next_value += 1;
        v
    }

    /// Allocates a fresh [`BlockId`] in this function.
    pub fn new_block(&mut self) -> BlockId {
        let b = BlockId(self.next_block);
        self.next_block += 1;
        b
    }
}

/// A global variable in the MIR module (corresponds to a WORKING-STORAGE
/// or LINKAGE SECTION data item).
#[derive(Debug, Clone, PartialEq)]
pub struct MirGlobal {
    /// The data item name.
    pub name: String,
    /// The MIR type.
    pub ty: MirType,
    /// Optional initial value from a VALUE clause.
    pub initial_value: Option<MirConst>,
    /// Byte offset in the global data segment.
    pub offset: u32,
    /// If this is a REDEFINES item, the name of the redefined global.
    pub redefines: Option<String>,
    /// If this is a child of a group item, (parent_name, offset_within_parent).
    /// The child shares the parent's memory at the given byte offset.
    pub parent_offset: Option<(String, u32)>,
}

/// A file descriptor in the MIR module.
#[derive(Debug, Clone, PartialEq)]
pub struct MirFileDescriptor {
    /// The file name.
    pub name: String,
    /// File organization from the HIR.
    pub organization: cobol_hir::FileOrganization,
    /// Access mode from the HIR.
    pub access_mode: cobol_hir::AccessMode,
    /// Total record size in bytes.
    pub record_size: u32,
    /// RELATIVE KEY data name (for ORGANIZATION IS RELATIVE).
    pub relative_key: Option<String>,
    /// RECORD KEY data name (for ORGANIZATION IS INDEXED).
    pub record_key: Option<String>,
    /// FILE STATUS data name.
    pub file_status: Option<String>,
}

/// The top-level MIR container for a single compilation unit.
#[derive(Debug, Clone, PartialEq)]
pub struct MirModule {
    /// Module name (typically the PROGRAM-ID).
    pub name: String,
    /// All functions in this module.
    pub functions: Vec<MirFunction>,
    /// Global data items.
    pub globals: Vec<MirGlobal>,
    /// File descriptors.
    pub file_descriptors: Vec<MirFileDescriptor>,
    /// Errors encountered during MIR lowering.
    pub errors: Vec<String>,
}

impl MirModule {
    /// Create an empty MIR module with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: Vec::new(),
            globals: Vec::new(),
            file_descriptors: Vec::new(),
            errors: Vec::new(),
        }
    }
}

impl Default for MirModule {
    fn default() -> Self {
        Self::new("")
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Lower HIR to MIR.
///
/// This is the main entry point for MIR construction. It takes a fully
/// validated [`cobol_hir::HirModule`] and produces an SSA-based
/// [`MirModule`] suitable for optimization and code generation.
///
/// When `is_main_program` is true, the emitted function is named `_cobol_main`
/// (the entry point). When false, the function is named after the PROGRAM-ID,
/// making it callable as a subprogram even without USING params.
pub fn lower(hir: &cobol_hir::HirModule, interner: &cobol_intern::Interner) -> MirModule {
    lower_with_options(hir, interner, true)
}

/// Lower HIR to MIR with explicit main/subprogram classification.
pub fn lower_with_options(
    hir: &cobol_hir::HirModule,
    interner: &cobol_intern::Interner,
    is_main_program: bool,
) -> MirModule {
    let mut lowerer = MirLowerer::new(hir, interner);
    lowerer.is_main_program = is_main_program;
    lowerer.lower_module();
    lowerer.finish()
}

// ---------------------------------------------------------------------------
// MIR Lowerer
// ---------------------------------------------------------------------------

/// Info about a LINKAGE SECTION item that is a function parameter.
struct LinkageInfo {
    /// MIR Value representing the pointer parameter.
    param_value: Value,
    /// Byte size of the data item.
    byte_size: u32,
    /// Decimal scale.
    scale: i32,
    /// Position of '.' in PIC string (-1 if none).
    dot_pos: i32,
}

struct MirLowerer<'a> {
    hir: &'a cobol_hir::HirModule,
    interner: &'a cobol_intern::Interner,
    module: MirModule,
    /// Maps data item names to their global indices.
    global_map: std::collections::HashMap<String, usize>,
    /// Maps linkage item names to their parameter info (for subprograms).
    linkage_map: std::collections::HashMap<String, LinkageInfo>,
    /// Maps SELECT file name to ASSIGN TO path.
    file_assign_map: std::collections::HashMap<String, String>,
    /// Maps SELECT file name to its record item name(s).
    file_record_map: std::collections::HashMap<String, Vec<String>>,
    /// Maps SELECT file name to its organization.
    file_org_map: std::collections::HashMap<String, cobol_hir::FileOrganization>,
    /// Maps SELECT file name to its access mode.
    file_access_map: std::collections::HashMap<String, cobol_hir::AccessMode>,
    /// Maps SELECT file name to its RELATIVE KEY data name.
    file_relative_key_map: std::collections::HashMap<String, String>,
    /// Maps SELECT file name to its FILE STATUS data name.
    file_status_map: std::collections::HashMap<String, String>,
    /// Maps paragraph names to MIR block IDs for GO TO resolution.
    paragraph_block_map: std::collections::HashMap<String, BlockId>,
    /// Maps data item names to their OCCURS element size (single element, before multiplication).
    occurs_element_size: std::collections::HashMap<String, u32>,
    /// Maps index names (from INDEXED BY) to their associated table info:
    /// (table_name, occurs_max, element_size).
    index_info: std::collections::HashMap<String, (String, u32, u32)>,
    /// Maps a simple child name to its qualified global name(s) when there are duplicates.
    /// Key: simple name (e.g. "WS-FNAME"), Value: vec of (qualified_global_name, parent_name).
    qualified_names: std::collections::HashMap<String, Vec<(String, String)>>,
    /// Maps group name to its direct children names (simple names).
    group_children: std::collections::HashMap<String, Vec<String>>,
    /// Whether this compilation unit is the main program (true) or a subprogram (false).
    is_main_program: bool,
    /// Block ID to jump to for EXIT PARAGRAPH (the start of the next paragraph,
    /// or the function epilogue block if at the last paragraph).
    current_paragraph_exit: Option<BlockId>,
    /// Block ID to jump to for EXIT SECTION (the first block after the current
    /// section, or the function epilogue block if at the last section).
    current_section_exit: Option<BlockId>,
}

impl<'a> MirLowerer<'a> {
    fn new(hir: &'a cobol_hir::HirModule, interner: &'a cobol_intern::Interner) -> Self {
        let name = interner.resolve(hir.program_name).to_string();
        Self {
            hir,
            interner,
            module: MirModule::new(name),
            global_map: std::collections::HashMap::new(),
            linkage_map: std::collections::HashMap::new(),
            file_assign_map: std::collections::HashMap::new(),
            file_record_map: std::collections::HashMap::new(),
            file_org_map: std::collections::HashMap::new(),
            file_access_map: std::collections::HashMap::new(),
            file_relative_key_map: std::collections::HashMap::new(),
            file_status_map: std::collections::HashMap::new(),
            paragraph_block_map: std::collections::HashMap::new(),
            occurs_element_size: std::collections::HashMap::new(),
            index_info: std::collections::HashMap::new(),
            qualified_names: std::collections::HashMap::new(),
            group_children: std::collections::HashMap::new(),
            is_main_program: true,
            current_paragraph_exit: None,
            current_section_exit: None,
        }
    }

    fn lower_module(&mut self) {
        // Build file info from HIR file descriptors
        self.prepare_file_info();

        // Lower WORKING-STORAGE to globals
        self.lower_globals();

        // Create file handle globals (one i32 per SELECT file, initialized to -1)
        self.create_file_handle_globals();

        // Build linkage parameter map for subprograms
        self.prepare_linkage();

        // Lower paragraphs into function(s)
        self.lower_procedure_division();
    }

    fn prepare_file_info(&mut self) {
        for fd in &self.hir.file_items {
            let file_name = self.interner.resolve(fd.name).to_string();
            self.file_org_map.insert(file_name.clone(), fd.organization);
            self.file_access_map
                .insert(file_name.clone(), fd.access_mode);

            if let Some(ref rk) = fd.relative_key {
                self.file_relative_key_map
                    .insert(file_name.clone(), rk.clone());
            }
            if let Some(ref fs) = fd.file_status {
                self.file_status_map.insert(file_name.clone(), fs.clone());
            }

            // Store the ASSIGN TO path from the file descriptor
            if !fd.assign_to.is_empty() {
                self.file_assign_map
                    .insert(file_name.clone(), fd.assign_to.clone());
            }

            // Record items
            let mut records = Vec::new();
            let mut record_size = 0u32;
            for &item_id in &fd.record_items {
                let item = &self.hir.data_items[item_id.into_raw()];
                if let Some(name) = item.name {
                    let rname = self.interner.resolve(name).to_string();
                    records.push(rname);
                }
                record_size = record_size.max(item.storage.byte_size);
            }
            self.file_record_map.insert(file_name.clone(), records);

            self.module.file_descriptors.push(MirFileDescriptor {
                name: file_name,
                organization: fd.organization,
                access_mode: fd.access_mode,
                record_size,
                relative_key: fd.relative_key.clone(),
                record_key: fd.record_key.clone(),
                file_status: fd.file_status.clone(),
            });
        }
    }

    fn create_file_handle_globals(&mut self) {
        let offset = self.module.globals.len() as u32 * 100; // rough offset
        for fd in &self.hir.file_items {
            let file_name = self.interner.resolve(fd.name).to_string();
            let handle_name = format!("_FILE_HANDLE_{}", file_name);

            // 4-byte integer for the file handle, initialized to -1 (closed)
            let global = MirGlobal {
                name: handle_name.clone(),
                ty: MirType::Bytes(4),
                initial_value: Some(MirConst::Bytes(vec![0xFF, 0xFF, 0xFF, 0xFF])),
                offset,
                redefines: None,
                parent_offset: None,
            };

            self.global_map
                .insert(handle_name, self.module.globals.len());
            self.module.globals.push(global);
        }
    }

    /// For subprograms (PROCEDURE DIVISION USING ...), build a mapping from
    /// linkage item names to their function parameter Value IDs.
    fn prepare_linkage(&mut self) {
        if self.hir.using_params.is_empty() {
            return;
        }
        for (i, &param_name) in self.hir.using_params.iter().enumerate() {
            let param_str = self.interner.resolve(param_name).to_string();
            // Find the linkage item with this name
            for &item_id in &self.hir.linkage_items {
                let item = &self.hir.data_items[item_id.into_raw()];
                let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
                if item_name.as_deref() == Some(&param_str) {
                    let byte_size = item.storage.byte_size;
                    let scale = item.storage.picture.as_ref().map(|p| p.scale).unwrap_or(0);
                    let dot_pos = item
                        .storage
                        .picture
                        .as_ref()
                        .map(|p| Self::pic_dot_position(&p.pic_string))
                        .unwrap_or(-1);
                    self.linkage_map.insert(
                        param_str.clone(),
                        LinkageInfo {
                            param_value: Value::from_raw(i as u32),
                            byte_size,
                            scale,
                            dot_pos,
                        },
                    );
                    break;
                }
            }
        }
    }

    fn lower_globals(&mut self) {
        // Pre-scan: detect which names appear more than once AND compute parent groups
        // by tracking level numbers.
        let mut name_counts: std::collections::HashMap<String, u32> =
            std::collections::HashMap::new();
        // Build (level, name) list and determine parent for each item
        let ws_info: Vec<(u8, String)> = self
            .hir
            .working_storage
            .iter()
            .map(|&id| {
                let it = &self.hir.data_items[id.into_raw()];
                let n = it
                    .name
                    .map(|n| self.interner.resolve(n).to_string())
                    .unwrap_or_default();
                (it.level, n)
            })
            .collect();

        // Count name occurrences (excluding 88-level)
        for &item_id in self.hir.working_storage.iter() {
            let item = &self.hir.data_items[item_id.into_raw()];
            if item.level == 88 {
                continue;
            }
            if let Some(n) = item.name {
                let name = self.interner.resolve(n).to_string();
                *name_counts.entry(name).or_insert(0) += 1;
            }
        }

        // Determine the immediate parent group for each item using level numbers.
        // Stack tracks (level, qualified_name) of parent groups.
        // When a name is a duplicate, we push its qualified form (name.parent)
        // so that children get unique qualified names like WS-FIELD-X.WS-INNER-A.WS-SRC-NESTED.
        let mut parent_stack: Vec<(u8, String)> = Vec::new();
        let mut parent_for_item: Vec<String> = Vec::new();
        for (level, ref name) in &ws_info {
            let level = *level;
            // Pop stack until we find a parent with a lower level number
            while let Some(&(stack_level, _)) = parent_stack.last() {
                if stack_level >= level {
                    parent_stack.pop();
                } else {
                    break;
                }
            }
            let parent_name = parent_stack
                .last()
                .map(|(_, n)| n.clone())
                .unwrap_or_default();
            parent_for_item.push(parent_name.clone());
            // Push this item as a potential parent for subsequent items.
            // Use qualified form if the name is a duplicate so that children
            // get unique multi-level qualified names.
            if !name.is_empty() {
                let is_dup = name_counts.get(name).is_some_and(|&c| c > 1);
                let push_name = if is_dup && !parent_name.is_empty() {
                    format!("{}.{}", name, parent_name)
                } else {
                    name.clone()
                };
                parent_stack.push((level, push_name));
            }
        }

        let mut offset = 0u32;
        // Collect the handle_name assigned to each working-storage item so that
        // the second and third passes can look up FILLER items by their unique
        // generated name instead of the empty string.
        let mut ws_handle_names: Vec<String> = Vec::new();
        for (idx, &item_id) in self.hir.working_storage.iter().enumerate() {
            let item = &self.hir.data_items[item_id.into_raw()];
            let name = item
                .name
                .map(|n| self.interner.resolve(n).to_string())
                .unwrap_or_else(|| format!("FILLER_{}", offset));

            // Skip 88-level items as globals (they are condition names, not storage)
            if item.level == 88 {
                ws_handle_names.push(String::new());
                continue;
            }

            // Determine if this name needs qualification (appears more than once)
            let is_duplicate = name_counts.get(&name).is_some_and(|&c| c > 1);
            let parent_name_str = &parent_for_item[idx];

            // Build group_children map
            if !parent_name_str.is_empty() {
                self.group_children
                    .entry(parent_name_str.clone())
                    .or_default()
                    .push(name.clone());
            }

            let handle_name = if is_duplicate && !parent_name_str.is_empty() {
                let qualified = format!("{}.{}", name, parent_name_str);
                self.qualified_names
                    .entry(name.clone())
                    .or_default()
                    .push((qualified.clone(), parent_name_str.clone()));
                qualified
            } else {
                name.clone()
            };
            ws_handle_names.push(handle_name.clone());

            let byte_size = item.storage.byte_size;

            // Track OCCURS element size for subscript handling, and INDEXED BY names.
            // For group OCCURS items, compute_group_sizes() sets byte_size to
            // the single-element size (group_sum), NOT multiplied by occurs.
            // For elementary OCCURS items, the HIR already multiplied byte_size
            // by occ.max, so we divide to recover the single-element size.
            if let Some(ref occ) = item.occurs {
                if occ.max > 1 && byte_size > 0 {
                    let elem_size = if item.is_group {
                        byte_size // already the single-element size for groups
                    } else {
                        byte_size / occ.max // undo the HIR multiplication for elementary items
                    };
                    self.occurs_element_size
                        .insert(handle_name.clone(), elem_size);
                    // Track index names
                    for &idx_name in &occ.indexed_by {
                        let idx_str = self.interner.resolve(idx_name).to_string();
                        self.index_info
                            .insert(idx_str, (handle_name.clone(), occ.max, elem_size));
                    }
                }
            }

            // Handle REDEFINES: use the same offset as the redefined item
            let item_offset = if let Some(redef_id) = item.redefines {
                let redef_item = &self.hir.data_items[redef_id.into_raw()];
                let redef_name = redef_item
                    .name
                    .map(|n| self.interner.resolve(n).to_string())
                    .unwrap_or_default();
                // Find the offset of the redefined item in our globals
                if let Some(&redef_idx) = self.global_map.get(&redef_name) {
                    self.module.globals[redef_idx].offset
                } else {
                    offset
                }
            } else {
                offset
            };

            // For group OCCURS items, byte_size is the single-element size (set by
            // compute_group_sizes), so multiply by occurs_max for total allocation.
            // For elementary OCCURS items, byte_size already includes the occurs
            // multiplier from the HIR.
            let occurs_max = item.occurs.as_ref().map(|o| o.max).unwrap_or(1);
            let alloc_size = if item.is_group && occurs_max > 1 {
                byte_size * occurs_max
            } else {
                byte_size
            };
            let mir_ty = if alloc_size == 0 {
                MirType::Bytes(1) // Minimum 1 byte for group items
            } else {
                MirType::Bytes(alloc_size)
            };

            let pic_str = item.storage.picture.as_ref().map(|p| p.pic_string.as_str());
            let encoding = &item.storage.encoding;
            let pic_digits = item.storage.picture.as_ref().map(|p| p.size).unwrap_or(0);
            let pic_is_signed = item
                .storage
                .picture
                .as_ref()
                .map(|p| p.sign != cobol_hir::SignPosition::None)
                .unwrap_or(false);
            let initial_value = item.value.as_ref().map(|v| {
                self.lower_initial_value(v, byte_size, pic_str, encoding, pic_digits, pic_is_signed)
            });

            let redefines_name = item.redefines.map(|redef_id| {
                let redef_item = &self.hir.data_items[redef_id.into_raw()];
                redef_item
                    .name
                    .map(|n| self.interner.resolve(n).to_string())
                    .unwrap_or_default()
            });

            let global = MirGlobal {
                name: handle_name.clone(),
                ty: mir_ty,
                initial_value,
                offset: item_offset,
                redefines: redefines_name,
                parent_offset: None,
            };

            self.global_map
                .insert(handle_name, self.module.globals.len());
            self.module.globals.push(global);

            // Only advance offset if this is not a REDEFINES
            if item.redefines.is_none() {
                offset += if alloc_size == 0 { 1 } else { alloc_size };
            }
        }

        // Second pass: propagate occurs_element_size to child items.
        // In COBOL, child items of an OCCURS group (e.g. WS-VALUE under
        // WS-ENTRY OCCURS 5) can be subscripted, but the OCCURS clause is
        // on the parent. We walk the working_storage list using level numbers
        // to find children of OCCURS items and register them too.
        // Also mark children as REDEFINES of the parent so they share memory,
        // and set parent_offset for their intra-element byte offset within
        // multi-item group OCCURS entries.
        {
            // Collect (level, handle_name, has_occurs, byte_size, is_group)
            let ws_ids: Vec<cobol_hir::DataItemId> = self.hir.working_storage.to_vec();
            let ws_items: Vec<(u8, String, bool, u32, bool)> = ws_ids
                .iter()
                .enumerate()
                .map(|(idx, &id)| {
                    let it = &self.hir.data_items[id.into_raw()];
                    let n = ws_handle_names[idx].clone();
                    let has_occurs = it.occurs.as_ref().is_some_and(|o| o.max > 1);
                    let byte_size = it.storage.byte_size;
                    let is_group = it.is_group;
                    (it.level, n, has_occurs, byte_size, is_group)
                })
                .collect();

            let mut i = 0;
            while i < ws_items.len() {
                let (parent_level, ref parent_name, has_occurs, _, parent_is_group) = ws_items[i];
                if has_occurs {
                    if let Some(&elem_size) = self.occurs_element_size.get(parent_name) {
                        // Walk subsequent items that are subordinate (higher level number)
                        // and compute their intra-element byte offsets (for group parents).
                        let mut j = i + 1;
                        let mut child_offset = 0u32;
                        while j < ws_items.len() {
                            let (child_level, ref child_name, _, child_byte_size, _) = ws_items[j];
                            if child_level <= parent_level {
                                break; // no longer subordinate
                            }
                            // Check if this is a direct child of the OCCURS group
                            let is_direct_child = {
                                let mut direct = true;
                                for item in ws_items.iter().take(j).skip(i + 1) {
                                    let (inter_level, _, _, _, inter_is_group) = item;
                                    if *inter_level <= parent_level {
                                        break;
                                    }
                                    if *inter_is_group && *inter_level < child_level {
                                        direct = false;
                                        break;
                                    }
                                }
                                direct
                            };
                            if !child_name.is_empty() {
                                // Try qualified name first, then simple name
                                let global_key = {
                                    let qualified = format!("{}.{}", child_name, parent_name);
                                    if self.global_map.contains_key(&qualified) {
                                        qualified
                                    } else {
                                        child_name.clone()
                                    }
                                };
                                self.occurs_element_size
                                    .entry(global_key.clone())
                                    .or_insert(elem_size);
                                // Make child share the parent's memory (like REDEFINES)
                                // and set its intra-element offset within the group.
                                if let Some(&child_idx) = self.global_map.get(&global_key) {
                                    self.module.globals[child_idx].redefines =
                                        Some(parent_name.clone());
                                    // For group OCCURS parents with multiple children,
                                    // set the child's offset within the group element so
                                    // subscript addressing lands on the correct field.
                                    if parent_is_group && is_direct_child {
                                        self.module.globals[child_idx].parent_offset =
                                            Some((parent_name.clone(), child_offset));
                                    }
                                }
                            }
                            // Advance offset for direct children only
                            if is_direct_child {
                                let child_id = ws_ids[j];
                                let child_item = &self.hir.data_items[child_id.into_raw()];
                                let child_occurs =
                                    child_item.occurs.as_ref().map(|o| o.max).unwrap_or(1);
                                child_offset += child_byte_size * child_occurs;
                            }
                            j += 1;
                        }
                    }
                }
                i += 1;
            }
        }

        // Third pass: establish parent-child memory sharing for group items.
        // Children of a group share the parent's memory at computed offsets.
        // This enables group MOVEs (e.g., MOVE WS-GROUP-A TO WS-GROUP-B).
        {
            let ws_items: Vec<(u8, String, bool, u32)> = self
                .hir
                .working_storage
                .iter()
                .enumerate()
                .map(|(idx, &id)| {
                    let it = &self.hir.data_items[id.into_raw()];
                    // Use the handle name from the first pass (includes unique
                    // FILLER_<offset> names) instead of re-deriving from HIR.
                    let n = ws_handle_names[idx].clone();
                    let is_group = it.is_group;
                    let byte_size = it.storage.byte_size;
                    (it.level, n, is_group, byte_size)
                })
                .collect();

            for i in 0..ws_items.len() {
                let (parent_level, ref parent_name, is_group, _parent_size) = ws_items[i];
                if !is_group || parent_name.is_empty() {
                    continue;
                }
                // Skip if parent is an OCCURS group (those use redefines mechanism already)
                if self.occurs_element_size.contains_key(parent_name) {
                    continue;
                }

                // Walk children and compute offsets
                let mut child_offset = 0u32;
                let mut j = i + 1;
                while j < ws_items.len() {
                    let (child_level, ref child_name, _child_is_group, child_byte_size) =
                        ws_items[j];
                    if child_level <= parent_level {
                        break; // no longer subordinate
                    }
                    // Skip 88-level condition names
                    if child_level == 88 {
                        j += 1;
                        continue;
                    }

                    // Check if this is a direct child (not nested under an intermediate group)
                    let is_direct_child = {
                        let mut direct = true;
                        for item in ws_items.iter().take(j).skip(i + 1) {
                            let (inter_level, _, inter_is_group, _) = item;
                            if *inter_level <= parent_level {
                                break;
                            }
                            if *inter_is_group && *inter_level < child_level {
                                direct = false;
                                break;
                            }
                        }
                        direct
                    };

                    if is_direct_child && !child_name.is_empty() {
                        // Use the handle name directly as the global key
                        // (it is already qualified or unique for FILLER items)
                        let global_key = {
                            let qualified = format!("{}.{}", child_name, parent_name);
                            if self.global_map.contains_key(&qualified) {
                                qualified
                            } else {
                                child_name.clone()
                            }
                        };
                        // Set parent_offset on the child global
                        if let Some(&child_idx) = self.global_map.get(&global_key) {
                            // Only set if not already redefines something
                            if self.module.globals[child_idx].redefines.is_none() {
                                self.module.globals[child_idx].parent_offset =
                                    Some((parent_name.clone(), child_offset));
                            }
                        }
                        // Advance offset by child's effective size
                        let occurs_max = self
                            .hir
                            .working_storage
                            .iter()
                            .find_map(|&id| {
                                let it = &self.hir.data_items[id.into_raw()];
                                let n = it.name.map(|n| self.interner.resolve(n).to_string());
                                if n.as_deref() == Some(child_name) {
                                    Some(it.occurs.as_ref().map(|o| o.max).unwrap_or(1))
                                } else {
                                    None
                                }
                            })
                            .unwrap_or(1);
                        child_offset += child_byte_size * occurs_max;
                    }
                    j += 1;
                }
            }
        }

        // Create globals for INDEXED BY index names (8-byte i64 each).
        // Initialized to 0, which is the byte offset for logical position 1:
        // (1 - 1) * element_size = 0.
        let idx_names: Vec<String> = self.index_info.keys().cloned().collect();
        for idx_name in idx_names {
            if !self.global_map.contains_key(&idx_name) {
                let global = MirGlobal {
                    name: idx_name.clone(),
                    ty: MirType::I64,
                    initial_value: Some(MirConst::Int(0)),
                    offset,
                    redefines: None,
                    parent_offset: None,
                };
                self.global_map.insert(idx_name, self.module.globals.len());
                self.module.globals.push(global);
                offset += 8;
            }
        }
    }

    /// Compute the dot position in a PIC string (byte offset of '.' in storage).
    /// Returns -1 if the PIC has no actual decimal point.
    fn pic_dot_position(pic_str: &str) -> i32 {
        let upper = pic_str.to_ascii_uppercase();
        let chars: Vec<char> = upper.chars().collect();
        let mut pos = 0i32;
        let mut i = 0;
        while i < chars.len() {
            let ch = chars[i];
            let count: i32 = if i + 1 < chars.len() && chars[i + 1] == '(' {
                let start = i + 2;
                if let Some(end_off) = chars[start..].iter().position(|&c| c == ')') {
                    let end = start + end_off;
                    let n: i32 = upper[start..end].parse().unwrap_or(1);
                    i = end + 1;
                    n
                } else {
                    i += 1;
                    1
                }
            } else {
                i += 1;
                1
            };
            if ch == '.' {
                return pos;
            }
            if ch == 'V' || ch == 'S' {
                // V and S don't take storage space
                continue;
            }
            pos += count;
        }
        -1
    }

    /// Build the "zero" template for an edited numeric PIC (includes '.').
    fn build_edited_zero_template(pic_str: &str, byte_size: u32) -> Vec<u8> {
        let dot_pos = Self::pic_dot_position(pic_str);
        let mut bytes = vec![b'0'; byte_size as usize];
        if dot_pos >= 0 && (dot_pos as usize) < bytes.len() {
            bytes[dot_pos as usize] = b'.';
        }
        bytes
    }

    /// Convert an HIR initial value to a MIR constant, respecting the
    /// storage encoding (Display, Binary/COMP, or Packed-Decimal/COMP-3).
    fn lower_initial_value(
        &self,
        val: &cobol_hir::InitialValue,
        byte_size: u32,
        pic_str: Option<&str>,
        _encoding: &cobol_hir::DataEncoding,
        _pic_digits: u32,
        _pic_is_signed: bool,
    ) -> MirConst {
        let has_dot = pic_str.map(|p| p.contains('.')).unwrap_or(false);

        // Helper: encode an integer value in display format.
        // NOTE: COMP/COMP-3 binary encoding is deferred until the full
        // MOVE/arithmetic pipeline supports encoding-aware operations.
        // For now, all fields use display format at the storage level.
        let encode_numeric = |n: i64| -> MirConst {
            let is_negative = n < 0;
            let is_signed = pic_str
                .map(|p| p.to_ascii_uppercase().contains('S'))
                .unwrap_or(false);
            let num_str = format!("{}", n.unsigned_abs());
            if has_dot {
                let pic = pic_str.unwrap();
                let mut bytes = Self::build_edited_zero_template(pic, byte_size);
                let digit_positions: Vec<usize> =
                    (0..bytes.len()).filter(|&i| bytes[i] != b'.').collect();
                let num_bytes = num_str.as_bytes();
                for (j, &pos) in digit_positions.iter().rev().enumerate() {
                    if j < num_bytes.len() {
                        bytes[pos] = num_bytes[num_bytes.len() - 1 - j];
                    }
                }
                if is_signed && is_negative && !bytes.is_empty() && bytes[0] == b'0' {
                    bytes[0] = b'-';
                }
                MirConst::Bytes(bytes)
            } else {
                let mut bytes = vec![b'0'; byte_size as usize];
                let start = if num_str.len() < byte_size as usize {
                    byte_size as usize - num_str.len()
                } else {
                    0
                };
                for (i, b) in num_str.as_bytes().iter().enumerate() {
                    if start + i < byte_size as usize {
                        bytes[start + i] = *b;
                    }
                }
                if is_signed && is_negative && !bytes.is_empty() && bytes[0] == b'0' {
                    bytes[0] = b'-';
                }
                MirConst::Bytes(bytes)
            }
        };

        match val {
            cobol_hir::InitialValue::String_(s) => {
                // Pad or truncate to byte_size
                let mut bytes = s.as_bytes().to_vec();
                bytes.resize(byte_size as usize, b' ');
                MirConst::Bytes(bytes)
            }
            cobol_hir::InitialValue::Numeric(n, _scale) => encode_numeric(*n),
            cobol_hir::InitialValue::Zero => encode_numeric(0),
            cobol_hir::InitialValue::Space => MirConst::Bytes(vec![b' '; byte_size as usize]),
            cobol_hir::InitialValue::HighValue => MirConst::Bytes(vec![0xFF; byte_size as usize]),
            cobol_hir::InitialValue::LowValue => MirConst::Bytes(vec![0x00; byte_size as usize]),
            cobol_hir::InitialValue::Quote => MirConst::Bytes(vec![b'"'; byte_size as usize]),
            cobol_hir::InitialValue::All(s) => {
                let pat = s.as_bytes();
                let mut bytes = vec![0u8; byte_size as usize];
                if !pat.is_empty() {
                    for i in 0..byte_size as usize {
                        bytes[i] = pat[i % pat.len()];
                    }
                }
                MirConst::Bytes(bytes)
            }
            cobol_hir::InitialValue::Figurative(_) => MirConst::Null,
        }
    }

    fn lower_procedure_division(&mut self) {
        // A program is a subprogram if:
        // 1. It has USING parameters, OR
        // 2. It was explicitly compiled as a subprogram (is_main_program == false)
        let is_subprogram = !self.is_main_program || !self.hir.using_params.is_empty();

        // Subprograms: function named after PROGRAM-ID, with pointer params
        // Main programs: function named "_cobol_main", no params
        let (func_name, params, num_params) = if is_subprogram {
            let name = self
                .interner
                .resolve(self.hir.program_name)
                .to_string()
                .replace('-', "_");
            let params: Vec<(String, MirType)> = self
                .hir
                .using_params
                .iter()
                .map(|&n| (self.interner.resolve(n).to_string(), MirType::Pointer))
                .collect();
            let n = params.len() as u32;
            (name, params, n)
        } else {
            ("_cobol_main".to_string(), Vec::new(), 0)
        };

        let entry_block = BlockId::from_raw(0);
        let mut func = MirFunction {
            name: func_name,
            params,
            return_type: MirType::Void,
            blocks: Vec::new(),
            entry_block,
            next_value: num_params, // Reserve 0..N-1 for parameter Values
            next_block: 1,
        };

        // Pre-allocate a block ID for each named paragraph so GO TO and
        // out-of-line PERFORM can reference them before or after the
        // paragraph is defined.
        let needs_paragraph_blocks = self.hir.paragraphs.iter().any(|p| {
            p.statements
                .iter()
                .any(|s| self.stmt_needs_paragraph_blocks(s))
        });

        if needs_paragraph_blocks {
            for para in &self.hir.paragraphs {
                let para_name = self.interner.resolve(para.name).to_string();
                let block_id = func.new_block();
                self.paragraph_block_map.insert(para_name, block_id);
            }
        }

        // Pre-allocate an epilogue block that EXIT PARAGRAPH / EXIT SECTION
        // can jump to when there is no subsequent paragraph or section.
        let epilogue_block = if needs_paragraph_blocks {
            Some(func.new_block())
        } else {
            None
        };

        // Build a mapping from paragraph index to its section exit block.
        // For each section, the exit block is the block of the first paragraph
        // AFTER the section, or the epilogue block if the section is last.
        let para_names: Vec<String> = self
            .hir
            .paragraphs
            .iter()
            .map(|p| self.interner.resolve(p.name).to_string())
            .collect();
        let mut para_section_exit: Vec<Option<BlockId>> = vec![None; self.hir.paragraphs.len()];
        for section in &self.hir.sections {
            if section.paragraphs.is_empty() {
                continue;
            }
            // The section exit is the block of the paragraph immediately after
            // the last paragraph in this section, or the epilogue.
            let last_para_idx = *section.paragraphs.last().unwrap();
            let section_exit = if last_para_idx + 1 < para_names.len() {
                self.paragraph_block_map
                    .get(&para_names[last_para_idx + 1])
                    .copied()
                    .or(epilogue_block)
            } else {
                epilogue_block
            };
            for &pidx in &section.paragraphs {
                para_section_exit[pidx] = section_exit;
            }
        }

        let mut current_block_id = entry_block;
        let mut instructions = Vec::new();

        for (para_idx, para) in self.hir.paragraphs.iter().enumerate() {
            let para_name = self.interner.resolve(para.name).to_string();

            // If paragraphs have block IDs assigned, finalize the current block
            // with a fallthrough Goto to this paragraph's block.
            if let Some(&para_block) = self.paragraph_block_map.get(&para_name) {
                let current_insts = std::mem::take(&mut instructions);
                func.blocks.push(BasicBlock {
                    id: current_block_id,
                    params: Vec::new(),
                    instructions: current_insts,
                    terminator: Terminator::Goto(para_block),
                });
                current_block_id = para_block;
            }

            // Set the EXIT PARAGRAPH target: jump to the next paragraph's
            // block, or the epilogue if this is the last paragraph.
            if needs_paragraph_blocks {
                let next_exit = if para_idx + 1 < para_names.len() {
                    self.paragraph_block_map
                        .get(&para_names[para_idx + 1])
                        .copied()
                        .or(epilogue_block)
                } else {
                    epilogue_block
                };
                self.current_paragraph_exit = next_exit;
            }

            // Set the EXIT SECTION target from the pre-computed map.
            // If the paragraph is not inside any section, EXIT SECTION
            // behaves like EXIT PARAGRAPH (jumps to next paragraph / epilogue).
            self.current_section_exit = if para_idx < para_section_exit.len() {
                para_section_exit[para_idx].or(self.current_paragraph_exit)
            } else {
                self.current_paragraph_exit
            };

            for stmt in &para.statements {
                self.lower_statement_to_blocks(
                    stmt,
                    &mut func,
                    &mut current_block_id,
                    &mut instructions,
                );
            }
        }

        // Clear the exit targets after the loop.
        self.current_paragraph_exit = None;
        self.current_section_exit = None;

        // If we allocated an epilogue block, finalize the last paragraph's
        // block with a fallthrough Goto to the epilogue, then start it.
        if let Some(epi) = epilogue_block {
            let current_insts = std::mem::take(&mut instructions);
            func.blocks.push(BasicBlock {
                id: current_block_id,
                params: Vec::new(),
                instructions: current_insts,
                terminator: Terminator::Goto(epi),
            });
            current_block_id = epi;
        }

        // Main programs get an implicit STOP RUN if none found
        if !is_subprogram {
            let has_stop = self.hir.paragraphs.iter().any(|p| {
                p.statements.iter().any(|s| {
                    matches!(
                        s,
                        cobol_hir::HirStatement::StopRun | cobol_hir::HirStatement::GoBack
                    )
                })
            });

            if !has_stop {
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_stop_run".to_string(),
                    args: vec![zero],
                });
            }
        }

        // Finalize the last block (or the epilogue block)
        let block = BasicBlock {
            id: current_block_id,
            params: Vec::new(),
            instructions,
            terminator: Terminator::Return,
        };
        func.blocks.push(block);

        self.module.functions.push(func);
    }

    /// Check if a statement (or any nested statement) contains a GO TO or
    /// an out-of-line PERFORM, which requires paragraph block pre-allocation.
    fn stmt_needs_paragraph_blocks(&self, stmt: &cobol_hir::HirStatement) -> bool {
        match stmt {
            cobol_hir::HirStatement::GoTo { .. } => true,
            cobol_hir::HirStatement::GoToDependingOn { .. } => true,
            cobol_hir::HirStatement::ExitParagraph => true,
            cobol_hir::HirStatement::ExitSection => true,
            cobol_hir::HirStatement::Perform(pt) => matches!(
                pt,
                cobol_hir::PerformType::OutOfLine { .. }
                    | cobol_hir::PerformType::Times { .. }
                    | cobol_hir::PerformType::Until { .. }
                    | cobol_hir::PerformType::Varying {
                        inline_body: None,
                        ..
                    }
            ),
            cobol_hir::HirStatement::If {
                then_branch,
                else_branch,
                ..
            } => {
                then_branch
                    .iter()
                    .any(|s| self.stmt_needs_paragraph_blocks(s))
                    || else_branch
                        .as_ref()
                        .is_some_and(|eb| eb.iter().any(|s| self.stmt_needs_paragraph_blocks(s)))
            }
            cobol_hir::HirStatement::Evaluate { whens, .. } => whens.iter().any(|w| {
                w.statements
                    .iter()
                    .any(|s| self.stmt_needs_paragraph_blocks(s))
            }),
            _ => false,
        }
    }

    /// Lower a statement. If the statement needs control flow (IF, PERFORM),
    /// the current block is finalized and new blocks are created. The
    /// `current_block_id` and `instructions` are updated to the continuation block.
    fn lower_statement_to_blocks(
        &mut self,
        stmt: &cobol_hir::HirStatement,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        match stmt {
            cobol_hir::HirStatement::Display { args, no_advancing } => {
                self.lower_display(args, *no_advancing, func, instructions);
            }
            cobol_hir::HirStatement::StopRun => {
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_stop_run".to_string(),
                    args: vec![zero],
                });
            }
            cobol_hir::HirStatement::GoBack => {
                // GOBACK returns from the current program.
                // In a subprogram, this is a function return.
                // In a main program, it's equivalent to STOP RUN.
                let is_subprogram = !self.is_main_program || !self.hir.using_params.is_empty();
                if !is_subprogram {
                    // Main program: GOBACK = STOP RUN
                    let zero = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: zero,
                        value: MirConst::Int(0),
                    });
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_stop_run".to_string(),
                        args: vec![zero],
                    });
                } else {
                    // Subprogram: GOBACK = return to caller
                    let current_insts = std::mem::take(instructions);
                    func.blocks.push(BasicBlock {
                        id: *current_block_id,
                        params: Vec::new(),
                        instructions: current_insts,
                        terminator: Terminator::Return,
                    });
                    *current_block_id = func.new_block();
                }
            }
            cobol_hir::HirStatement::Move { from, to } => {
                self.lower_move(from, to, func, instructions);
            }
            cobol_hir::HirStatement::Add {
                operands,
                to,
                giving,
                on_size_error,
                not_on_size_error,
                ..
            } => {
                let has_handlers = on_size_error.is_some() || not_on_size_error.is_some();
                self.emit_clear_size_error(func, instructions);
                if giving.is_some() {
                    self.lower_add(operands, giving.as_deref(), func, instructions);
                } else if !to.is_empty() {
                    self.lower_add_to(operands, to, func, instructions);
                }
                if has_handlers {
                    self.lower_size_error_check(
                        on_size_error.as_deref().unwrap_or(&[]),
                        not_on_size_error.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::Subtract {
                operands,
                from: targets,
                giving,
                on_size_error,
                not_on_size_error,
                ..
            } => {
                let has_handlers = on_size_error.is_some() || not_on_size_error.is_some();
                self.emit_clear_size_error(func, instructions);
                self.lower_subtract(operands, targets, giving.as_deref(), func, instructions);
                if has_handlers {
                    self.lower_size_error_check(
                        on_size_error.as_deref().unwrap_or(&[]),
                        not_on_size_error.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::Multiply {
                operand1,
                by,
                giving,
                on_size_error,
                not_on_size_error,
                ..
            } => {
                let has_handlers = on_size_error.is_some() || not_on_size_error.is_some();
                self.emit_clear_size_error(func, instructions);
                self.lower_multiply(operand1, by, giving.as_deref(), func, instructions);
                if has_handlers {
                    self.lower_size_error_check(
                        on_size_error.as_deref().unwrap_or(&[]),
                        not_on_size_error.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::Divide {
                operand1,
                into_or_by,
                giving,
                remainder,
                on_size_error,
                not_on_size_error,
                rounded,
                is_into,
            } => {
                let has_handlers = on_size_error.is_some() || not_on_size_error.is_some();
                self.emit_clear_size_error(func, instructions);
                self.lower_divide(
                    operand1,
                    into_or_by,
                    giving.as_deref(),
                    remainder.as_ref(),
                    *rounded,
                    *is_into,
                    func,
                    instructions,
                );
                if has_handlers {
                    self.lower_size_error_check(
                        on_size_error.as_deref().unwrap_or(&[]),
                        not_on_size_error.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.lower_if(
                    condition,
                    then_branch,
                    else_branch.as_deref(),
                    func,
                    current_block_id,
                    instructions,
                );
            }
            cobol_hir::HirStatement::Perform(perform_type) => {
                self.lower_perform(perform_type, func, current_block_id, instructions);
            }
            cobol_hir::HirStatement::StringStmt {
                sources,
                into,
                pointer,
                on_overflow,
                not_on_overflow,
            } => {
                let has_handlers = on_overflow.is_some() || not_on_overflow.is_some();
                self.emit_clear_size_error(func, instructions);
                self.lower_string(sources, into, pointer.as_ref(), func, instructions);
                if has_handlers {
                    self.lower_size_error_check(
                        on_overflow.as_deref().unwrap_or(&[]),
                        not_on_overflow.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::Call {
                program,
                using,
                returning,
                on_exception,
                not_on_exception,
            } => {
                self.lower_call(program, using, returning, func, instructions);
                // After the call, NOT ON EXCEPTION handlers execute (call succeeded).
                // ON EXCEPTION handlers would execute only on call failure.
                // Since our AOT compiler links all subprograms at compile time,
                // a successful link means the call will succeed, so we emit
                // NOT ON EXCEPTION statements unconditionally and skip ON EXCEPTION.
                if !not_on_exception.is_empty() {
                    for stmt in not_on_exception {
                        self.lower_statement_to_blocks(stmt, func, current_block_id, instructions);
                    }
                }
                // ON EXCEPTION: in our AOT model, a linked call cannot fail at
                // runtime, so these are dead code. We skip them intentionally.
                let _ = on_exception;
            }
            cobol_hir::HirStatement::ExitProgram => {
                // Finalize current block with Return (returns to caller)
                let current_insts = std::mem::take(instructions);
                func.blocks.push(BasicBlock {
                    id: *current_block_id,
                    params: Vec::new(),
                    instructions: current_insts,
                    terminator: Terminator::Return,
                });
                // Create continuation block (may be dead code after EXIT PROGRAM)
                *current_block_id = func.new_block();
            }
            cobol_hir::HirStatement::Open { file, mode } => {
                self.lower_file_open(*file, *mode, func, instructions);
            }
            cobol_hir::HirStatement::Close { file } => {
                self.lower_file_close(*file, func, instructions);
            }
            cobol_hir::HirStatement::Write {
                record,
                from,
                advancing,
            } => {
                self.lower_file_write(*record, *from, *advancing, func, instructions);
            }
            cobol_hir::HirStatement::Read {
                file, into, at_end, ..
            } => {
                self.lower_file_read(*file, *into, at_end, func, current_block_id, instructions);
            }
            cobol_hir::HirStatement::Rewrite { record, .. } => {
                self.lower_file_rewrite(*record, func, instructions);
            }
            cobol_hir::HirStatement::Delete { file, .. } => {
                self.lower_file_delete(*file, func, instructions);
            }
            cobol_hir::HirStatement::Start { file, .. } => {
                self.lower_file_start(*file, func, instructions);
            }
            cobol_hir::HirStatement::Evaluate { subjects, whens } => {
                self.lower_evaluate(subjects, whens, func, current_block_id, instructions);
            }
            cobol_hir::HirStatement::GoTo { target } => {
                let target_name = self.interner.resolve(*target).to_string();
                if let Some(&target_block) = self.paragraph_block_map.get(&target_name) {
                    // Finalize current block with Goto to target paragraph
                    let current_insts = std::mem::take(instructions);
                    func.blocks.push(BasicBlock {
                        id: *current_block_id,
                        params: Vec::new(),
                        instructions: current_insts,
                        terminator: Terminator::Goto(target_block),
                    });
                    // Create a continuation block (dead code after GO TO,
                    // but needed to keep the builder consistent)
                    *current_block_id = func.new_block();
                }
            }
            cobol_hir::HirStatement::GoToDependingOn { targets, index } => {
                // GO TO para-1 para-2 ... para-n DEPENDING ON idx
                // COBOL semantics: idx=1 → para-1, idx=2 → para-2, ...
                // If idx < 1 or idx > n, fall through to next statement.

                // Load the index variable as an integer.
                let index_val = if let Some((gname, size)) = self.resolve_data_ref(index) {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: addr,
                        name: gname,
                    });
                    let sz = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sz,
                        value: MirConst::Int(size as i64),
                    });
                    let int_val = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(int_val),
                        func: "cobolrt_display_to_int".to_string(),
                        args: vec![addr, sz],
                    });
                    int_val
                } else {
                    // Fallback: constant 0 (will fall through)
                    let v = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: v,
                        value: MirConst::Int(0),
                    });
                    v
                };

                // Build the switch cases: 1-based index to block mapping.
                let fallthrough_block = func.new_block();
                let mut cases: Vec<(i64, BlockId)> = Vec::new();
                for (i, target) in targets.iter().enumerate() {
                    let target_name = self.interner.resolve(*target).to_string();
                    if let Some(&target_block) = self.paragraph_block_map.get(&target_name) {
                        // COBOL is 1-based: case value = i + 1
                        cases.push(((i as i64) + 1, target_block));
                    }
                }

                // Finalize the current block with a Switch terminator.
                // Default (out-of-range) falls through to the next statement.
                let current_insts = std::mem::take(instructions);
                func.blocks.push(BasicBlock {
                    id: *current_block_id,
                    params: Vec::new(),
                    instructions: current_insts,
                    terminator: Terminator::Switch {
                        value: index_val,
                        cases,
                        default: fallthrough_block,
                    },
                });

                *current_block_id = fallthrough_block;
            }
            cobol_hir::HirStatement::Compute {
                targets,
                expr,
                on_size_error,
                not_on_size_error,
                rounded,
            } => {
                let has_handlers = on_size_error.is_some() || not_on_size_error.is_some();
                self.emit_clear_size_error(func, instructions);
                self.lower_compute(targets, expr, *rounded, func, instructions);
                if has_handlers {
                    self.lower_size_error_check(
                        on_size_error.as_deref().unwrap_or(&[]),
                        not_on_size_error.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::Accept { target, source } => {
                self.lower_accept(target, *source, func, instructions);
            }
            cobol_hir::HirStatement::Inspect {
                target,
                inspect_type,
            } => {
                self.lower_inspect(target, inspect_type, func, instructions);
            }
            cobol_hir::HirStatement::Unstring {
                source,
                delimiters,
                targets,
                pointer,
                tallying,
            } => {
                self.lower_unstring(
                    source,
                    delimiters,
                    targets,
                    pointer.as_ref(),
                    tallying.as_ref(),
                    func,
                    instructions,
                );
            }
            cobol_hir::HirStatement::Initialize { targets, replacing } => {
                self.lower_initialize(targets, replacing, func, instructions);
            }
            cobol_hir::HirStatement::Set { target, action } => {
                self.lower_set(target, action, func, instructions);
            }
            cobol_hir::HirStatement::Search {
                table,
                at_end,
                whens,
            } => {
                self.lower_search(table, at_end, whens, func, current_block_id, instructions);
            }
            cobol_hir::HirStatement::MoveCorresponding { from, to } => {
                self.lower_move_corresponding(*from, *to, func, instructions);
            }
            cobol_hir::HirStatement::AddCorresponding {
                from,
                to,
                on_size_error,
                not_on_size_error,
            } => {
                let has_handlers = on_size_error.is_some() || not_on_size_error.is_some();
                self.emit_clear_size_error(func, instructions);
                self.lower_add_corresponding(*from, *to, func, instructions);
                if has_handlers {
                    self.lower_size_error_check(
                        on_size_error.as_deref().unwrap_or(&[]),
                        not_on_size_error.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::SubtractCorresponding {
                from,
                to,
                on_size_error,
                not_on_size_error,
            } => {
                let has_handlers = on_size_error.is_some() || not_on_size_error.is_some();
                self.emit_clear_size_error(func, instructions);
                self.lower_subtract_corresponding(*from, *to, func, instructions);
                if has_handlers {
                    self.lower_size_error_check(
                        on_size_error.as_deref().unwrap_or(&[]),
                        not_on_size_error.as_deref().unwrap_or(&[]),
                        func,
                        current_block_id,
                        instructions,
                    );
                }
            }
            cobol_hir::HirStatement::ExitParagraph => {
                // EXIT PARAGRAPH: jump to the end of the current paragraph.
                // This terminates the current basic block and creates a new
                // (dead-code) continuation block for any remaining statements.
                if let Some(exit_block) = self.current_paragraph_exit {
                    let current_insts = std::mem::take(instructions);
                    func.blocks.push(BasicBlock {
                        id: *current_block_id,
                        params: Vec::new(),
                        instructions: current_insts,
                        terminator: Terminator::Goto(exit_block),
                    });
                    // Create a continuation block (dead code after EXIT PARAGRAPH,
                    // but needed to keep the builder consistent)
                    *current_block_id = func.new_block();
                }
            }
            cobol_hir::HirStatement::ExitSection => {
                // EXIT SECTION: jump to the end of the current section.
                // This terminates the current basic block and creates a new
                // (dead-code) continuation block for any remaining statements.
                if let Some(exit_block) = self.current_section_exit {
                    let current_insts = std::mem::take(instructions);
                    func.blocks.push(BasicBlock {
                        id: *current_block_id,
                        params: Vec::new(),
                        instructions: current_insts,
                        terminator: Terminator::Goto(exit_block),
                    });
                    // Create a continuation block (dead code after EXIT SECTION,
                    // but needed to keep the builder consistent)
                    *current_block_id = func.new_block();
                }
            }
            cobol_hir::HirStatement::Continue => {
                // CONTINUE and plain EXIT are no-ops
            }
            cobol_hir::HirStatement::Sort {
                sort_file,
                keys,
                using_files,
                giving_files,
            } => {
                self.lower_sort(
                    *sort_file,
                    keys,
                    using_files,
                    giving_files,
                    func,
                    instructions,
                );
            }
        }
    }

    fn lower_display(
        &self,
        args: &[cobol_hir::HirExpr],
        no_advancing: bool,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // For each argument, emit a call to cobolrt_display (no newline)
        // then after all args, emit cobolrt_display_line if !no_advancing
        for arg in args {
            match arg {
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                    // Emit string constant + call to display
                    let str_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: str_val,
                        value: MirConst::Str(s.clone()),
                    });
                    let len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: len_val,
                        value: MirConst::Int(s.len() as i64),
                    });

                    if no_advancing {
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_display".to_string(),
                            args: vec![str_val, len_val],
                        });
                    } else {
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_display_line".to_string(),
                            args: vec![str_val, len_val],
                        });
                    }
                }
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                    let s = n.to_string();
                    let str_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: str_val,
                        value: MirConst::Str(s.clone()),
                    });
                    let len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: len_val,
                        value: MirConst::Int(s.len() as i64),
                    });

                    let func_name = if no_advancing {
                        "cobolrt_display"
                    } else {
                        "cobolrt_display_line"
                    };
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: func_name.to_string(),
                        args: vec![str_val, len_val],
                    });
                }
                cobol_hir::HirExpr::DataRef(_) => {
                    // Use emit_expr_addr which handles subscripts, ref-mod, and qualifiers
                    if let Some((addr, size)) = self.emit_expr_addr(arg, func, instructions) {
                        let len_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: len_val,
                            value: MirConst::Int(size as i64),
                        });
                        let func_name = if no_advancing {
                            "cobolrt_display"
                        } else {
                            "cobolrt_display_line"
                        };
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: func_name.to_string(),
                            args: vec![addr, len_val],
                        });
                    }
                }
                cobol_hir::HirExpr::FunctionCall { name, args: fargs } => {
                    // Evaluate the function call, then display the result
                    if let Some((result_addr, result_size)) =
                        self.emit_function_call(name, fargs, func, instructions)
                    {
                        let len_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: len_val,
                            value: MirConst::Int(result_size as i64),
                        });
                        let display_fn = if no_advancing {
                            "cobolrt_display"
                        } else {
                            "cobolrt_display_line"
                        };
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: display_fn.to_string(),
                            args: vec![result_addr, len_val],
                        });
                    }
                }
                _ => {}
            }
        }
    }

    /// Determine whether a data item (by name) is numeric.
    /// Returns (is_numeric, scale, dot_pos, is_signed) if found.
    fn data_item_pic_info(&self, name: &str) -> Option<(bool, i32, i32, bool)> {
        // Check linkage items first
        if let Some(info) = self.linkage_map.get(name) {
            // Linkage items with scale > 0 or no X/A in PIC are numeric.
            // For now, treat linkage items as numeric if they have a scale, or
            // look them up in the HIR for their PIC category.
            for &item_id in &self.hir.linkage_items {
                let item = &self.hir.data_items[item_id.into_raw()];
                let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
                if item_name.as_deref() == Some(name) {
                    if let Some(ref pic) = item.storage.picture {
                        let is_numeric = pic.category == cobol_hir::PictureCategory::Numeric
                            || pic.category == cobol_hir::PictureCategory::NumericEdited;
                        let dot_pos = Self::pic_dot_position(&pic.pic_string);
                        let is_signed = pic.sign != cobol_hir::SignPosition::None;
                        return Some((is_numeric, pic.scale, dot_pos, is_signed));
                    }
                    // No PIC means group item, treat as alphanumeric
                    return Some((false, 0, -1, false));
                }
            }
            // Default: treat as numeric if scale is set
            return Some((info.scale != 0, info.scale, info.dot_pos, false));
        }

        // Check working storage
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                if item.is_group {
                    // Group items are always treated as alphanumeric
                    return Some((false, 0, -1, false));
                }
                if let Some(ref pic) = item.storage.picture {
                    let is_numeric = pic.category == cobol_hir::PictureCategory::Numeric
                        || pic.category == cobol_hir::PictureCategory::NumericEdited;
                    let dot_pos = Self::pic_dot_position(&pic.pic_string);
                    let is_signed = pic.sign != cobol_hir::SignPosition::None;
                    return Some((is_numeric, pic.scale, dot_pos, is_signed));
                }
                // No PIC, treat as alphanumeric
                return Some((false, 0, -1, false));
            }
        }
        None
    }

    /// Return the encoding type for a data item: 0=Display, 1=COMP(binary), 2=COMP-3(packed BCD).
    /// Also returns the PIC digit count for the field.
    #[allow(dead_code)]
    fn data_item_encoding_type(&self, name: &str) -> (i64, u32) {
        // Helper to map UsageType to encoding integer
        fn usage_to_enc(usage: cobol_hir::UsageType) -> i64 {
            match usage {
                cobol_hir::UsageType::Comp
                | cobol_hir::UsageType::Comp4
                | cobol_hir::UsageType::Comp5
                | cobol_hir::UsageType::Binary => 1,
                cobol_hir::UsageType::Comp3 | cobol_hir::UsageType::PackedDecimal => 2,
                _ => 0,
            }
        }

        // Check linkage items
        for &item_id in &self.hir.linkage_items {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                let digits = item.storage.picture.as_ref().map(|p| p.size).unwrap_or(0);
                return (usage_to_enc(item.storage.usage), digits);
            }
        }

        // Check working storage
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                let digits = item.storage.picture.as_ref().map(|p| p.size).unwrap_or(0);
                return (usage_to_enc(item.storage.usage), digits);
            }
        }

        (0, 0) // default: display encoding
    }

    /// Get encoding type from an HirExpr (if it's a DataRef). Returns encoding flag.
    fn expr_encoding_type(&self, expr: &cobol_hir::HirExpr) -> (i64, u32) {
        match expr {
            cobol_hir::HirExpr::DataRef(dr) => {
                let name = self.interner.resolve(dr.name).to_string();
                self.data_item_encoding_type(&name)
            }
            _ => (0, 0),
        }
    }

    /// Get encoding flag for an expression: 0=display, 1=comp, 2=comp3.
    fn operand_encoding(&self, expr: &cobol_hir::HirExpr) -> i64 {
        self.expr_encoding_type(expr).0
    }

    /// Emit an encoding-aware arithmetic runtime call.
    /// If all encodings are 0 (display), uses the legacy function for backward compat.
    /// Otherwise, uses the _enc variant with per-field encoding flags.
    #[allow(clippy::too_many_arguments)]
    fn emit_arith_call(
        &self,
        op_name: &str,
        a_addr: Value,
        a_size: u32,
        a_enc: i64,
        b_addr: Value,
        b_size: u32,
        b_enc: i64,
        target_addr: Value,
        target_size: u32,
        target_enc: i64,
        target_is_signed: bool,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let needs_enc = a_enc != 0 || b_enc != 0 || target_enc != 0;

        let a_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: a_len,
            value: MirConst::Int(a_size as i64),
        });
        let b_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: b_len,
            value: MirConst::Int(b_size as i64),
        });
        let t_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: t_len,
            value: MirConst::Int(target_size as i64),
        });
        let signed_flag = func.new_value();
        instructions.push(MirInst::Const {
            dest: signed_flag,
            value: MirConst::Int(if target_is_signed { 1 } else { 0 }),
        });

        if needs_enc {
            let ae = func.new_value();
            instructions.push(MirInst::Const {
                dest: ae,
                value: MirConst::Int(a_enc),
            });
            let be = func.new_value();
            instructions.push(MirInst::Const {
                dest: be,
                value: MirConst::Int(b_enc),
            });
            let te = func.new_value();
            instructions.push(MirInst::Const {
                dest: te,
                value: MirConst::Int(target_enc),
            });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: format!("cobolrt_{}_numeric_enc", op_name),
                args: vec![
                    a_addr,
                    a_len,
                    ae,
                    b_addr,
                    b_len,
                    be,
                    target_addr,
                    t_len,
                    te,
                    signed_flag,
                ],
            });
        } else {
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: format!("cobolrt_{}_numeric", op_name),
                args: vec![
                    a_addr,
                    a_len,
                    b_addr,
                    b_len,
                    target_addr,
                    t_len,
                    signed_flag,
                ],
            });
        }
    }

    /// Return the REPLACING-clause category string for a data item.
    /// Maps PictureCategory to the category names used in INITIALIZE REPLACING:
    /// "NUMERIC", "ALPHANUMERIC", "ALPHABETIC", "NUMERIC-EDITED", "ALPHANUMERIC-EDITED".
    fn data_item_replacing_category(&self, name: &str) -> &'static str {
        // Check linkage items
        for &item_id in &self.hir.linkage_items {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                if let Some(ref pic) = item.storage.picture {
                    return Self::pic_category_to_replacing(&pic.category);
                }
                return "ALPHANUMERIC";
            }
        }
        // Check working storage
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                if item.is_group {
                    return "ALPHANUMERIC";
                }
                if let Some(ref pic) = item.storage.picture {
                    return Self::pic_category_to_replacing(&pic.category);
                }
                return "ALPHANUMERIC";
            }
        }
        "ALPHANUMERIC"
    }

    /// Map a PictureCategory to the REPLACING clause category name.
    fn pic_category_to_replacing(cat: &cobol_hir::PictureCategory) -> &'static str {
        match cat {
            cobol_hir::PictureCategory::Numeric => "NUMERIC",
            cobol_hir::PictureCategory::NumericEdited => "NUMERIC-EDITED",
            cobol_hir::PictureCategory::Alphabetic => "ALPHABETIC",
            cobol_hir::PictureCategory::Alphanumeric => "ALPHANUMERIC",
            cobol_hir::PictureCategory::AlphanumericEdited => "ALPHANUMERIC-EDITED",
            _ => "ALPHANUMERIC",
        }
    }

    /// Check if a data item is numeric-edited (has Z, *, $, +, -, comma editing).
    fn is_numeric_edited(&self, name: &str) -> bool {
        // Check working storage
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                if let Some(ref pic) = item.storage.picture {
                    return pic.category == cobol_hir::PictureCategory::NumericEdited;
                }
                return false;
            }
        }
        // Check linkage items
        for &item_id in &self.hir.linkage_items {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                if let Some(ref pic) = item.storage.picture {
                    return pic.category == cobol_hir::PictureCategory::NumericEdited;
                }
                return false;
            }
        }
        false
    }

    /// Check if a data item has JUSTIFIED RIGHT.
    fn is_justified_right(&self, name: &str) -> bool {
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                return item.justified_right;
            }
        }
        for &item_id in &self.hir.linkage_items {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                return item.justified_right;
            }
        }
        false
    }

    /// Check if a data item has BLANK WHEN ZERO.
    fn is_blank_when_zero(&self, name: &str) -> bool {
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                return item.blank_when_zero;
            }
        }
        for &item_id in &self.hir.linkage_items {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                return item.blank_when_zero;
            }
        }
        false
    }

    /// Emit a BLANK WHEN ZERO check after a MOVE to a numeric target.
    fn emit_blank_when_zero(
        &self,
        target_addr: Value,
        target_size: u32,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let len_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: len_val,
            value: MirConst::Int(target_size as i64),
        });
        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_blank_when_zero".to_string(),
            args: vec![target_addr, len_val],
        });
    }

    /// Find the children names of a group item by looking at group_children map.
    /// Returns a list of (simple_child_name, qualified_global_name).
    fn find_group_children_globals(&self, group_name: &str) -> Vec<(String, String)> {
        let mut result = Vec::new();
        if let Some(children) = self.group_children.get(group_name) {
            for child_name in children {
                // Check if this child has a qualified name for this group
                let qualified = format!("{}.{}", child_name, group_name);
                if self.global_map.contains_key(&qualified) {
                    result.push((child_name.clone(), qualified));
                } else if self.global_map.contains_key(child_name) {
                    result.push((child_name.clone(), child_name.clone()));
                }
            }
        }
        result
    }

    fn lower_move_corresponding(
        &self,
        from_name: cobol_intern::Name,
        to_name: cobol_intern::Name,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let source_grp = self.interner.resolve(from_name).to_string();
        let dest_grp = self.interner.resolve(to_name).to_string();

        let source_children = self.find_group_children_globals(&source_grp);
        let dest_children = self.find_group_children_globals(&dest_grp);

        // For each matching simple child name, emit a MOVE
        for (src_simple, src_global) in &source_children {
            for (dst_simple, dst_global) in &dest_children {
                if src_simple == dst_simple {
                    // Get source address and size
                    let src_size = self
                        .global_map
                        .get(src_global)
                        .map(|&idx| match &self.module.globals[idx].ty {
                            MirType::Bytes(n) => *n,
                            _ => 1,
                        })
                        .unwrap_or(1);

                    let dst_size = self
                        .global_map
                        .get(dst_global)
                        .map(|&idx| match &self.module.globals[idx].ty {
                            MirType::Bytes(n) => *n,
                            _ => 1,
                        })
                        .unwrap_or(1);

                    let src_addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: src_addr,
                        name: src_global.clone(),
                    });
                    let dst_addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: dst_addr,
                        name: dst_global.clone(),
                    });

                    let src_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len,
                        value: MirConst::Int(src_size as i64),
                    });
                    let dst_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dst_len,
                        value: MirConst::Int(dst_size as i64),
                    });

                    // Use alphanumeric move for all CORRESPONDING fields.
                    // For fields with the same PIC, this is a simple byte copy
                    // which handles both alphanumeric and numeric fields correctly.
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_move_alphanumeric".to_string(),
                        args: vec![src_addr, src_len, dst_addr, dst_len],
                    });
                    break;
                }
            }
        }
    }

    fn lower_add_corresponding(
        &self,
        from_name: cobol_intern::Name,
        to_name: cobol_intern::Name,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let source_grp = self.interner.resolve(from_name).to_string();
        let dest_grp = self.interner.resolve(to_name).to_string();

        let source_children = self.find_group_children_globals(&source_grp);
        let dest_children = self.find_group_children_globals(&dest_grp);

        // For each matching simple child name that is numeric, emit ADD
        for (src_simple, src_global) in &source_children {
            for (dst_simple, dst_global) in &dest_children {
                if src_simple == dst_simple {
                    // Both must be numeric for ADD CORRESPONDING
                    let (src_is_numeric, _, _, _) = self
                        .data_item_pic_info(src_simple)
                        .unwrap_or((false, 0, -1, false));
                    let (dst_is_numeric, _, _, _) = self
                        .data_item_pic_info(dst_simple)
                        .unwrap_or((false, 0, -1, false));

                    if !src_is_numeric || !dst_is_numeric {
                        break;
                    }

                    let src_size = self
                        .global_map
                        .get(src_global)
                        .map(|&idx| match &self.module.globals[idx].ty {
                            MirType::Bytes(n) => *n,
                            _ => 1,
                        })
                        .unwrap_or(1);

                    let dst_size = self
                        .global_map
                        .get(dst_global)
                        .map(|&idx| match &self.module.globals[idx].ty {
                            MirType::Bytes(n) => *n,
                            _ => 1,
                        })
                        .unwrap_or(1);

                    let src_addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: src_addr,
                        name: src_global.clone(),
                    });
                    let src_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len,
                        value: MirConst::Int(src_size as i64),
                    });

                    // dest is both source and target for ADD ... TO
                    let dst_addr1 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: dst_addr1,
                        name: dst_global.clone(),
                    });
                    let dst_len1 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dst_len1,
                        value: MirConst::Int(dst_size as i64),
                    });

                    let dst_addr2 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: dst_addr2,
                        name: dst_global.clone(),
                    });
                    let dst_len2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dst_len2,
                        value: MirConst::Int(dst_size as i64),
                    });

                    // Look up signedness of destination
                    let dst_is_signed = self
                        .data_item_pic_info(dst_simple)
                        .map(|(_, _, _, is_signed)| is_signed)
                        .unwrap_or(false);
                    let sf = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sf,
                        value: MirConst::Int(if dst_is_signed { 1 } else { 0 }),
                    });

                    // cobolrt_add_numeric(src, src_len, dst, dst_len, result, result_len, dest_is_signed)
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_add_numeric".to_string(),
                        args: vec![
                            src_addr, src_len, dst_addr1, dst_len1, dst_addr2, dst_len2, sf,
                        ],
                    });
                    break;
                }
            }
        }
    }

    fn lower_subtract_corresponding(
        &self,
        from_name: cobol_intern::Name,
        to_name: cobol_intern::Name,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let source_grp = self.interner.resolve(from_name).to_string();
        let dest_grp = self.interner.resolve(to_name).to_string();

        let source_children = self.find_group_children_globals(&source_grp);
        let dest_children = self.find_group_children_globals(&dest_grp);

        // Accumulator for size error flag across all field operations.
        // Each cobolrt_subtract_numeric call clears the runtime flag,
        // so we read it after each operation and sum the results.
        // A non-zero accumulator at the end means at least one field
        // caused a size error.
        let mut accum = func.new_value();
        instructions.push(MirInst::Const {
            dest: accum,
            value: MirConst::Int(0),
        });

        // For each matching simple child name that is numeric, emit SUBTRACT
        for (src_simple, src_global) in &source_children {
            for (dst_simple, dst_global) in &dest_children {
                if src_simple == dst_simple {
                    // Both must be numeric for SUBTRACT CORRESPONDING
                    let (src_is_numeric, _, _, _) = self
                        .data_item_pic_info(src_simple)
                        .unwrap_or((false, 0, -1, false));
                    let (dst_is_numeric, _, _, _) = self
                        .data_item_pic_info(dst_simple)
                        .unwrap_or((false, 0, -1, false));

                    if !src_is_numeric || !dst_is_numeric {
                        break;
                    }

                    let src_size = self
                        .global_map
                        .get(src_global)
                        .map(|&idx| match &self.module.globals[idx].ty {
                            MirType::Bytes(n) => *n,
                            _ => 1,
                        })
                        .unwrap_or(1);

                    let dst_size = self
                        .global_map
                        .get(dst_global)
                        .map(|&idx| match &self.module.globals[idx].ty {
                            MirType::Bytes(n) => *n,
                            _ => 1,
                        })
                        .unwrap_or(1);

                    // src operand (the value to subtract)
                    let src_addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: src_addr,
                        name: src_global.clone(),
                    });
                    let src_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len,
                        value: MirConst::Int(src_size as i64),
                    });

                    // dst as the "from" operand (minuend): dst - src
                    let dst_addr1 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: dst_addr1,
                        name: dst_global.clone(),
                    });
                    let dst_len1 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dst_len1,
                        value: MirConst::Int(dst_size as i64),
                    });

                    // dst as the result target
                    let dst_addr2 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: dst_addr2,
                        name: dst_global.clone(),
                    });
                    let dst_len2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dst_len2,
                        value: MirConst::Int(dst_size as i64),
                    });

                    // Look up signedness of destination
                    let dst_is_signed = self
                        .data_item_pic_info(dst_simple)
                        .map(|(_, _, _, is_signed)| is_signed)
                        .unwrap_or(false);
                    let sf = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sf,
                        value: MirConst::Int(if dst_is_signed { 1 } else { 0 }),
                    });

                    // cobolrt_subtract_numeric(from, from_len, operand, op_len, target, target_len, dest_is_signed)
                    // Computes: target = from - operand = dst - src
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_subtract_numeric".to_string(),
                        args: vec![
                            dst_addr1, dst_len1, src_addr, src_len, dst_addr2, dst_len2, sf,
                        ],
                    });

                    // Read and accumulate the size error flag from this
                    // operation.  cobolrt_last_size_error() returns 0/1 and
                    // clears the runtime flag.  We add the result to our
                    // accumulator so we can restore it after all fields.
                    let this_err = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(this_err),
                        func: "cobolrt_last_size_error".to_string(),
                        args: vec![],
                    });
                    let new_accum = func.new_value();
                    instructions.push(MirInst::IAdd {
                        dest: new_accum,
                        left: accum,
                        right: this_err,
                    });
                    accum = new_accum;

                    break;
                }
            }
        }

        // Restore the accumulated size error flag so that the subsequent
        // lower_size_error_check (if any) can read it via
        // cobolrt_last_size_error().
        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_set_size_error".to_string(),
            args: vec![accum],
        });
    }

    fn lower_move(
        &self,
        from: &cobol_hir::HirExpr,
        to: &[cobol_hir::HirDataRef],
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        for target_ref in to {
            let target_name_str = self.interner.resolve(target_ref.name).to_string();

            // Resolve target address and size, handling subscripts
            let target_expr = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
            let (target_addr, target_size) = if let Some(info) =
                self.linkage_map.get(&target_name_str)
            {
                (info.param_value, info.byte_size)
            } else if let Some((addr, size)) = self.emit_expr_addr(&target_expr, func, instructions)
            {
                (addr, size)
            } else {
                continue;
            };

            // Get target PIC info
            let (target_is_numeric, target_scale, target_dot_pos, target_is_signed) = self
                .data_item_pic_info(&target_name_str)
                .unwrap_or((false, 0, -1, false));

            // Check if target has JUSTIFIED RIGHT clause
            let target_is_justified = self.is_justified_right(&target_name_str);
            let move_alpha_func = if target_is_justified {
                "cobolrt_move_alphanumeric_justified"
            } else {
                "cobolrt_move_alphanumeric"
            };

            match from {
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                    // Integer literal to target.
                    // If target is numeric, generate display-format digits
                    // If target is alphanumeric, still convert to display string
                    if target_is_numeric {
                        // For numeric targets with scale, we need to handle the
                        // implied decimal point.
                        if target_scale > 0 {
                            // Scale the integer: e.g. MOVE 42 TO PIC 9(5)V99
                            // means 42.00, so internal representation is 4200
                            let scaled = (*n).unsigned_abs() * 10u64.pow(target_scale as u32);
                            let is_negative = *n < 0;
                            if target_dot_pos >= 0 {
                                // Edited numeric with literal '.'
                                let mut bytes = Self::build_edited_zero_template(
                                    &self.get_pic_string(&target_name_str).unwrap_or_default(),
                                    target_size,
                                );
                                // Fill digits right-to-left, skipping '.'
                                let digit_positions: Vec<usize> =
                                    (0..bytes.len()).filter(|&i| bytes[i] != b'.').collect();
                                let num_str = format!("{}", scaled);
                                let num_bytes = num_str.as_bytes();
                                for (j, &pos) in digit_positions.iter().rev().enumerate() {
                                    if j < num_bytes.len() {
                                        bytes[pos] = num_bytes[num_bytes.len() - 1 - j];
                                    }
                                }
                                // For negative signed values, replace leading zero with '-'
                                if is_negative
                                    && target_is_signed
                                    && !bytes.is_empty()
                                    && bytes[0] == b'0'
                                {
                                    bytes[0] = b'-';
                                }
                                let src_val = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: src_val,
                                    value: MirConst::Str(
                                        String::from_utf8_lossy(&bytes).to_string(),
                                    ),
                                });
                                let len_val = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: len_val,
                                    value: MirConst::Int(target_size as i64),
                                });
                                instructions.push(MirInst::CallRuntime {
                                    dest: None,
                                    func: move_alpha_func.to_string(),
                                    args: vec![src_val, len_val, target_addr, len_val],
                                });
                            } else {
                                // Numeric without literal dot (V implies decimal)
                                let display_str =
                                    format!("{:0>width$}", scaled, width = target_size as usize);
                                // For negative signed values, replace leading zero with '-'
                                let display_str = if is_negative && target_is_signed {
                                    let mut bytes = display_str.into_bytes();
                                    if !bytes.is_empty() && bytes[0] == b'0' {
                                        bytes[0] = b'-';
                                    }
                                    String::from_utf8(bytes).unwrap()
                                } else {
                                    display_str
                                };
                                let src_val = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: src_val,
                                    value: MirConst::Str(display_str),
                                });
                                let len_val = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: len_val,
                                    value: MirConst::Int(target_size as i64),
                                });
                                instructions.push(MirInst::CallRuntime {
                                    dest: None,
                                    func: move_alpha_func.to_string(),
                                    args: vec![src_val, len_val, target_addr, len_val],
                                });
                            }
                        } else {
                            // Simple numeric, no scale
                            let display_str = format!(
                                "{:0>width$}",
                                n.unsigned_abs(),
                                width = target_size as usize
                            );
                            // For negative values into signed targets, replace
                            // the leading zero with '-' so the runtime can
                            // interpret the sign.
                            let display_str = if *n < 0 && target_is_signed {
                                let mut bytes = display_str.into_bytes();
                                if !bytes.is_empty() && bytes[0] == b'0' {
                                    bytes[0] = b'-';
                                }
                                String::from_utf8(bytes).unwrap()
                            } else {
                                display_str
                            };
                            let src_val = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: src_val,
                                value: MirConst::Str(display_str),
                            });
                            let len_val = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: len_val,
                                value: MirConst::Int(target_size as i64),
                            });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: move_alpha_func.to_string(),
                                args: vec![src_val, len_val, target_addr, len_val],
                            });
                        }
                    } else {
                        // Alphanumeric target: convert integer to string, left-justify
                        let s = format!("{}", n);
                        let src_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_val,
                            value: MirConst::Str(s.clone()),
                        });
                        let src_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_len,
                            value: MirConst::Int(s.len() as i64),
                        });
                        let dest_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: dest_len,
                            value: MirConst::Int(target_size as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: move_alpha_func.to_string(),
                            args: vec![src_val, src_len, target_addr, dest_len],
                        });
                    }
                }
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                    if target_is_numeric {
                        // String to numeric: parse number from string, zero-fill
                        // Use cobolrt_move_alpha_to_num at runtime
                        let src_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_val,
                            value: MirConst::Str(s.clone()),
                        });
                        let src_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_len,
                            value: MirConst::Int(s.len() as i64),
                        });
                        let dest_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: dest_len,
                            value: MirConst::Int(target_size as i64),
                        });
                        let v_dot_pos = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v_dot_pos,
                            value: MirConst::Int(target_dot_pos as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_move_alpha_to_num".to_string(),
                            args: vec![src_val, src_len, target_addr, dest_len, v_dot_pos],
                        });
                    } else {
                        // String to alphanumeric: left-justify, space-pad
                        let src_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_val,
                            value: MirConst::Str(s.clone()),
                        });
                        let src_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_len,
                            value: MirConst::Int(s.len() as i64),
                        });
                        let dest_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: dest_len,
                            value: MirConst::Int(target_size as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: move_alpha_func.to_string(),
                            args: vec![src_val, src_len, target_addr, dest_len],
                        });
                    }
                }
                cobol_hir::HirExpr::DataRef(src_ref) => {
                    let src_name = self.interner.resolve(src_ref.name).to_string();
                    let src_expr = cobol_hir::HirExpr::DataRef(src_ref.clone());
                    let (src_addr, src_size) = if let Some(info) = self.linkage_map.get(&src_name) {
                        (info.param_value, info.byte_size)
                    } else if let Some((addr, size)) =
                        self.emit_expr_addr(&src_expr, func, instructions)
                    {
                        (addr, size)
                    } else {
                        continue;
                    };

                    // Get source PIC info
                    let (src_is_numeric, src_scale, src_dot_pos, _src_is_signed) = self
                        .data_item_pic_info(&src_name)
                        .unwrap_or((false, 0, -1, false));

                    if src_is_numeric && target_is_numeric {
                        // Check if source/target is numeric-edited (has Z, *, $, etc.)
                        let src_is_edited = self.is_numeric_edited(&src_name);
                        let target_is_edited = self.is_numeric_edited(&target_name_str);

                        if src_is_edited && !target_is_edited {
                            // NumericEdited to Numeric: de-edit the source.
                            // Strip editing characters ($, commas, Z-spaces, etc.)
                            // and extract the raw numeric value.
                            let v_src_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_len,
                                value: MirConst::Int(src_size as i64),
                            });
                            let src_pic_str = self.get_pic_string(&src_name).unwrap_or_default();
                            let v_src_pic = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_pic,
                                value: MirConst::Str(src_pic_str.clone()),
                            });
                            let v_src_pic_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_pic_len,
                                value: MirConst::Int(src_pic_str.len() as i64),
                            });
                            let v_dest_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_len,
                                value: MirConst::Int(target_size as i64),
                            });
                            let v_dest_scale = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_scale,
                                value: MirConst::Int(target_scale as i64),
                            });
                            let v_dest_dot = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_dot,
                                value: MirConst::Int(target_dot_pos as i64),
                            });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_deedit".to_string(),
                                args: vec![
                                    src_addr,
                                    v_src_len,
                                    v_src_pic,
                                    v_src_pic_len,
                                    target_addr,
                                    v_dest_len,
                                    v_dest_scale,
                                    v_dest_dot,
                                ],
                            });
                        } else if src_is_edited && target_is_edited {
                            // NumericEdited to NumericEdited: de-edit source into
                            // target (raw digits), then re-apply target editing.
                            let v_src_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_len,
                                value: MirConst::Int(src_size as i64),
                            });
                            let src_pic_str = self.get_pic_string(&src_name).unwrap_or_default();
                            let v_src_pic = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_pic,
                                value: MirConst::Str(src_pic_str.clone()),
                            });
                            let v_src_pic_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_pic_len,
                                value: MirConst::Int(src_pic_str.len() as i64),
                            });
                            let v_dest_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_len,
                                value: MirConst::Int(target_size as i64),
                            });
                            let v_dest_scale = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_scale,
                                value: MirConst::Int(target_scale as i64),
                            });
                            let v_dest_dot = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_dot,
                                value: MirConst::Int(target_dot_pos as i64),
                            });
                            // Step 1: de-edit into target (raw numeric digits)
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_deedit".to_string(),
                                args: vec![
                                    src_addr,
                                    v_src_len,
                                    v_src_pic,
                                    v_src_pic_len,
                                    target_addr,
                                    v_dest_len,
                                    v_dest_scale,
                                    v_dest_dot,
                                ],
                            });
                            // Step 2: apply numeric editing (Z suppress, comma, $, etc.)
                            let pic_str = self.get_pic_string(&target_name_str).unwrap_or_default();
                            let v_pic = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_pic,
                                value: MirConst::Str(pic_str.clone()),
                            });
                            let v_pic_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_pic_len,
                                value: MirConst::Int(pic_str.len() as i64),
                            });
                            let v_dest_len2 = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_len2,
                                value: MirConst::Int(target_size as i64),
                            });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_format_numeric_edited".to_string(),
                                args: vec![target_addr, v_dest_len2, v_pic, v_pic_len],
                            });
                        } else if target_is_edited {
                            // Numeric (plain) to NumericEdited: first do numeric
                            // move to align digits, then apply numeric editing.
                            let v_src_len_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_len_ne,
                                value: MirConst::Int(src_size as i64),
                            });
                            let v_src_scale_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_scale_ne,
                                value: MirConst::Int(src_scale as i64),
                            });
                            let v_src_dot_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_dot_ne,
                                value: MirConst::Int(src_dot_pos as i64),
                            });
                            let v_dest_len_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_len_ne,
                                value: MirConst::Int(target_size as i64),
                            });
                            let v_dest_scale_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_scale_ne,
                                value: MirConst::Int(target_scale as i64),
                            });
                            let v_dest_dot_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_dot_ne,
                                value: MirConst::Int(target_dot_pos as i64),
                            });
                            let v_rounded_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_rounded_ne,
                                value: MirConst::Int(0),
                            });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_move_numeric".to_string(),
                                args: vec![
                                    src_addr,
                                    v_src_len_ne,
                                    v_src_scale_ne,
                                    v_src_dot_ne,
                                    target_addr,
                                    v_dest_len_ne,
                                    v_dest_scale_ne,
                                    v_dest_dot_ne,
                                    v_rounded_ne,
                                ],
                            });
                            let pic_str_ne =
                                self.get_pic_string(&target_name_str).unwrap_or_default();
                            let v_pic_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_pic_ne,
                                value: MirConst::Str(pic_str_ne.clone()),
                            });
                            let v_pic_len_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_pic_len_ne,
                                value: MirConst::Int(pic_str_ne.len() as i64),
                            });
                            let v_dest_len2_ne = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_len2_ne,
                                value: MirConst::Int(target_size as i64),
                            });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_format_numeric_edited".to_string(),
                                args: vec![target_addr, v_dest_len2_ne, v_pic_ne, v_pic_len_ne],
                            });
                        } else if src_scale == target_scale
                            && src_dot_pos == target_dot_pos
                            && src_size == target_size
                        {
                            // Same format: simple byte copy
                            let src_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: src_len,
                                value: MirConst::Int(src_size as i64),
                            });
                            let dest_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: dest_len,
                                value: MirConst::Int(target_size as i64),
                            });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: move_alpha_func.to_string(),
                                args: vec![src_addr, src_len, target_addr, dest_len],
                            });
                        } else {
                            // Different scales or sizes: use numeric move
                            let v_src_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_len,
                                value: MirConst::Int(src_size as i64),
                            });
                            let v_src_scale = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_scale,
                                value: MirConst::Int(src_scale as i64),
                            });
                            let v_src_dot = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_src_dot,
                                value: MirConst::Int(src_dot_pos as i64),
                            });
                            let v_dest_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_len,
                                value: MirConst::Int(target_size as i64),
                            });
                            let v_dest_scale = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_scale,
                                value: MirConst::Int(target_scale as i64),
                            });
                            let v_dest_dot = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_dest_dot,
                                value: MirConst::Int(target_dot_pos as i64),
                            });
                            let v_rounded_0b = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v_rounded_0b,
                                value: MirConst::Int(0),
                            });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_move_numeric".to_string(),
                                args: vec![
                                    src_addr,
                                    v_src_len,
                                    v_src_scale,
                                    v_src_dot,
                                    target_addr,
                                    v_dest_len,
                                    v_dest_scale,
                                    v_dest_dot,
                                    v_rounded_0b,
                                ],
                            });
                        }
                    } else if src_is_numeric && !target_is_numeric {
                        // Numeric to Alphanumeric: copy display representation left-justified
                        let v_src_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v_src_len,
                            value: MirConst::Int(src_size as i64),
                        });
                        let v_dest_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v_dest_len,
                            value: MirConst::Int(target_size as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_move_num_to_alpha".to_string(),
                            args: vec![src_addr, v_src_len, target_addr, v_dest_len],
                        });
                    } else if !src_is_numeric && target_is_numeric {
                        // Alphanumeric to Numeric: parse and convert
                        let v_src_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v_src_len,
                            value: MirConst::Int(src_size as i64),
                        });
                        let v_dest_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v_dest_len,
                            value: MirConst::Int(target_size as i64),
                        });
                        let v_dot_pos = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v_dot_pos,
                            value: MirConst::Int(target_dot_pos as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_move_alpha_to_num".to_string(),
                            args: vec![src_addr, v_src_len, target_addr, v_dest_len, v_dot_pos],
                        });
                    } else {
                        // Alphanumeric to Alphanumeric: left-justify, space-pad
                        let src_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_len,
                            value: MirConst::Int(src_size as i64),
                        });
                        let dest_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: dest_len,
                            value: MirConst::Int(target_size as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: move_alpha_func.to_string(),
                            args: vec![src_addr, src_len, target_addr, dest_len],
                        });
                    }
                }
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Figurative(fig)) => {
                    // Figurative constants: fill the target field with the appropriate byte.
                    let fill_str = match fig {
                        cobol_hir::FigurativeConstant::Zero => {
                            // Fill with ASCII '0' (for both numeric and alphanumeric targets)
                            "0".repeat(target_size as usize)
                        }
                        cobol_hir::FigurativeConstant::Space => {
                            // Fill with spaces
                            " ".repeat(target_size as usize)
                        }
                        cobol_hir::FigurativeConstant::HighValue => {
                            // Fill with 0xFF bytes
                            String::from_utf8(vec![0xFF; target_size as usize])
                                .unwrap_or_else(|_| "\u{00FF}".repeat(target_size as usize))
                        }
                        cobol_hir::FigurativeConstant::LowValue => {
                            // Fill with 0x00 bytes
                            String::from_utf8(vec![0x00; target_size as usize])
                                .unwrap_or_else(|_| "\x00".repeat(target_size as usize))
                        }
                        cobol_hir::FigurativeConstant::Quote => {
                            // Fill with quote characters
                            "\"".repeat(target_size as usize)
                        }
                        cobol_hir::FigurativeConstant::All => {
                            // ALL is handled separately (with an associated literal)
                            " ".repeat(target_size as usize)
                        }
                    };
                    let src_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_val,
                        value: MirConst::Str(fill_str.clone()),
                    });
                    let src_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len,
                        value: MirConst::Int(fill_str.len() as i64),
                    });
                    let dest_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dest_len,
                        value: MirConst::Int(target_size as i64),
                    });
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: move_alpha_func.to_string(),
                        args: vec![src_val, src_len, target_addr, dest_len],
                    });
                }
                cobol_hir::HirExpr::FunctionCall { name, args: fargs } => {
                    // Evaluate the function call, then move the result to target
                    if let Some((result_addr, result_size)) =
                        self.emit_function_call(name, fargs, func, instructions)
                    {
                        let src_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_len,
                            value: MirConst::Int(result_size as i64),
                        });
                        let dest_len = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: dest_len,
                            value: MirConst::Int(target_size as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: move_alpha_func.to_string(),
                            args: vec![result_addr, src_len, target_addr, dest_len],
                        });
                    }
                }
                _ => {
                    // Other expression types not yet supported in MOVE
                }
            }

            // Post-MOVE fixup: BLANK WHEN ZERO
            if self.is_blank_when_zero(&target_name_str) {
                let bwz_addr = func.new_value();
                let bwz_target_expr = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                if let Some((addr, _sz)) = self.emit_expr_addr(&bwz_target_expr, func, instructions)
                {
                    let _ = bwz_addr; // use addr from emit_expr_addr
                    self.emit_blank_when_zero(addr, target_size, func, instructions);
                }
            }
        }
    }

    fn lower_initialize(
        &self,
        targets: &[cobol_hir::HirDataRef],
        replacing: &[(String, cobol_hir::HirExpr)],
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        for target_ref in targets {
            let target_name_str = self.interner.resolve(target_ref.name).to_string();

            // Find the target item index in working_storage (flat list)
            let mut found_idx = None;
            for (i, &item_id) in self.hir.working_storage.iter().enumerate() {
                let item = &self.hir.data_items[item_id.into_raw()];
                let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
                if item_name.as_deref() == Some(&target_name_str) {
                    found_idx = Some(i);
                    break;
                }
            }

            let target_idx = match found_idx {
                Some(i) => i,
                None => continue,
            };

            let target_item_id = self.hir.working_storage[target_idx];
            let item = &self.hir.data_items[target_item_id.into_raw()];

            if item.is_group {
                let group_level = item.level;
                for j in (target_idx + 1)..self.hir.working_storage.len() {
                    let child_item_id = self.hir.working_storage[j];
                    let child = &self.hir.data_items[child_item_id.into_raw()];

                    if child.level <= group_level {
                        break;
                    }
                    if child.level == 88 || child.is_group {
                        continue;
                    }

                    if let Some(child_name) = child.name {
                        let name_str = self.interner.resolve(child_name).to_string();
                        self.initialize_elementary_with_replacing(
                            &name_str,
                            replacing,
                            func,
                            instructions,
                        );
                    }
                }
            } else {
                self.initialize_elementary_with_replacing(
                    &target_name_str,
                    replacing,
                    func,
                    instructions,
                );
            }
        }
    }

    /// Initialize with optional REPLACING clause support.
    fn initialize_elementary_with_replacing(
        &self,
        name: &str,
        replacing: &[(String, cobol_hir::HirExpr)],
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let (is_numeric, _scale, _dot_pos, _is_signed) = self
            .data_item_pic_info(name)
            .unwrap_or((false, 0, -1, false));

        // Check if there's a REPLACING clause for this field's category
        let category = self.data_item_replacing_category(name);
        let replacement = replacing.iter().find(|(cat, _)| cat == category);

        if let Some((_, replace_expr)) = replacement {
            // Use the REPLACING value
            let (target_addr, target_size) = if let Some(&idx) = self.global_map.get(name) {
                let global = &self.module.globals[idx];
                let size = match &global.ty {
                    MirType::Bytes(n) => *n,
                    _ => 1,
                };
                let addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: addr,
                    name: name.to_string(),
                });
                (addr, size)
            } else {
                return;
            };

            // Generate the replacement value
            match replace_expr {
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                    if is_numeric {
                        // Format as zero-padded display numeric
                        let val_str = format!("{:0>width$}", n, width = target_size as usize);
                        let src_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: src_val,
                            value: MirConst::Str(val_str),
                        });
                        let len_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: len_val,
                            value: MirConst::Int(target_size as i64),
                        });
                        instructions.push(MirInst::CallRuntime {
                            dest: None,
                            func: "cobolrt_move_alphanumeric".to_string(),
                            args: vec![src_val, len_val, target_addr, len_val],
                        });
                    }
                }
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                    let val_str = if s.len() >= target_size as usize {
                        s[..target_size as usize].to_string()
                    } else {
                        format!("{:<width$}", s, width = target_size as usize)
                    };
                    let src_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_val,
                        value: MirConst::Str(val_str),
                    });
                    let len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: len_val,
                        value: MirConst::Int(target_size as i64),
                    });
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_move_alphanumeric".to_string(),
                        args: vec![src_val, len_val, target_addr, len_val],
                    });
                }
                _ => {
                    // Default: use standard initialization
                    self.initialize_elementary_item(name, func, instructions);
                }
            }
        } else {
            // No REPLACING match for this field's category:
            // COBOL-85 says INITIALIZE always initializes ALL elementary items.
            // REPLACING only overrides the value for the specified category;
            // unmentioned categories still get their defaults (SPACES/ZEROS).
            self.initialize_elementary_item(name, func, instructions);
        }
    }

    /// Initialize a single elementary item to its default value:
    /// - Numeric fields -> ZEROS (ASCII '0')
    /// - Alphabetic/Alphanumeric fields -> SPACES
    fn initialize_elementary_item(
        &self,
        name: &str,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // Resolve the target address and size
        let (target_addr, target_size) = if let Some(info) = self.linkage_map.get(name) {
            (info.param_value, info.byte_size)
        } else if let Some(&idx) = self.global_map.get(name) {
            let global = &self.module.globals[idx];
            let size = match &global.ty {
                MirType::Bytes(n) => *n,
                _ => 1,
            };
            let addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: addr,
                name: name.to_string(),
            });
            (addr, size)
        } else {
            return;
        };

        let (is_numeric, _scale, _dot_pos, _is_signed) = self
            .data_item_pic_info(name)
            .unwrap_or((false, 0, -1, false));

        if is_numeric {
            // Fill with ASCII '0' characters
            let zeros = "0".repeat(target_size as usize);
            let src_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: src_val,
                value: MirConst::Str(zeros),
            });
            let len_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: len_val,
                value: MirConst::Int(target_size as i64),
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_move_alphanumeric".to_string(),
                args: vec![src_val, len_val, target_addr, len_val],
            });
        } else {
            // Fill with spaces
            let spaces = " ".repeat(target_size as usize);
            let src_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: src_val,
                value: MirConst::Str(spaces),
            });
            let len_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: len_val,
                value: MirConst::Int(target_size as i64),
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_move_alphanumeric".to_string(),
                args: vec![src_val, len_val, target_addr, len_val],
            });
        }
    }

    /// Get the PIC string for a named data item.
    fn get_pic_string(&self, name: &str) -> Option<String> {
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                return item.storage.picture.as_ref().map(|p| p.pic_string.clone());
            }
        }
        for &item_id in &self.hir.linkage_items {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(name) {
                return item.storage.picture.as_ref().map(|p| p.pic_string.clone());
            }
        }
        None
    }

    /// Resolve a data ref name to its global_map key, considering OF qualifiers.
    /// If qualifiers are present, constructs "CHILD.PARENT" and looks it up.
    /// If no qualifiers but the simple name exists, returns the simple name.
    /// If the simple name doesn't exist but qualified versions do, tries to find a match.
    fn resolve_global_name(&self, data_ref: &cobol_hir::HirDataRef) -> Option<String> {
        let name = self.interner.resolve(data_ref.name).to_string();

        // If qualifiers are present (OF phrase), construct qualified name
        if !data_ref.qualifiers.is_empty() {
            let qualifier = self.interner.resolve(data_ref.qualifiers[0]).to_string();
            let qualified = format!("{}.{}", name, qualifier);
            if self.global_map.contains_key(&qualified) {
                return Some(qualified);
            }
            // Fallback: maybe the name is not qualified in global_map
            if self.global_map.contains_key(&name) {
                return Some(name);
            }
            return None;
        }

        // No qualifiers: try simple name first
        if self.global_map.contains_key(&name) {
            return Some(name);
        }

        // If not found, maybe it's a qualified name — try to find any match
        if let Some(entries) = self.qualified_names.get(&name) {
            if entries.len() == 1 {
                // Only one qualified version, use it
                return Some(entries[0].0.clone());
            }
            // Multiple qualified versions, ambiguous without qualifier
            // Return the first one as a fallback
            if !entries.is_empty() {
                return Some(entries[0].0.clone());
            }
        }

        None
    }

    /// Resolve a data ref to (global_name, byte_size)
    fn resolve_data_ref(&self, data_ref: &cobol_hir::HirDataRef) -> Option<(String, u32)> {
        let global_name = self.resolve_global_name(data_ref)?;
        if let Some(&idx) = self.global_map.get(&global_name) {
            let global = &self.module.globals[idx];
            let size = match &global.ty {
                MirType::Bytes(n) => *n,
                _ => 1,
            };
            Some((global_name, size))
        } else {
            None
        }
    }

    /// Look up the HIR data item for a data ref, returning (name, byte_size, scale, dot_pos).
    fn resolve_data_ref_full(
        &self,
        data_ref: &cobol_hir::HirDataRef,
    ) -> Option<(String, u32, i32, i32)> {
        let name = self.interner.resolve(data_ref.name).to_string();
        // Check linkage map first
        if let Some(info) = self.linkage_map.get(&name) {
            return Some((name, info.byte_size, info.scale, info.dot_pos));
        }
        let global_name = self.resolve_global_name(data_ref)?;
        self.global_map.get(&global_name)?;
        // Find the HIR data item to get scale and PIC info
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(&name) {
                // If qualified, verify the parent matches
                if global_name.contains('.') {
                    let parent_match = item.parent.and_then(|pid| {
                        let parent = &self.hir.data_items[pid.into_raw()];
                        parent.name.map(|n| self.interner.resolve(n).to_string())
                    });
                    let expected_parent = global_name.split('.').nth(1).unwrap_or("");
                    if parent_match.as_deref() != Some(expected_parent) {
                        continue;
                    }
                }
                let byte_size = item.storage.byte_size;
                let scale = item.storage.picture.as_ref().map(|p| p.scale).unwrap_or(0);
                let dot_pos = item
                    .storage
                    .picture
                    .as_ref()
                    .map(|p| Self::pic_dot_position(&p.pic_string))
                    .unwrap_or(-1);
                return Some((global_name, byte_size, scale, dot_pos));
            }
        }
        None
    }

    /// Resolve an HirExpr to a global address + size, or emit a constant.
    /// For linkage items, returns the function parameter pointer directly.
    fn emit_expr_addr(
        &self,
        expr: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> Option<(Value, u32)> {
        match expr {
            cobol_hir::HirExpr::DataRef(dr) => {
                let name = self.interner.resolve(dr.name).to_string();
                // Check linkage params first (subprogram parameters)
                if let Some(info) = self.linkage_map.get(&name) {
                    return Some((info.param_value, info.byte_size));
                }
                // Fall through to globals
                if let Some((gname, size)) = self.resolve_data_ref(dr) {
                    let mut addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: addr,
                        name: gname.clone(),
                    });

                    // Handle subscripts (OCCURS arrays)
                    // If the data item has an OCCURS clause and subscripts are provided,
                    // compute the element address using GetElementAddr.
                    // COBOL arrays are 1-based, so we subtract 1 from the index.
                    // Exception: INDEXED BY names store byte offsets and are used directly.
                    let effective_size = if !dr.subscripts.is_empty() {
                        if let Some(&elem_size) = self.occurs_element_size.get(&gname) {
                            // Get the first subscript (single-dimension for now)
                            let subscript = &dr.subscripts[0];

                            // Check if the subscript is an index name (INDEXED BY).
                            // Index names store byte offsets directly, so we use IAdd
                            // instead of GetElementAddr.
                            let is_index_subscript = matches!(subscript,
                                cobol_hir::HirExpr::DataRef(sub_ref)
                                    if self.index_info.contains_key(
                                        self.interner.resolve(sub_ref.name)));

                            if is_index_subscript {
                                // Index name: load byte offset and add directly to base
                                if let cobol_hir::HirExpr::DataRef(sub_ref) = subscript {
                                    let sub_name = self.interner.resolve(sub_ref.name).to_string();
                                    let sub_addr = func.new_value();
                                    instructions.push(MirInst::GlobalAddr {
                                        dest: sub_addr,
                                        name: sub_name,
                                    });
                                    let byte_off = func.new_value();
                                    instructions.push(MirInst::Load {
                                        dest: byte_off,
                                        addr: sub_addr,
                                        ty: MirType::I64,
                                    });
                                    let elem_addr = func.new_value();
                                    instructions.push(MirInst::IAdd {
                                        dest: elem_addr,
                                        left: addr,
                                        right: byte_off,
                                    });
                                    addr = elem_addr;
                                }
                                elem_size
                            } else {
                                let index_val = match subscript {
                                    cobol_hir::HirExpr::Literal(
                                        cobol_hir::LiteralValue::Integer(n),
                                    ) => {
                                        // Convert 1-based to 0-based at compile time
                                        let v = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: v,
                                            value: MirConst::Int(*n - 1),
                                        });
                                        v
                                    }
                                    cobol_hir::HirExpr::DataRef(sub_ref) => {
                                        // Load the subscript variable value and convert to integer
                                        let sub_name =
                                            self.interner.resolve(sub_ref.name).to_string();
                                        if let Some(sub_info) = self.linkage_map.get(&sub_name) {
                                            // Linkage param
                                            let sz = func.new_value();
                                            instructions.push(MirInst::Const {
                                                dest: sz,
                                                value: MirConst::Int(sub_info.byte_size as i64),
                                            });
                                            let raw_val = func.new_value();
                                            instructions.push(MirInst::CallRuntime {
                                                dest: Some(raw_val),
                                                func: "cobolrt_display_to_int".to_string(),
                                                args: vec![sub_info.param_value, sz],
                                            });
                                            // Subtract 1 for 0-based indexing
                                            let one = func.new_value();
                                            instructions.push(MirInst::Const {
                                                dest: one,
                                                value: MirConst::Int(1),
                                            });
                                            let idx = func.new_value();
                                            instructions.push(MirInst::ISub {
                                                dest: idx,
                                                left: raw_val,
                                                right: one,
                                            });
                                            idx
                                        } else if let Some(&sub_idx) =
                                            self.global_map.get(&sub_name)
                                        {
                                            let sub_global = &self.module.globals[sub_idx];
                                            let sub_size = match &sub_global.ty {
                                                MirType::Bytes(n) => *n,
                                                _ => 1,
                                            };
                                            let sub_addr = func.new_value();
                                            instructions.push(MirInst::GlobalAddr {
                                                dest: sub_addr,
                                                name: sub_name,
                                            });
                                            let sub_len = func.new_value();
                                            instructions.push(MirInst::Const {
                                                dest: sub_len,
                                                value: MirConst::Int(sub_size as i64),
                                            });
                                            let raw_val = func.new_value();
                                            instructions.push(MirInst::CallRuntime {
                                                dest: Some(raw_val),
                                                func: "cobolrt_display_to_int".to_string(),
                                                args: vec![sub_addr, sub_len],
                                            });
                                            // Subtract 1 for 0-based indexing
                                            let one = func.new_value();
                                            instructions.push(MirInst::Const {
                                                dest: one,
                                                value: MirConst::Int(1),
                                            });
                                            let idx = func.new_value();
                                            instructions.push(MirInst::ISub {
                                                dest: idx,
                                                left: raw_val,
                                                right: one,
                                            });
                                            idx
                                        } else {
                                            let v = func.new_value();
                                            instructions.push(MirInst::Const {
                                                dest: v,
                                                value: MirConst::Int(0),
                                            });
                                            v
                                        }
                                    }
                                    _ => {
                                        let v = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: v,
                                            value: MirConst::Int(0),
                                        });
                                        v
                                    }
                                };

                                // Compute element address: base + index * element_size
                                let elem_addr = func.new_value();
                                instructions.push(MirInst::GetElementAddr {
                                    dest: elem_addr,
                                    base: addr,
                                    index: index_val,
                                    element_size: elem_size,
                                });
                                addr = elem_addr;
                                elem_size
                            }
                        } else {
                            size
                        }
                    } else {
                        size
                    };

                    // Handle reference modification: (offset, length)
                    // COBOL syntax: identifier(start:length)
                    // start is 1-based, so actual byte offset = start - 1
                    // length is optional (defaults to rest of string)
                    if let Some((ref offset_expr, ref length_expr)) = dr.ref_mod {
                        // Compute the offset (1-based in COBOL, convert to 0-based)
                        let offset_val = match offset_expr.as_ref() {
                            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                                let offset_0 = (*n - 1) as u32;
                                let v = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: v,
                                    value: MirConst::Int(offset_0 as i64),
                                });
                                v
                            }
                            cobol_hir::HirExpr::DataRef(ref_dr) => {
                                // Dynamic offset from a data reference variable
                                let ref_name = self.interner.resolve(ref_dr.name).to_string();
                                let raw_val =
                                    if let Some(ref_info) = self.linkage_map.get(&ref_name) {
                                        let sz = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: sz,
                                            value: MirConst::Int(ref_info.byte_size as i64),
                                        });
                                        let rv = func.new_value();
                                        instructions.push(MirInst::CallRuntime {
                                            dest: Some(rv),
                                            func: "cobolrt_display_to_int".to_string(),
                                            args: vec![ref_info.param_value, sz],
                                        });
                                        rv
                                    } else if let Some(&ref_idx) = self.global_map.get(&ref_name) {
                                        let ref_global = &self.module.globals[ref_idx];
                                        let ref_size = match &ref_global.ty {
                                            MirType::Bytes(n) => *n,
                                            _ => 1,
                                        };
                                        let ref_addr = func.new_value();
                                        instructions.push(MirInst::GlobalAddr {
                                            dest: ref_addr,
                                            name: ref_name,
                                        });
                                        let ref_len = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: ref_len,
                                            value: MirConst::Int(ref_size as i64),
                                        });
                                        let rv = func.new_value();
                                        instructions.push(MirInst::CallRuntime {
                                            dest: Some(rv),
                                            func: "cobolrt_display_to_int".to_string(),
                                            args: vec![ref_addr, ref_len],
                                        });
                                        rv
                                    } else {
                                        let v = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: v,
                                            value: MirConst::Int(0),
                                        });
                                        v
                                    };
                                // Subtract 1 to convert from 1-based to 0-based
                                let one = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: one,
                                    value: MirConst::Int(1),
                                });
                                let offset_0 = func.new_value();
                                instructions.push(MirInst::ISub {
                                    dest: offset_0,
                                    left: raw_val,
                                    right: one,
                                });
                                offset_0
                            }
                            _ => {
                                let v = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: v,
                                    value: MirConst::Int(0),
                                });
                                v
                            }
                        };

                        // Apply offset to address: adjusted_addr = base + offset
                        let adjusted_addr = func.new_value();
                        instructions.push(MirInst::IAdd {
                            dest: adjusted_addr,
                            left: addr,
                            right: offset_val,
                        });

                        // Determine the effective length
                        let ref_mod_size = if let Some(len_expr) = length_expr {
                            match len_expr.as_ref() {
                                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(
                                    n,
                                )) => *n as u32,
                                _ => effective_size,
                            }
                        } else {
                            // No length specified: rest of the string from the offset
                            effective_size
                        };

                        return Some((adjusted_addr, ref_mod_size));
                    }

                    Some((addr, effective_size))
                } else {
                    None
                }
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                // Materialize the integer literal as a display-format string constant.
                let s = format!("{}", n);
                let len = s.len() as u32;
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Str(s),
                });
                Some((v, len))
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                let len = s.len() as u32;
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Str(s.clone()),
                });
                Some((v, len))
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Decimal(digits)) => {
                // Render decimal as display-format digits (strip the dot for raw storage)
                let s = digits.replace('.', "");
                let len = s.len() as u32;
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Str(s),
                });
                Some((v, len))
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Figurative(fig)) => {
                // Figurative constants as expression addresses: ZEROS -> "0", etc.
                let s = match fig {
                    cobol_hir::FigurativeConstant::Zero => "0".to_string(),
                    cobol_hir::FigurativeConstant::Space => " ".to_string(),
                    cobol_hir::FigurativeConstant::HighValue => {
                        String::from_utf8(vec![0xFF]).unwrap_or_default()
                    }
                    cobol_hir::FigurativeConstant::LowValue => {
                        String::from_utf8(vec![0x00]).unwrap_or_default()
                    }
                    cobol_hir::FigurativeConstant::Quote => "\"".to_string(),
                    cobol_hir::FigurativeConstant::All => " ".to_string(),
                };
                let len = s.len() as u32;
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Str(s),
                });
                Some((v, len))
            }
            _ => None,
        }
    }

    /// Emit instructions to update the FILE STATUS variable for a file, if one is defined.
    /// `status_value` is a MIR Value containing the COBOL status code (0, 10, 35, etc.).
    fn emit_file_status_update(
        &self,
        file_name: &str,
        status_value: Value,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        if let Some(status_var) = self.file_status_map.get(file_name) {
            if let Some(&_idx) = self.global_map.get(status_var) {
                let status_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: status_addr,
                    name: status_var.clone(),
                });
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_set_file_status".to_string(),
                    args: vec![status_addr, status_value],
                });
            }
        }
    }

    /// Lower OPEN statement.
    fn lower_file_open(
        &self,
        file: cobol_intern::Name,
        mode: cobol_hir::OpenMode,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let file_name = self.interner.resolve(file).to_string();
        let handle_name = format!("_FILE_HANDLE_{}", file_name);

        // Get the ASSIGN TO path from our file_assign_map (populated from HIR)
        let actual_assign = self
            .file_assign_map
            .get(&file_name)
            .cloned()
            .unwrap_or_else(|| format!("{}.DAT", file_name));

        // Get organization and record_size for this file
        let org = self
            .file_org_map
            .get(&file_name)
            .copied()
            .unwrap_or(cobol_hir::FileOrganization::Sequential);
        let org_int: i64 = match org {
            cobol_hir::FileOrganization::Sequential => 0,
            cobol_hir::FileOrganization::Relative => 1,
            cobol_hir::FileOrganization::Indexed => 2,
            cobol_hir::FileOrganization::LineSequential => 3,
        };
        let access = self
            .file_access_map
            .get(&file_name)
            .copied()
            .unwrap_or(cobol_hir::AccessMode::Sequential);
        let access_int: i64 = match access {
            cobol_hir::AccessMode::Sequential => 0,
            cobol_hir::AccessMode::Random => 1,
            cobol_hir::AccessMode::Dynamic => 2,
        };
        // Compute record size from file descriptor
        let rec_size: i64 = self
            .module
            .file_descriptors
            .iter()
            .find(|fd| fd.name == file_name)
            .map(|fd| fd.record_size as i64)
            .unwrap_or(80);

        // Emit: handle = cobolrt_file_open(filename_ptr, filename_len, mode, org, access, rec_size)
        let filename_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: filename_val,
            value: MirConst::Str(actual_assign.clone()),
        });
        let filename_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: filename_len,
            value: MirConst::Int(actual_assign.len() as i64),
        });
        let mode_val = func.new_value();
        let mode_int = match mode {
            cobol_hir::OpenMode::Input => 0,
            cobol_hir::OpenMode::Output => 1,
            cobol_hir::OpenMode::IoMode => 2,
            cobol_hir::OpenMode::Extend => 3,
        };
        instructions.push(MirInst::Const {
            dest: mode_val,
            value: MirConst::Int(mode_int),
        });
        let org_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: org_val,
            value: MirConst::Int(org_int),
        });
        let access_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: access_val,
            value: MirConst::Int(access_int),
        });
        let rec_size_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: rec_size_val,
            value: MirConst::Int(rec_size),
        });

        let handle_result = func.new_value();
        instructions.push(MirInst::CallRuntime {
            dest: Some(handle_result),
            func: "cobolrt_file_open".to_string(),
            args: vec![
                filename_val,
                filename_len,
                mode_val,
                org_val,
                access_val,
                rec_size_val,
            ],
        });

        // Store handle to the global
        if self.global_map.contains_key(&handle_name) {
            let handle_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: handle_addr,
                name: handle_name,
            });
            instructions.push(MirInst::Store {
                addr: handle_addr,
                value: handle_result,
            });
        }

        // Update FILE STATUS: OPEN always succeeds (status 00)
        let zero_status = func.new_value();
        instructions.push(MirInst::Const {
            dest: zero_status,
            value: MirConst::Int(0),
        });
        self.emit_file_status_update(&file_name, zero_status, func, instructions);
    }

    /// Lower CLOSE statement.
    fn lower_file_close(
        &self,
        file: cobol_intern::Name,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let file_name = self.interner.resolve(file).to_string();
        let handle_name = format!("_FILE_HANDLE_{}", file_name);

        if self.global_map.contains_key(&handle_name) {
            let handle_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: handle_addr,
                name: handle_name,
            });
            let handle_val = func.new_value();
            instructions.push(MirInst::Load {
                dest: handle_val,
                addr: handle_addr,
                ty: MirType::I32,
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_file_close".to_string(),
                args: vec![handle_val],
            });
        }

        // Update FILE STATUS: CLOSE always succeeds (status 00)
        let zero_status = func.new_value();
        instructions.push(MirInst::Const {
            dest: zero_status,
            value: MirConst::Int(0),
        });
        self.emit_file_status_update(&file_name, zero_status, func, instructions);
    }

    /// Lower WRITE statement with optional FROM and ADVANCING.
    fn lower_file_write(
        &self,
        record: cobol_intern::Name,
        from: Option<cobol_intern::Name>,
        advancing: Option<cobol_hir::WriteAdvancing>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let record_name = self.interner.resolve(record).to_string();

        // Find which file this record belongs to
        let mut file_name = None;
        for (fname, records) in &self.file_record_map {
            if records.contains(&record_name) {
                file_name = Some(fname.clone());
                break;
            }
        }
        let file_name = match file_name {
            Some(n) => n,
            None => return,
        };

        let handle_name = format!("_FILE_HANDLE_{}", file_name);

        // Get record address and size
        let (record_addr, record_size) = if let Some(&idx) = self.global_map.get(&record_name) {
            let global = &self.module.globals[idx];
            let size = match &global.ty {
                MirType::Bytes(n) => *n,
                _ => 1,
            };
            let addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: addr,
                name: record_name.clone(),
            });
            (addr, size)
        } else {
            return;
        };

        // Handle WRITE FROM: copy source data into the record area before writing
        if let Some(from_name) = from {
            let from_str = self.interner.resolve(from_name).to_string();
            if let Some(&idx) = self.global_map.get(&from_str) {
                let from_global = &self.module.globals[idx];
                let from_size = match &from_global.ty {
                    MirType::Bytes(n) => *n,
                    _ => 1,
                };
                let from_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: from_addr,
                    name: from_str,
                });
                let src_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: src_len,
                    value: MirConst::Int(from_size as i64),
                });
                let dest_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: dest_len,
                    value: MirConst::Int(record_size as i64),
                });
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_move_alphanumeric".to_string(),
                    args: vec![from_addr, src_len, record_addr, dest_len],
                });
            }
        }

        if self.global_map.contains_key(&handle_name) {
            let handle_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: handle_addr,
                name: handle_name.clone(),
            });
            let handle_val = func.new_value();
            instructions.push(MirInst::Load {
                dest: handle_val,
                addr: handle_addr,
                ty: MirType::I32,
            });
            let size_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: size_val,
                value: MirConst::Int(record_size as i64),
            });

            let org = self
                .file_org_map
                .get(&file_name)
                .copied()
                .unwrap_or(cobol_hir::FileOrganization::Sequential);

            // ADVANCING only applies to plain SEQUENTIAL (print files), not LINE SEQUENTIAL
            let seq_advancing =
                advancing.filter(|_| matches!(org, cobol_hir::FileOrganization::Sequential));

            if let Some(adv) = seq_advancing {
                // Use the ADVANCING-aware write function
                let (before_lines, after_lines) = match adv {
                    cobol_hir::WriteAdvancing::BeforeLines(n) => (n as i64, 0),
                    cobol_hir::WriteAdvancing::AfterLines(n) => (0, n as i64),
                    cobol_hir::WriteAdvancing::BeforePage => (-1, 0),
                    cobol_hir::WriteAdvancing::AfterPage => (0, -1),
                };
                let before_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: before_val,
                    value: MirConst::Int(before_lines),
                });
                let after_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: after_val,
                    value: MirConst::Int(after_lines),
                });
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_file_write_advancing".to_string(),
                    args: vec![handle_val, record_addr, size_val, before_val, after_val],
                });
            } else {
                let write_func = match org {
                    cobol_hir::FileOrganization::Relative
                    | cobol_hir::FileOrganization::Indexed => "cobolrt_file_write_record",
                    _ => "cobolrt_file_write_line",
                };
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: write_func.to_string(),
                    args: vec![handle_val, record_addr, size_val],
                });
            }

            // Update FILE STATUS: WRITE always succeeds (status 00)
            let zero_status = func.new_value();
            instructions.push(MirInst::Const {
                dest: zero_status,
                value: MirConst::Int(0),
            });
            self.emit_file_status_update(&file_name, zero_status, func, instructions);
        }
    }

    /// Lower READ statement with AT END handling.
    fn lower_file_read(
        &mut self,
        file: cobol_intern::Name,
        into: Option<cobol_intern::Name>,
        at_end: &[cobol_hir::HirStatement],
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let file_name = self.interner.resolve(file).to_string();
        let handle_name = format!("_FILE_HANDLE_{}", file_name);

        // Find the first record of this file
        let record_name = self
            .file_record_map
            .get(&file_name)
            .and_then(|records| records.first().cloned());
        let record_name = match record_name {
            Some(n) => n,
            None => return,
        };

        // Get record address and size
        let (record_addr, record_size) = if let Some(&idx) = self.global_map.get(&record_name) {
            let global = &self.module.globals[idx];
            let size = match &global.ty {
                MirType::Bytes(n) => *n,
                _ => 1,
            };
            let addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: addr,
                name: record_name.clone(),
            });
            (addr, size)
        } else {
            return;
        };

        if !self.global_map.contains_key(&handle_name) {
            return;
        }

        let handle_addr = func.new_value();
        instructions.push(MirInst::GlobalAddr {
            dest: handle_addr,
            name: handle_name,
        });
        let handle_val = func.new_value();
        instructions.push(MirInst::Load {
            dest: handle_val,
            addr: handle_addr,
            ty: MirType::I32,
        });
        let size_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: size_val,
            value: MirConst::Int(record_size as i64),
        });

        // Determine which runtime function to call based on organization
        let org = self
            .file_org_map
            .get(&file_name)
            .copied()
            .unwrap_or(cobol_hir::FileOrganization::Sequential);
        let read_func = match org {
            cobol_hir::FileOrganization::Relative | cobol_hir::FileOrganization::Indexed => {
                "cobolrt_file_read_record"
            }
            _ => "cobolrt_file_read_line",
        };

        // Call read function -> eof_flag (0=ok, 1=eof)
        let eof_flag = func.new_value();
        instructions.push(MirInst::CallRuntime {
            dest: Some(eof_flag),
            func: read_func.to_string(),
            args: vec![handle_val, record_addr, size_val],
        });

        // Branch on EOF: copy INTO only on success, execute AT END only on failure
        let zero_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: zero_val,
            value: MirConst::Int(0),
        });
        let is_eof = func.new_value();
        instructions.push(MirInst::ICmpNe {
            dest: is_eof,
            left: eof_flag,
            right: zero_val,
        });

        let success_block = func.new_block();
        let at_end_block = func.new_block();
        let continue_block = func.new_block();

        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Branch {
                cond: is_eof,
                then_block: at_end_block,
                else_block: success_block,
            },
        });

        // Success block: set FILE STATUS 00, copy INTO, then jump to continue
        let mut success_insts = Vec::new();

        // FILE STATUS: 00 = successful read
        let status_00 = func.new_value();
        success_insts.push(MirInst::Const {
            dest: status_00,
            value: MirConst::Int(0),
        });
        self.emit_file_status_update(&file_name, status_00, func, &mut success_insts);

        if let Some(into_name) = into {
            let into_str = self.interner.resolve(into_name).to_string();
            if let Some(&idx) = self.global_map.get(&into_str) {
                let into_global = &self.module.globals[idx];
                let into_size = match &into_global.ty {
                    MirType::Bytes(n) => *n,
                    _ => 1,
                };
                let into_addr = func.new_value();
                success_insts.push(MirInst::GlobalAddr {
                    dest: into_addr,
                    name: into_str,
                });
                let src_len = func.new_value();
                success_insts.push(MirInst::Const {
                    dest: src_len,
                    value: MirConst::Int(record_size as i64),
                });
                let dest_len = func.new_value();
                success_insts.push(MirInst::Const {
                    dest: dest_len,
                    value: MirConst::Int(into_size as i64),
                });
                let record_addr2 = func.new_value();
                success_insts.push(MirInst::GlobalAddr {
                    dest: record_addr2,
                    name: record_name.clone(),
                });
                success_insts.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_move_alphanumeric".to_string(),
                    args: vec![record_addr2, src_len, into_addr, dest_len],
                });
            }
        }
        func.blocks.push(BasicBlock {
            id: success_block,
            params: Vec::new(),
            instructions: success_insts,
            terminator: Terminator::Goto(continue_block),
        });

        // At-end block: set FILE STATUS 10, execute AT END statements, jump to continue
        let mut at_end_insts = Vec::new();

        // FILE STATUS: 10 = at end / end of file
        let status_10 = func.new_value();
        at_end_insts.push(MirInst::Const {
            dest: status_10,
            value: MirConst::Int(10),
        });
        self.emit_file_status_update(&file_name, status_10, func, &mut at_end_insts);

        let mut at_end_bid = at_end_block;
        for stmt in at_end {
            self.lower_statement_to_blocks(stmt, func, &mut at_end_bid, &mut at_end_insts);
        }
        func.blocks.push(BasicBlock {
            id: at_end_bid,
            params: Vec::new(),
            instructions: at_end_insts,
            terminator: Terminator::Goto(continue_block),
        });

        *current_block_id = continue_block;
    }

    /// Lower REWRITE statement — update current record in place.
    fn lower_file_rewrite(
        &self,
        record: cobol_intern::Name,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let record_name = self.interner.resolve(record).to_string();

        // Find which file this record belongs to
        let mut file_name = None;
        for (fname, records) in &self.file_record_map {
            if records.contains(&record_name) {
                file_name = Some(fname.clone());
                break;
            }
        }
        let file_name = match file_name {
            Some(n) => n,
            None => return,
        };

        let handle_name = format!("_FILE_HANDLE_{}", file_name);

        // Get record address and size
        let (record_addr, record_size) = if let Some(&idx) = self.global_map.get(&record_name) {
            let global = &self.module.globals[idx];
            let size = match &global.ty {
                MirType::Bytes(n) => *n,
                _ => 1,
            };
            let addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: addr,
                name: record_name.clone(),
            });
            (addr, size)
        } else {
            return;
        };

        if self.global_map.contains_key(&handle_name) {
            let handle_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: handle_addr,
                name: handle_name,
            });
            let handle_val = func.new_value();
            instructions.push(MirInst::Load {
                dest: handle_val,
                addr: handle_addr,
                ty: MirType::I32,
            });
            let size_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: size_val,
                value: MirConst::Int(record_size as i64),
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_file_rewrite".to_string(),
                args: vec![handle_val, record_addr, size_val],
            });

            // Update FILE STATUS: REWRITE succeeds (status 00)
            let zero_status = func.new_value();
            instructions.push(MirInst::Const {
                dest: zero_status,
                value: MirConst::Int(0),
            });
            self.emit_file_status_update(&file_name, zero_status, func, instructions);
        }
    }

    /// Lower DELETE statement — delete current record.
    fn lower_file_delete(
        &self,
        file: cobol_intern::Name,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let file_name = self.interner.resolve(file).to_string();
        let handle_name = format!("_FILE_HANDLE_{}", file_name);

        if self.global_map.contains_key(&handle_name) {
            let handle_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: handle_addr,
                name: handle_name,
            });
            let handle_val = func.new_value();
            instructions.push(MirInst::Load {
                dest: handle_val,
                addr: handle_addr,
                ty: MirType::I32,
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_file_delete".to_string(),
                args: vec![handle_val],
            });
        }
    }

    /// Lower START statement — position file for sequential reading.
    fn lower_file_start(
        &self,
        file: cobol_intern::Name,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let file_name = self.interner.resolve(file).to_string();
        let handle_name = format!("_FILE_HANDLE_{}", file_name);

        if self.global_map.contains_key(&handle_name) {
            let handle_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: handle_addr,
                name: handle_name,
            });
            let handle_val = func.new_value();
            instructions.push(MirInst::Load {
                dest: handle_val,
                addr: handle_addr,
                ty: MirType::I32,
            });

            // Pass the relative key value if available
            if let Some(rk_name) = self.file_relative_key_map.get(&file_name) {
                if let Some(&idx) = self.global_map.get(rk_name) {
                    let global = &self.module.globals[idx];
                    let rk_size = match &global.ty {
                        MirType::Bytes(n) => *n,
                        _ => 4,
                    };
                    let rk_addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: rk_addr,
                        name: rk_name.clone(),
                    });
                    let rk_size_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: rk_size_val,
                        value: MirConst::Int(rk_size as i64),
                    });
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_file_start".to_string(),
                        args: vec![handle_val, rk_addr, rk_size_val],
                    });
                    return;
                }
            }

            // No relative key — just call with zero
            let zero_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: zero_val,
                value: MirConst::Int(0),
            });
            let zero_val2 = func.new_value();
            instructions.push(MirInst::Const {
                dest: zero_val2,
                value: MirConst::Int(0),
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_file_start".to_string(),
                args: vec![handle_val, zero_val, zero_val2],
            });
        }
    }

    /// Lower SORT ... USING ... GIVING statement.
    ///
    /// For the simple USING/GIVING form we emit a single runtime call:
    ///   cobolrt_sort_using_giving(
    ///       input_filename_ptr, input_filename_len,
    ///       output_filename_ptr, output_filename_len,
    ///       record_size,
    ///       num_keys,
    ///       key_offsets_ptr, key_lengths_ptr, key_ascending_ptr
    ///   )
    ///
    /// The runtime opens the input file, reads all records, sorts them, and
    /// writes the sorted records to the output file.
    fn lower_sort(
        &self,
        sort_file: cobol_intern::Name,
        keys: &[cobol_hir::SortKey],
        using_files: &[cobol_intern::Name],
        giving_files: &[cobol_intern::Name],
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // Only handle the USING/GIVING form for now (most common simple case)
        if using_files.is_empty() || giving_files.is_empty() {
            return;
        }

        // Get the sort file name and look up its record size
        let sort_file_name = self.interner.resolve(sort_file).to_string();
        let record_size: u32 = self
            .file_record_map
            .get(&sort_file_name)
            .and_then(|records| records.first())
            .and_then(|rec_name| self.global_map.get(rec_name))
            .map(|&idx| {
                let global = &self.module.globals[idx];
                match &global.ty {
                    MirType::Bytes(n) => *n,
                    _ => 80, // default record size
                }
            })
            .unwrap_or(80);

        // Resolve key offsets and lengths from the sort file's record structure.
        // For each key, find its offset within the record and its byte size.
        let mut key_offsets: Vec<u32> = Vec::new();
        let mut key_lengths: Vec<u32> = Vec::new();
        let mut key_ascending: Vec<u32> = Vec::new(); // 1=ascending, 0=descending

        for key in keys {
            let key_name = self.interner.resolve(key.name).to_string();
            // Look up the key in the global map to find its offset and size
            if let Some(&idx) = self.global_map.get(&key_name) {
                let global = &self.module.globals[idx];
                let size = match &global.ty {
                    MirType::Bytes(n) => *n,
                    _ => 1,
                };
                // Compute offset relative to the record start
                // The parent_offset gives us the offset within the parent record
                let offset = global.parent_offset.as_ref().map(|(_, o)| *o).unwrap_or(0);
                key_offsets.push(offset);
                key_lengths.push(size);
            } else {
                // Key not found; use defaults
                key_offsets.push(0);
                key_lengths.push(record_size);
            }
            key_ascending.push(if key.ascending { 1 } else { 0 });
        }

        let num_keys = keys.len() as u32;

        // Get input file path from ASSIGN TO
        let using_file_name = self.interner.resolve(using_files[0]).to_string();
        let input_path = self
            .file_assign_map
            .get(&using_file_name)
            .cloned()
            .unwrap_or_else(|| format!("{}.DAT", using_file_name));

        // Get output file path from ASSIGN TO
        let giving_file_name = self.interner.resolve(giving_files[0]).to_string();
        let output_path = self
            .file_assign_map
            .get(&giving_file_name)
            .cloned()
            .unwrap_or_else(|| format!("{}.DAT", giving_file_name));

        // Emit constants for input filename
        let input_ptr = func.new_value();
        instructions.push(MirInst::Const {
            dest: input_ptr,
            value: MirConst::Str(input_path.clone()),
        });
        let input_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: input_len,
            value: MirConst::Int(input_path.len() as i64),
        });

        // Emit constants for output filename
        let output_ptr = func.new_value();
        instructions.push(MirInst::Const {
            dest: output_ptr,
            value: MirConst::Str(output_path.clone()),
        });
        let output_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: output_len,
            value: MirConst::Int(output_path.len() as i64),
        });

        // Record size
        let rec_size_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: rec_size_val,
            value: MirConst::Int(record_size as i64),
        });

        // Number of keys
        let num_keys_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: num_keys_val,
            value: MirConst::Int(num_keys as i64),
        });

        // Key arrays as packed byte arrays: offsets, lengths, ascending flags
        // We pack them as space-separated integers in a string that the runtime parses,
        // OR we store them as inline byte arrays. For simplicity, we use a combined
        // format string: "offset1,length1,asc1;offset2,length2,asc2;..."
        let key_info_str = keys
            .iter()
            .enumerate()
            .map(|(i, _)| format!("{},{},{}", key_offsets[i], key_lengths[i], key_ascending[i]))
            .collect::<Vec<_>>()
            .join(";");

        let key_info_ptr = func.new_value();
        instructions.push(MirInst::Const {
            dest: key_info_ptr,
            value: MirConst::Str(key_info_str.clone()),
        });
        let key_info_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: key_info_len,
            value: MirConst::Int(key_info_str.len() as i64),
        });

        // Call cobolrt_sort_using_giving(
        //     input_ptr, input_len,
        //     output_ptr, output_len,
        //     record_size,
        //     num_keys,
        //     key_info_ptr, key_info_len
        // )
        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_sort_using_giving".to_string(),
            args: vec![
                input_ptr,
                input_len,
                output_ptr,
                output_len,
                rec_size_val,
                num_keys_val,
                key_info_ptr,
                key_info_len,
            ],
        });
    }

    /// Lower CALL statement: call an external subprogram.
    fn lower_call(
        &mut self,
        program: &cobol_hir::HirExpr,
        using: &[cobol_hir::CallArg],
        returning: &Option<cobol_hir::HirDataRef>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // Get program name and mangle it (COBOL hyphens → underscores)
        let prog_name = match program {
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                s.to_ascii_uppercase().replace('-', "_")
            }
            cobol_hir::HirExpr::DataRef(dr) => {
                // Dynamic CALL via identifier — resolve from initial VALUE.
                // True runtime dispatch is not yet supported; we resolve at
                // compile time from the data item's initial VALUE clause.
                let name = self.interner.resolve(dr.name).to_string();
                let initial = self.hir.working_storage.iter().find_map(|&item_id| {
                    let item = &self.hir.data_items[item_id.into_raw()];
                    let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
                    if item_name.as_deref() == Some(&name) {
                        if let Some(cobol_hir::InitialValue::String_(ref s)) = item.value {
                            let trimmed = s.trim().to_ascii_uppercase().replace('-', "_");
                            if !trimmed.is_empty() {
                                Some(trimmed)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                });
                match initial {
                    Some(pname) => pname,
                    None => {
                        self.module.errors.push(format!(
                            "CALL via identifier '{}' cannot be resolved at compile time \
                             (no VALUE clause found). Runtime dynamic CALL is not yet supported.",
                            name
                        ));
                        return;
                    }
                }
            }
            _ => return,
        };

        // Collect argument addresses (BY REFERENCE → pointer to data item)
        let mut arg_values = Vec::new();
        for call_arg in using {
            if let cobol_hir::HirExpr::DataRef(dr) = &call_arg.value {
                let name = self.interner.resolve(dr.name).to_string();
                if let Some(info) = self.linkage_map.get(&name) {
                    arg_values.push(info.param_value);
                } else if let Some((gname, _size)) = self.resolve_data_ref(dr) {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: addr,
                        name: gname,
                    });
                    arg_values.push(addr);
                }
            }
        }

        // RETURNING is not yet implemented — emit a structured error so the
        // compiler exits gracefully instead of panicking.
        if returning.is_some() {
            self.module.errors.push(
                "CALL ... RETURNING is not yet supported. \
                 Remove the RETURNING clause or use BY REFERENCE parameters instead."
                    .to_string(),
            );
            return;
        }

        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: prog_name,
            args: arg_values,
        });
    }

    /// Lower ADD statement: ADD op1 op2 ... GIVING target
    /// Strategy: call cobolrt_add_numeric(src1, src1_len, src2, src2_len, dest, dest_len)
    fn lower_add(
        &self,
        operands: &[cobol_hir::HirExpr],
        giving: Option<&[cobol_hir::HirDataRef]>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // For ADD op1 op2 GIVING target:
        // We need all operand addresses and the target address.
        // For simplicity, handle the common case: ADD a b GIVING c
        // → cobolrt_add_numeric(a_ptr, a_len, b_ptr, b_len, c_ptr, c_len)
        let giving_refs = match giving {
            Some(refs) if !refs.is_empty() => refs,
            _ => return,
        };

        // Get all operand addresses and encodings
        let mut operand_addrs: Vec<(Value, u32, i64)> = Vec::new();
        for op in operands {
            if let Some((addr, size)) = self.emit_expr_addr(op, func, instructions) {
                let enc = self.operand_encoding(op);
                operand_addrs.push((addr, size, enc));
            }
        }

        if operand_addrs.len() < 2 {
            return;
        }

        // Store the result to ALL GIVING targets, not just the first one
        for target_ref in giving_refs {
            let target_expr = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
            let (_, target_size) = match self.emit_expr_addr(&target_expr, func, instructions) {
                Some(v) => v,
                None => continue,
            };

            let target_name_str = self.interner.resolve(target_ref.name).to_string();
            let target_is_signed = self
                .data_item_pic_info(&target_name_str)
                .map(|(_, _, _, is_signed)| is_signed)
                .unwrap_or(false);
            let (target_enc, _) = self.data_item_encoding_type(&target_name_str);

            let target_expr_a = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
            let (target_addr, _) = self
                .emit_expr_addr(&target_expr_a, func, instructions)
                .unwrap();

            // First, add operands[0] + operands[1] -> target
            let (a_addr, a_size, a_enc) = operand_addrs[0];
            let (b_addr, b_size, b_enc) = operand_addrs[1];

            self.emit_arith_call(
                "add",
                a_addr,
                a_size,
                a_enc,
                b_addr,
                b_size,
                b_enc,
                target_addr,
                target_size,
                target_enc,
                target_is_signed,
                func,
                instructions,
            );

            // If more than 2 operands, accumulate: target = target + operands[i]
            for &(op_addr, op_size, op_enc) in operand_addrs.iter().skip(2) {
                let target_expr_b = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                let (target_addr2, _) = self
                    .emit_expr_addr(&target_expr_b, func, instructions)
                    .unwrap();

                let target_expr_c = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                let (target_addr3, _) = self
                    .emit_expr_addr(&target_expr_c, func, instructions)
                    .unwrap();

                self.emit_arith_call(
                    "add",
                    target_addr2,
                    target_size,
                    target_enc,
                    op_addr,
                    op_size,
                    op_enc,
                    target_addr3,
                    target_size,
                    target_enc,
                    target_is_signed,
                    func,
                    instructions,
                );
            }
        }
    }

    fn lower_subtract(
        &self,
        operands: &[cobol_hir::HirExpr],
        from: &[cobol_hir::HirDataRef],
        giving: Option<&[cobol_hir::HirDataRef]>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // COBOL SUBTRACT semantics:
        //   SUBTRACT A B FROM C GIVING D  =>  D = C - (A + B)
        //   SUBTRACT A FROM B             =>  B = B - A  (no GIVING, accumulator)
        //
        // `operands` = values to subtract (A, B)
        // `from`     = the minuend(s) (C)
        // `giving`   = result target(s) (D), or None for accumulator form

        // Collect all operand addresses
        let mut operand_addrs: Vec<(Value, u32)> = Vec::new();
        for op in operands {
            if let Some(addr_size) = self.emit_expr_addr(op, func, instructions) {
                operand_addrs.push(addr_size);
            }
        }
        if operand_addrs.is_empty() {
            return;
        }

        if let Some(giving_refs) = giving {
            // SUBTRACT ... FROM ... GIVING form
            // Result = from[0] - sum(operands) => all giving targets
            if from.is_empty() || giving_refs.is_empty() {
                return;
            }

            let from_ref = &from[0];
            let from_expr = cobol_hir::HirExpr::DataRef(Box::new(from_ref.clone()));
            let (_, from_size) = match self.emit_expr_addr(&from_expr, func, instructions) {
                Some(v) => v,
                None => return,
            };

            // Store the result to ALL GIVING targets, not just the first one
            for target_ref in giving_refs {
                let target_expr = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                let (_, target_size) = match self.emit_expr_addr(&target_expr, func, instructions) {
                    Some(v) => v,
                    None => continue,
                };

                // Look up whether the target field is signed (PIC S9 vs PIC 9)
                let target_name_str = self.interner.resolve(target_ref.name).to_string();
                let target_is_signed = self
                    .data_item_pic_info(&target_name_str)
                    .map(|(_, _, _, is_signed)| is_signed)
                    .unwrap_or(false);

                // First operand: compute target = from - operands[0]
                let from_expr2 = cobol_hir::HirExpr::DataRef(Box::new(from_ref.clone()));
                let (from_addr, _) = self
                    .emit_expr_addr(&from_expr2, func, instructions)
                    .unwrap();
                let from_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: from_len,
                    value: MirConst::Int(from_size as i64),
                });

                let (op0_addr, op0_size) = operand_addrs[0];
                let op0_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: op0_len,
                    value: MirConst::Int(op0_size as i64),
                });

                let target_expr2 = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                let (target_addr, _) = self
                    .emit_expr_addr(&target_expr2, func, instructions)
                    .unwrap();
                let target_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: target_len,
                    value: MirConst::Int(target_size as i64),
                });

                let signed_flag = func.new_value();
                instructions.push(MirInst::Const {
                    dest: signed_flag,
                    value: MirConst::Int(if target_is_signed { 1 } else { 0 }),
                });

                // cobolrt_subtract_numeric(from, from_len, operand, op_len, target, target_len, dest_is_signed)
                // C shim computes: target = src1 - src2 = from - operand
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_subtract_numeric".to_string(),
                    args: vec![
                        from_addr,
                        from_len,
                        op0_addr,
                        op0_len,
                        target_addr,
                        target_len,
                        signed_flag,
                    ],
                });

                // If more operands, subtract each from the target in place:
                // target = target - operands[i]
                for &(opi_addr, opi_size) in operand_addrs.iter().skip(1) {
                    let opi_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: opi_len,
                        value: MirConst::Int(opi_size as i64),
                    });

                    let te1 = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                    let (t_addr1, _) = self.emit_expr_addr(&te1, func, instructions).unwrap();
                    let t_len1 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: t_len1,
                        value: MirConst::Int(target_size as i64),
                    });

                    let te2 = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                    let (t_addr2, _) = self.emit_expr_addr(&te2, func, instructions).unwrap();
                    let t_len2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: t_len2,
                        value: MirConst::Int(target_size as i64),
                    });

                    let sf = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sf,
                        value: MirConst::Int(if target_is_signed { 1 } else { 0 }),
                    });

                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_subtract_numeric".to_string(),
                        args: vec![t_addr1, t_len1, opi_addr, opi_len, t_addr2, t_len2, sf],
                    });
                }
            }
        } else {
            // SUBTRACT ... FROM ... (accumulator form, no GIVING)
            // For each target in `from`: target = target - sum(operands)
            for target_ref in from {
                let te_check = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                let (_, target_size) = match self.emit_expr_addr(&te_check, func, instructions) {
                    Some(v) => v,
                    None => continue,
                };

                // Look up whether the target field is signed
                let tgt_name = self.interner.resolve(target_ref.name).to_string();
                let tgt_is_signed = self
                    .data_item_pic_info(&tgt_name)
                    .map(|(_, _, _, is_signed)| is_signed)
                    .unwrap_or(false);

                for &(op_addr, op_size) in &operand_addrs {
                    let op_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: op_len,
                        value: MirConst::Int(op_size as i64),
                    });

                    // target is both src1 and dest: target = target - operand
                    let te1 = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                    let (t_addr1, _) = self.emit_expr_addr(&te1, func, instructions).unwrap();
                    let t_len1 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: t_len1,
                        value: MirConst::Int(target_size as i64),
                    });

                    let te2 = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                    let (t_addr2, _) = self.emit_expr_addr(&te2, func, instructions).unwrap();
                    let t_len2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: t_len2,
                        value: MirConst::Int(target_size as i64),
                    });

                    let sf = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sf,
                        value: MirConst::Int(if tgt_is_signed { 1 } else { 0 }),
                    });

                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_subtract_numeric".to_string(),
                        args: vec![t_addr1, t_len1, op_addr, op_len, t_addr2, t_len2, sf],
                    });
                }
            }
        }
    }

    fn lower_multiply(
        &self,
        operand_a: &cobol_hir::HirExpr,
        by: &cobol_hir::HirExpr,
        giving: Option<&[cobol_hir::HirDataRef]>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // COBOL MULTIPLY semantics:
        //   MULTIPLY A BY B             =>  B = A * B  (accumulator)
        //   MULTIPLY A BY B GIVING C    =>  C = A * B

        let (a_addr, a_size) = match self.emit_expr_addr(operand_a, func, instructions) {
            Some(v) => v,
            None => return,
        };
        let a_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: a_len,
            value: MirConst::Int(a_size as i64),
        });

        let (b_addr, b_size) = match self.emit_expr_addr(by, func, instructions) {
            Some(v) => v,
            None => return,
        };
        let b_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: b_len,
            value: MirConst::Int(b_size as i64),
        });

        if let Some(giving_refs) = giving {
            // MULTIPLY A BY B GIVING C D E ...
            if giving_refs.is_empty() {
                return;
            }
            // Store the result to ALL GIVING targets, not just the first one
            for target_ref in giving_refs {
                let te = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                let (target_addr, target_size) = match self.emit_expr_addr(&te, func, instructions)
                {
                    Some(v) => v,
                    None => continue,
                };
                let target_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: target_len,
                    value: MirConst::Int(target_size as i64),
                });

                let target_name_str = self.interner.resolve(target_ref.name).to_string();
                let target_is_signed = self
                    .data_item_pic_info(&target_name_str)
                    .map(|(_, _, _, is_signed)| is_signed)
                    .unwrap_or(false);
                let sf = func.new_value();
                instructions.push(MirInst::Const {
                    dest: sf,
                    value: MirConst::Int(if target_is_signed { 1 } else { 0 }),
                });

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_multiply_numeric".to_string(),
                    args: vec![a_addr, a_len, b_addr, b_len, target_addr, target_len, sf],
                });
            }
        } else {
            // MULTIPLY A BY B (accumulator: B = A * B)
            // The BY operand is both source and destination
            let (dest_addr, dest_size) = match self.emit_expr_addr(by, func, instructions) {
                Some(v) => v,
                None => return,
            };
            let dest_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: dest_len,
                value: MirConst::Int(dest_size as i64),
            });

            // Determine if the BY operand (accumulator target) is signed
            let by_is_signed = if let cobol_hir::HirExpr::DataRef(ref dr) = by {
                let by_name = self.interner.resolve(dr.name).to_string();
                self.data_item_pic_info(&by_name)
                    .map(|(_, _, _, is_signed)| is_signed)
                    .unwrap_or(false)
            } else {
                false
            };
            let sf = func.new_value();
            instructions.push(MirInst::Const {
                dest: sf,
                value: MirConst::Int(if by_is_signed { 1 } else { 0 }),
            });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_multiply_numeric".to_string(),
                args: vec![a_addr, a_len, b_addr, b_len, dest_addr, dest_len, sf],
            });
        }
    }

    /// Emit a call to cobolrt_clear_size_error() to reset the overflow flag
    /// before an arithmetic operation.
    fn emit_clear_size_error(&self, _func: &mut MirFunction, instructions: &mut Vec<MirInst>) {
        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_clear_size_error".to_string(),
            args: vec![],
        });
    }

    /// Shared helper: check cobolrt_last_size_error() and branch to
    /// ON SIZE ERROR or NOT ON SIZE ERROR handler based on overflow flag.
    fn lower_size_error_check(
        &mut self,
        on_error_stmts: &[cobol_hir::HirStatement],
        not_on_error_stmts: &[cobol_hir::HirStatement],
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        // Call cobolrt_last_size_error() — returns non-zero if overflow occurred
        let overflow = func.new_value();
        instructions.push(MirInst::CallRuntime {
            dest: Some(overflow),
            func: "cobolrt_last_size_error".to_string(),
            args: vec![],
        });

        let zero = func.new_value();
        instructions.push(MirInst::Const {
            dest: zero,
            value: MirConst::Int(0),
        });
        let has_error = func.new_value();
        instructions.push(MirInst::ICmpNe {
            dest: has_error,
            left: overflow,
            right: zero,
        });

        let error_block = func.new_block();
        let no_error_block = func.new_block();
        let cont_block = func.new_block();

        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Branch {
                cond: has_error,
                then_block: error_block,
                else_block: no_error_block,
            },
        });

        // ON SIZE ERROR block
        let mut error_insts = Vec::new();
        let mut error_bid = error_block;
        for stmt in on_error_stmts {
            self.lower_statement_to_blocks(stmt, func, &mut error_bid, &mut error_insts);
        }
        func.blocks.push(BasicBlock {
            id: error_bid,
            params: Vec::new(),
            instructions: error_insts,
            terminator: Terminator::Goto(cont_block),
        });

        // NOT ON SIZE ERROR block
        let mut no_error_insts = Vec::new();
        let mut no_error_bid = no_error_block;
        for stmt in not_on_error_stmts {
            self.lower_statement_to_blocks(stmt, func, &mut no_error_bid, &mut no_error_insts);
        }
        func.blocks.push(BasicBlock {
            id: no_error_bid,
            params: Vec::new(),
            instructions: no_error_insts,
            terminator: Terminator::Goto(cont_block),
        });

        *current_block_id = cont_block;
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_divide(
        &self,
        operand_a: &cobol_hir::HirExpr,
        by_operand: &cobol_hir::HirExpr,
        giving: Option<&[cobol_hir::HirDataRef]>,
        remainder: Option<&cobol_hir::HirDataRef>,
        rounded: bool,
        is_into: bool,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // COBOL DIVIDE semantics:
        //   DIVIDE A INTO B       → B = B / A  (A is divisor, B is dividend & target)
        //   DIVIDE A INTO B GIVING C → C = B / A
        //   DIVIDE A BY B GIVING C   → C = A / B
        //   + optional REMAINDER D

        // Resolve both operands
        let resolve_expr = |expr: &cobol_hir::HirExpr,
                            f: &mut MirFunction,
                            insts: &mut Vec<MirInst>|
         -> Option<(Value, u32, i32)> {
            match expr {
                cobol_hir::HirExpr::DataRef(dr) => {
                    if let Some((name, size, scale, _dot)) = self.resolve_data_ref_full(dr) {
                        let addr = f.new_value();
                        insts.push(MirInst::GlobalAddr { dest: addr, name });
                        Some((addr, size, scale))
                    } else {
                        None
                    }
                }
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                    let lit_str = format!("{}", n.unsigned_abs());
                    let temp_name = format!("__div_lit_{}", f.next_value);
                    if !self.global_map.contains_key(&temp_name) {
                        // Use a temporary constant — store literal as padded string
                        let padded = format!("{:0>4}", lit_str);
                        let const_val = f.new_value();
                        insts.push(MirInst::Const {
                            dest: const_val,
                            value: MirConst::Str(padded.clone()),
                        });
                        let len_val = f.new_value();
                        insts.push(MirInst::Const {
                            dest: len_val,
                            value: MirConst::Int(padded.len() as i64),
                        });
                        // For literal operands in divide, we emit a move to a temp buffer
                        // using cobolrt_move_to_temp and get back a pointer
                        let temp_addr = f.new_value();
                        insts.push(MirInst::CallRuntime {
                            dest: Some(temp_addr),
                            func: "cobolrt_alloc_temp".to_string(),
                            args: vec![const_val, len_val],
                        });
                        return Some((temp_addr, padded.len() as u32, 0));
                    }
                    None
                }
                _ => None,
            }
        };

        let (op_a_addr, op_a_size, op_a_scale) = match resolve_expr(operand_a, func, instructions) {
            Some(info) => info,
            None => return,
        };
        let (op_b_addr, op_b_size, op_b_scale) = match resolve_expr(by_operand, func, instructions)
        {
            Some(info) => info,
            None => return,
        };

        // Determine dividend, divisor, and target based on INTO vs BY
        let (
            dividend_addr,
            dividend_size,
            dividend_scale,
            divisor_addr,
            divisor_size,
            divisor_scale,
        ) = if is_into {
            // DIVIDE A INTO B → dividend=B, divisor=A
            (
                op_b_addr, op_b_size, op_b_scale, op_a_addr, op_a_size, op_a_scale,
            )
        } else {
            // DIVIDE A BY B → dividend=A, divisor=B
            (
                op_a_addr, op_a_size, op_a_scale, op_b_addr, op_b_size, op_b_scale,
            )
        };

        // Emit division call helper
        let emit_divide_call = |f: &mut MirFunction,
                                insts: &mut Vec<MirInst>,
                                dv_addr,
                                dv_size: u32,
                                dv_scale: i32,
                                ds_addr,
                                ds_size: u32,
                                ds_scale: i32,
                                t_addr,
                                t_size: u32,
                                t_scale: i32,
                                t_dot: i32,
                                rnd: bool| {
            let v1 = f.new_value();
            insts.push(MirInst::Const {
                dest: v1,
                value: MirConst::Int(dv_size as i64),
            });
            let v2 = f.new_value();
            insts.push(MirInst::Const {
                dest: v2,
                value: MirConst::Int(dv_scale as i64),
            });
            let v3 = f.new_value();
            insts.push(MirInst::Const {
                dest: v3,
                value: MirConst::Int(ds_size as i64),
            });
            let v4 = f.new_value();
            insts.push(MirInst::Const {
                dest: v4,
                value: MirConst::Int(ds_scale as i64),
            });
            let v5 = f.new_value();
            insts.push(MirInst::Const {
                dest: v5,
                value: MirConst::Int(t_size as i64),
            });
            let v6 = f.new_value();
            insts.push(MirInst::Const {
                dest: v6,
                value: MirConst::Int(t_scale as i64),
            });
            let v7 = f.new_value();
            insts.push(MirInst::Const {
                dest: v7,
                value: MirConst::Int(t_dot as i64),
            });
            let v8 = f.new_value();
            insts.push(MirInst::Const {
                dest: v8,
                value: MirConst::Int(if rnd { 1 } else { 0 }),
            });
            insts.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_divide_scaled".to_string(),
                args: vec![dv_addr, v1, v2, ds_addr, v3, v4, t_addr, v5, v6, v7, v8],
            });
        };

        // Determine target(s): GIVING targets, or into_or_by (when no GIVING + INTO)
        // Collect all targets to emit divide calls for
        let mut targets: Vec<(Value, u32, i32, i32)> = Vec::new();
        if let Some(refs) = giving {
            // Store the result to ALL GIVING targets, not just the first one
            for target_ref in refs {
                match self.resolve_data_ref_full(target_ref) {
                    Some((name, size, scale, dot_pos)) => {
                        let addr = func.new_value();
                        instructions.push(MirInst::GlobalAddr { dest: addr, name });
                        targets.push((addr, size, scale, dot_pos));
                    }
                    None => continue,
                }
            }
        } else if is_into {
            // DIVIDE A INTO B (no GIVING) → target = B (same as dividend)
            match by_operand {
                cobol_hir::HirExpr::DataRef(dr) => match self.resolve_data_ref_full(dr) {
                    Some((name, size, scale, dot_pos)) => {
                        let addr = func.new_value();
                        instructions.push(MirInst::GlobalAddr { dest: addr, name });
                        targets.push((addr, size, scale, dot_pos));
                    }
                    None => return,
                },
                _ => return,
            }
        } else {
            return; // DIVIDE A BY B without GIVING is invalid
        };

        if targets.is_empty() {
            return;
        }

        // Use the first target for remainder computation reference
        let (target_addr, target_size, target_scale, _target_dot_pos) = targets[0];

        for &(t_addr, t_size, t_scale, t_dot) in &targets {
            emit_divide_call(
                func,
                instructions,
                dividend_addr,
                dividend_size,
                dividend_scale,
                divisor_addr,
                divisor_size,
                divisor_scale,
                t_addr,
                t_size,
                t_scale,
                t_dot,
                rounded,
            );
        }

        // Handle REMAINDER: quotient is already computed in target,
        // now compute remainder = dividend - quotient * divisor
        // For simplicity, call cobolrt_remainder_scaled
        if let Some(rem_ref) = remainder {
            if let Some((rem_name, rem_size, rem_scale, rem_dot_pos)) =
                self.resolve_data_ref_full(rem_ref)
            {
                let rem_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: rem_addr,
                    name: rem_name,
                });

                let v1 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v1,
                    value: MirConst::Int(dividend_size as i64),
                });
                let v2 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v2,
                    value: MirConst::Int(dividend_scale as i64),
                });
                let v3 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v3,
                    value: MirConst::Int(divisor_size as i64),
                });
                let v4 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v4,
                    value: MirConst::Int(divisor_scale as i64),
                });
                let v5 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v5,
                    value: MirConst::Int(target_size as i64),
                });
                let v6 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v6,
                    value: MirConst::Int(target_scale as i64),
                });
                let v7 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v7,
                    value: MirConst::Int(rem_size as i64),
                });
                let v8 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v8,
                    value: MirConst::Int(rem_scale as i64),
                });
                let v9 = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v9,
                    value: MirConst::Int(rem_dot_pos as i64),
                });
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_remainder_scaled".to_string(),
                    args: vec![
                        dividend_addr,
                        v1,
                        v2,
                        divisor_addr,
                        v3,
                        v4,
                        target_addr,
                        v5,
                        v6,
                        rem_addr,
                        v7,
                        v8,
                        v9,
                    ],
                });
            }
        }
    }

    /// Lower STRING statement: for each source, call cobolrt_string_append
    /// which copies the source into the target at the current pointer position
    /// and advances the pointer.
    fn lower_string(
        &mut self,
        sources: &[(cobol_hir::HirExpr, Option<cobol_hir::HirExpr>)],
        into: &cobol_hir::HirDataRef,
        pointer: Option<&cobol_hir::HirDataRef>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // Get target address and size
        let (target_name, target_size) = match self.resolve_data_ref(into) {
            Some(info) => info,
            None => return,
        };

        // Get pointer address and size (if WITH POINTER specified)
        let (ptr_name, ptr_size) = match pointer {
            Some(p) => match self.resolve_data_ref(p) {
                Some(info) => info,
                None => return,
            },
            None => {
                // No explicit POINTER — synthesize a temp pointer global
                let tmp_ptr_name = "__STRING_PTR_TEMP__".to_string();
                let tmp_ptr_size = 4u32;
                if !self.global_map.contains_key(&tmp_ptr_name) {
                    let offset = self
                        .module
                        .globals
                        .last()
                        .map(|g| {
                            g.offset
                                + match &g.ty {
                                    MirType::Bytes(n) => *n,
                                    MirType::I32 | MirType::Bool => 4,
                                    MirType::I64 => 8,
                                    MirType::I128 => 16,
                                    _ => 4,
                                }
                        })
                        .unwrap_or(0);
                    self.module.globals.push(MirGlobal {
                        name: tmp_ptr_name.clone(),
                        ty: MirType::Bytes(tmp_ptr_size),
                        initial_value: Some(MirConst::Str("0001".to_string())),
                        offset,
                        redefines: None,
                        parent_offset: None,
                    });
                    self.global_map
                        .insert(tmp_ptr_name.clone(), self.module.globals.len() - 1);
                }
                // Reset the temp pointer to "0001" (COBOL pointer value 1)
                let v_src = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_src,
                    value: MirConst::Str("0001".to_string()),
                });
                let v_tmp_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: v_tmp_addr,
                    name: tmp_ptr_name.clone(),
                });
                instructions.push(MirInst::MoveAlphanumeric {
                    src: v_src,
                    src_len: 4,
                    dest: v_tmp_addr,
                    dest_len: tmp_ptr_size,
                    justified: false,
                });
                (tmp_ptr_name, tmp_ptr_size)
            }
        };

        // Clear any residual size-error flag before starting the STRING operation.
        // cobolrt_last_size_error() reads and resets the flag.
        let _discard = func.new_value();
        instructions.push(MirInst::CallRuntime {
            dest: Some(_discard),
            func: "cobolrt_last_size_error".to_string(),
            args: vec![],
        });

        // For each source, call cobolrt_string_append(src, src_len, dest, dest_len,
        //   ptr, ptr_len, delim, delim_len)
        for (source, delimiter) in sources {
            let (src_addr, src_size) = match source {
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                    let addr = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: addr,
                        value: MirConst::Str(s.clone()),
                    });
                    (addr, s.len() as u32)
                }
                cobol_hir::HirExpr::DataRef(dr) => match self.resolve_data_ref(dr) {
                    Some((name, size)) => {
                        let addr = func.new_value();
                        instructions.push(MirInst::GlobalAddr { dest: addr, name });
                        (addr, size)
                    }
                    None => continue,
                },
                _ => continue,
            };

            let v_src_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_src_len,
                value: MirConst::Int(src_size as i64),
            });

            let dest_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: dest_addr,
                name: target_name.clone(),
            });
            let v_dest_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_dest_len,
                value: MirConst::Int(target_size as i64),
            });

            let v_ptr_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: v_ptr_addr,
                name: ptr_name.clone(),
            });
            let v_ptr_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_ptr_len,
                value: MirConst::Int(ptr_size as i64),
            });

            // Emit delimiter addr/len (0/0 means DELIMITED BY SIZE)
            let (v_delim, v_delim_len) = if let Some(delim) = delimiter {
                self.emit_expr_as_addr_and_len(delim, func, instructions)
            } else {
                let null = func.new_value();
                instructions.push(MirInst::Const {
                    dest: null,
                    value: MirConst::Int(0),
                });
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                (null, zero)
            };

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_string_append".to_string(),
                args: vec![
                    src_addr,
                    v_src_len,
                    dest_addr,
                    v_dest_len,
                    v_ptr_addr,
                    v_ptr_len,
                    v_delim,
                    v_delim_len,
                ],
            });
        }
    }

    /// Lower INSPECT statement.
    fn lower_inspect(
        &self,
        target: &cobol_hir::HirDataRef,
        inspect_type: &cobol_hir::InspectType,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let (target_name, target_size) = match self.resolve_data_ref(target) {
            Some(info) => info,
            None => return,
        };

        match inspect_type {
            cobol_hir::InspectType::Tallying {
                tally_var,
                mode,
                search,
                before_initial,
                after_initial,
            } => {
                let (tally_name, tally_size) = match self.resolve_data_ref(tally_var) {
                    Some(info) => info,
                    None => return,
                };

                // target addr + len
                let v_target_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: v_target_addr,
                    name: target_name.clone(),
                });
                let v_target_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_target_len,
                    value: MirConst::Int(target_size as i64),
                });

                // tally addr + len
                let v_tally_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: v_tally_addr,
                    name: tally_name,
                });
                let v_tally_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_tally_len,
                    value: MirConst::Int(tally_size as i64),
                });

                // mode: 0=CHARACTERS, 1=ALL, 2=LEADING
                let mode_val = match mode {
                    cobol_hir::InspectTallyMode::Characters => 0,
                    cobol_hir::InspectTallyMode::All => 1,
                    cobol_hir::InspectTallyMode::Leading => 2,
                    cobol_hir::InspectTallyMode::Trailing => 3,
                };
                let v_mode = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_mode,
                    value: MirConst::Int(mode_val),
                });

                // search literal
                let (v_search, v_search_len) = self.emit_optional_expr(search, func, instructions);

                // before/after initial
                let (v_before, v_before_len) =
                    self.emit_optional_expr(before_initial, func, instructions);
                let (v_after, v_after_len) =
                    self.emit_optional_expr(after_initial, func, instructions);

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_inspect_tallying".to_string(),
                    args: vec![
                        v_target_addr,
                        v_target_len,
                        v_tally_addr,
                        v_tally_len,
                        v_mode,
                        v_search,
                        v_search_len,
                        v_before,
                        v_before_len,
                        v_after,
                        v_after_len,
                    ],
                });
            }
            cobol_hir::InspectType::Replacing {
                mode,
                search,
                replacement,
                before_initial,
                after_initial,
            } => {
                // target addr + len
                let v_target_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: v_target_addr,
                    name: target_name.clone(),
                });
                let v_target_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_target_len,
                    value: MirConst::Int(target_size as i64),
                });

                // mode: 0=CHARACTERS, 1=ALL, 2=LEADING, 3=FIRST, 4=TRAILING
                let mode_val = match mode {
                    cobol_hir::InspectReplaceMode::Characters => 0,
                    cobol_hir::InspectReplaceMode::All => 1,
                    cobol_hir::InspectReplaceMode::Leading => 2,
                    cobol_hir::InspectReplaceMode::First => 3,
                    cobol_hir::InspectReplaceMode::Trailing => 4,
                };
                let v_mode = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_mode,
                    value: MirConst::Int(mode_val),
                });

                // search literal
                let (v_search, v_search_len) = self.emit_optional_expr(search, func, instructions);

                // replacement
                let (v_repl, v_repl_len) =
                    self.emit_expr_as_addr_and_len(replacement, func, instructions);

                // before/after initial
                let (v_before, v_before_len) =
                    self.emit_optional_expr(before_initial, func, instructions);
                let (v_after, v_after_len) =
                    self.emit_optional_expr(after_initial, func, instructions);

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_inspect_replacing".to_string(),
                    args: vec![
                        v_target_addr,
                        v_target_len,
                        v_mode,
                        v_search,
                        v_search_len,
                        v_repl,
                        v_repl_len,
                        v_before,
                        v_before_len,
                        v_after,
                        v_after_len,
                    ],
                });
            }
            cobol_hir::InspectType::Converting {
                from,
                to,
                before_initial,
                after_initial,
            } => {
                // target addr + len
                let v_target_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: v_target_addr,
                    name: target_name.clone(),
                });
                let v_target_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_target_len,
                    value: MirConst::Int(target_size as i64),
                });

                // from chars
                let (v_from, v_from_len) = self.emit_expr_as_addr_and_len(from, func, instructions);

                // to chars
                let (v_to, v_to_len) = self.emit_expr_as_addr_and_len(to, func, instructions);

                // before/after initial
                let (v_before, v_before_len) =
                    self.emit_optional_expr(before_initial, func, instructions);
                let (v_after, v_after_len) =
                    self.emit_optional_expr(after_initial, func, instructions);

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_inspect_converting".to_string(),
                    args: vec![
                        v_target_addr,
                        v_target_len,
                        v_from,
                        v_from_len,
                        v_to,
                        v_to_len,
                        v_before,
                        v_before_len,
                        v_after,
                        v_after_len,
                    ],
                });
            }
        }
    }

    /// Lower UNSTRING statement.
    #[allow(clippy::too_many_arguments)]
    fn lower_unstring(
        &mut self,
        source: &cobol_hir::HirDataRef,
        delimiters: &[cobol_hir::UnstringDelimiter],
        targets: &[cobol_hir::UnstringTarget],
        pointer: Option<&cobol_hir::HirDataRef>,
        tallying: Option<&cobol_hir::HirDataRef>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let (source_name, source_size) = match self.resolve_data_ref(source) {
            Some(info) => info,
            None => return,
        };

        // Source addr + len
        let v_src_addr = func.new_value();
        instructions.push(MirInst::GlobalAddr {
            dest: v_src_addr,
            name: source_name,
        });
        let v_src_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: v_src_len,
            value: MirConst::Int(source_size as i64),
        });

        // Register all delimiters via cobolrt_unstring_begin + add_delim
        let v_num_delims = func.new_value();
        instructions.push(MirInst::Const {
            dest: v_num_delims,
            value: MirConst::Int(delimiters.len() as i64),
        });
        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_unstring_begin".to_string(),
            args: vec![v_num_delims],
        });
        for d in delimiters {
            let (addr, len) = self.emit_expr_as_addr_and_len(&d.value, func, instructions);
            let all_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: all_val,
                value: MirConst::Int(if d.all { 1 } else { 0 }),
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_unstring_add_delim".to_string(),
                args: vec![addr, len, all_val],
            });
        }

        // When no explicit POINTER clause, create a temporary global for the
        // implicit pointer (COBOL requires tracking position across target fields).
        let implicit_ptr_name = if pointer.is_none() {
            let name = format!("__UNSTRING_PTR_{}", self.module.globals.len());
            let idx = self.module.globals.len();
            self.module.globals.push(MirGlobal {
                name: name.clone(),
                ty: MirType::Bytes(4),
                initial_value: Some(MirConst::Str("0001".to_string())),
                offset: 0,
                redefines: None,
                parent_offset: None,
            });
            self.global_map.insert(name.clone(), idx);
            Some(name)
        } else {
            None
        };

        // Call cobolrt_unstring_field for each target sequentially
        for tgt in targets {
            let (tgt_name, tgt_size) = match self.resolve_data_ref(&tgt.target) {
                Some(info) => info,
                None => continue,
            };

            let v_tgt_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: v_tgt_addr,
                name: tgt_name,
            });
            let v_tgt_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_tgt_len,
                value: MirConst::Int(tgt_size as i64),
            });

            // Pointer field: use explicit pointer or implicit temporary
            let (v_ptr, v_ptr_len) = if let Some(p) = pointer {
                match self.resolve_data_ref(p) {
                    Some((pname, psize)) => {
                        let pa = func.new_value();
                        instructions.push(MirInst::GlobalAddr {
                            dest: pa,
                            name: pname,
                        });
                        let pl = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: pl,
                            value: MirConst::Int(psize as i64),
                        });
                        (pa, pl)
                    }
                    None => {
                        let null = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: null,
                            value: MirConst::Int(0),
                        });
                        let zero = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: zero,
                            value: MirConst::Int(0),
                        });
                        (null, zero)
                    }
                }
            } else if let Some(ref iname) = implicit_ptr_name {
                let pa = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: pa,
                    name: iname.clone(),
                });
                let pl = func.new_value();
                instructions.push(MirInst::Const {
                    dest: pl,
                    value: MirConst::Int(4),
                });
                (pa, pl)
            } else {
                let null = func.new_value();
                instructions.push(MirInst::Const {
                    dest: null,
                    value: MirConst::Int(0),
                });
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                (null, zero)
            };

            // Tally field
            let (v_tally, v_tally_len) = if let Some(t) = tallying {
                match self.resolve_data_ref(t) {
                    Some((tname, tsize)) => {
                        let ta = func.new_value();
                        instructions.push(MirInst::GlobalAddr {
                            dest: ta,
                            name: tname,
                        });
                        let tl = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: tl,
                            value: MirConst::Int(tsize as i64),
                        });
                        (ta, tl)
                    }
                    None => {
                        let null = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: null,
                            value: MirConst::Int(0),
                        });
                        let zero = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: zero,
                            value: MirConst::Int(0),
                        });
                        (null, zero)
                    }
                }
            } else {
                let null = func.new_value();
                instructions.push(MirInst::Const {
                    dest: null,
                    value: MirConst::Int(0),
                });
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                (null, zero)
            };

            // COUNT IN field
            let (v_count, v_count_len) = if let Some(ref ci) = tgt.count_in {
                match self.resolve_data_ref(ci) {
                    Some((cname, csize)) => {
                        let ca = func.new_value();
                        instructions.push(MirInst::GlobalAddr {
                            dest: ca,
                            name: cname,
                        });
                        let cl = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: cl,
                            value: MirConst::Int(csize as i64),
                        });
                        (ca, cl)
                    }
                    None => {
                        let null = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: null,
                            value: MirConst::Int(0),
                        });
                        let zero = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: zero,
                            value: MirConst::Int(0),
                        });
                        (null, zero)
                    }
                }
            } else {
                let null = func.new_value();
                instructions.push(MirInst::Const {
                    dest: null,
                    value: MirConst::Int(0),
                });
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                (null, zero)
            };

            // DELIMITER IN field
            let (v_delim_in, v_delim_in_len) = if let Some(ref di) = tgt.delimiter_in {
                match self.resolve_data_ref(di) {
                    Some((dname, dsize)) => {
                        let da = func.new_value();
                        instructions.push(MirInst::GlobalAddr {
                            dest: da,
                            name: dname,
                        });
                        let dl = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: dl,
                            value: MirConst::Int(dsize as i64),
                        });
                        (da, dl)
                    }
                    None => {
                        let null = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: null,
                            value: MirConst::Int(0),
                        });
                        let zero = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: zero,
                            value: MirConst::Int(0),
                        });
                        (null, zero)
                    }
                }
            } else {
                let null = func.new_value();
                instructions.push(MirInst::Const {
                    dest: null,
                    value: MirConst::Int(0),
                });
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                (null, zero)
            };

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_unstring_field".to_string(),
                args: vec![
                    v_src_addr,
                    v_src_len,
                    v_tgt_addr,
                    v_tgt_len,
                    v_ptr,
                    v_ptr_len,
                    v_tally,
                    v_tally_len,
                    v_count,
                    v_count_len,
                    v_delim_in,
                    v_delim_in_len,
                ],
            });
        }
    }

    /// Emit an optional HirExpr as (addr, len) pair. Returns null/0 if None.
    fn emit_optional_expr(
        &self,
        expr: &Option<cobol_hir::HirExpr>,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> (Value, Value) {
        match expr {
            Some(e) => self.emit_expr_as_addr_and_len(e, func, instructions),
            None => {
                let null = func.new_value();
                instructions.push(MirInst::Const {
                    dest: null,
                    value: MirConst::Int(0),
                });
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                (null, zero)
            }
        }
    }

    /// Emit an HirExpr as (addr_value, len_value) for passing to runtime.
    fn emit_expr_as_addr_and_len(
        &self,
        expr: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> (Value, Value) {
        match expr {
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                let addr = func.new_value();
                instructions.push(MirInst::Const {
                    dest: addr,
                    value: MirConst::Str(s.clone()),
                });
                let len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: len,
                    value: MirConst::Int(s.len() as i64),
                });
                (addr, len)
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                let s = n.to_string();
                let addr = func.new_value();
                instructions.push(MirInst::Const {
                    dest: addr,
                    value: MirConst::Str(s.clone()),
                });
                let len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: len,
                    value: MirConst::Int(s.len() as i64),
                });
                (addr, len)
            }
            cobol_hir::HirExpr::DataRef(dr) => match self.resolve_data_ref(dr) {
                Some((name, size)) => {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr { dest: addr, name });
                    let len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: len,
                        value: MirConst::Int(size as i64),
                    });
                    (addr, len)
                }
                None => {
                    let null = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: null,
                        value: MirConst::Int(0),
                    });
                    let zero = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: zero,
                        value: MirConst::Int(0),
                    });
                    (null, zero)
                }
            },
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Figurative(fig)) => {
                let s = match fig {
                    cobol_hir::FigurativeConstant::Space => " ".to_string(),
                    cobol_hir::FigurativeConstant::Zero => "0".to_string(),
                    cobol_hir::FigurativeConstant::HighValue => "\x7F".to_string(),
                    cobol_hir::FigurativeConstant::LowValue => "\x00".to_string(),
                    cobol_hir::FigurativeConstant::Quote => "\"".to_string(),
                    cobol_hir::FigurativeConstant::All => " ".to_string(),
                };
                let addr = func.new_value();
                instructions.push(MirInst::Const {
                    dest: addr,
                    value: MirConst::Str(s.clone()),
                });
                let len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: len,
                    value: MirConst::Int(s.len() as i64),
                });
                (addr, len)
            }
            _ => {
                let null = func.new_value();
                instructions.push(MirInst::Const {
                    dest: null,
                    value: MirConst::Int(0),
                });
                let zero = func.new_value();
                instructions.push(MirInst::Const {
                    dest: zero,
                    value: MirConst::Int(0),
                });
                (null, zero)
            }
        }
    }

    /// Lower ADD op1 [op2...] TO target (accumulate form)
    fn lower_add_to(
        &self,
        operands: &[cobol_hir::HirExpr],
        to: &[cobol_hir::HirDataRef],
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        for target_ref in to {
            // Use emit_expr_addr so subscripted targets work (e.g. ADD 1 TO WS-ITEM(I))
            let target_expr = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
            let (_target_base_addr, target_size) =
                match self.emit_expr_addr(&target_expr, func, instructions) {
                    Some(v) => v,
                    None => continue,
                };

            // Look up whether the target field is signed
            let tgt_name = self.interner.resolve(target_ref.name).to_string();
            let tgt_is_signed = self
                .data_item_pic_info(&tgt_name)
                .map(|(_, _, _, is_signed)| is_signed)
                .unwrap_or(false);

            for op in operands {
                if let Some((op_addr, op_size)) = self.emit_expr_addr(op, func, instructions) {
                    let op_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: op_len,
                        value: MirConst::Int(op_size as i64),
                    });

                    // For ADD ... TO target, target is both a source operand and the destination.
                    // We need two separate address computations since the runtime reads src and writes dest.
                    // Re-emit target addr for each use since runtime may read then write.
                    let target_expr2 = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                    let (target_addr1, _) =
                        match self.emit_expr_addr(&target_expr2, func, instructions) {
                            Some(v) => v,
                            None => continue,
                        };
                    let target_len1 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: target_len1,
                        value: MirConst::Int(target_size as i64),
                    });

                    let target_expr3 = cobol_hir::HirExpr::DataRef(Box::new(target_ref.clone()));
                    let (target_addr2, _) =
                        match self.emit_expr_addr(&target_expr3, func, instructions) {
                            Some(v) => v,
                            None => continue,
                        };
                    let target_len2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: target_len2,
                        value: MirConst::Int(target_size as i64),
                    });

                    let sf = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sf,
                        value: MirConst::Int(if tgt_is_signed { 1 } else { 0 }),
                    });

                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_add_numeric".to_string(),
                        args: vec![
                            op_addr,
                            op_len,
                            target_addr1,
                            target_len1,
                            target_addr2,
                            target_len2,
                            sf,
                        ],
                    });
                }
            }
        }
    }

    /// Lower a COMPUTE statement: evaluate the arithmetic expression and
    /// store the result in each target variable.
    ///
    /// The strategy:
    /// 1. Emit the arithmetic expression tree as a series of runtime calls,
    ///    producing a temporary display-format numeric result.
    /// 2. For each target, move the result into the target's storage using
    ///    cobolrt_move_numeric (which handles scale alignment and truncation).
    fn lower_compute(
        &self,
        targets: &[cobol_hir::HirDataRef],
        expr: &cobol_hir::HirExpr,
        rounded: bool,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // Evaluate the expression, producing (addr, size) of a temporary or
        // existing data item holding the result in display format.
        let (result_addr, result_size) = match self.emit_arithmetic_expr(expr, func, instructions) {
            Some(v) => v,
            None => return,
        };

        // Store the result into each target
        for target_ref in targets {
            let target_name_str = self.interner.resolve(target_ref.name).to_string();

            // Resolve target address and size (linkage or global)
            let (target_addr, target_size) =
                if let Some(info) = self.linkage_map.get(&target_name_str) {
                    (info.param_value, info.byte_size)
                } else if let Some((ref gname, size)) = self.resolve_data_ref(target_ref) {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: addr,
                        name: gname.clone(),
                    });
                    (addr, size)
                } else {
                    continue;
                };

            // Get target PIC info for proper numeric formatting
            let (_target_is_numeric, target_scale, target_dot_pos, _target_is_signed) = self
                .data_item_pic_info(&target_name_str)
                .unwrap_or((false, 0, -1, false));

            // Emit: cobolrt_store_compute_result(src, src_len,
            //          dest, dest_len, dest_scale, dest_dot_pos, rounded)
            // This runtime function auto-detects src_scale from '.' position in
            // the arithmetic result string (e.g. "25.0000" → scale=4).
            let v_result_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_result_len,
                value: MirConst::Int(result_size as i64),
            });

            let v_target_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_target_len,
                value: MirConst::Int(target_size as i64),
            });

            let v_target_scale = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_target_scale,
                value: MirConst::Int(target_scale as i64),
            });

            let v_dot_pos = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_dot_pos,
                value: MirConst::Int(target_dot_pos as i64),
            });

            let v_rounded = func.new_value();
            instructions.push(MirInst::Const {
                dest: v_rounded,
                value: MirConst::Int(if rounded { 1 } else { 0 }),
            });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_store_compute_result".to_string(),
                args: vec![
                    result_addr,
                    v_result_len,
                    target_addr,
                    v_target_len,
                    v_target_scale,
                    v_dot_pos,
                    v_rounded,
                ],
            });
        }
    }

    /// Recursively emit MIR instructions for an arithmetic expression tree.
    /// Returns (addr, size) of a value in display format (either an existing
    /// data item's address or a temporary buffer allocated by the runtime).
    fn emit_arithmetic_expr(
        &self,
        expr: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> Option<(Value, u32)> {
        match expr {
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                // Create a display-format string constant for the integer.
                // For negative values, include a leading '-' so the runtime
                // arithmetic functions can determine the sign.
                let s = if *n < 0 {
                    format!("-{}", n.unsigned_abs())
                } else {
                    format!("{}", n)
                };
                let size = s.len().max(1) as u32;
                let str_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: str_val,
                    value: MirConst::Str(s),
                });
                let len_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: len_val,
                    value: MirConst::Int(size as i64),
                });
                Some((str_val, size))
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Decimal(s)) => {
                // Decimal literal — pass through as a display-format string
                let size = s.len() as u32;
                let str_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: str_val,
                    value: MirConst::Str(s.clone()),
                });
                Some((str_val, size))
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Figurative(fig)) => {
                // Figurative constants in arithmetic: ZEROS is 0, others treated as 0
                let s = match fig {
                    cobol_hir::FigurativeConstant::Zero => "0".to_string(),
                    _ => "0".to_string(),
                };
                let size = s.len() as u32;
                let str_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: str_val,
                    value: MirConst::Str(s),
                });
                Some((str_val, size))
            }
            cobol_hir::HirExpr::DataRef(_) => {
                // Resolve data reference using existing emit_expr_addr which
                // handles subscripts, qualifiers, and reference modification.
                self.emit_expr_addr(expr, func, instructions)
            }
            cobol_hir::HirExpr::UnaryOp {
                op: cobol_hir::UnaryOp::Neg,
                operand,
            } => {
                // Negate: compute the operand, then call cobolrt_negate_numeric
                let (op_addr, op_size) = self.emit_arithmetic_expr(operand, func, instructions)?;
                let op_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: op_len,
                    value: MirConst::Int(op_size as i64),
                });

                // The result is written into a temporary buffer; the runtime
                // returns a pointer to it. We use a generous result size.
                let result_size = op_size + 1; // extra byte for potential sign
                let v_result_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_result_len,
                    value: MirConst::Int(result_size as i64),
                });

                let result_addr = func.new_value();
                instructions.push(MirInst::CallRuntime {
                    dest: Some(result_addr),
                    func: "cobolrt_negate_numeric".to_string(),
                    args: vec![op_addr, op_len],
                });

                Some((result_addr, result_size))
            }
            cobol_hir::HirExpr::BinaryOp { op, left, right } => {
                // Recursively evaluate both operands
                let (left_addr, left_size) = self.emit_arithmetic_expr(left, func, instructions)?;
                let (right_addr, right_size) =
                    self.emit_arithmetic_expr(right, func, instructions)?;

                let left_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: left_len,
                    value: MirConst::Int(left_size as i64),
                });

                let right_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: right_len,
                    value: MirConst::Int(right_size as i64),
                });

                // Choose the runtime function based on the operator.
                let rt_func = match op {
                    cobol_hir::BinaryOp::Add => "cobolrt_decimal_add",
                    cobol_hir::BinaryOp::Sub => "cobolrt_decimal_sub",
                    cobol_hir::BinaryOp::Mul => "cobolrt_decimal_mul",
                    cobol_hir::BinaryOp::Div => "cobolrt_decimal_div",
                    cobol_hir::BinaryOp::Pow => "cobolrt_decimal_pow",
                    _ => return None, // comparison operators not expected here
                };

                // The result size: for add/sub use max of both + 1, for mul use sum,
                // for div use left_size + a reasonable precision buffer.
                let result_size = match op {
                    cobol_hir::BinaryOp::Add | cobol_hir::BinaryOp::Sub => {
                        left_size.max(right_size) + 1
                    }
                    cobol_hir::BinaryOp::Mul => left_size + right_size,
                    cobol_hir::BinaryOp::Div | cobol_hir::BinaryOp::Pow => {
                        // Use a generous size for division/exponentiation results
                        left_size.max(right_size) + 10
                    }
                    _ => left_size.max(right_size),
                };

                let v_result_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v_result_len,
                    value: MirConst::Int(result_size as i64),
                });

                // Call the runtime function: result_addr = rt_func(left, left_len, right, right_len, result_len)
                // The runtime allocates a temporary buffer and returns its address.
                let result_addr = func.new_value();
                instructions.push(MirInst::CallRuntime {
                    dest: Some(result_addr),
                    func: rt_func.to_string(),
                    args: vec![left_addr, left_len, right_addr, right_len, v_result_len],
                });

                Some((result_addr, result_size))
            }
            cobol_hir::HirExpr::FunctionCall { name, args } => {
                self.emit_function_call(name, args, func, instructions)
            }
            _ => None,
        }
    }

    /// Emit MIR instructions for a COBOL intrinsic FUNCTION call.
    /// Returns (addr, size) of a temporary holding the result.
    fn emit_function_call(
        &self,
        name: &cobol_intern::Name,
        args: &[cobol_hir::HirExpr],
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> Option<(Value, u32)> {
        let func_name = self.interner.resolve(*name).to_string();

        match func_name.as_str() {
            "LENGTH" => {
                // FUNCTION LENGTH(field) returns the byte size of the field.
                // This is known at compile time from the data layout.
                if let Some(arg) = args.first() {
                    let size = match arg {
                        cobol_hir::HirExpr::DataRef(dr) => {
                            let arg_name = self.interner.resolve(dr.name).to_string();
                            if let Some(info) = self.linkage_map.get(&arg_name) {
                                info.byte_size
                            } else if let Some((_gname, sz)) = self.resolve_data_ref(dr) {
                                sz
                            } else {
                                0
                            }
                        }
                        cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                            s.len() as u32
                        }
                        cobol_hir::HirExpr::FunctionCall {
                            name: inner_name,
                            args: inner_args,
                        } => {
                            // Evaluate inner function to get result size
                            if let Some((_addr, sz)) =
                                self.emit_function_call(inner_name, inner_args, func, instructions)
                            {
                                sz
                            } else {
                                0
                            }
                        }
                        _ => 0,
                    };
                    // Return the length as a display-format numeric string
                    let s = format!("{}", size);
                    let result_size = s.len().max(1) as u32;
                    let str_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: str_val,
                        value: MirConst::Str(format!(
                            "{:0>width$}",
                            size,
                            width = result_size as usize
                        )),
                    });
                    Some((str_val, result_size))
                } else {
                    None
                }
            }
            "UPPER-CASE" | "LOWER-CASE" | "REVERSE" | "TRIM" => {
                // String functions: emit a runtime call.
                // All take (src, src_len, dest, dest_len) and write to a temp buffer.
                if let Some(arg) = args.first() {
                    let (src_addr, src_size) = match arg {
                        cobol_hir::HirExpr::DataRef(_) => {
                            self.emit_expr_addr(arg, func, instructions)?
                        }
                        cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                            let str_val = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: str_val,
                                value: MirConst::Str(s.clone()),
                            });
                            (str_val, s.len() as u32)
                        }
                        cobol_hir::HirExpr::FunctionCall {
                            name: inner_name,
                            args: inner_args,
                        } => self.emit_function_call(inner_name, inner_args, func, instructions)?,
                        _ => return None,
                    };

                    let src_len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len_val,
                        value: MirConst::Int(src_size as i64),
                    });

                    // The result size: for TRIM it could be smaller, but we allocate
                    // the same size and the runtime returns the actual length.
                    let result_size = src_size;
                    let result_len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: result_len_val,
                        value: MirConst::Int(result_size as i64),
                    });

                    let rt_func = match func_name.as_str() {
                        "UPPER-CASE" => "cobolrt_upper_case",
                        "LOWER-CASE" => "cobolrt_lower_case",
                        "REVERSE" => "cobolrt_reverse",
                        "TRIM" => "cobolrt_trim",
                        _ => unreachable!(),
                    };

                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: rt_func.to_string(),
                        args: vec![src_addr, src_len_val, result_len_val],
                    });

                    Some((result_addr, result_size))
                } else {
                    None
                }
            }
            "MAX" | "MIN" => {
                // MAX/MIN take two or more numeric arguments.
                // Evaluate each, then call the appropriate runtime function
                // which compares display-format values.
                if args.len() < 2 {
                    // With a single arg, just return that arg
                    if let Some(arg) = args.first() {
                        return self.emit_arithmetic_expr(arg, func, instructions);
                    }
                    return None;
                }
                // Start with the first argument
                let (mut best_addr, mut best_size) =
                    self.emit_arithmetic_expr(&args[0], func, instructions)?;

                let rt_func = match func_name.as_str() {
                    "MAX" => "cobolrt_max_numeric",
                    "MIN" => "cobolrt_min_numeric",
                    _ => unreachable!(),
                };

                for arg in &args[1..] {
                    let (arg_addr, arg_size) =
                        self.emit_arithmetic_expr(arg, func, instructions)?;

                    let best_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: best_len,
                        value: MirConst::Int(best_size as i64),
                    });
                    let arg_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: arg_len,
                        value: MirConst::Int(arg_size as i64),
                    });

                    let new_best = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(new_best),
                        func: rt_func.to_string(),
                        args: vec![best_addr, best_len, arg_addr, arg_len],
                    });
                    let new_size = best_size.max(arg_size);
                    best_addr = new_best;
                    best_size = new_size;
                }

                Some((best_addr, best_size))
            }
            "ORD" => {
                // FUNCTION ORD(char) returns ordinal position (1-based).
                // cobolrt_ord returns a display-format string pointer.
                if let Some(arg) = args.first() {
                    let (src_addr, src_size) = match arg {
                        cobol_hir::HirExpr::DataRef(_) => {
                            self.emit_expr_addr(arg, func, instructions)?
                        }
                        cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                            let str_val = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: str_val,
                                value: MirConst::Str(s.clone()),
                            });
                            (str_val, s.len() as u32)
                        }
                        _ => return None,
                    };
                    let src_len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len_val,
                        value: MirConst::Int(src_size as i64),
                    });
                    let result_size = 3u32; // 3 digits for 0-256
                    let dest_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dest_len,
                        value: MirConst::Int(result_size as i64),
                    });
                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: "cobolrt_ord".to_string(),
                        args: vec![src_addr, src_len_val, dest_len],
                    });
                    Some((result_addr, result_size))
                } else {
                    None
                }
            }
            "CHAR" => {
                // FUNCTION CHAR(n) returns the character at ordinal position n.
                // Argument is numeric; result is a 1-byte string.
                if let Some(arg) = args.first() {
                    let (src_addr, src_size) =
                        self.emit_arithmetic_expr(arg, func, instructions)?;
                    // Convert display-format to integer, then call cobolrt_char
                    let src_len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len_val,
                        value: MirConst::Int(src_size as i64),
                    });
                    let int_val = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(int_val),
                        func: "cobolrt_display_to_int".to_string(),
                        args: vec![src_addr, src_len_val],
                    });
                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: "cobolrt_char".to_string(),
                        args: vec![int_val],
                    });
                    Some((result_addr, 1))
                } else {
                    None
                }
            }
            "MOD" | "REM" => {
                // FUNCTION MOD(a, b) / FUNCTION REM(a, b)
                if args.len() >= 2 {
                    let (a_addr, a_size) =
                        self.emit_arithmetic_expr(&args[0], func, instructions)?;
                    let (b_addr, b_size) =
                        self.emit_arithmetic_expr(&args[1], func, instructions)?;
                    let a_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: a_len,
                        value: MirConst::Int(a_size as i64),
                    });
                    let b_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: b_len,
                        value: MirConst::Int(b_size as i64),
                    });
                    let rt_func = if func_name == "MOD" {
                        "cobolrt_mod"
                    } else {
                        "cobolrt_rem"
                    };
                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: rt_func.to_string(),
                        args: vec![a_addr, a_len, b_addr, b_len],
                    });
                    Some((result_addr, a_size))
                } else {
                    None
                }
            }
            "ABS" => {
                // FUNCTION ABS(n) returns absolute value
                if let Some(arg) = args.first() {
                    let (src_addr, src_size) =
                        self.emit_arithmetic_expr(arg, func, instructions)?;
                    let src_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len,
                        value: MirConst::Int(src_size as i64),
                    });
                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: "cobolrt_abs".to_string(),
                        args: vec![src_addr, src_len],
                    });
                    Some((result_addr, src_size))
                } else {
                    None
                }
            }
            "SIGN" => {
                // FUNCTION SIGN(n) returns +1, 0, or -1
                if let Some(arg) = args.first() {
                    let (src_addr, src_size) =
                        self.emit_arithmetic_expr(arg, func, instructions)?;
                    let src_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len,
                        value: MirConst::Int(src_size as i64),
                    });
                    let result_size = src_size.max(2); // need room for sign
                    let dest_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dest_len,
                        value: MirConst::Int(result_size as i64),
                    });
                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: "cobolrt_sign".to_string(),
                        args: vec![src_addr, src_len, dest_len],
                    });
                    Some((result_addr, result_size))
                } else {
                    None
                }
            }
            "INTEGER" | "INTEGER-PART" => {
                // For integer display values these are identity.
                if let Some(arg) = args.first() {
                    let (src_addr, src_size) =
                        self.emit_arithmetic_expr(arg, func, instructions)?;
                    let src_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len,
                        value: MirConst::Int(src_size as i64),
                    });
                    let rt = if func_name == "INTEGER" {
                        "cobolrt_integer"
                    } else {
                        "cobolrt_integer_part"
                    };
                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: rt.to_string(),
                        args: vec![src_addr, src_len],
                    });
                    Some((result_addr, src_size))
                } else {
                    None
                }
            }
            "NUMVAL" => {
                // FUNCTION NUMVAL(string) converts alphanumeric to numeric
                if let Some(arg) = args.first() {
                    let (src_addr, src_size) = match arg {
                        cobol_hir::HirExpr::DataRef(_) => {
                            self.emit_expr_addr(arg, func, instructions)?
                        }
                        cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                            let str_val = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: str_val,
                                value: MirConst::Str(s.clone()),
                            });
                            (str_val, s.len() as u32)
                        }
                        _ => return None,
                    };
                    let src_len_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: src_len_val,
                        value: MirConst::Int(src_size as i64),
                    });
                    let result_size = 18u32; // enough for most COBOL numerics
                    let dest_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: dest_len,
                        value: MirConst::Int(result_size as i64),
                    });
                    let result_addr = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(result_addr),
                        func: "cobolrt_numval".to_string(),
                        args: vec![src_addr, src_len_val, dest_len],
                    });
                    Some((result_addr, result_size))
                } else {
                    None
                }
            }
            "CURRENT-DATE" => {
                // FUNCTION CURRENT-DATE returns a 21-character date string
                let result_addr = func.new_value();
                instructions.push(MirInst::CallRuntime {
                    dest: Some(result_addr),
                    func: "cobolrt_current_date".to_string(),
                    args: vec![],
                });
                Some((result_addr, 21))
            }
            "WHEN-COMPILED" => {
                // FUNCTION WHEN-COMPILED returns a 21-character timestamp
                let result_addr = func.new_value();
                instructions.push(MirInst::CallRuntime {
                    dest: Some(result_addr),
                    func: "cobolrt_when_compiled".to_string(),
                    args: vec![],
                });
                Some((result_addr, 21))
            }
            _ => {
                // Unknown function -- return None
                None
            }
        }
    }

    /// Determine whether a comparison should use alphanumeric (byte-wise)
    /// or numeric comparison. Returns true if either operand is a string
    /// literal or the left operand has a PIC X (alphanumeric) type.
    fn is_alphanumeric_comparison(
        &self,
        left: &cobol_hir::HirExpr,
        right: &cobol_hir::HirExpr,
    ) -> bool {
        // If the right side is a string literal, it's alphanumeric
        if matches!(
            right,
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(_))
        ) {
            return true;
        }
        // If left side is a string literal, it's alphanumeric
        if matches!(
            left,
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(_))
        ) {
            return true;
        }
        // Figurative constants: SPACES, HIGH-VALUE, LOW-VALUE, QUOTE are alphanumeric;
        // ZEROS defers to the left operand's type
        if let cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Figurative(fig)) = right {
            match fig {
                cobol_hir::FigurativeConstant::Space
                | cobol_hir::FigurativeConstant::HighValue
                | cobol_hir::FigurativeConstant::LowValue
                | cobol_hir::FigurativeConstant::Quote
                | cobol_hir::FigurativeConstant::All => return true,
                cobol_hir::FigurativeConstant::Zero => { /* fall through to PIC check */ }
            }
        }
        if let cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Figurative(fig)) = left {
            match fig {
                cobol_hir::FigurativeConstant::Space
                | cobol_hir::FigurativeConstant::HighValue
                | cobol_hir::FigurativeConstant::LowValue
                | cobol_hir::FigurativeConstant::Quote
                | cobol_hir::FigurativeConstant::All => return true,
                cobol_hir::FigurativeConstant::Zero => { /* fall through to PIC check */ }
            }
        }
        // Check if the left data ref has an alphanumeric PIC (X)
        if let cobol_hir::HirExpr::DataRef(dr) = left {
            let name = self.interner.resolve(dr.name).to_string();
            for &item_id in &self.hir.working_storage {
                let item = &self.hir.data_items[item_id.into_raw()];
                let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
                if item_name.as_deref() == Some(&name) {
                    if let Some(ref pic) = item.storage.picture {
                        let upper = pic.pic_string.to_ascii_uppercase();
                        if upper.contains('X') || upper.contains('A') {
                            return true;
                        }
                    }
                    break;
                }
            }
        }
        false
    }

    /// Evaluate a condition expression and return the boolean MIR Value.
    fn eval_condition(
        &self,
        condition: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> Value {
        match condition {
            cobol_hir::HirExpr::Condition(cond) => match cond.as_ref() {
                cobol_hir::HirCondition::Comparison { left, op, right } => {
                    // Special case: index name comparison.
                    // Index names store byte offsets as i64.  We convert back to
                    // a logical 1-based value and compare as integers.
                    let left_index_name = match left {
                        cobol_hir::HirExpr::DataRef(dr) => {
                            let n = self.interner.resolve(dr.name).to_string();
                            if self.index_info.contains_key(&n) {
                                Some(n)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    if let Some(ref idx_name) = left_index_name {
                        let (_, _, elem_size) = self.index_info.get(idx_name).unwrap();
                        // Load index byte offset and convert to logical value
                        let idx_addr = func.new_value();
                        instructions.push(MirInst::GlobalAddr {
                            dest: idx_addr,
                            name: idx_name.clone(),
                        });
                        let byte_off = func.new_value();
                        instructions.push(MirInst::Load {
                            dest: byte_off,
                            addr: idx_addr,
                            ty: MirType::I64,
                        });
                        let esz = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: esz,
                            value: MirConst::Int(*elem_size as i64),
                        });
                        let zero_idx = func.new_value();
                        instructions.push(MirInst::IDiv {
                            dest: zero_idx,
                            left: byte_off,
                            right: esz,
                        });
                        let one_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: one_val,
                            value: MirConst::Int(1),
                        });
                        let logical_val = func.new_value();
                        instructions.push(MirInst::IAdd {
                            dest: logical_val,
                            left: zero_idx,
                            right: one_val,
                        });

                        // Evaluate the right-hand side as an integer
                        let rhs_val = match right {
                            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                                let v = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: v,
                                    value: MirConst::Int(*n),
                                });
                                v
                            }
                            cobol_hir::HirExpr::DataRef(rdr) => {
                                let rname = self.interner.resolve(rdr.name).to_string();
                                if let Some((_, _, r_esz)) = self.index_info.get(&rname) {
                                    // Another index: convert to logical
                                    let ra = func.new_value();
                                    instructions.push(MirInst::GlobalAddr {
                                        dest: ra,
                                        name: rname,
                                    });
                                    let rb = func.new_value();
                                    instructions.push(MirInst::Load {
                                        dest: rb,
                                        addr: ra,
                                        ty: MirType::I64,
                                    });
                                    let re = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: re,
                                        value: MirConst::Int(*r_esz as i64),
                                    });
                                    let r0 = func.new_value();
                                    instructions.push(MirInst::IDiv {
                                        dest: r0,
                                        left: rb,
                                        right: re,
                                    });
                                    let r1v = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: r1v,
                                        value: MirConst::Int(1),
                                    });
                                    let rl = func.new_value();
                                    instructions.push(MirInst::IAdd {
                                        dest: rl,
                                        left: r0,
                                        right: r1v,
                                    });
                                    rl
                                } else if let Some(&g_idx) = self.global_map.get(&rname) {
                                    let sz = match &self.module.globals[g_idx].ty {
                                        MirType::Bytes(n) => *n,
                                        _ => 1,
                                    };
                                    let ra = func.new_value();
                                    instructions.push(MirInst::GlobalAddr {
                                        dest: ra,
                                        name: rname,
                                    });
                                    let rs = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: rs,
                                        value: MirConst::Int(sz as i64),
                                    });
                                    let rv = func.new_value();
                                    instructions.push(MirInst::CallRuntime {
                                        dest: Some(rv),
                                        func: "cobolrt_display_to_int".to_string(),
                                        args: vec![ra, rs],
                                    });
                                    rv
                                } else {
                                    let v = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: v,
                                        value: MirConst::Int(0),
                                    });
                                    v
                                }
                            }
                            _ => {
                                let v = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: v,
                                    value: MirConst::Int(0),
                                });
                                v
                            }
                        };

                        // Integer comparison
                        let bool_result = func.new_value();
                        let cmp_inst = match op {
                            cobol_hir::BinaryOp::Eq => MirInst::ICmpEq {
                                dest: bool_result,
                                left: logical_val,
                                right: rhs_val,
                            },
                            cobol_hir::BinaryOp::Ne => MirInst::ICmpNe {
                                dest: bool_result,
                                left: logical_val,
                                right: rhs_val,
                            },
                            cobol_hir::BinaryOp::Lt => MirInst::ICmpLt {
                                dest: bool_result,
                                left: logical_val,
                                right: rhs_val,
                            },
                            cobol_hir::BinaryOp::Gt => MirInst::ICmpGt {
                                dest: bool_result,
                                left: logical_val,
                                right: rhs_val,
                            },
                            cobol_hir::BinaryOp::Le => MirInst::ICmpLe {
                                dest: bool_result,
                                left: logical_val,
                                right: rhs_val,
                            },
                            cobol_hir::BinaryOp::Ge => MirInst::ICmpGe {
                                dest: bool_result,
                                left: logical_val,
                                right: rhs_val,
                            },
                            _ => MirInst::ICmpEq {
                                dest: bool_result,
                                left: logical_val,
                                right: rhs_val,
                            },
                        };
                        instructions.push(cmp_inst);
                        return bool_result;
                    }

                    let left_info = self.emit_expr_addr(left, func, instructions);
                    let right_info = self.emit_expr_for_comparison(right, func, instructions);

                    // Determine if this is an alphanumeric comparison.
                    // Use alphanumeric compare when either operand is a string literal
                    // or the left operand has a PIC X type (non-numeric).
                    let is_alpha = self.is_alphanumeric_comparison(left, right);

                    match (left_info, right_info) {
                        (Some((l_addr, l_size)), Some((r_addr, r_size))) => {
                            let l_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: l_len,
                                value: MirConst::Int(l_size as i64),
                            });
                            let r_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: r_len,
                                value: MirConst::Int(r_size as i64),
                            });
                            let cmp_result = func.new_value();
                            if is_alpha {
                                instructions.push(MirInst::CallRuntime {
                                    dest: Some(cmp_result),
                                    func: "cobolrt_compare_alphanumeric".to_string(),
                                    args: vec![l_addr, l_len, r_addr, r_len],
                                });
                            } else {
                                // Check if either operand has a non-display encoding
                                let l_enc = self.operand_encoding(left);
                                let r_enc = self.operand_encoding(right);
                                if l_enc != 0 || r_enc != 0 {
                                    let le = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: le,
                                        value: MirConst::Int(l_enc),
                                    });
                                    let re = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: re,
                                        value: MirConst::Int(r_enc),
                                    });
                                    instructions.push(MirInst::CallRuntime {
                                        dest: Some(cmp_result),
                                        func: "cobolrt_compare_numeric_enc".to_string(),
                                        args: vec![l_addr, l_len, le, r_addr, r_len, re],
                                    });
                                } else {
                                    instructions.push(MirInst::CallRuntime {
                                        dest: Some(cmp_result),
                                        func: "cobolrt_compare_numeric".to_string(),
                                        args: vec![l_addr, l_len, r_addr, r_len],
                                    });
                                }
                            }

                            let zero_val = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: zero_val,
                                value: MirConst::Int(0),
                            });

                            let bool_result = func.new_value();
                            let cmp_inst = match op {
                                cobol_hir::BinaryOp::Eq => MirInst::ICmpEq {
                                    dest: bool_result,
                                    left: cmp_result,
                                    right: zero_val,
                                },
                                cobol_hir::BinaryOp::Ne => MirInst::ICmpNe {
                                    dest: bool_result,
                                    left: cmp_result,
                                    right: zero_val,
                                },
                                cobol_hir::BinaryOp::Lt => MirInst::ICmpLt {
                                    dest: bool_result,
                                    left: cmp_result,
                                    right: zero_val,
                                },
                                cobol_hir::BinaryOp::Gt => MirInst::ICmpGt {
                                    dest: bool_result,
                                    left: cmp_result,
                                    right: zero_val,
                                },
                                cobol_hir::BinaryOp::Le => MirInst::ICmpLe {
                                    dest: bool_result,
                                    left: cmp_result,
                                    right: zero_val,
                                },
                                cobol_hir::BinaryOp::Ge => MirInst::ICmpGe {
                                    dest: bool_result,
                                    left: cmp_result,
                                    right: zero_val,
                                },
                                _ => MirInst::ICmpEq {
                                    dest: bool_result,
                                    left: cmp_result,
                                    right: zero_val,
                                },
                            };
                            instructions.push(cmp_inst);
                            bool_result
                        }
                        _ => {
                            let v = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v,
                                value: MirConst::Bool(true),
                            });
                            v
                        }
                    }
                }
                cobol_hir::HirCondition::ConditionName(data_ref) => {
                    // 88-level condition name: resolve to comparison against parent
                    let cond_name = self.interner.resolve(data_ref.name).to_string();
                    if let Some((parent_id, ref value_pairs)) =
                        self.hir.condition_names.get(&cond_name)
                    {
                        let parent_item = &self.hir.data_items[parent_id.into_raw()];
                        let parent_name = parent_item
                            .name
                            .map(|n| self.interner.resolve(n).to_string())
                            .unwrap_or_default();
                        let parent_size = parent_item.storage.byte_size;

                        if self.global_map.contains_key(&parent_name) {
                            let format_cond_val =
                                |cv: &Option<cobol_hir::InitialValue>, psize: u32| -> String {
                                    match cv {
                                        Some(cobol_hir::InitialValue::Numeric(n, _)) => {
                                            let mut s = format!(
                                                "{:0>width$}",
                                                n.unsigned_abs(),
                                                width = psize as usize
                                            );
                                            if *n < 0 {
                                                let mut bytes = s.into_bytes();
                                                if !bytes.is_empty() && bytes[0] == b'0' {
                                                    bytes[0] = b'-';
                                                }
                                                s = String::from_utf8(bytes).unwrap();
                                            }
                                            s
                                        }
                                        Some(cobol_hir::InitialValue::String_(s)) => s.clone(),
                                        Some(cobol_hir::InitialValue::Zero) => {
                                            "0".repeat(psize as usize)
                                        }
                                        Some(cobol_hir::InitialValue::Space) => {
                                            " ".repeat(psize as usize)
                                        }
                                        _ => "0".repeat(psize as usize),
                                    }
                                };

                            // Evaluate each (value, optional thru) pair and OR them together.
                            // The condition is true if ANY pair matches.
                            let mut accumulated_result: Option<Value> = None;

                            for (cond_value, thru_value) in value_pairs.iter() {
                                // Each pair needs its own parent_addr/parent_len
                                let pair_parent_addr = func.new_value();
                                instructions.push(MirInst::GlobalAddr {
                                    dest: pair_parent_addr,
                                    name: parent_name.clone(),
                                });
                                let pair_parent_len = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: pair_parent_len,
                                    value: MirConst::Int(parent_size as i64),
                                });

                                let pair_result = if thru_value.is_some() {
                                    // THRU range: parent >= low_value AND parent <= high_value
                                    let low_str = format_cond_val(cond_value, parent_size);
                                    let high_str = format_cond_val(thru_value, parent_size);

                                    // Compare parent >= low
                                    let low_val = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: low_val,
                                        value: MirConst::Str(low_str.clone()),
                                    });
                                    let low_len = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: low_len,
                                        value: MirConst::Int(low_str.len() as i64),
                                    });
                                    let cmp_low = func.new_value();
                                    instructions.push(MirInst::CallRuntime {
                                        dest: Some(cmp_low),
                                        func: "cobolrt_compare_numeric".to_string(),
                                        args: vec![
                                            pair_parent_addr,
                                            pair_parent_len,
                                            low_val,
                                            low_len,
                                        ],
                                    });
                                    let zero1 = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: zero1,
                                        value: MirConst::Int(0),
                                    });
                                    let ge_result = func.new_value();
                                    instructions.push(MirInst::ICmpGe {
                                        dest: ge_result,
                                        left: cmp_low,
                                        right: zero1,
                                    });

                                    // Need a new parent_addr for the second comparison
                                    let parent_addr2 = func.new_value();
                                    instructions.push(MirInst::GlobalAddr {
                                        dest: parent_addr2,
                                        name: parent_name.clone(),
                                    });
                                    let parent_len2 = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: parent_len2,
                                        value: MirConst::Int(parent_size as i64),
                                    });

                                    // Compare parent <= high
                                    let high_val = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: high_val,
                                        value: MirConst::Str(high_str.clone()),
                                    });
                                    let high_len = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: high_len,
                                        value: MirConst::Int(high_str.len() as i64),
                                    });
                                    let cmp_high = func.new_value();
                                    instructions.push(MirInst::CallRuntime {
                                        dest: Some(cmp_high),
                                        func: "cobolrt_compare_numeric".to_string(),
                                        args: vec![parent_addr2, parent_len2, high_val, high_len],
                                    });
                                    let zero2 = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: zero2,
                                        value: MirConst::Int(0),
                                    });
                                    let le_result = func.new_value();
                                    instructions.push(MirInst::ICmpLe {
                                        dest: le_result,
                                        left: cmp_high,
                                        right: zero2,
                                    });

                                    // AND the two conditions (1*1=1, else 0)
                                    let and_result = func.new_value();
                                    instructions.push(MirInst::IMul {
                                        dest: and_result,
                                        left: ge_result,
                                        right: le_result,
                                    });
                                    and_result
                                } else {
                                    // Single value comparison
                                    let cond_str = format_cond_val(cond_value, parent_size);
                                    let cond_val = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: cond_val,
                                        value: MirConst::Str(cond_str.clone()),
                                    });
                                    let cond_len = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: cond_len,
                                        value: MirConst::Int(cond_str.len() as i64),
                                    });

                                    let cmp_result = func.new_value();
                                    instructions.push(MirInst::CallRuntime {
                                        dest: Some(cmp_result),
                                        func: "cobolrt_compare_numeric".to_string(),
                                        args: vec![
                                            pair_parent_addr,
                                            pair_parent_len,
                                            cond_val,
                                            cond_len,
                                        ],
                                    });

                                    let zero_val = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: zero_val,
                                        value: MirConst::Int(0),
                                    });

                                    let bool_result = func.new_value();
                                    instructions.push(MirInst::ICmpEq {
                                        dest: bool_result,
                                        left: cmp_result,
                                        right: zero_val,
                                    });
                                    bool_result
                                };

                                // OR this pair's result with the accumulated result
                                accumulated_result = Some(match accumulated_result {
                                    None => pair_result,
                                    Some(prev) => {
                                        // OR: add then check != 0
                                        let sum = func.new_value();
                                        instructions.push(MirInst::IAdd {
                                            dest: sum,
                                            left: prev,
                                            right: pair_result,
                                        });
                                        let zero = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: zero,
                                            value: MirConst::Int(0),
                                        });
                                        let or_result = func.new_value();
                                        instructions.push(MirInst::ICmpNe {
                                            dest: or_result,
                                            left: sum,
                                            right: zero,
                                        });
                                        or_result
                                    }
                                });
                            }

                            // If no value pairs existed, default to true
                            accumulated_result.unwrap_or_else(|| {
                                let v = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: v,
                                    value: MirConst::Bool(true),
                                });
                                v
                            })
                        } else {
                            let v = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v,
                                value: MirConst::Bool(true),
                            });
                            v
                        }
                    } else {
                        let v = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v,
                            value: MirConst::Bool(true),
                        });
                        v
                    }
                }
                cobol_hir::HirCondition::And(left, right) => {
                    let left_cond = cobol_hir::HirExpr::Condition(Box::new(left.as_ref().clone()));
                    let right_cond =
                        cobol_hir::HirExpr::Condition(Box::new(right.as_ref().clone()));
                    let l_val = self.eval_condition(&left_cond, func, instructions);
                    let r_val = self.eval_condition(&right_cond, func, instructions);
                    // AND: both must be 1, so multiply (1*1=1, else 0)
                    let result = func.new_value();
                    instructions.push(MirInst::IMul {
                        dest: result,
                        left: l_val,
                        right: r_val,
                    });
                    result
                }
                cobol_hir::HirCondition::Or(left, right) => {
                    let left_cond = cobol_hir::HirExpr::Condition(Box::new(left.as_ref().clone()));
                    let right_cond =
                        cobol_hir::HirExpr::Condition(Box::new(right.as_ref().clone()));
                    let l_val = self.eval_condition(&left_cond, func, instructions);
                    let r_val = self.eval_condition(&right_cond, func, instructions);
                    // OR: add then check != 0
                    let sum = func.new_value();
                    instructions.push(MirInst::IAdd {
                        dest: sum,
                        left: l_val,
                        right: r_val,
                    });
                    let zero = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: zero,
                        value: MirConst::Int(0),
                    });
                    let result = func.new_value();
                    instructions.push(MirInst::ICmpNe {
                        dest: result,
                        left: sum,
                        right: zero,
                    });
                    result
                }
                cobol_hir::HirCondition::Not(inner) => {
                    let inner_cond =
                        cobol_hir::HirExpr::Condition(Box::new(inner.as_ref().clone()));
                    let inner_val = self.eval_condition(&inner_cond, func, instructions);
                    // NOT: flip 0↔1
                    let zero = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: zero,
                        value: MirConst::Int(0),
                    });
                    let result = func.new_value();
                    instructions.push(MirInst::ICmpEq {
                        dest: result,
                        left: inner_val,
                        right: zero,
                    });
                    result
                }
                cobol_hir::HirCondition::ClassCheck { operand, class } => {
                    // Class condition: call runtime to check if data matches class
                    if let Some((addr, size)) = self.emit_expr_addr(operand, func, instructions) {
                        let len_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: len_val,
                            value: MirConst::Int(size as i64),
                        });

                        // Select the appropriate runtime function
                        let rt_func = match class {
                            cobol_hir::ClassType::Numeric => "cobolrt_is_numeric",
                            cobol_hir::ClassType::Alphabetic => "cobolrt_is_alphabetic",
                            cobol_hir::ClassType::AlphabeticLower => "cobolrt_is_alphabetic_lower",
                            cobol_hir::ClassType::AlphabeticUpper => "cobolrt_is_alphabetic_upper",
                        };

                        let call_result = func.new_value();
                        instructions.push(MirInst::CallRuntime {
                            dest: Some(call_result),
                            func: rt_func.to_string(),
                            args: vec![addr, len_val],
                        });

                        // Runtime returns 1 (true) or 0 (false); compare != 0
                        let zero_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: zero_val,
                            value: MirConst::Int(0),
                        });
                        let bool_result = func.new_value();
                        instructions.push(MirInst::ICmpNe {
                            dest: bool_result,
                            left: call_result,
                            right: zero_val,
                        });
                        bool_result
                    } else {
                        // Could not resolve operand; fall back to true
                        let v = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v,
                            value: MirConst::Bool(true),
                        });
                        v
                    }
                }
                cobol_hir::HirCondition::SignCheck { operand, sign } => {
                    if let Some((addr, size)) = self.emit_expr_addr(operand, func, instructions) {
                        let len_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: len_val,
                            value: MirConst::Int(size as i64),
                        });

                        // Scale is not needed for sign determination; pass 0
                        let scale_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: scale_val,
                            value: MirConst::Int(0),
                        });

                        // mode: 0 = POSITIVE, 1 = NEGATIVE, 2 = ZERO
                        let mode = match sign {
                            cobol_hir::SignCheckType::Positive => 0i64,
                            cobol_hir::SignCheckType::Negative => 1i64,
                            cobol_hir::SignCheckType::Zero => 2i64,
                        };
                        let mode_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: mode_val,
                            value: MirConst::Int(mode),
                        });

                        let call_result = func.new_value();
                        instructions.push(MirInst::CallRuntime {
                            dest: Some(call_result),
                            func: "cobolrt_sign_check".to_string(),
                            args: vec![addr, len_val, scale_val, mode_val],
                        });

                        // Runtime returns 1 (true) or 0 (false); compare != 0
                        let zero_val = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: zero_val,
                            value: MirConst::Int(0),
                        });
                        let bool_result = func.new_value();
                        instructions.push(MirInst::ICmpNe {
                            dest: bool_result,
                            left: call_result,
                            right: zero_val,
                        });
                        bool_result
                    } else {
                        // Could not resolve operand; fall back to true
                        let v = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v,
                            value: MirConst::Bool(true),
                        });
                        v
                    }
                }
            },
            _ => {
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Bool(true),
                });
                v
            }
        }
    }

    /// Emit an expression for use as a comparison operand.
    fn emit_expr_for_comparison(
        &self,
        expr: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> Option<(Value, u32)> {
        match expr {
            cobol_hir::HirExpr::DataRef(_) => self.emit_expr_addr(expr, func, instructions),
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                // Create a string constant in display format for comparison.
                // For negative values, include a leading '-' so the runtime
                // parse_display_numeric can determine the sign.
                let s = if *n < 0 {
                    format!("-{}", n.unsigned_abs())
                } else {
                    format!("{}", n)
                };
                let size = s.len() as u32;
                let str_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: str_val,
                    value: MirConst::Str(s),
                });
                Some((str_val, size))
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                let size = s.len() as u32;
                let str_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: str_val,
                    value: MirConst::Str(s.clone()),
                });
                Some((str_val, size))
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Figurative(fig)) => {
                // Figurative constants in comparisons: generate a 1-byte constant.
                // The runtime comparison functions handle length differences by padding:
                //   - cobolrt_compare_alphanumeric pads shorter with spaces
                //   - cobolrt_compare_numeric converts to integer values
                let s = match fig {
                    cobol_hir::FigurativeConstant::Zero => "0".to_string(),
                    cobol_hir::FigurativeConstant::Space => " ".to_string(),
                    cobol_hir::FigurativeConstant::HighValue => {
                        String::from_utf8(vec![0xFF]).unwrap_or_default()
                    }
                    cobol_hir::FigurativeConstant::LowValue => {
                        String::from_utf8(vec![0x00]).unwrap_or_default()
                    }
                    cobol_hir::FigurativeConstant::Quote => "\"".to_string(),
                    cobol_hir::FigurativeConstant::All => " ".to_string(),
                };
                let size = s.len() as u32;
                let str_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: str_val,
                    value: MirConst::Str(s),
                });
                Some((str_val, size))
            }
            cobol_hir::HirExpr::UnaryOp { .. }
            | cobol_hir::HirExpr::BinaryOp { .. }
            | cobol_hir::HirExpr::FunctionCall { .. } => {
                // Delegate arithmetic expressions (including unary negation)
                // to the arithmetic expression emitter.
                self.emit_arithmetic_expr(expr, func, instructions)
            }
            _ => None,
        }
    }

    /// Lower an IF statement into multiple basic blocks.
    fn lower_if(
        &mut self,
        condition: &cobol_hir::HirExpr,
        then_branch: &[cobol_hir::HirStatement],
        else_branch: Option<&[cobol_hir::HirStatement]>,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let cond_val = self.eval_condition(condition, func, instructions);

        let then_block_id = func.new_block();
        let else_block_id = func.new_block();
        let merge_block_id = func.new_block();

        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Branch {
                cond: cond_val,
                then_block: then_block_id,
                else_block: else_block_id,
            },
        });

        // Then block
        let mut then_insts = Vec::new();
        let mut then_bid = then_block_id;
        for stmt in then_branch {
            self.lower_statement_to_blocks(stmt, func, &mut then_bid, &mut then_insts);
        }
        func.blocks.push(BasicBlock {
            id: then_bid,
            params: Vec::new(),
            instructions: then_insts,
            terminator: Terminator::Goto(merge_block_id),
        });

        // Else block
        let mut else_insts = Vec::new();
        let mut else_bid = else_block_id;
        if let Some(else_stmts) = else_branch {
            for stmt in else_stmts {
                self.lower_statement_to_blocks(stmt, func, &mut else_bid, &mut else_insts);
            }
        }
        func.blocks.push(BasicBlock {
            id: else_bid,
            params: Vec::new(),
            instructions: else_insts,
            terminator: Terminator::Goto(merge_block_id),
        });

        *current_block_id = merge_block_id;
    }

    /// Lower an EVALUATE statement into a chain of conditional branches.
    ///
    /// Each WHEN clause becomes a comparison + conditional branch:
    ///   - If the condition matches, jump to the WHEN body block
    ///   - Otherwise, fall through to the next WHEN test
    ///     WHEN OTHER (indicated by empty conditions) is the default fallthrough.
    ///     After any body block, jump to the merge block.
    fn lower_evaluate(
        &mut self,
        _subjects: &[cobol_hir::HirExpr],
        whens: &[cobol_hir::WhenClause],
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        if whens.is_empty() {
            return;
        }

        let merge_block_id = func.new_block();

        // Separate WHEN OTHER (empty conditions) from regular WHEN clauses
        let mut regular_whens: Vec<&cobol_hir::WhenClause> = Vec::new();
        let mut other_when: Option<&cobol_hir::WhenClause> = None;

        for w in whens {
            if w.conditions.is_empty() {
                other_when = Some(w);
            } else {
                regular_whens.push(w);
            }
        }

        // Create body blocks for each WHEN and one for WHEN OTHER / default
        let mut body_blocks: Vec<BlockId> = Vec::new();
        for _ in &regular_whens {
            body_blocks.push(func.new_block());
        }
        let default_block = func.new_block();

        // Build the chain of comparisons. For each regular WHEN, we emit:
        //   test block: evaluate condition -> branch to body or next test
        for (i, when_clause) in regular_whens.iter().enumerate() {
            // The condition in HIR is already a comparison (subject == value).
            // When multiple WHEN values are stacked (fall-through), there will
            // be multiple conditions that should be OR-ed together.
            let cond_val = if when_clause.conditions.is_empty() {
                // Should not happen for regular whens, but handle gracefully
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Bool(false),
                });
                v
            } else if when_clause.conditions.len() == 1 {
                self.eval_condition(&when_clause.conditions[0], func, instructions)
            } else {
                // Multiple stacked WHEN values: OR all conditions together
                let first = self.eval_condition(&when_clause.conditions[0], func, instructions);
                let mut combined = first;
                for cond in &when_clause.conditions[1..] {
                    let next = self.eval_condition(cond, func, instructions);
                    let sum = func.new_value();
                    instructions.push(MirInst::IAdd {
                        dest: sum,
                        left: combined,
                        right: next,
                    });
                    let zero = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: zero,
                        value: MirConst::Int(0),
                    });
                    let result = func.new_value();
                    instructions.push(MirInst::ICmpNe {
                        dest: result,
                        left: sum,
                        right: zero,
                    });
                    combined = result;
                }
                combined
            };

            let body_block = body_blocks[i];
            let next_test_block = if i + 1 < regular_whens.len() {
                func.new_block() // next test block
            } else {
                default_block // no more tests, fall to default
            };

            // Finalize current test block
            let current_insts = std::mem::take(instructions);
            func.blocks.push(BasicBlock {
                id: *current_block_id,
                params: Vec::new(),
                instructions: current_insts,
                terminator: Terminator::Branch {
                    cond: cond_val,
                    then_block: body_block,
                    else_block: next_test_block,
                },
            });

            *current_block_id = next_test_block;
        }

        // Emit body blocks for each regular WHEN
        for (i, when_clause) in regular_whens.iter().enumerate() {
            let mut body_insts = Vec::new();
            let mut body_bid = body_blocks[i];
            for stmt in &when_clause.statements {
                self.lower_statement_to_blocks(stmt, func, &mut body_bid, &mut body_insts);
            }
            func.blocks.push(BasicBlock {
                id: body_bid,
                params: Vec::new(),
                instructions: body_insts,
                terminator: Terminator::Goto(merge_block_id),
            });
        }

        // Emit default block (WHEN OTHER or empty fallthrough)
        let mut default_insts = Vec::new();
        let mut default_bid = default_block;
        if let Some(other) = other_when {
            for stmt in &other.statements {
                self.lower_statement_to_blocks(stmt, func, &mut default_bid, &mut default_insts);
            }
        }
        func.blocks.push(BasicBlock {
            id: default_bid,
            params: Vec::new(),
            instructions: default_insts,
            terminator: Terminator::Goto(merge_block_id),
        });

        *current_block_id = merge_block_id;
    }

    /// Lower SET statement.
    fn lower_set(
        &self,
        target: &cobol_hir::HirDataRef,
        action: &cobol_hir::SetAction,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let target_name = self.interner.resolve(target.name).to_string();

        // Handle SET condition-name TO TRUE
        if matches!(action, cobol_hir::SetAction::ConditionTrue) {
            if let Some((parent_id, ref value_pairs)) = self.hir.condition_names.get(&target_name) {
                let parent_item = &self.hir.data_items[parent_id.into_raw()];
                let parent_name = parent_item
                    .name
                    .map(|n| self.interner.resolve(n).to_string())
                    .unwrap_or_default();
                let parent_size = parent_item.storage.byte_size;

                // Use the first value from the value pairs (standard COBOL behavior)
                let cond_value = value_pairs.first().map(|(v, _)| v);

                if self.global_map.contains_key(&parent_name) {
                    let parent_addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: parent_addr,
                        name: parent_name.clone(),
                    });

                    // Format the condition value as a string and move it to the parent
                    let val_str = match cond_value {
                        Some(Some(cobol_hir::InitialValue::Numeric(n, _))) => {
                            let mut s = format!(
                                "{:0>width$}",
                                n.unsigned_abs(),
                                width = parent_size as usize
                            );
                            if *n < 0 {
                                let mut bytes = s.into_bytes();
                                if !bytes.is_empty() && bytes[0] == b'0' {
                                    bytes[0] = b'-';
                                }
                                s = String::from_utf8(bytes).unwrap();
                            }
                            s
                        }
                        Some(Some(cobol_hir::InitialValue::String_(s))) => {
                            format!("{:<width$}", s, width = parent_size as usize)
                        }
                        Some(Some(cobol_hir::InitialValue::Zero)) => {
                            "0".repeat(parent_size as usize)
                        }
                        Some(Some(cobol_hir::InitialValue::Space)) => {
                            " ".repeat(parent_size as usize)
                        }
                        _ => "0".repeat(parent_size as usize),
                    };

                    let val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: val,
                        value: MirConst::Str(val_str.clone()),
                    });
                    let val_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: val_len,
                        value: MirConst::Int(val_str.len() as i64),
                    });
                    let parent_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: parent_len,
                        value: MirConst::Int(parent_size as i64),
                    });
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_move_alphanumeric".to_string(),
                        args: vec![val, val_len, parent_addr, parent_len],
                    });
                }
            }
            return;
        }

        let is_index = self.index_info.contains_key(&target_name);

        if !self.global_map.contains_key(&target_name) {
            return;
        }

        let target_addr = func.new_value();
        instructions.push(MirInst::GlobalAddr {
            dest: target_addr,
            name: target_name.clone(),
        });

        if is_index {
            // Index names store byte offsets: (logical_value - 1) * element_size.
            let elem_size = self
                .index_info
                .get(&target_name)
                .map(|(_, _, esz)| *esz)
                .unwrap_or(1);
            match action {
                cobol_hir::SetAction::To(expr) => {
                    // SET idx TO n  =>  store (n - 1) * element_size
                    let val = self.eval_set_expr(expr, func, instructions);
                    let one = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: one,
                        value: MirConst::Int(1),
                    });
                    let val_minus1 = func.new_value();
                    instructions.push(MirInst::ISub {
                        dest: val_minus1,
                        left: val,
                        right: one,
                    });
                    let esz_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: esz_val,
                        value: MirConst::Int(elem_size as i64),
                    });
                    let byte_offset = func.new_value();
                    instructions.push(MirInst::IMul {
                        dest: byte_offset,
                        left: val_minus1,
                        right: esz_val,
                    });
                    instructions.push(MirInst::Store {
                        addr: target_addr,
                        value: byte_offset,
                    });
                }
                cobol_hir::SetAction::UpBy(expr) => {
                    // SET idx UP BY n  =>  add n * element_size
                    let add_val = self.eval_set_expr(expr, func, instructions);
                    let esz_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: esz_val,
                        value: MirConst::Int(elem_size as i64),
                    });
                    let delta = func.new_value();
                    instructions.push(MirInst::IMul {
                        dest: delta,
                        left: add_val,
                        right: esz_val,
                    });
                    let current = func.new_value();
                    instructions.push(MirInst::Load {
                        dest: current,
                        addr: target_addr,
                        ty: MirType::I64,
                    });
                    let result = func.new_value();
                    instructions.push(MirInst::IAdd {
                        dest: result,
                        left: current,
                        right: delta,
                    });
                    instructions.push(MirInst::Store {
                        addr: target_addr,
                        value: result,
                    });
                }
                cobol_hir::SetAction::DownBy(expr) => {
                    // SET idx DOWN BY n  =>  subtract n * element_size
                    let sub_val = self.eval_set_expr(expr, func, instructions);
                    let esz_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: esz_val,
                        value: MirConst::Int(elem_size as i64),
                    });
                    let delta = func.new_value();
                    instructions.push(MirInst::IMul {
                        dest: delta,
                        left: sub_val,
                        right: esz_val,
                    });
                    let current = func.new_value();
                    instructions.push(MirInst::Load {
                        dest: current,
                        addr: target_addr,
                        ty: MirType::I64,
                    });
                    let result = func.new_value();
                    instructions.push(MirInst::ISub {
                        dest: result,
                        left: current,
                        right: delta,
                    });
                    instructions.push(MirInst::Store {
                        addr: target_addr,
                        value: result,
                    });
                }
                cobol_hir::SetAction::ConditionTrue => unreachable!(),
            }
        } else {
            let target_size = if let Some(&idx) = self.global_map.get(&target_name) {
                match &self.module.globals[idx].ty {
                    MirType::Bytes(n) => *n,
                    _ => 4,
                }
            } else {
                4
            };
            match action {
                cobol_hir::SetAction::To(expr) => {
                    let val = self.eval_set_expr(expr, func, instructions);
                    let size_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: size_val,
                        value: MirConst::Int(target_size as i64),
                    });
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_int_to_display".to_string(),
                        args: vec![val, target_addr, size_val],
                    });
                }
                cobol_hir::SetAction::UpBy(e) | cobol_hir::SetAction::DownBy(e) => {
                    let size_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: size_val,
                        value: MirConst::Int(target_size as i64),
                    });
                    let current = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(current),
                        func: "cobolrt_display_to_int".to_string(),
                        args: vec![target_addr, size_val],
                    });
                    let delta = self.eval_set_expr(e, func, instructions);
                    let result = func.new_value();
                    match action {
                        cobol_hir::SetAction::UpBy(_) => {
                            instructions.push(MirInst::IAdd {
                                dest: result,
                                left: current,
                                right: delta,
                            });
                        }
                        cobol_hir::SetAction::DownBy(_) => {
                            instructions.push(MirInst::ISub {
                                dest: result,
                                left: current,
                                right: delta,
                            });
                        }
                        _ => unreachable!(),
                    }
                    let size_val2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: size_val2,
                        value: MirConst::Int(target_size as i64),
                    });
                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_int_to_display".to_string(),
                        args: vec![result, target_addr, size_val2],
                    });
                }
                cobol_hir::SetAction::ConditionTrue => unreachable!(),
            }
        }
    }

    /// Evaluate a SET expression to an i64 value.
    fn eval_set_expr(
        &self,
        expr: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) -> Value {
        match expr {
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Int(*n),
                });
                v
            }
            cobol_hir::HirExpr::DataRef(dr) => {
                let name = self.interner.resolve(dr.name).to_string();
                if self.index_info.contains_key(&name) && self.global_map.contains_key(&name) {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: addr,
                        name: name.clone(),
                    });
                    let val = func.new_value();
                    instructions.push(MirInst::Load {
                        dest: val,
                        addr,
                        ty: MirType::I64,
                    });
                    return val;
                }
                if let Some(&g_idx) = self.global_map.get(&name) {
                    let size = match &self.module.globals[g_idx].ty {
                        MirType::Bytes(n) => *n,
                        _ => 1,
                    };
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr { dest: addr, name });
                    let size_val = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: size_val,
                        value: MirConst::Int(size as i64),
                    });
                    let val = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(val),
                        func: "cobolrt_display_to_int".to_string(),
                        args: vec![addr, size_val],
                    });
                    val
                } else {
                    let v = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: v,
                        value: MirConst::Int(0),
                    });
                    v
                }
            }
            _ => {
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Int(0),
                });
                v
            }
        }
    }

    /// Lower SEARCH statement into a loop.
    fn lower_search(
        &mut self,
        table: &cobol_hir::HirDataRef,
        at_end: &[cobol_hir::HirStatement],
        whens: &[cobol_hir::SearchWhen],
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let table_name = self.interner.resolve(table.name).to_string();

        let (index_name, occurs_max) = {
            let mut found = None;
            for (idx_name, (tbl, max, _elem_sz)) in &self.index_info {
                if *tbl == table_name {
                    found = Some((idx_name.clone(), *max));
                    break;
                }
            }
            match found {
                Some(f) => f,
                None => return,
            }
        };

        let check_block = func.new_block();
        let at_end_block = func.new_block();
        let increment_block = func.new_block();
        let merge_block = func.new_block();

        let mut when_body_blocks: Vec<BlockId> = Vec::new();
        for _ in whens {
            when_body_blocks.push(func.new_block());
        }

        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Goto(check_block),
        });

        // --- Check block: bounds check ---
        // Index stores a byte offset. Convert to logical 1-based value for bounds check.
        let elem_size = self
            .index_info
            .get(&index_name)
            .map(|(_, _, esz)| *esz)
            .unwrap_or(1);
        let mut check_insts = Vec::new();
        let idx_addr = func.new_value();
        check_insts.push(MirInst::GlobalAddr {
            dest: idx_addr,
            name: index_name.clone(),
        });
        let idx_byte_off = func.new_value();
        check_insts.push(MirInst::Load {
            dest: idx_byte_off,
            addr: idx_addr,
            ty: MirType::I64,
        });
        let esz_val = func.new_value();
        check_insts.push(MirInst::Const {
            dest: esz_val,
            value: MirConst::Int(elem_size as i64),
        });
        let zero_based = func.new_value();
        check_insts.push(MirInst::IDiv {
            dest: zero_based,
            left: idx_byte_off,
            right: esz_val,
        });
        let one_c = func.new_value();
        check_insts.push(MirInst::Const {
            dest: one_c,
            value: MirConst::Int(1),
        });
        let idx_val = func.new_value();
        check_insts.push(MirInst::IAdd {
            dest: idx_val,
            left: zero_based,
            right: one_c,
        });
        let max_val = func.new_value();
        check_insts.push(MirInst::Const {
            dest: max_val,
            value: MirConst::Int(occurs_max as i64),
        });
        let exceeded = func.new_value();
        check_insts.push(MirInst::ICmpGt {
            dest: exceeded,
            left: idx_val,
            right: max_val,
        });

        let first_when_test = if whens.is_empty() {
            at_end_block
        } else {
            func.new_block()
        };

        func.blocks.push(BasicBlock {
            id: check_block,
            params: Vec::new(),
            instructions: check_insts,
            terminator: Terminator::Branch {
                cond: exceeded,
                then_block: at_end_block,
                else_block: first_when_test,
            },
        });

        // --- WHEN test blocks ---
        let mut current_test_block = first_when_test;
        for (i, when_clause) in whens.iter().enumerate() {
            let mut test_insts = Vec::new();
            let cond_val = self.eval_condition(&when_clause.condition, func, &mut test_insts);
            let body_block = when_body_blocks[i];
            let next_test = if i + 1 < whens.len() {
                func.new_block()
            } else {
                increment_block
            };

            func.blocks.push(BasicBlock {
                id: current_test_block,
                params: Vec::new(),
                instructions: test_insts,
                terminator: Terminator::Branch {
                    cond: cond_val,
                    then_block: body_block,
                    else_block: next_test,
                },
            });
            current_test_block = next_test;
        }

        // --- WHEN body blocks ---
        for (i, when_clause) in whens.iter().enumerate() {
            let mut body_insts = Vec::new();
            let mut body_bid = when_body_blocks[i];
            for stmt in &when_clause.body {
                self.lower_statement_to_blocks(stmt, func, &mut body_bid, &mut body_insts);
            }
            func.blocks.push(BasicBlock {
                id: body_bid,
                params: Vec::new(),
                instructions: body_insts,
                terminator: Terminator::Goto(merge_block),
            });
        }

        // --- AT END block ---
        let mut at_end_insts = Vec::new();
        let mut at_end_bid = at_end_block;
        for stmt in at_end {
            self.lower_statement_to_blocks(stmt, func, &mut at_end_bid, &mut at_end_insts);
        }
        func.blocks.push(BasicBlock {
            id: at_end_bid,
            params: Vec::new(),
            instructions: at_end_insts,
            terminator: Terminator::Goto(merge_block),
        });

        // --- Increment block ---
        // Increment by element_size (one logical position = element_size bytes).
        let mut inc_insts = Vec::new();
        let inc_addr = func.new_value();
        inc_insts.push(MirInst::GlobalAddr {
            dest: inc_addr,
            name: index_name.clone(),
        });
        let inc_val = func.new_value();
        inc_insts.push(MirInst::Load {
            dest: inc_val,
            addr: inc_addr,
            ty: MirType::I64,
        });
        let step = func.new_value();
        inc_insts.push(MirInst::Const {
            dest: step,
            value: MirConst::Int(elem_size as i64),
        });
        let new_val = func.new_value();
        inc_insts.push(MirInst::IAdd {
            dest: new_val,
            left: inc_val,
            right: step,
        });
        inc_insts.push(MirInst::Store {
            addr: inc_addr,
            value: new_val,
        });
        func.blocks.push(BasicBlock {
            id: increment_block,
            params: Vec::new(),
            instructions: inc_insts,
            terminator: Terminator::Goto(check_block),
        });

        *current_block_id = merge_block;
    }

    /// Lower PERFORM statement.
    fn lower_perform(
        &mut self,
        perform_type: &cobol_hir::PerformType,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        match perform_type {
            cobol_hir::PerformType::Inline { statements } => {
                self.lower_perform_inline(statements, func, current_block_id, instructions);
            }
            cobol_hir::PerformType::OutOfLine { target, thru } => {
                self.lower_perform_out_of_line(
                    *target,
                    thru.as_ref().copied(),
                    func,
                    current_block_id,
                    instructions,
                );
            }
            cobol_hir::PerformType::Times {
                target,
                thru,
                times,
            } => {
                self.lower_perform_times(
                    *target,
                    thru.as_ref().copied(),
                    times,
                    func,
                    current_block_id,
                    instructions,
                );
            }
            cobol_hir::PerformType::Until {
                target,
                thru,
                condition,
                test_before,
            } => {
                self.lower_perform_until(
                    *target,
                    thru.as_ref().copied(),
                    condition,
                    *test_before,
                    func,
                    current_block_id,
                    instructions,
                );
            }
            cobol_hir::PerformType::InlineTimes { times, statements } => {
                self.lower_perform_inline_times(
                    times,
                    statements,
                    func,
                    current_block_id,
                    instructions,
                );
            }
            cobol_hir::PerformType::InlineUntil {
                condition,
                test_before,
                statements,
            } => {
                self.lower_perform_inline_until(
                    condition,
                    *test_before,
                    statements,
                    func,
                    current_block_id,
                    instructions,
                );
            }
            cobol_hir::PerformType::Varying {
                target,
                thru,
                varying,
                inline_body,
            } => {
                self.lower_perform_varying(
                    *target,
                    thru.as_ref().copied(),
                    varying,
                    inline_body.as_deref(),
                    func,
                    current_block_id,
                    instructions,
                );
            }
        }
    }

    /// Lower PERFORM (inline) — just emit the body statements in sequence.
    fn lower_perform_inline(
        &mut self,
        statements: &[cobol_hir::HirStatement],
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        for stmt in statements {
            self.lower_statement_to_blocks(stmt, func, current_block_id, instructions);
        }
    }

    /// Inline the body of a PERFORM paragraph-name [THRU paragraph-name].
    ///
    /// Since the Cranelift backend doesn't implement the perform stack
    /// (PerformPush/PerformReturn), we inline the paragraph body directly.
    /// For PERFORM THRU, we inline all paragraphs from target through thru.
    fn lower_perform_out_of_line(
        &mut self,
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let target_name = self.interner.resolve(target).to_string();

        // Collect the range of paragraphs to inline
        let mut para_range: Vec<Vec<cobol_hir::HirStatement>> = Vec::new();
        let mut in_range = false;
        let thru_name = thru.map(|t| self.interner.resolve(t).to_string());

        for p in &self.hir.paragraphs {
            let pname = self.interner.resolve(p.name).to_string();
            if pname == target_name {
                in_range = true;
            }
            if in_range {
                para_range.push(p.statements.clone());
                if let Some(ref tn) = thru_name {
                    if pname == *tn {
                        break; // End of THRU range
                    }
                } else {
                    break; // No THRU, just the target paragraph
                }
            }
        }

        // Inline all statements from the collected paragraphs
        for stmts in &para_range {
            for stmt in stmts {
                self.lower_statement_to_blocks(stmt, func, current_block_id, instructions);
            }
        }
    }

    /// Lower PERFORM ... TIMES — execute the target paragraph N times.
    ///
    /// Pattern:
    ///   init: counter = eval(times_expr)
    ///         goto cond
    ///   cond: if counter > 0 goto body else goto exit
    ///   body: perform target (out-of-line)
    ///         counter = counter - 1
    ///         goto cond
    ///   exit: (continuation)
    fn lower_perform_times(
        &mut self,
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        times_expr: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let cond_block_id = func.new_block();
        let body_block_id = func.new_block();
        let exit_block_id = func.new_block();

        // Evaluate the TIMES expression to get the loop count.
        // We convert it to an integer value via the runtime.
        let counter_val = match times_expr {
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Int(*n),
                });
                v
            }
            cobol_hir::HirExpr::DataRef(dr) => {
                let name = self.interner.resolve(dr.name).to_string();
                if let Some(info) = self.linkage_map.get(&name) {
                    let sz = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sz,
                        value: MirConst::Int(info.byte_size as i64),
                    });
                    let int_val = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(int_val),
                        func: "cobolrt_display_to_int".to_string(),
                        args: vec![info.param_value, sz],
                    });
                    int_val
                } else if let Some((gname, size)) = self.resolve_data_ref(dr) {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: addr,
                        name: gname,
                    });
                    let sz = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: sz,
                        value: MirConst::Int(size as i64),
                    });
                    let int_val = func.new_value();
                    instructions.push(MirInst::CallRuntime {
                        dest: Some(int_val),
                        func: "cobolrt_display_to_int".to_string(),
                        args: vec![addr, sz],
                    });
                    int_val
                } else {
                    let v = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: v,
                        value: MirConst::Int(0),
                    });
                    v
                }
            }
            _ => {
                // Fallback: default to 0 iterations
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Int(0),
                });
                v
            }
        };

        // Store counter into a temporary global-like location.
        // We use a runtime approach: allocate a scratch area via a named
        // temp string that holds the integer counter.  However, since MIR
        // uses SSA and we need a mutable counter, we use a simple approach:
        // store the initial count in a dedicated temp variable, and use
        // runtime calls to decrement and check.
        //
        // Simpler approach for SSA: use a counter variable stored as a
        // global scratch buffer.  We emit runtime calls:
        //   cobolrt_init_counter(counter_val) -> counter_id
        //   cobolrt_counter_gt_zero(counter_id) -> bool
        //   cobolrt_decrement_counter(counter_id)
        //
        // But since the runtime may not have these, we use the MIR integer
        // ops directly.  The challenge is SSA: we cannot re-assign a Value.
        // We work around this by using a scratch global variable.
        //
        // Alternative approach matching the VARYING pattern: use a display
        // format counter stored in a temporary buffer via CallRuntime.
        //
        // Simplest working approach: allocate a temporary 8-byte buffer,
        // store the count, and use runtime for decrement/compare.

        // We'll use a named temporary for the counter — declare it as a global
        let counter_name = format!("_PERFORM_COUNTER_{}", func.next_value);

        // Register the counter as a global data item (8 bytes, zero-initialized)
        if !self.global_map.contains_key(&counter_name) {
            let offset = self
                .module
                .globals
                .iter()
                .map(|g| g.offset + 8)
                .max()
                .unwrap_or(0);
            self.global_map
                .insert(counter_name.clone(), self.module.globals.len());
            self.module.globals.push(MirGlobal {
                name: counter_name.clone(),
                ty: MirType::Bytes(8),
                initial_value: None,
                offset,
                redefines: None,
                parent_offset: None,
            });
        }

        let counter_addr = func.new_value();
        instructions.push(MirInst::GlobalAddr {
            dest: counter_addr,
            name: counter_name.clone(),
        });
        // Store initial count
        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_int_to_display".to_string(),
            args: vec![counter_val, counter_addr],
        });

        // Finalize current block -> goto cond
        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Goto(cond_block_id),
        });

        // Condition block: load counter, compare > 0, branch
        let mut cond_insts = Vec::new();
        let cond_counter_addr = func.new_value();
        cond_insts.push(MirInst::GlobalAddr {
            dest: cond_counter_addr,
            name: counter_name.clone(),
        });
        let counter_size = func.new_value();
        cond_insts.push(MirInst::Const {
            dest: counter_size,
            value: MirConst::Int(8),
        });
        let current_count = func.new_value();
        cond_insts.push(MirInst::CallRuntime {
            dest: Some(current_count),
            func: "cobolrt_display_to_int".to_string(),
            args: vec![cond_counter_addr, counter_size],
        });
        let zero = func.new_value();
        cond_insts.push(MirInst::Const {
            dest: zero,
            value: MirConst::Int(0),
        });
        let cond_val = func.new_value();
        cond_insts.push(MirInst::ICmpGt {
            dest: cond_val,
            left: current_count,
            right: zero,
        });
        func.blocks.push(BasicBlock {
            id: cond_block_id,
            params: Vec::new(),
            instructions: cond_insts,
            terminator: Terminator::Branch {
                cond: cond_val,
                then_block: body_block_id,
                else_block: exit_block_id,
            },
        });

        // Body block: inline the target paragraph's statements, then
        // decrement counter.  We inline instead of using the out-of-line
        // PERFORM mechanism (PerformPush/PerformReturn) because the
        // Cranelift backend does not yet support the perform stack.
        let mut body_insts = Vec::new();
        let mut body_bid = body_block_id;

        // Collect and inline the full paragraph range (target THRU end)
        let target_name = self.interner.resolve(target).to_string();
        let thru_name = thru.map(|t| self.interner.resolve(t).to_string());
        let mut para_range: Vec<Vec<cobol_hir::HirStatement>> = Vec::new();
        let mut in_range = false;
        for p in &self.hir.paragraphs {
            let pname = self.interner.resolve(p.name).to_string();
            if pname == target_name {
                in_range = true;
            }
            if in_range {
                para_range.push(p.statements.clone());
                if let Some(ref tn) = thru_name {
                    if pname == *tn {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        for stmts in &para_range {
            for stmt in stmts {
                self.lower_statement_to_blocks(stmt, func, &mut body_bid, &mut body_insts);
            }
        }

        // Decrement counter
        let dec_counter_addr = func.new_value();
        body_insts.push(MirInst::GlobalAddr {
            dest: dec_counter_addr,
            name: counter_name.clone(),
        });
        let dec_size = func.new_value();
        body_insts.push(MirInst::Const {
            dest: dec_size,
            value: MirConst::Int(8),
        });
        let dec_current = func.new_value();
        body_insts.push(MirInst::CallRuntime {
            dest: Some(dec_current),
            func: "cobolrt_display_to_int".to_string(),
            args: vec![dec_counter_addr, dec_size],
        });
        let one = func.new_value();
        body_insts.push(MirInst::Const {
            dest: one,
            value: MirConst::Int(1),
        });
        let new_count = func.new_value();
        body_insts.push(MirInst::ISub {
            dest: new_count,
            left: dec_current,
            right: one,
        });
        let store_addr = func.new_value();
        body_insts.push(MirInst::GlobalAddr {
            dest: store_addr,
            name: counter_name,
        });
        body_insts.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_int_to_display".to_string(),
            args: vec![new_count, store_addr],
        });

        func.blocks.push(BasicBlock {
            id: body_bid,
            params: Vec::new(),
            instructions: body_insts,
            terminator: Terminator::Goto(cond_block_id),
        });

        *current_block_id = exit_block_id;
    }

    /// Lower inline PERFORM n TIMES ... END-PERFORM
    fn lower_perform_inline_times(
        &mut self,
        times_expr: &cobol_hir::HirExpr,
        statements: &[cobol_hir::HirStatement],
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let cond_block_id = func.new_block();
        let body_block_id = func.new_block();
        let exit_block_id = func.new_block();

        // Evaluate TIMES expression
        let counter_val = match times_expr {
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Int(*n),
                });
                v
            }
            _ => {
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v,
                    value: MirConst::Int(0),
                });
                v
            }
        };

        // Allocate counter variable
        let counter_name = format!("_PERFORM_COUNTER_{}", func.next_value);
        if !self.global_map.contains_key(&counter_name) {
            let offset = self
                .module
                .globals
                .iter()
                .map(|g| g.offset + 8)
                .max()
                .unwrap_or(0);
            self.global_map
                .insert(counter_name.clone(), self.module.globals.len());
            self.module.globals.push(MirGlobal {
                name: counter_name.clone(),
                ty: MirType::Bytes(8),
                initial_value: None,
                offset,
                redefines: None,
                parent_offset: None,
            });
        }

        let counter_addr = func.new_value();
        instructions.push(MirInst::GlobalAddr {
            dest: counter_addr,
            name: counter_name.clone(),
        });
        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_int_to_display".to_string(),
            args: vec![counter_val, counter_addr],
        });

        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Goto(cond_block_id),
        });

        // Condition block
        let mut cond_insts = Vec::new();
        let cond_counter_addr = func.new_value();
        cond_insts.push(MirInst::GlobalAddr {
            dest: cond_counter_addr,
            name: counter_name.clone(),
        });
        let counter_size = func.new_value();
        cond_insts.push(MirInst::Const {
            dest: counter_size,
            value: MirConst::Int(8),
        });
        let current_count = func.new_value();
        cond_insts.push(MirInst::CallRuntime {
            dest: Some(current_count),
            func: "cobolrt_display_to_int".to_string(),
            args: vec![cond_counter_addr, counter_size],
        });
        let zero = func.new_value();
        cond_insts.push(MirInst::Const {
            dest: zero,
            value: MirConst::Int(0),
        });
        let cond_val = func.new_value();
        cond_insts.push(MirInst::ICmpGt {
            dest: cond_val,
            left: current_count,
            right: zero,
        });
        func.blocks.push(BasicBlock {
            id: cond_block_id,
            params: Vec::new(),
            instructions: cond_insts,
            terminator: Terminator::Branch {
                cond: cond_val,
                then_block: body_block_id,
                else_block: exit_block_id,
            },
        });

        // Body block: emit inline statements
        let mut body_insts = Vec::new();
        let mut body_bid = body_block_id;
        for stmt in statements {
            self.lower_statement_to_blocks(stmt, func, &mut body_bid, &mut body_insts);
        }

        // Decrement counter
        let dec_counter_addr = func.new_value();
        body_insts.push(MirInst::GlobalAddr {
            dest: dec_counter_addr,
            name: counter_name.clone(),
        });
        let dec_size = func.new_value();
        body_insts.push(MirInst::Const {
            dest: dec_size,
            value: MirConst::Int(8),
        });
        let dec_current = func.new_value();
        body_insts.push(MirInst::CallRuntime {
            dest: Some(dec_current),
            func: "cobolrt_display_to_int".to_string(),
            args: vec![dec_counter_addr, dec_size],
        });
        let one = func.new_value();
        body_insts.push(MirInst::Const {
            dest: one,
            value: MirConst::Int(1),
        });
        let new_count = func.new_value();
        body_insts.push(MirInst::ISub {
            dest: new_count,
            left: dec_current,
            right: one,
        });
        let store_addr = func.new_value();
        body_insts.push(MirInst::GlobalAddr {
            dest: store_addr,
            name: counter_name,
        });
        body_insts.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_int_to_display".to_string(),
            args: vec![new_count, store_addr],
        });

        func.blocks.push(BasicBlock {
            id: body_bid,
            params: Vec::new(),
            instructions: body_insts,
            terminator: Terminator::Goto(cond_block_id),
        });

        *current_block_id = exit_block_id;
    }

    /// Lower inline PERFORM UNTIL condition ... END-PERFORM
    fn lower_perform_inline_until(
        &mut self,
        condition: &cobol_hir::HirExpr,
        test_before: bool,
        statements: &[cobol_hir::HirStatement],
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let cond_block_id = func.new_block();
        let body_block_id = func.new_block();
        let exit_block_id = func.new_block();

        // Finalize current block → goto cond (test before) or goto body (test after)
        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: if test_before {
                Terminator::Goto(cond_block_id)
            } else {
                Terminator::Goto(body_block_id)
            },
        });

        // Condition block: evaluate condition, branch to exit or body
        let mut cond_insts = Vec::new();
        let cond_bid = cond_block_id;
        let cond_val = self.eval_condition(condition, func, &mut cond_insts);
        func.blocks.push(BasicBlock {
            id: cond_bid,
            params: Vec::new(),
            instructions: cond_insts,
            terminator: Terminator::Branch {
                cond: cond_val,
                then_block: exit_block_id,
                else_block: body_block_id,
            },
        });

        // Body block: inline statements, then goto cond
        let mut body_insts = Vec::new();
        let mut body_bid = body_block_id;
        for stmt in statements {
            self.lower_statement_to_blocks(stmt, func, &mut body_bid, &mut body_insts);
        }
        func.blocks.push(BasicBlock {
            id: body_bid,
            params: Vec::new(),
            instructions: body_insts,
            terminator: Terminator::Goto(cond_block_id),
        });

        *current_block_id = exit_block_id;
    }

    /// Lower PERFORM ... UNTIL — execute the target paragraph until condition.
    ///
    /// With TEST BEFORE (default):
    ///   cond: if condition goto exit else goto body
    ///   body: perform target (out-of-line)
    ///         goto cond
    ///   exit: (continuation)
    ///
    /// With TEST AFTER:
    ///   body: perform target (out-of-line)
    ///         if condition goto exit else goto body
    ///   exit: (continuation)
    #[allow(clippy::too_many_arguments)]
    fn lower_perform_until(
        &mut self,
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        condition: &cobol_hir::HirExpr,
        test_before: bool,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        if test_before {
            // TEST BEFORE: check condition first
            let cond_block_id = func.new_block();
            let body_block_id = func.new_block();
            let exit_block_id = func.new_block();

            // Finalize current block -> goto cond
            let current_insts = std::mem::take(instructions);
            func.blocks.push(BasicBlock {
                id: *current_block_id,
                params: Vec::new(),
                instructions: current_insts,
                terminator: Terminator::Goto(cond_block_id),
            });

            // Condition block: evaluate UNTIL condition, branch
            let mut cond_insts = Vec::new();
            let cond_val = self.eval_condition(condition, func, &mut cond_insts);
            func.blocks.push(BasicBlock {
                id: cond_block_id,
                params: Vec::new(),
                instructions: cond_insts,
                terminator: Terminator::Branch {
                    cond: cond_val,
                    then_block: exit_block_id, // condition met -> exit
                    else_block: body_block_id, // condition not met -> body
                },
            });

            // Body block: perform target, then goto cond
            let mut body_insts = Vec::new();
            let mut body_bid = body_block_id;
            self.lower_perform_out_of_line(target, thru, func, &mut body_bid, &mut body_insts);
            func.blocks.push(BasicBlock {
                id: body_bid,
                params: Vec::new(),
                instructions: body_insts,
                terminator: Terminator::Goto(cond_block_id),
            });

            *current_block_id = exit_block_id;
        } else {
            // TEST AFTER: execute body first, then check condition
            let body_block_id = func.new_block();
            let cond_block_id = func.new_block();
            let exit_block_id = func.new_block();

            // Finalize current block -> goto body (execute at least once)
            let current_insts = std::mem::take(instructions);
            func.blocks.push(BasicBlock {
                id: *current_block_id,
                params: Vec::new(),
                instructions: current_insts,
                terminator: Terminator::Goto(body_block_id),
            });

            // Body block: perform target, then goto cond
            let mut body_insts = Vec::new();
            let mut body_bid = body_block_id;
            self.lower_perform_out_of_line(target, thru, func, &mut body_bid, &mut body_insts);
            func.blocks.push(BasicBlock {
                id: body_bid,
                params: Vec::new(),
                instructions: body_insts,
                terminator: Terminator::Goto(cond_block_id),
            });

            // Condition block: evaluate condition, branch
            let mut cond_insts = Vec::new();
            let cond_val = self.eval_condition(condition, func, &mut cond_insts);
            func.blocks.push(BasicBlock {
                id: cond_block_id,
                params: Vec::new(),
                instructions: cond_insts,
                terminator: Terminator::Branch {
                    cond: cond_val,
                    then_block: exit_block_id, // condition met -> exit
                    else_block: body_block_id, // condition not met -> loop again
                },
            });

            *current_block_id = exit_block_id;
        }
    }

    /// Lower PERFORM VARYING with inline body into a loop.
    /// Emit instructions to initialize a loop variable with its FROM value.
    fn emit_varying_init(
        &self,
        var_name: &str,
        var_size: u32,
        from: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        if let cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) = from {
            // Index names are i64 and store byte offsets: (value - 1) * element_size.
            if let Some((_, _, elem_size)) = self.index_info.get(var_name) {
                let byte_offset = (*n - 1) * (*elem_size as i64);
                let addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: addr,
                    name: var_name.to_string(),
                });
                let init_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: init_val,
                    value: MirConst::Int(byte_offset),
                });
                instructions.push(MirInst::Store {
                    addr,
                    value: init_val,
                });
                return;
            }

            let init_str = if *n < 0 {
                // Format negative: "-" followed by zero-padded absolute value
                let abs_str = format!(
                    "{:0>width$}",
                    n.unsigned_abs(),
                    width = (var_size as usize).saturating_sub(1)
                );
                format!("-{}", abs_str)
            } else {
                format!("{:0>width$}", n, width = var_size as usize)
            };
            let addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: addr,
                name: var_name.to_string(),
            });
            let init_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: init_val,
                value: MirConst::Str(init_str),
            });
            let len_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: len_val,
                value: MirConst::Int(var_size as i64),
            });
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_move_alphanumeric".to_string(),
                args: vec![init_val, len_val, addr, len_val],
            });
        }
    }

    /// Emit instructions to increment a loop variable by its BY value.
    fn emit_varying_increment(
        &self,
        var_name: &str,
        var_size: u32,
        by: &cobol_hir::HirExpr,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        if let cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(by_val)) = by {
            // Index names: add by_val * element_size to the stored byte offset.
            if let Some((_, _, elem_size)) = self.index_info.get(var_name) {
                let delta = (*by_val) * (*elem_size as i64);
                let var_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: var_addr,
                    name: var_name.to_string(),
                });
                let current = func.new_value();
                instructions.push(MirInst::Load {
                    dest: current,
                    addr: var_addr,
                    ty: MirType::I64,
                });
                let delta_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: delta_val,
                    value: MirConst::Int(delta),
                });
                let result = func.new_value();
                instructions.push(MirInst::IAdd {
                    dest: result,
                    left: current,
                    right: delta_val,
                });
                let var_addr2 = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: var_addr2,
                    name: var_name.to_string(),
                });
                instructions.push(MirInst::Store {
                    addr: var_addr2,
                    value: result,
                });
                return;
            }

            let by_str = if *by_val < 0 {
                // Format negative: "-" followed by zero-padded absolute value
                let abs_str = format!(
                    "{:0>width$}",
                    by_val.unsigned_abs(),
                    width = (var_size as usize).saturating_sub(1)
                );
                format!("-{}", abs_str)
            } else {
                format!("{:0>width$}", by_val, width = var_size as usize)
            };
            let by_addr = func.new_value();
            instructions.push(MirInst::Const {
                dest: by_addr,
                value: MirConst::Str(by_str),
            });
            let by_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: by_len,
                value: MirConst::Int(var_size as i64),
            });
            let var_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: var_addr,
                name: var_name.to_string(),
            });
            let var_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: var_len,
                value: MirConst::Int(var_size as i64),
            });
            let var_addr2 = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: var_addr2,
                name: var_name.to_string(),
            });
            let var_len2 = func.new_value();
            instructions.push(MirInst::Const {
                dest: var_len2,
                value: MirConst::Int(var_size as i64),
            });
            // PERFORM VARYING increment: look up signedness of the counter variable
            let var_is_signed = self
                .data_item_pic_info(var_name)
                .map(|(_, _, _, is_signed)| is_signed)
                .unwrap_or(false);
            let sf = func.new_value();
            instructions.push(MirInst::Const {
                dest: sf,
                value: MirConst::Int(if var_is_signed { 1 } else { 0 }),
            });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_add_numeric".to_string(),
                args: vec![by_addr, by_len, var_addr, var_len, var_addr2, var_len2, sf],
            });
        }
    }

    fn resolve_var_size(&self, name: &str) -> u32 {
        self.global_map
            .get(name)
            .map(|&idx| {
                let g = &self.module.globals[idx];
                match &g.ty {
                    MirType::Bytes(n) => *n,
                    _ => 1,
                }
            })
            .unwrap_or(1)
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_perform_varying(
        &mut self,
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        varying: &cobol_hir::VaryingClause,
        inline_body: Option<&[cobol_hir::HirStatement]>,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let outer_cond_id = func.new_block();
        let outer_body_id = func.new_block();
        let exit_block_id = func.new_block();

        let var_name = self.interner.resolve(varying.identifier.name).to_string();
        let var_size = self.resolve_var_size(&var_name);

        // Initialize outer loop variable
        self.emit_varying_init(&var_name, var_size, &varying.from, func, instructions);

        // Finalize current block → goto outer_cond
        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Goto(outer_cond_id),
        });

        // Outer condition block
        let mut cond_insts = Vec::new();
        let cond_val = self.eval_condition(&varying.until, func, &mut cond_insts);
        func.blocks.push(BasicBlock {
            id: outer_cond_id,
            params: Vec::new(),
            instructions: cond_insts,
            terminator: Terminator::Branch {
                cond: cond_val,
                then_block: exit_block_id,
                else_block: outer_body_id,
            },
        });

        // Generate the body of the outer loop.
        // If there are AFTER clauses, we generate nested inner loops recursively.
        // If there are no AFTER clauses, we just emit the body + increment.
        self.lower_varying_body(
            target,
            thru,
            &varying.after,
            inline_body,
            &var_name,
            var_size,
            &varying.by,
            outer_body_id,
            outer_cond_id,
            func,
        );

        *current_block_id = exit_block_id;
    }

    /// Recursively generate the body of a PERFORM VARYING loop level.
    ///
    /// `after_clauses` is the remaining AFTER clauses to process.
    /// If empty, we emit the inline body statements and increment the parent
    /// variable. Otherwise, we generate a nested inner loop for the first
    /// AFTER clause and recurse for any remaining ones.
    ///
    /// `parent_body_id` is the block ID for the body of the parent loop level
    /// (i.e. the block entered when the parent condition is false).
    /// `parent_cond_id` is the condition-check block of the parent loop, which
    /// we jump back to after incrementing the parent variable.
    #[allow(clippy::too_many_arguments)]
    fn lower_varying_body(
        &mut self,
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        after_clauses: &[cobol_hir::VaryingClause],
        inline_body: Option<&[cobol_hir::HirStatement]>,
        parent_var_name: &str,
        parent_var_size: u32,
        parent_by: &cobol_hir::HirExpr,
        parent_body_id: BlockId,
        parent_cond_id: BlockId,
        func: &mut MirFunction,
    ) {
        if after_clauses.is_empty() {
            // Base case: no more AFTER clauses.
            // Emit inline body + increment parent variable, loop back.
            let mut body_insts = Vec::new();
            let mut body_bid = parent_body_id;
            if let Some(stmts) = inline_body {
                for stmt in stmts {
                    self.lower_statement_to_blocks(stmt, func, &mut body_bid, &mut body_insts);
                }
            } else {
                // Out-of-line PERFORM VARYING: inline the paragraph range
                let target_name = self.interner.resolve(target).to_string();
                if target_name != "_INLINE" {
                    self.lower_perform_out_of_line(
                        target,
                        thru,
                        func,
                        &mut body_bid,
                        &mut body_insts,
                    );
                }
            }
            self.emit_varying_increment(
                parent_var_name,
                parent_var_size,
                parent_by,
                func,
                &mut body_insts,
            );
            func.blocks.push(BasicBlock {
                id: body_bid,
                params: Vec::new(),
                instructions: body_insts,
                terminator: Terminator::Goto(parent_cond_id),
            });
        } else {
            // Recursive case: generate inner loop for after_clauses[0],
            // then recurse for after_clauses[1..].
            let after = &after_clauses[0];
            let after_var_name = self.interner.resolve(after.identifier.name).to_string();
            let after_var_size = self.resolve_var_size(&after_var_name);

            let after_cond_id = func.new_block();
            let after_body_id = func.new_block();
            let after_exit_id = func.new_block();

            // parent_body block: init AFTER var, goto after_cond
            let mut init_insts = Vec::new();
            self.emit_varying_init(
                &after_var_name,
                after_var_size,
                &after.from,
                func,
                &mut init_insts,
            );
            func.blocks.push(BasicBlock {
                id: parent_body_id,
                params: Vec::new(),
                instructions: init_insts,
                terminator: Terminator::Goto(after_cond_id),
            });

            // after_cond block: check AFTER UNTIL condition
            let mut after_cond_insts = Vec::new();
            let after_cond_val = self.eval_condition(&after.until, func, &mut after_cond_insts);
            func.blocks.push(BasicBlock {
                id: after_cond_id,
                params: Vec::new(),
                instructions: after_cond_insts,
                terminator: Terminator::Branch {
                    cond: after_cond_val,
                    then_block: after_exit_id,
                    else_block: after_body_id,
                },
            });

            // Recurse: the body of this AFTER level either contains
            // deeper AFTER levels or the actual inline body.
            self.lower_varying_body(
                target,
                thru,
                &after_clauses[1..],
                inline_body,
                &after_var_name,
                after_var_size,
                &after.by,
                after_body_id,
                after_cond_id,
                func,
            );

            // after_exit block: increment parent (outer) var, goto parent_cond
            let mut exit_insts = Vec::new();
            self.emit_varying_increment(
                parent_var_name,
                parent_var_size,
                parent_by,
                func,
                &mut exit_insts,
            );
            func.blocks.push(BasicBlock {
                id: after_exit_id,
                params: Vec::new(),
                instructions: exit_insts,
                terminator: Terminator::Goto(parent_cond_id),
            });
        }
    }

    /// Lower ACCEPT statement: read from console or system info into a data item.
    fn lower_accept(
        &self,
        target: &cobol_hir::HirDataRef,
        source: cobol_hir::AcceptSource,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        let name = self.interner.resolve(target.name).to_string();

        // Get the target address and size
        let (target_addr, target_size) = if let Some(info) = self.linkage_map.get(&name) {
            (info.param_value, info.byte_size)
        } else if let Some((ref gname, size)) = self.resolve_data_ref(target) {
            let addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: addr,
                name: gname.clone(),
            });
            (addr, size)
        } else {
            return;
        };

        let len_val = func.new_value();
        instructions.push(MirInst::Const {
            dest: len_val,
            value: MirConst::Int(target_size as i64),
        });

        let runtime_func = match source {
            cobol_hir::AcceptSource::Console => "cobolrt_accept",
            cobol_hir::AcceptSource::Date => "cobolrt_accept_date",
            cobol_hir::AcceptSource::Day => "cobolrt_accept_day",
            cobol_hir::AcceptSource::Time => "cobolrt_accept_time",
            cobol_hir::AcceptSource::DayOfWeek => "cobolrt_accept_day_of_week",
        };

        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: runtime_func.to_string(),
            args: vec![target_addr, len_val],
        });
    }

    fn finish(self) -> MirModule {
        self.module
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use cobol_ast::AstNode;

    /// Helper: build a minimal MIR module with one function containing
    /// a single basic block that returns immediately.
    fn make_minimal_module() -> MirModule {
        let entry = BlockId::from_raw(0);
        let block = BasicBlock {
            id: entry,
            params: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Return,
        };

        let func = MirFunction {
            name: "MAIN-PARAGRAPH".to_string(),
            params: Vec::new(),
            return_type: MirType::Void,
            blocks: vec![block],
            entry_block: entry,
            next_value: 0,
            next_block: 1,
        };

        MirModule {
            name: "HELLO-WORLD".to_string(),
            functions: vec![func],
            globals: Vec::new(),
            file_descriptors: Vec::new(),
            errors: Vec::new(),
        }
    }

    #[test]
    fn mir_module_structure() {
        let module = make_minimal_module();
        assert_eq!(module.name, "HELLO-WORLD");
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.globals.len(), 0);
        assert_eq!(module.file_descriptors.len(), 0);

        let func = &module.functions[0];
        assert_eq!(func.name, "MAIN-PARAGRAPH");
        assert_eq!(func.params.len(), 0);
        assert_eq!(func.return_type, MirType::Void);
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.entry_block, BlockId::from_raw(0));

        let block = &func.blocks[0];
        assert_eq!(block.id, BlockId::from_raw(0));
        assert!(block.params.is_empty());
        assert!(block.instructions.is_empty());
        assert_eq!(block.terminator, Terminator::Return);
    }

    #[test]
    fn value_and_block_id_identity() {
        let v1 = Value::from_raw(0);
        let v2 = Value::from_raw(1);
        let v3 = Value::from_raw(0);
        assert_eq!(v1, v3);
        assert_ne!(v1, v2);
        assert_eq!(v1.raw(), 0);

        let b1 = BlockId::from_raw(0);
        let b2 = BlockId::from_raw(1);
        assert_ne!(b1, b2);
        assert_eq!(b1.raw(), 0);
    }

    #[test]
    fn mir_type_equality() {
        assert_eq!(MirType::I32, MirType::I32);
        assert_ne!(MirType::I32, MirType::I64);
        assert_eq!(
            MirType::Decimal {
                digits: 9,
                scale: 2
            },
            MirType::Decimal {
                digits: 9,
                scale: 2
            },
        );
        assert_ne!(
            MirType::Decimal {
                digits: 9,
                scale: 2
            },
            MirType::Decimal {
                digits: 9,
                scale: 3
            },
        );
        assert_eq!(MirType::Bytes(20), MirType::Bytes(20));
        assert_ne!(MirType::Bytes(20), MirType::Bytes(30));
    }

    #[test]
    fn mir_const_variants() {
        assert_eq!(MirConst::Int(42), MirConst::Int(42));
        assert_ne!(MirConst::Int(42), MirConst::Int(99));
        assert_eq!(MirConst::Bool(true), MirConst::Bool(true));
        assert_eq!(MirConst::Null, MirConst::Null);
        assert_eq!(
            MirConst::Str("HELLO".to_string()),
            MirConst::Str("HELLO".to_string()),
        );
    }

    #[test]
    fn function_new_value_and_block() {
        let entry = BlockId::from_raw(0);
        let mut func = MirFunction {
            name: "TEST".to_string(),
            params: Vec::new(),
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: entry,
                params: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Return,
            }],
            entry_block: entry,
            next_value: 0,
            next_block: 1,
        };

        let v0 = func.new_value();
        let v1 = func.new_value();
        assert_eq!(v0, Value::from_raw(0));
        assert_eq!(v1, Value::from_raw(1));
        assert_eq!(func.next_value, 2);

        let b1 = func.new_block();
        assert_eq!(b1, BlockId::from_raw(1));
        assert_eq!(func.next_block, 2);
    }

    #[test]
    fn instructions_with_decimal_ops() {
        let v0 = Value::from_raw(0);
        let v1 = Value::from_raw(1);
        let v2 = Value::from_raw(2);

        let inst = MirInst::DecimalAdd {
            dest: v2,
            left: v0,
            right: v1,
            result_scale: 2,
            rounded: true,
        };

        if let MirInst::DecimalAdd {
            dest,
            result_scale,
            rounded,
            ..
        } = &inst
        {
            assert_eq!(*dest, v2);
            assert_eq!(*result_scale, 2);
            assert!(*rounded);
        } else {
            panic!("expected DecimalAdd");
        }
    }

    #[test]
    fn terminator_variants() {
        let t1 = Terminator::Goto(BlockId::from_raw(1));
        let t2 = Terminator::Return;
        let t3 = Terminator::Unreachable;
        let t4 = Terminator::PerformReturn;
        assert_ne!(t1, t2);
        assert_ne!(t2, t3);
        assert_ne!(t3, t4);
    }

    #[test]
    fn global_construction() {
        let global = MirGlobal {
            name: "WS-COUNTER".to_string(),
            ty: MirType::I32,
            initial_value: Some(MirConst::Int(0)),
            offset: 0,
            redefines: None,
            parent_offset: None,
        };
        assert_eq!(global.name, "WS-COUNTER");
        assert_eq!(global.ty, MirType::I32);
        assert_eq!(global.initial_value, Some(MirConst::Int(0)));
    }

    #[test]
    fn file_descriptor_construction() {
        let fd = MirFileDescriptor {
            name: "INPUT-FILE".to_string(),
            organization: cobol_hir::FileOrganization::Sequential,
            access_mode: cobol_hir::AccessMode::Sequential,
            record_size: 80,
            relative_key: None,
            record_key: None,
            file_status: None,
        };
        assert_eq!(fd.name, "INPUT-FILE");
        assert_eq!(fd.organization, cobol_hir::FileOrganization::Sequential);
        assert_eq!(fd.record_size, 80);
    }

    #[test]
    fn decimal_format_construction() {
        let fmt = DecimalFormat {
            digits: 9,
            scale: 2,
            signed: true,
        };
        assert_eq!(fmt.digits, 9);
        assert_eq!(fmt.scale, 2);
        assert!(fmt.signed);
    }

    #[test]
    fn inspect_mode_variants() {
        assert_ne!(InspectMode::Tallying, InspectMode::Replacing);
        assert_ne!(InspectMode::Replacing, InspectMode::Converting);
    }

    #[test]
    fn file_open_mode_variants() {
        assert_ne!(FileOpenMode::Input, FileOpenMode::Output);
        assert_ne!(FileOpenMode::IoMode, FileOpenMode::Extend);
    }

    #[test]
    fn phi_instruction() {
        let dest = Value::from_raw(5);
        let inst = MirInst::Phi {
            dest,
            incoming: vec![
                (BlockId::from_raw(0), Value::from_raw(1)),
                (BlockId::from_raw(1), Value::from_raw(3)),
            ],
        };
        if let MirInst::Phi { dest: d, incoming } = &inst {
            assert_eq!(*d, dest);
            assert_eq!(incoming.len(), 2);
        } else {
            panic!("expected Phi");
        }
    }

    #[test]
    fn branch_terminator() {
        let cond = Value::from_raw(0);
        let term = Terminator::Branch {
            cond,
            then_block: BlockId::from_raw(1),
            else_block: BlockId::from_raw(2),
        };
        if let Terminator::Branch {
            cond: c,
            then_block,
            else_block,
        } = &term
        {
            assert_eq!(*c, cond);
            assert_eq!(*then_block, BlockId::from_raw(1));
            assert_eq!(*else_block, BlockId::from_raw(2));
        } else {
            panic!("expected Branch");
        }
    }

    #[test]
    fn switch_terminator() {
        let term = Terminator::Switch {
            value: Value::from_raw(0),
            cases: vec![(1, BlockId::from_raw(1)), (2, BlockId::from_raw(2))],
            default: BlockId::from_raw(3),
        };
        if let Terminator::Switch { cases, default, .. } = &term {
            assert_eq!(cases.len(), 2);
            assert_eq!(*default, BlockId::from_raw(3));
        } else {
            panic!("expected Switch");
        }
    }

    #[test]
    fn module_with_globals_and_files() {
        let entry = BlockId::from_raw(0);
        let module = MirModule {
            name: "PAYROLL".to_string(),
            functions: vec![MirFunction {
                name: "MAIN".to_string(),
                params: Vec::new(),
                return_type: MirType::Void,
                blocks: vec![BasicBlock {
                    id: entry,
                    params: Vec::new(),
                    instructions: Vec::new(),
                    terminator: Terminator::Return,
                }],
                entry_block: entry,
                next_value: 0,
                next_block: 1,
            }],
            globals: vec![
                MirGlobal {
                    name: "WS-SALARY".to_string(),
                    ty: MirType::Decimal {
                        digits: 9,
                        scale: 2,
                    },
                    initial_value: Some(MirConst::Int(0)),
                    offset: 0,
                    redefines: None,
                    parent_offset: None,
                },
                MirGlobal {
                    name: "WS-NAME".to_string(),
                    ty: MirType::Bytes(30),
                    initial_value: Some(MirConst::Str("SPACES".to_string())),
                    offset: 8,
                    redefines: None,
                    parent_offset: None,
                },
            ],
            file_descriptors: vec![MirFileDescriptor {
                name: "EMPLOYEE-FILE".to_string(),
                organization: cobol_hir::FileOrganization::Indexed,
                access_mode: cobol_hir::AccessMode::Sequential,
                record_size: 200,
                relative_key: None,
                record_key: None,
                file_status: None,
            }],
            errors: Vec::new(),
        };

        assert_eq!(module.name, "PAYROLL");
        assert_eq!(module.globals.len(), 2);
        assert_eq!(module.file_descriptors.len(), 1);
        assert_eq!(
            module.file_descriptors[0].organization,
            cobol_hir::FileOrganization::Indexed
        );
    }

    // ------------------------------------------------------------------
    // CALL RETURNING graceful error test
    // ------------------------------------------------------------------

    #[test]
    fn call_returning_produces_error_not_panic() {
        // CALL ... RETURNING should produce a structured error in
        // mir.errors rather than panicking.
        let src = r#"
IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-RET.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-X PIC 9 VALUE 0.
01 WS-RET PIC 9 VALUE 0.
PROCEDURE DIVISION.
    CALL "SUBPROG" USING WS-X RETURNING WS-RET.
    STOP RUN.
"#;
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let hir = cobol_hir::lower(&sf, &mut interner, file_id);
        let mir = lower(&hir, &interner);

        // Should NOT have panicked. Instead, errors should contain a message
        // about RETURNING not being supported.
        assert!(
            !mir.errors.is_empty(),
            "MIR should contain an error for CALL RETURNING"
        );
        assert!(
            mir.errors[0].contains("RETURNING"),
            "error message should mention RETURNING, got: {}",
            mir.errors[0]
        );
    }
}
