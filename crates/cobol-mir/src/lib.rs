//! SSA-based mid-level IR with COBOL-specific operations.
//!
//! MIR sits between HIR and the codegen backends. It represents a COBOL
//! program as a collection of functions, each containing a control-flow graph
//! of basic blocks. Instructions use SSA form (each value is defined exactly
//! once) and include COBOL-specific operations for decimal arithmetic, packed
//! decimal conversions, and PERFORM stack management.

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
    Decimal { digits: u8, scale: i8 },
    /// Packed decimal (BCD) stored in raw bytes.
    PackedDecimal { digits: u8, scale: i8 },
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
    Const { dest: Value, value: MirConst },
    /// Get the address of a named global variable.
    GlobalAddr { dest: Value, name: String },
    Load { dest: Value, addr: Value, ty: MirType },
    Store { addr: Value, value: Value },
    GetFieldAddr { dest: Value, base: Value, offset: u32 },
    GetElementAddr { dest: Value, base: Value, index: Value, element_size: u32 },

    // -- Integer arithmetic --
    IAdd { dest: Value, left: Value, right: Value },
    ISub { dest: Value, left: Value, right: Value },
    IMul { dest: Value, left: Value, right: Value },
    IDiv { dest: Value, left: Value, right: Value },
    IRem { dest: Value, left: Value, right: Value },
    INeg { dest: Value, operand: Value },

    // -- COBOL-specific decimal operations --
    DecimalAdd { dest: Value, left: Value, right: Value, result_scale: i8, rounded: bool },
    DecimalSub { dest: Value, left: Value, right: Value, result_scale: i8, rounded: bool },
    DecimalMul { dest: Value, left: Value, right: Value, result_scale: i8, rounded: bool },
    DecimalDiv { dest: Value, left: Value, right: Value, result_scale: i8, rounded: bool },
    DecimalCmp { dest: Value, left: Value, right: Value },

    // -- Conversions --
    DecimalToDisplay { dest: Value, value: Value, pic: DecimalFormat },
    DisplayToDecimal { dest: Value, value: Value, pic: DecimalFormat },
    PackToBinary { dest: Value, value: Value },
    BinaryToPack { dest: Value, value: Value },
    IntToDecimal { dest: Value, value: Value, scale: i8 },
    DecimalToInt { dest: Value, value: Value },

    // -- String operations --
    MoveAlphanumeric { dest: Value, src: Value, dest_len: u32, src_len: u32 },
    StringConcat { dest: Value, parts: Vec<Value>, delimiter: Option<Value>, pointer: Option<Value> },
    StringSplit { src: Value, delimiters: Vec<Value>, targets: Vec<Value> },
    Inspect { target: Value, mode: InspectMode },

    // -- Comparisons --
    ICmpEq { dest: Value, left: Value, right: Value },
    ICmpNe { dest: Value, left: Value, right: Value },
    ICmpLt { dest: Value, left: Value, right: Value },
    ICmpGt { dest: Value, left: Value, right: Value },
    ICmpLe { dest: Value, left: Value, right: Value },
    ICmpGe { dest: Value, left: Value, right: Value },

    // -- PERFORM stack management --
    PerformPush { return_block: BlockId },
    PerformPop { dest: Value },

    // -- File I/O --
    FileOpen { file_handle: Value, mode: FileOpenMode },
    FileClose { file_handle: Value },
    FileRead { file_handle: Value, into: Value, at_end: BlockId, not_at_end: BlockId },
    FileWrite { file_handle: Value, from: Value },

    // -- Runtime calls --
    CallRuntime { dest: Option<Value>, func: String, args: Vec<Value> },
    CallProgram { dest: Option<Value>, program: Value, args: Vec<Value> },

    // -- SSA --
    Phi { dest: Value, incoming: Vec<(BlockId, Value)> },

    // -- Size error --
    SizeError { value: Value, on_error: BlockId, no_error: BlockId },
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
    Branch { cond: Value, then_block: BlockId, else_block: BlockId },
    /// Multi-way switch.
    Switch { value: Value, cases: Vec<(i64, BlockId)>, default: BlockId },
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
}

/// A file descriptor in the MIR module.
#[derive(Debug, Clone, PartialEq)]
pub struct MirFileDescriptor {
    /// The file name.
    pub name: String,
    /// File organization from the HIR.
    pub organization: cobol_hir::FileOrganization,
    /// Total record size in bytes.
    pub record_size: u32,
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
}

impl MirModule {
    /// Create an empty MIR module with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: Vec::new(),
            globals: Vec::new(),
            file_descriptors: Vec::new(),
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
pub fn lower(hir: &cobol_hir::HirModule, interner: &cobol_intern::Interner) -> MirModule {
    let mut lowerer = MirLowerer::new(hir, interner);
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
    /// Maps paragraph names to MIR block IDs for GO TO resolution.
    paragraph_block_map: std::collections::HashMap<String, BlockId>,
    /// Maps data item names to their OCCURS element size (single element, before multiplication).
    occurs_element_size: std::collections::HashMap<String, u32>,
    /// Maps index names (from INDEXED BY) to their associated table info:
    /// (table_name, occurs_max, element_size).
    index_info: std::collections::HashMap<String, (String, u32, u32)>,
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
            paragraph_block_map: std::collections::HashMap::new(),
            occurs_element_size: std::collections::HashMap::new(),
            index_info: std::collections::HashMap::new(),
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

            // Store the ASSIGN TO path from the file descriptor
            if !fd.assign_to.is_empty() {
                self.file_assign_map.insert(file_name.clone(), fd.assign_to.clone());
            }

            // Record items
            let mut records = Vec::new();
            for &item_id in &fd.record_items {
                let item = &self.hir.data_items[item_id.into_raw()];
                if let Some(name) = item.name {
                    records.push(self.interner.resolve(name).to_string());
                }
            }
            self.file_record_map.insert(file_name, records);
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
            };

            self.global_map.insert(handle_name, self.module.globals.len());
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
                    let scale = item.storage.picture.as_ref()
                        .map(|p| p.scale).unwrap_or(0);
                    let dot_pos = item.storage.picture.as_ref()
                        .map(|p| Self::pic_dot_position(&p.pic_string))
                        .unwrap_or(-1);
                    self.linkage_map.insert(param_str.clone(), LinkageInfo {
                        param_value: Value::from_raw(i as u32),
                        byte_size,
                        scale,
                        dot_pos,
                    });
                    break;
                }
            }
        }
    }

    fn lower_globals(&mut self) {
        let mut offset = 0u32;
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let name = item.name.map(|n| self.interner.resolve(n).to_string())
                .unwrap_or_else(|| format!("FILLER_{}", offset));

            // Skip 88-level items as globals (they are condition names, not storage)
            if item.level == 88 {
                continue;
            }

            let byte_size = item.storage.byte_size;

            // Track OCCURS element size for subscript handling, and INDEXED BY names.
            // byte_size is the size of ONE occurrence (the element size).
            if let Some(ref occ) = item.occurs {
                if occ.max > 1 && byte_size > 0 {
                    let elem_size = byte_size;
                    self.occurs_element_size.insert(name.clone(), elem_size);
                    // Track index names
                    for &idx_name in &occ.indexed_by {
                        let idx_str = self.interner.resolve(idx_name).to_string();
                        self.index_info.insert(idx_str, (name.clone(), occ.max, elem_size));
                    }
                }
            }

            // Handle REDEFINES: use the same offset as the redefined item
            let item_offset = if let Some(redef_id) = item.redefines {
                let redef_item = &self.hir.data_items[redef_id.into_raw()];
                let redef_name = redef_item.name.map(|n| self.interner.resolve(n).to_string())
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

            // For OCCURS items, allocate total array size (element_size * max)
            let occurs_max = item.occurs.as_ref().map(|o| o.max).unwrap_or(1);
            let alloc_size = byte_size * occurs_max;
            let mir_ty = if alloc_size == 0 {
                MirType::Bytes(1) // Minimum 1 byte for group items
            } else {
                MirType::Bytes(alloc_size)
            };

            let pic_str = item.storage.picture.as_ref().map(|p| p.pic_string.as_str());
            let initial_value = item.value.as_ref().map(|v| {
                self.lower_initial_value(v, byte_size, pic_str)
            });

            let redefines_name = item.redefines.map(|redef_id| {
                let redef_item = &self.hir.data_items[redef_id.into_raw()];
                redef_item.name.map(|n| self.interner.resolve(n).to_string())
                    .unwrap_or_default()
            });

            let global = MirGlobal {
                name: name.clone(),
                ty: mir_ty,
                initial_value,
                offset: item_offset,
                redefines: redefines_name,
            };

            self.global_map.insert(name, self.module.globals.len());
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
        // Also mark children as REDEFINES of the parent so they share memory.
        {
            let ws_items: Vec<(u8, String, bool)> = self.hir.working_storage.iter().map(|&id| {
                let it = &self.hir.data_items[id.into_raw()];
                let n = it.name.map(|n| self.interner.resolve(n).to_string())
                    .unwrap_or_default();
                let has_occurs = it.occurs.as_ref().map_or(false, |o| o.max > 1);
                (it.level, n, has_occurs)
            }).collect();

            let mut i = 0;
            while i < ws_items.len() {
                let (parent_level, ref parent_name, has_occurs) = ws_items[i];
                if has_occurs {
                    if let Some(&elem_size) = self.occurs_element_size.get(parent_name) {
                        // Walk subsequent items that are subordinate (higher level number)
                        let mut j = i + 1;
                        while j < ws_items.len() {
                            let (child_level, ref child_name, _) = ws_items[j];
                            if child_level <= parent_level {
                                break; // no longer subordinate
                            }
                            if !child_name.is_empty() {
                                self.occurs_element_size
                                    .entry(child_name.clone())
                                    .or_insert(elem_size);
                                // Make child share the parent's memory (like REDEFINES)
                                if let Some(&child_idx) = self.global_map.get(child_name) {
                                    self.module.globals[child_idx].redefines =
                                        Some(parent_name.clone());
                                }
                            }
                            j += 1;
                        }
                    }
                }
                i += 1;
            }
        }

        // Create globals for INDEXED BY index names (8-byte i64 each, initialized to 1)
        let idx_names: Vec<String> = self.index_info.keys().cloned().collect();
        for idx_name in idx_names {
            if !self.global_map.contains_key(&idx_name) {
                let global = MirGlobal {
                    name: idx_name.clone(),
                    ty: MirType::I64,
                    initial_value: Some(MirConst::Int(1)),
                    offset,
                    redefines: None,
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
    /// display-format representation of COBOL numerics.
    fn lower_initial_value(&self, val: &cobol_hir::InitialValue, byte_size: u32, pic_str: Option<&str>) -> MirConst {
        let has_dot = pic_str.map(|p| p.contains('.')).unwrap_or(false);

        match val {
            cobol_hir::InitialValue::String_(s) => {
                // Pad or truncate to byte_size
                let mut bytes = s.as_bytes().to_vec();
                bytes.resize(byte_size as usize, b' ');
                MirConst::Bytes(bytes)
            }
            cobol_hir::InitialValue::Numeric(n, _scale) => {
                // Display format: ASCII digits, right-justified, zero-padded.
                // For signed fields (PIC contains 'S'), negative values use
                // trailing overpunch: last byte += 0x40.
                let is_negative = *n < 0;
                let is_signed = pic_str
                    .map(|p| p.to_ascii_uppercase().contains('S'))
                    .unwrap_or(false);
                let num_str = format!("{}", n.unsigned_abs());
                if has_dot {
                    let pic = pic_str.unwrap();
                    let mut bytes = Self::build_edited_zero_template(pic, byte_size);
                    // Fill digits right-to-left, skipping the '.' position
                    let digit_positions: Vec<usize> = (0..bytes.len())
                        .filter(|&i| bytes[i] != b'.')
                        .collect();
                    let num_bytes = num_str.as_bytes();
                    for (j, &pos) in digit_positions.iter().rev().enumerate() {
                        if j < num_bytes.len() {
                            bytes[pos] = num_bytes[num_bytes.len() - 1 - j];
                        }
                    }
                    // Apply trailing overpunch for negative signed values
                    if is_signed && is_negative && !bytes.is_empty() {
                        let last = bytes.len() - 1;
                        bytes[last] = bytes[last].wrapping_add(0x40);
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
                    // Apply trailing overpunch for negative signed values
                    if is_signed && is_negative && !bytes.is_empty() {
                        let last = bytes.len() - 1;
                        bytes[last] = bytes[last].wrapping_add(0x40);
                    }
                    MirConst::Bytes(bytes)
                }
            }
            cobol_hir::InitialValue::Zero => {
                if has_dot {
                    MirConst::Bytes(Self::build_edited_zero_template(pic_str.unwrap(), byte_size))
                } else {
                    MirConst::Bytes(vec![b'0'; byte_size as usize])
                }
            }
            cobol_hir::InitialValue::Space => {
                MirConst::Bytes(vec![b' '; byte_size as usize])
            }
            cobol_hir::InitialValue::HighValue => {
                MirConst::Bytes(vec![0xFF; byte_size as usize])
            }
            cobol_hir::InitialValue::LowValue => {
                MirConst::Bytes(vec![0x00; byte_size as usize])
            }
            cobol_hir::InitialValue::Quote => {
                MirConst::Bytes(vec![b'"'; byte_size as usize])
            }
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
        let is_subprogram = !self.hir.using_params.is_empty();

        // Subprograms: function named after PROGRAM-ID, with pointer params
        // Main programs: function named "_cobol_main", no params
        let (func_name, params, num_params) = if is_subprogram {
            let name = self.interner.resolve(self.hir.program_name)
                .to_string()
                .replace('-', "_");
            let params: Vec<(String, MirType)> = self.hir.using_params.iter()
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
            p.statements.iter().any(|s| self.stmt_needs_paragraph_blocks(s))
        });

        if needs_paragraph_blocks {
            for para in &self.hir.paragraphs {
                let para_name = self.interner.resolve(para.name).to_string();
                let block_id = func.new_block();
                self.paragraph_block_map.insert(para_name, block_id);
            }
        }

        let mut current_block_id = entry_block;
        let mut instructions = Vec::new();

        for para in &self.hir.paragraphs {
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

            for stmt in &para.statements {
                self.lower_statement_to_blocks(
                    stmt,
                    &mut func,
                    &mut current_block_id,
                    &mut instructions,
                );
            }
        }

        // Main programs get an implicit STOP RUN if none found
        if !is_subprogram {
            let has_stop = self.hir.paragraphs.iter().any(|p| {
                p.statements.iter().any(|s| matches!(s, cobol_hir::HirStatement::StopRun))
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

        // Finalize the last block
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
            cobol_hir::HirStatement::Perform(pt) => matches!(
                pt,
                cobol_hir::PerformType::OutOfLine { .. }
                | cobol_hir::PerformType::Times { .. }
                | cobol_hir::PerformType::Until { .. }
                | cobol_hir::PerformType::Varying { inline_body: None, .. }
            ),
            cobol_hir::HirStatement::If { then_branch, else_branch, .. } => {
                then_branch.iter().any(|s| self.stmt_needs_paragraph_blocks(s))
                    || else_branch.as_ref().map_or(false, |eb| eb.iter().any(|s| self.stmt_needs_paragraph_blocks(s)))
            }
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
            cobol_hir::HirStatement::Move { from, to } => {
                self.lower_move(from, to, func, instructions);
            }
            cobol_hir::HirStatement::Add { operands, to, giving, .. } => {
                if giving.is_some() {
                    self.lower_add(operands, giving.as_deref(), func, instructions);
                } else if !to.is_empty() {
                    self.lower_add_to(operands, to, func, instructions);
                }
            }
            cobol_hir::HirStatement::Subtract { operands, from: targets, giving, .. } => {
                self.lower_subtract(operands, targets, giving.as_deref(), func, instructions);
            }
            cobol_hir::HirStatement::Multiply { operand1, by, giving, .. } => {
                self.lower_multiply(operand1, by, giving.as_deref(), func, instructions);
            }
            cobol_hir::HirStatement::Divide { operand1, into_or_by, giving, rounded, .. } => {
                self.lower_divide(operand1, into_or_by, giving.as_deref(), *rounded, func, instructions);
            }
            cobol_hir::HirStatement::If { condition, then_branch, else_branch } => {
                self.lower_if(condition, then_branch, else_branch.as_deref(),
                    func, current_block_id, instructions);
            }
            cobol_hir::HirStatement::Perform(perform_type) => {
                self.lower_perform(perform_type, func, current_block_id, instructions);
            }
            cobol_hir::HirStatement::StringStmt { sources, into, pointer } => {
                self.lower_string(sources, into, pointer.as_ref(), func, instructions);
            }
            cobol_hir::HirStatement::Call { program, using, .. } => {
                self.lower_call(program, using, func, instructions);
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
            cobol_hir::HirStatement::Write { record } => {
                self.lower_file_write(*record, func, instructions);
            }
            cobol_hir::HirStatement::Read { file, into, at_end } => {
                self.lower_file_read(*file, *into, at_end,
                    func, current_block_id, instructions);
            }
            cobol_hir::HirStatement::Evaluate { subject, whens } => {
                self.lower_evaluate(subject, whens, func, current_block_id, instructions);
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
            cobol_hir::HirStatement::Compute { targets, expr, rounded, .. } => {
                self.lower_compute(targets, expr, *rounded, func, instructions);
            }
            cobol_hir::HirStatement::Accept { target, source } => {
                self.lower_accept(target, *source, func, instructions);
            }
            cobol_hir::HirStatement::Inspect { target, inspect_type } => {
                self.lower_inspect(target, inspect_type, func, instructions);
            }
            cobol_hir::HirStatement::Unstring { source, delimiters, targets, pointer, tallying } => {
                self.lower_unstring(source, delimiters, targets, pointer.as_ref(), tallying.as_ref(), func, instructions);
            }
            cobol_hir::HirStatement::Initialize { targets } => {
                self.lower_initialize(targets, func, instructions);
            }
            cobol_hir::HirStatement::Set { target, action } => {
                self.lower_set(target, action, func, instructions);
            }
            cobol_hir::HirStatement::Search { table, at_end, whens } => {
                self.lower_search(table, at_end, whens, func, current_block_id, instructions);
            }
            _ => {
                // Other statements: not yet implemented
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
                cobol_hir::HirExpr::DataRef(data_ref) => {
                    // Look up address and size (linkage params or globals)
                    let name = self.interner.resolve(data_ref.name).to_string();
                    let addr_size = if let Some(info) = self.linkage_map.get(&name) {
                        Some((info.param_value, info.byte_size))
                    } else if let Some(&idx) = self.global_map.get(&name) {
                        let global = &self.module.globals[idx];
                        let size = match &global.ty {
                            MirType::Bytes(n) => *n,
                            _ => 1,
                        };
                        let addr = func.new_value();
                        instructions.push(MirInst::GlobalAddr {
                            dest: addr,
                            name: name.clone(),
                        });
                        Some((addr, size))
                    } else {
                        None
                    };

                    if let Some((addr, size)) = addr_size {
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
            let (target_addr, target_size) = if let Some(info) = self.linkage_map.get(&target_name_str) {
                (info.param_value, info.byte_size)
            } else if let Some((addr, size)) = self.emit_expr_addr(&target_expr, func, instructions) {
                (addr, size)
            } else {
                continue;
            };

            // Get target PIC info
            let (target_is_numeric, target_scale, target_dot_pos, _target_is_signed) =
                self.data_item_pic_info(&target_name_str).unwrap_or((false, 0, -1, false));

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
                            let scaled = (*n as i64).unsigned_abs() * 10u64.pow(target_scale as u32);
                            if target_dot_pos >= 0 {
                                // Edited numeric with literal '.'
                                let mut bytes = Self::build_edited_zero_template(
                                    &self.get_pic_string(&target_name_str).unwrap_or_default(),
                                    target_size,
                                );
                                // Fill digits right-to-left, skipping '.'
                                let digit_positions: Vec<usize> = (0..bytes.len())
                                    .filter(|&i| bytes[i] != b'.')
                                    .collect();
                                let num_str = format!("{}", scaled);
                                let num_bytes = num_str.as_bytes();
                                for (j, &pos) in digit_positions.iter().rev().enumerate() {
                                    if j < num_bytes.len() {
                                        bytes[pos] = num_bytes[num_bytes.len() - 1 - j];
                                    }
                                }
                                let src_val = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: src_val,
                                    value: MirConst::Str(String::from_utf8_lossy(&bytes).to_string()),
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
                                // Numeric without literal dot (V implies decimal)
                                let display_str = format!("{:0>width$}", scaled, width = target_size as usize);
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
                                    func: "cobolrt_move_alphanumeric".to_string(),
                                    args: vec![src_val, len_val, target_addr, len_val],
                                });
                            }
                        } else {
                            // Simple numeric, no scale
                            let display_str = format!("{:0>width$}", n.unsigned_abs(), width = target_size as usize);
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
                                func: "cobolrt_move_alphanumeric".to_string(),
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
                            func: "cobolrt_move_alphanumeric".to_string(),
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
                            func: "cobolrt_move_alphanumeric".to_string(),
                            args: vec![src_val, src_len, target_addr, dest_len],
                        });
                    }
                }
                cobol_hir::HirExpr::DataRef(src_ref) => {
                    let src_name = self.interner.resolve(src_ref.name).to_string();
                    let src_expr = cobol_hir::HirExpr::DataRef(src_ref.clone());
                    let (src_addr, src_size) = if let Some(info) = self.linkage_map.get(&src_name) {
                        (info.param_value, info.byte_size)
                    } else if let Some((addr, size)) = self.emit_expr_addr(&src_expr, func, instructions) {
                        (addr, size)
                    } else {
                        continue;
                    };

                    // Get source PIC info
                    let (src_is_numeric, src_scale, src_dot_pos, _src_is_signed) =
                        self.data_item_pic_info(&src_name).unwrap_or((false, 0, -1, false));

                    if src_is_numeric && target_is_numeric {
                        // Check if the target is numeric-edited (has Z, *, $, etc.)
                        let target_is_edited = self.is_numeric_edited(&target_name_str);

                        if target_is_edited {
                            // Numeric to NumericEdited: first do numeric move to
                            // align digits, then apply numeric editing format.
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
                            // Step 1: numeric move to get digits aligned
                            let v_rounded_0a = func.new_value();
                            instructions.push(MirInst::Const { dest: v_rounded_0a, value: MirConst::Int(0) });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_move_numeric".to_string(),
                                args: vec![
                                    src_addr, v_src_len, v_src_scale, v_src_dot,
                                    target_addr, v_dest_len, v_dest_scale, v_dest_dot, v_rounded_0a,
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
                        } else if src_scale == target_scale && src_dot_pos == target_dot_pos
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
                                func: "cobolrt_move_alphanumeric".to_string(),
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
                            instructions.push(MirInst::Const { dest: v_rounded_0b, value: MirConst::Int(0) });
                            instructions.push(MirInst::CallRuntime {
                                dest: None,
                                func: "cobolrt_move_numeric".to_string(),
                                args: vec![
                                    src_addr, v_src_len, v_src_scale, v_src_dot,
                                    target_addr, v_dest_len, v_dest_scale, v_dest_dot, v_rounded_0b,
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
                            func: "cobolrt_move_alphanumeric".to_string(),
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
                        func: "cobolrt_move_alphanumeric".to_string(),
                        args: vec![src_val, src_len, target_addr, dest_len],
                    });
                }
                _ => {
                    // Other expression types not yet supported in MOVE
                }
            }
        }
    }

    fn lower_initialize(
        &self,
        targets: &[cobol_hir::HirDataRef],
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
                // Group item: find all subordinate elementary items by level number.
                // In the flat working_storage list, items following a group item
                // with a higher level number are its children, until we hit an item
                // with the same or lower level number.
                let group_level = item.level;
                for j in (target_idx + 1)..self.hir.working_storage.len() {
                    let child_item_id = self.hir.working_storage[j];
                    let child = &self.hir.data_items[child_item_id.into_raw()];

                    // Stop when we reach an item at the same or lower level
                    if child.level <= group_level {
                        break;
                    }

                    // Skip 88-level condition names
                    if child.level == 88 {
                        continue;
                    }

                    // Skip nested group items (they have no storage of their own)
                    if child.is_group {
                        continue;
                    }

                    // Elementary item: initialize it
                    if let Some(child_name) = child.name {
                        let name_str = self.interner.resolve(child_name).to_string();
                        self.initialize_elementary_item(&name_str, func, instructions);
                    }
                }
            } else {
                // Elementary item: initialize based on PIC category
                self.initialize_elementary_item(&target_name_str, func, instructions);
            }
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

        let (is_numeric, _scale, _dot_pos, _is_signed) =
            self.data_item_pic_info(name).unwrap_or((false, 0, -1, false));

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

    /// Resolve a data ref to (global_name, byte_size)
    fn resolve_data_ref(&self, data_ref: &cobol_hir::HirDataRef) -> Option<(String, u32)> {
        let name = self.interner.resolve(data_ref.name).to_string();
        if let Some(&idx) = self.global_map.get(&name) {
            let global = &self.module.globals[idx];
            let size = match &global.ty {
                MirType::Bytes(n) => *n,
                _ => 1,
            };
            Some((name, size))
        } else {
            None
        }
    }

    /// Look up the HIR data item for a data ref, returning (name, byte_size, scale, dot_pos).
    fn resolve_data_ref_full(&self, data_ref: &cobol_hir::HirDataRef) -> Option<(String, u32, i32, i32)> {
        let name = self.interner.resolve(data_ref.name).to_string();
        // Check linkage map first
        if let Some(info) = self.linkage_map.get(&name) {
            return Some((name, info.byte_size, info.scale, info.dot_pos));
        }
        if self.global_map.get(&name).is_none() {
            return None;
        }
        // Find the HIR data item to get scale and PIC info
        for &item_id in &self.hir.working_storage {
            let item = &self.hir.data_items[item_id.into_raw()];
            let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
            if item_name.as_deref() == Some(&name) {
                let byte_size = item.storage.byte_size;
                let scale = item.storage.picture.as_ref()
                    .map(|p| p.scale)
                    .unwrap_or(0);
                let dot_pos = item.storage.picture.as_ref()
                    .map(|p| Self::pic_dot_position(&p.pic_string))
                    .unwrap_or(-1);
                return Some((name, byte_size, scale, dot_pos));
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
                    let effective_size = if !dr.subscripts.is_empty() {
                        if let Some(&elem_size) = self.occurs_element_size.get(&gname) {
                            // Get the first subscript (single-dimension for now)
                            let subscript = &dr.subscripts[0];
                            let index_val = match subscript {
                                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
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
                                    let sub_name = self.interner.resolve(sub_ref.name).to_string();
                                    if self.index_info.contains_key(&sub_name) {
                                        // Index variable: stored as i64, load directly
                                        let sub_addr = func.new_value();
                                        instructions.push(MirInst::GlobalAddr {
                                            dest: sub_addr, name: sub_name,
                                        });
                                        let raw_val = func.new_value();
                                        instructions.push(MirInst::Load {
                                            dest: raw_val,
                                            addr: sub_addr,
                                            ty: MirType::I64,
                                        });
                                        // Subtract 1 for 0-based indexing
                                        let one = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: one, value: MirConst::Int(1),
                                        });
                                        let idx = func.new_value();
                                        instructions.push(MirInst::ISub {
                                            dest: idx, left: raw_val, right: one,
                                        });
                                        idx
                                    } else if let Some(sub_info) = self.linkage_map.get(&sub_name) {
                                        // Linkage param
                                        let sz = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: sz, value: MirConst::Int(sub_info.byte_size as i64),
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
                                            dest: one, value: MirConst::Int(1),
                                        });
                                        let idx = func.new_value();
                                        instructions.push(MirInst::ISub {
                                            dest: idx, left: raw_val, right: one,
                                        });
                                        idx
                                    } else if let Some(&sub_idx) = self.global_map.get(&sub_name) {
                                        let sub_global = &self.module.globals[sub_idx];
                                        let sub_size = match &sub_global.ty {
                                            MirType::Bytes(n) => *n,
                                            _ => 1,
                                        };
                                        let sub_addr = func.new_value();
                                        instructions.push(MirInst::GlobalAddr {
                                            dest: sub_addr, name: sub_name,
                                        });
                                        let sub_len = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: sub_len, value: MirConst::Int(sub_size as i64),
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
                                            dest: one, value: MirConst::Int(1),
                                        });
                                        let idx = func.new_value();
                                        instructions.push(MirInst::ISub {
                                            dest: idx, left: raw_val, right: one,
                                        });
                                        idx
                                    } else {
                                        let v = func.new_value();
                                        instructions.push(MirInst::Const {
                                            dest: v, value: MirConst::Int(0),
                                        });
                                        v
                                    }
                                }
                                _ => {
                                    let v = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: v, value: MirConst::Int(0),
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
                                let raw_val = if let Some(ref_info) = self.linkage_map.get(&ref_name) {
                                    let sz = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: sz, value: MirConst::Int(ref_info.byte_size as i64),
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
                                        dest: ref_addr, name: ref_name,
                                    });
                                    let ref_len = func.new_value();
                                    instructions.push(MirInst::Const {
                                        dest: ref_len, value: MirConst::Int(ref_size as i64),
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
                                        dest: v, value: MirConst::Int(0),
                                    });
                                    v
                                };
                                // Subtract 1 to convert from 1-based to 0-based
                                let one = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: one, value: MirConst::Int(1),
                                });
                                let offset_0 = func.new_value();
                                instructions.push(MirInst::ISub {
                                    dest: offset_0, left: raw_val, right: one,
                                });
                                offset_0
                            }
                            _ => {
                                let v = func.new_value();
                                instructions.push(MirInst::Const {
                                    dest: v, value: MirConst::Int(0),
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
                                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                                    *n as u32
                                }
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
                    cobol_hir::FigurativeConstant::HighValue => String::from_utf8(vec![0xFF]).unwrap_or_default(),
                    cobol_hir::FigurativeConstant::LowValue => String::from_utf8(vec![0x00]).unwrap_or_default(),
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
        let actual_assign = self.file_assign_map.get(&file_name)
            .cloned()
            .unwrap_or_else(|| format!("{}.DAT", file_name));

        // Emit: handle = cobolrt_file_open(filename_ptr, filename_len, mode)
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

        let handle_result = func.new_value();
        instructions.push(MirInst::CallRuntime {
            dest: Some(handle_result),
            func: "cobolrt_file_open".to_string(),
            args: vec![filename_val, filename_len, mode_val],
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
    }

    /// Lower WRITE statement.
    fn lower_file_write(
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
                func: "cobolrt_file_write_line".to_string(),
                args: vec![handle_val, record_addr, size_val],
            });
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
        let record_name = self.file_record_map.get(&file_name)
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

        // Call cobolrt_file_read_line(handle, data_ptr, data_len) -> eof_flag (0=ok, 1=eof)
        let eof_flag = func.new_value();
        instructions.push(MirInst::CallRuntime {
            dest: Some(eof_flag),
            func: "cobolrt_file_read_line".to_string(),
            args: vec![handle_val, record_addr, size_val],
        });

        // If INTO ws-record, copy file record to ws-record
        if let Some(into_name) = into {
            let into_str = self.interner.resolve(into_name).to_string();
            if let Some(&idx) = self.global_map.get(&into_str) {
                let into_global = &self.module.globals[idx];
                let into_size = match &into_global.ty {
                    MirType::Bytes(n) => *n,
                    _ => 1,
                };
                let into_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: into_addr,
                    name: into_str,
                });
                let src_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: src_len,
                    value: MirConst::Int(record_size as i64),
                });
                let dest_len = func.new_value();
                instructions.push(MirInst::Const {
                    dest: dest_len,
                    value: MirConst::Int(into_size as i64),
                });
                // Re-get record address since previous addr was consumed
                let record_addr2 = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: record_addr2,
                    name: record_name.clone(),
                });
                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_move_alphanumeric".to_string(),
                    args: vec![record_addr2, src_len, into_addr, dest_len],
                });
            }
        }

        // Handle AT END: if eof_flag != 0, execute at_end statements
        if !at_end.is_empty() {
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
                    else_block: continue_block,
                },
            });

            // At-end block
            let mut at_end_insts = Vec::new();
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
    }

    /// Lower CALL statement: call an external subprogram.
    fn lower_call(
        &self,
        program: &cobol_hir::HirExpr,
        using: &[cobol_hir::CallArg],
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // Get program name and mangle it (COBOL hyphens → underscores)
        let prog_name = match program {
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                s.to_ascii_uppercase().replace('-', "_")
            }
            _ => return,
        };

        // Collect argument addresses (BY REFERENCE → pointer to data item)
        let mut arg_values = Vec::new();
        for call_arg in using {
            match &call_arg.value {
                cobol_hir::HirExpr::DataRef(dr) => {
                    let name = self.interner.resolve(dr.name).to_string();
                    if let Some(info) = self.linkage_map.get(&name) {
                        arg_values.push(info.param_value);
                    } else if let Some((gname, _size)) = self.resolve_data_ref(dr) {
                        let addr = func.new_value();
                        instructions.push(MirInst::GlobalAddr { dest: addr, name: gname });
                        arg_values.push(addr);
                    }
                }
                _ => {}
            }
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

        // Get target address
        let target_ref = &giving_refs[0];
        let (target_name, target_size) = match self.resolve_data_ref(target_ref) {
            Some(v) => v,
            None => return,
        };

        // Get all operand addresses
        let mut operand_addrs: Vec<(Value, u32)> = Vec::new();
        for op in operands {
            if let Some(addr_size) = self.emit_expr_addr(op, func, instructions) {
                operand_addrs.push(addr_size);
            }
        }

        if operand_addrs.len() < 2 {
            return;
        }

        let target_addr = func.new_value();
        instructions.push(MirInst::GlobalAddr {
            dest: target_addr,
            name: target_name,
        });
        let target_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: target_len,
            value: MirConst::Int(target_size as i64),
        });

        // First, add operands[0] + operands[1] → target
        let (a_addr, a_size) = operand_addrs[0];
        let a_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: a_len,
            value: MirConst::Int(a_size as i64),
        });

        let (b_addr, b_size) = operand_addrs[1];
        let b_len = func.new_value();
        instructions.push(MirInst::Const {
            dest: b_len,
            value: MirConst::Int(b_size as i64),
        });

        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_add_numeric".to_string(),
            args: vec![a_addr, a_len, b_addr, b_len, target_addr, target_len],
        });

        // If more than 2 operands, accumulate: target = target + operands[i]
        for i in 2..operand_addrs.len() {
            let target_addr2 = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: target_addr2,
                name: self.interner.resolve(target_ref.name).to_string(),
            });
            let target_len2 = func.new_value();
            instructions.push(MirInst::Const {
                dest: target_len2,
                value: MirConst::Int(target_size as i64),
            });

            let (op_addr, op_size) = operand_addrs[i];
            let op_len = func.new_value();
            instructions.push(MirInst::Const {
                dest: op_len,
                value: MirConst::Int(op_size as i64),
            });

            // target_addr2 is both source and dest
            let target_addr3 = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: target_addr3,
                name: self.interner.resolve(target_ref.name).to_string(),
            });
            let target_len3 = func.new_value();
            instructions.push(MirInst::Const {
                dest: target_len3,
                value: MirConst::Int(target_size as i64),
            });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_add_numeric".to_string(),
                args: vec![target_addr2, target_len2, op_addr, op_len, target_addr3, target_len3],
            });
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
            // Result = from[0] - sum(operands) => giving[0]
            if from.is_empty() || giving_refs.is_empty() {
                return;
            }

            let from_ref = &from[0];
            let (from_name, from_size) = match self.resolve_data_ref(from_ref) {
                Some(v) => v,
                None => return,
            };

            let target_ref = &giving_refs[0];
            let (target_name, target_size) = match self.resolve_data_ref(target_ref) {
                Some(v) => v,
                None => return,
            };

            // First operand: compute target = from - operands[0]
            let from_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr { dest: from_addr, name: from_name.clone() });
            let from_len = func.new_value();
            instructions.push(MirInst::Const { dest: from_len, value: MirConst::Int(from_size as i64) });

            let (op0_addr, op0_size) = operand_addrs[0];
            let op0_len = func.new_value();
            instructions.push(MirInst::Const { dest: op0_len, value: MirConst::Int(op0_size as i64) });

            let target_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr { dest: target_addr, name: target_name.clone() });
            let target_len = func.new_value();
            instructions.push(MirInst::Const { dest: target_len, value: MirConst::Int(target_size as i64) });

            // cobolrt_subtract_numeric(from, from_len, operand, op_len, target, target_len)
            // C shim computes: target = src1 - src2 = from - operand
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_subtract_numeric".to_string(),
                args: vec![from_addr, from_len, op0_addr, op0_len, target_addr, target_len],
            });

            // If more operands, subtract each from the target in place:
            // target = target - operands[i]
            for i in 1..operand_addrs.len() {
                let (opi_addr, opi_size) = operand_addrs[i];
                let opi_len = func.new_value();
                instructions.push(MirInst::Const { dest: opi_len, value: MirConst::Int(opi_size as i64) });

                let t_addr1 = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: t_addr1,
                    name: self.interner.resolve(target_ref.name).to_string(),
                });
                let t_len1 = func.new_value();
                instructions.push(MirInst::Const { dest: t_len1, value: MirConst::Int(target_size as i64) });

                let t_addr2 = func.new_value();
                instructions.push(MirInst::GlobalAddr {
                    dest: t_addr2,
                    name: self.interner.resolve(target_ref.name).to_string(),
                });
                let t_len2 = func.new_value();
                instructions.push(MirInst::Const { dest: t_len2, value: MirConst::Int(target_size as i64) });

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_subtract_numeric".to_string(),
                    args: vec![t_addr1, t_len1, opi_addr, opi_len, t_addr2, t_len2],
                });
            }
        } else {
            // SUBTRACT ... FROM ... (accumulator form, no GIVING)
            // For each target in `from`: target = target - sum(operands)
            for target_ref in from {
                let (target_name, target_size) = match self.resolve_data_ref(target_ref) {
                    Some(v) => v,
                    None => continue,
                };

                for &(op_addr, op_size) in &operand_addrs {
                    let op_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: op_len,
                        value: MirConst::Int(op_size as i64),
                    });

                    // target is both src1 and dest: target = target - operand
                    let t_addr1 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: t_addr1,
                        name: target_name.clone(),
                    });
                    let t_len1 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: t_len1,
                        value: MirConst::Int(target_size as i64),
                    });

                    let t_addr2 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: t_addr2,
                        name: target_name.clone(),
                    });
                    let t_len2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: t_len2,
                        value: MirConst::Int(target_size as i64),
                    });

                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_subtract_numeric".to_string(),
                        args: vec![t_addr1, t_len1, op_addr, op_len, t_addr2, t_len2],
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
        instructions.push(MirInst::Const { dest: a_len, value: MirConst::Int(a_size as i64) });

        let (b_addr, b_size) = match self.emit_expr_addr(by, func, instructions) {
            Some(v) => v,
            None => return,
        };
        let b_len = func.new_value();
        instructions.push(MirInst::Const { dest: b_len, value: MirConst::Int(b_size as i64) });

        if let Some(giving_refs) = giving {
            // MULTIPLY A BY B GIVING C
            if giving_refs.is_empty() { return; }
            let target_ref = &giving_refs[0];
            let (target_name, target_size) = match self.resolve_data_ref(target_ref) {
                Some(v) => v,
                None => return,
            };
            let target_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr { dest: target_addr, name: target_name });
            let target_len = func.new_value();
            instructions.push(MirInst::Const { dest: target_len, value: MirConst::Int(target_size as i64) });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_multiply_numeric".to_string(),
                args: vec![a_addr, a_len, b_addr, b_len, target_addr, target_len],
            });
        } else {
            // MULTIPLY A BY B (accumulator: B = A * B)
            // The BY operand is both source and destination
            if let cobol_hir::HirExpr::DataRef(dr) = by {
                let (by_name, by_size) = match self.resolve_data_ref(dr) {
                    Some(v) => v,
                    None => return,
                };
                let dest_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr { dest: dest_addr, name: by_name });
                let dest_len = func.new_value();
                instructions.push(MirInst::Const { dest: dest_len, value: MirConst::Int(by_size as i64) });

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_multiply_numeric".to_string(),
                    args: vec![a_addr, a_len, b_addr, b_len, dest_addr, dest_len],
                });
            }
        }
    }

    fn lower_divide(
        &self,
        operand_a: &cobol_hir::HirExpr,
        by_operand: &cobol_hir::HirExpr,
        giving: Option<&[cobol_hir::HirDataRef]>,
        rounded: bool,
        func: &mut MirFunction,
        instructions: &mut Vec<MirInst>,
    ) {
        // DIVIDE operand_a BY by_operand GIVING target [ROUNDED]
        // We need addresses + sizes + scales for all three operands.

        // Get dividend info
        let (src1_addr, src1_size, src1_scale) = match operand_a {
            cobol_hir::HirExpr::DataRef(dr) => {
                if let Some((name, size, scale, _dot)) = self.resolve_data_ref_full(dr) {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr { dest: addr, name });
                    (addr, size, scale)
                } else { return; }
            }
            _ => return,
        };

        // Get divisor info
        let (src2_addr, src2_size, src2_scale) = match by_operand {
            cobol_hir::HirExpr::DataRef(dr) => {
                if let Some((name, size, scale, _dot)) = self.resolve_data_ref_full(dr) {
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr { dest: addr, name });
                    (addr, size, scale)
                } else { return; }
            }
            _ => return,
        };

        // Get target info
        let giving_refs = match giving {
            Some(refs) if !refs.is_empty() => refs,
            _ => return,
        };
        let target_ref = &giving_refs[0];
        let (target_name, target_size, target_scale, target_dot_pos) =
            match self.resolve_data_ref_full(target_ref) {
                Some(info) => info,
                None => return,
            };

        let target_addr = func.new_value();
        instructions.push(MirInst::GlobalAddr { dest: target_addr, name: target_name });

        // Emit all parameter values
        let v_src1_len = func.new_value();
        instructions.push(MirInst::Const { dest: v_src1_len, value: MirConst::Int(src1_size as i64) });
        let v_src1_scale = func.new_value();
        instructions.push(MirInst::Const { dest: v_src1_scale, value: MirConst::Int(src1_scale as i64) });

        let v_src2_len = func.new_value();
        instructions.push(MirInst::Const { dest: v_src2_len, value: MirConst::Int(src2_size as i64) });
        let v_src2_scale = func.new_value();
        instructions.push(MirInst::Const { dest: v_src2_scale, value: MirConst::Int(src2_scale as i64) });

        let v_dest_len = func.new_value();
        instructions.push(MirInst::Const { dest: v_dest_len, value: MirConst::Int(target_size as i64) });
        let v_dest_scale = func.new_value();
        instructions.push(MirInst::Const { dest: v_dest_scale, value: MirConst::Int(target_scale as i64) });

        let v_dot_pos = func.new_value();
        instructions.push(MirInst::Const { dest: v_dot_pos, value: MirConst::Int(target_dot_pos as i64) });

        let v_rounded = func.new_value();
        instructions.push(MirInst::Const { dest: v_rounded, value: MirConst::Int(if rounded { 1 } else { 0 }) });

        instructions.push(MirInst::CallRuntime {
            dest: None,
            func: "cobolrt_divide_scaled".to_string(),
            args: vec![
                src1_addr, v_src1_len, v_src1_scale,
                src2_addr, v_src2_len, v_src2_scale,
                target_addr, v_dest_len, v_dest_scale,
                v_dot_pos, v_rounded,
            ],
        });
    }

    /// Lower STRING statement: for each source, call cobolrt_string_append
    /// which copies the source into the target at the current pointer position
    /// and advances the pointer.
    fn lower_string(
        &self,
        sources: &[cobol_hir::HirExpr],
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
            None => return, // For now, POINTER is required
        };

        // For each source, call cobolrt_string_append(src, src_len, dest, dest_len, ptr, ptr_len)
        for source in sources {
            let (src_addr, src_size) = match source {
                cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(s)) => {
                    // Create a string constant
                    let addr = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: addr,
                        value: MirConst::Str(s.clone()),
                    });
                    (addr, s.len() as u32)
                }
                cobol_hir::HirExpr::DataRef(dr) => {
                    match self.resolve_data_ref(dr) {
                        Some((name, size)) => {
                            let addr = func.new_value();
                            instructions.push(MirInst::GlobalAddr { dest: addr, name });
                            (addr, size)
                        }
                        None => continue,
                    }
                }
                _ => continue,
            };

            let v_src_len = func.new_value();
            instructions.push(MirInst::Const { dest: v_src_len, value: MirConst::Int(src_size as i64) });

            let dest_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr { dest: dest_addr, name: target_name.clone() });
            let v_dest_len = func.new_value();
            instructions.push(MirInst::Const { dest: v_dest_len, value: MirConst::Int(target_size as i64) });

            let v_ptr_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr { dest: v_ptr_addr, name: ptr_name.clone() });
            let v_ptr_len = func.new_value();
            instructions.push(MirInst::Const { dest: v_ptr_len, value: MirConst::Int(ptr_size as i64) });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_string_append".to_string(),
                args: vec![src_addr, v_src_len, dest_addr, v_dest_len, v_ptr_addr, v_ptr_len],
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
                tally_var, mode, search, before_initial, after_initial,
            } => {
                let (tally_name, tally_size) = match self.resolve_data_ref(tally_var) {
                    Some(info) => info,
                    None => return,
                };

                // target addr + len
                let v_target_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr { dest: v_target_addr, name: target_name.clone() });
                let v_target_len = func.new_value();
                instructions.push(MirInst::Const { dest: v_target_len, value: MirConst::Int(target_size as i64) });

                // tally addr + len
                let v_tally_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr { dest: v_tally_addr, name: tally_name });
                let v_tally_len = func.new_value();
                instructions.push(MirInst::Const { dest: v_tally_len, value: MirConst::Int(tally_size as i64) });

                // mode: 0=CHARACTERS, 1=ALL, 2=LEADING
                let mode_val = match mode {
                    cobol_hir::InspectTallyMode::Characters => 0,
                    cobol_hir::InspectTallyMode::All => 1,
                    cobol_hir::InspectTallyMode::Leading => 2,
                };
                let v_mode = func.new_value();
                instructions.push(MirInst::Const { dest: v_mode, value: MirConst::Int(mode_val) });

                // search literal
                let (v_search, v_search_len) = self.emit_optional_expr(search, func, instructions);

                // before/after initial
                let (v_before, v_before_len) = self.emit_optional_expr(before_initial, func, instructions);
                let (v_after, v_after_len) = self.emit_optional_expr(after_initial, func, instructions);

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_inspect_tallying".to_string(),
                    args: vec![
                        v_target_addr, v_target_len,
                        v_tally_addr, v_tally_len,
                        v_mode,
                        v_search, v_search_len,
                        v_before, v_before_len,
                        v_after, v_after_len,
                    ],
                });
            }
            cobol_hir::InspectType::Replacing {
                mode, search, replacement, before_initial, after_initial,
            } => {
                // target addr + len
                let v_target_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr { dest: v_target_addr, name: target_name.clone() });
                let v_target_len = func.new_value();
                instructions.push(MirInst::Const { dest: v_target_len, value: MirConst::Int(target_size as i64) });

                // mode: 0=CHARACTERS, 1=ALL, 2=LEADING, 3=FIRST
                let mode_val = match mode {
                    cobol_hir::InspectReplaceMode::Characters => 0,
                    cobol_hir::InspectReplaceMode::All => 1,
                    cobol_hir::InspectReplaceMode::Leading => 2,
                    cobol_hir::InspectReplaceMode::First => 3,
                };
                let v_mode = func.new_value();
                instructions.push(MirInst::Const { dest: v_mode, value: MirConst::Int(mode_val) });

                // search literal
                let (v_search, v_search_len) = self.emit_optional_expr(search, func, instructions);

                // replacement
                let (v_repl, v_repl_len) = self.emit_expr_as_addr_and_len(replacement, func, instructions);

                // before/after initial
                let (v_before, v_before_len) = self.emit_optional_expr(before_initial, func, instructions);
                let (v_after, v_after_len) = self.emit_optional_expr(after_initial, func, instructions);

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_inspect_replacing".to_string(),
                    args: vec![
                        v_target_addr, v_target_len,
                        v_mode,
                        v_search, v_search_len,
                        v_repl, v_repl_len,
                        v_before, v_before_len,
                        v_after, v_after_len,
                    ],
                });
            }
            cobol_hir::InspectType::Converting {
                from, to, before_initial, after_initial,
            } => {
                // target addr + len
                let v_target_addr = func.new_value();
                instructions.push(MirInst::GlobalAddr { dest: v_target_addr, name: target_name.clone() });
                let v_target_len = func.new_value();
                instructions.push(MirInst::Const { dest: v_target_len, value: MirConst::Int(target_size as i64) });

                // from chars
                let (v_from, v_from_len) = self.emit_expr_as_addr_and_len(from, func, instructions);

                // to chars
                let (v_to, v_to_len) = self.emit_expr_as_addr_and_len(to, func, instructions);

                // before/after initial
                let (v_before, v_before_len) = self.emit_optional_expr(before_initial, func, instructions);
                let (v_after, v_after_len) = self.emit_optional_expr(after_initial, func, instructions);

                instructions.push(MirInst::CallRuntime {
                    dest: None,
                    func: "cobolrt_inspect_converting".to_string(),
                    args: vec![
                        v_target_addr, v_target_len,
                        v_from, v_from_len,
                        v_to, v_to_len,
                        v_before, v_before_len,
                        v_after, v_after_len,
                    ],
                });
            }
        }
    }

    /// Lower UNSTRING statement.
    fn lower_unstring(
        &self,
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
        instructions.push(MirInst::GlobalAddr { dest: v_src_addr, name: source_name });
        let v_src_len = func.new_value();
        instructions.push(MirInst::Const { dest: v_src_len, value: MirConst::Int(source_size as i64) });

        // First delimiter (simplified: use first delimiter only)
        let (v_delim, v_delim_len, v_all_delim) = if !delimiters.is_empty() {
            let d = &delimiters[0];
            let (addr, len) = self.emit_expr_as_addr_and_len(&d.value, func, instructions);
            let all_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: all_val,
                value: MirConst::Int(if d.all { 1 } else { 0 }),
            });
            (addr, len, all_val)
        } else {
            let null = func.new_value();
            instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
            let zero = func.new_value();
            instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
            let zero2 = func.new_value();
            instructions.push(MirInst::Const { dest: zero2, value: MirConst::Int(0) });
            (null, zero, zero2)
        };

        // Build target arrays: we need to emit pointer-to-array-of-pointers and
        // pointer-to-array-of-lengths. For MIR/codegen simplicity, we'll call
        // cobolrt_unstring with individual targets in a loop instead.
        // For the simplified approach, use individual calls to a simpler function
        // or pass the arrays. Since our C shim handles arrays, we'll emit multiple
        // calls to a per-field version.

        // Emit per-target calls using cobolrt_unstring_into for each target field.
        // Actually, let's use the full cobolrt_unstring approach. Build the arrays as
        // globals would be complex, so instead we'll call a simplified per-field
        // unstring runtime.

        // Simplified approach: call cobolrt_unstring for each target sequentially
        // by using pointer tracking
        for tgt in targets {
            let (tgt_name, tgt_size) = match self.resolve_data_ref(&tgt.target) {
                Some(info) => info,
                None => continue,
            };

            let v_tgt_addr = func.new_value();
            instructions.push(MirInst::GlobalAddr { dest: v_tgt_addr, name: tgt_name });
            let v_tgt_len = func.new_value();
            instructions.push(MirInst::Const { dest: v_tgt_len, value: MirConst::Int(tgt_size as i64) });

            // Pointer field
            let (v_ptr, v_ptr_len) = if let Some(p) = pointer {
                match self.resolve_data_ref(p) {
                    Some((pname, psize)) => {
                        let pa = func.new_value();
                        instructions.push(MirInst::GlobalAddr { dest: pa, name: pname });
                        let pl = func.new_value();
                        instructions.push(MirInst::Const { dest: pl, value: MirConst::Int(psize as i64) });
                        (pa, pl)
                    }
                    None => {
                        let null = func.new_value();
                        instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
                        let zero = func.new_value();
                        instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
                        (null, zero)
                    }
                }
            } else {
                let null = func.new_value();
                instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
                let zero = func.new_value();
                instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
                (null, zero)
            };

            // Tally field
            let (v_tally, v_tally_len) = if let Some(t) = tallying {
                match self.resolve_data_ref(t) {
                    Some((tname, tsize)) => {
                        let ta = func.new_value();
                        instructions.push(MirInst::GlobalAddr { dest: ta, name: tname });
                        let tl = func.new_value();
                        instructions.push(MirInst::Const { dest: tl, value: MirConst::Int(tsize as i64) });
                        (ta, tl)
                    }
                    None => {
                        let null = func.new_value();
                        instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
                        let zero = func.new_value();
                        instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
                        (null, zero)
                    }
                }
            } else {
                let null = func.new_value();
                instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
                let zero = func.new_value();
                instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
                (null, zero)
            };

            // num_targets = 1 for per-field call
            let v_num = func.new_value();
            instructions.push(MirInst::Const { dest: v_num, value: MirConst::Int(1) });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_unstring_field".to_string(),
                args: vec![
                    v_src_addr, v_src_len,
                    v_delim, v_delim_len, v_all_delim,
                    v_tgt_addr, v_tgt_len,
                    v_ptr, v_ptr_len,
                    v_tally, v_tally_len,
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
                instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
                let zero = func.new_value();
                instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
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
                instructions.push(MirInst::Const { dest: addr, value: MirConst::Str(s.clone()) });
                let len = func.new_value();
                instructions.push(MirInst::Const { dest: len, value: MirConst::Int(s.len() as i64) });
                (addr, len)
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                let s = n.to_string();
                let addr = func.new_value();
                instructions.push(MirInst::Const { dest: addr, value: MirConst::Str(s.clone()) });
                let len = func.new_value();
                instructions.push(MirInst::Const { dest: len, value: MirConst::Int(s.len() as i64) });
                (addr, len)
            }
            cobol_hir::HirExpr::DataRef(dr) => {
                match self.resolve_data_ref(dr) {
                    Some((name, size)) => {
                        let addr = func.new_value();
                        instructions.push(MirInst::GlobalAddr { dest: addr, name });
                        let len = func.new_value();
                        instructions.push(MirInst::Const { dest: len, value: MirConst::Int(size as i64) });
                        (addr, len)
                    }
                    None => {
                        let null = func.new_value();
                        instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
                        let zero = func.new_value();
                        instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
                        (null, zero)
                    }
                }
            }
            _ => {
                let null = func.new_value();
                instructions.push(MirInst::Const { dest: null, value: MirConst::Int(0) });
                let zero = func.new_value();
                instructions.push(MirInst::Const { dest: zero, value: MirConst::Int(0) });
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
            let (target_name, target_size) = match self.resolve_data_ref(target_ref) {
                Some(v) => v,
                None => continue,
            };

            for op in operands {
                if let Some((op_addr, op_size)) = self.emit_expr_addr(op, func, instructions) {
                    let op_len = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: op_len,
                        value: MirConst::Int(op_size as i64),
                    });

                    let target_addr1 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: target_addr1,
                        name: target_name.clone(),
                    });
                    let target_len1 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: target_len1,
                        value: MirConst::Int(target_size as i64),
                    });

                    let target_addr2 = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: target_addr2,
                        name: target_name.clone(),
                    });
                    let target_len2 = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: target_len2,
                        value: MirConst::Int(target_size as i64),
                    });

                    instructions.push(MirInst::CallRuntime {
                        dest: None,
                        func: "cobolrt_add_numeric".to_string(),
                        args: vec![op_addr, op_len, target_addr1, target_len1, target_addr2, target_len2],
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
            let (target_addr, target_size) = if let Some(info) = self.linkage_map.get(&target_name_str) {
                (info.param_value, info.byte_size)
            } else if let Some((ref gname, size)) = self.resolve_data_ref(target_ref) {
                let addr = func.new_value();
                instructions.push(MirInst::GlobalAddr { dest: addr, name: gname.clone() });
                (addr, size)
            } else {
                continue;
            };

            // Get target PIC info for proper numeric formatting
            let (_target_is_numeric, target_scale, target_dot_pos, _target_is_signed) =
                self.data_item_pic_info(&target_name_str).unwrap_or((false, 0, -1, false));

            // Emit: cobolrt_move_numeric(src, src_len, src_scale, src_dot_pos,
            //                           dest, dest_len, dest_scale, dest_dot_pos, rounded)
            // The source is a temporary display-format buffer from arithmetic,
            // so src_scale=0 and src_dot_pos=-1.
            let v_result_len = func.new_value();
            instructions.push(MirInst::Const { dest: v_result_len, value: MirConst::Int(result_size as i64) });

            let v_src_scale = func.new_value();
            instructions.push(MirInst::Const { dest: v_src_scale, value: MirConst::Int(0) });

            let v_src_dot_pos = func.new_value();
            instructions.push(MirInst::Const { dest: v_src_dot_pos, value: MirConst::Int(-1) });

            let v_target_len = func.new_value();
            instructions.push(MirInst::Const { dest: v_target_len, value: MirConst::Int(target_size as i64) });

            let v_target_scale = func.new_value();
            instructions.push(MirInst::Const { dest: v_target_scale, value: MirConst::Int(target_scale as i64) });

            let v_dot_pos = func.new_value();
            instructions.push(MirInst::Const { dest: v_dot_pos, value: MirConst::Int(target_dot_pos as i64) });

            let v_rounded = func.new_value();
            instructions.push(MirInst::Const { dest: v_rounded, value: MirConst::Int(if rounded { 1 } else { 0 }) });

            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_move_numeric".to_string(),
                args: vec![result_addr, v_result_len, v_src_scale, v_src_dot_pos,
                           target_addr, v_target_len, v_target_scale, v_dot_pos, v_rounded],
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
                // Create a display-format string constant for the integer
                let s = format!("{}", n.unsigned_abs());
                let size = s.len().max(1) as u32;
                let str_val = func.new_value();
                instructions.push(MirInst::Const {
                    dest: str_val,
                    value: MirConst::Str(format!("{:0>width$}", n.unsigned_abs(), width = size as usize)),
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
            cobol_hir::HirExpr::UnaryOp { op: cobol_hir::UnaryOp::Neg, operand } => {
                // Negate: compute the operand, then call cobolrt_negate_numeric
                let (op_addr, op_size) = self.emit_arithmetic_expr(operand, func, instructions)?;
                let op_len = func.new_value();
                instructions.push(MirInst::Const { dest: op_len, value: MirConst::Int(op_size as i64) });

                // The result is written into a temporary buffer; the runtime
                // returns a pointer to it. We use a generous result size.
                let result_size = op_size + 1; // extra byte for potential sign
                let v_result_len = func.new_value();
                instructions.push(MirInst::Const { dest: v_result_len, value: MirConst::Int(result_size as i64) });

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
                let (right_addr, right_size) = self.emit_arithmetic_expr(right, func, instructions)?;

                let left_len = func.new_value();
                instructions.push(MirInst::Const { dest: left_len, value: MirConst::Int(left_size as i64) });

                let right_len = func.new_value();
                instructions.push(MirInst::Const { dest: right_len, value: MirConst::Int(right_size as i64) });

                // Choose the runtime function based on the operator
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
                    cobol_hir::BinaryOp::Mul => {
                        left_size + right_size
                    }
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
            _ => None,
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
        if matches!(right, cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(_))) {
            return true;
        }
        // If left side is a string literal, it's alphanumeric
        if matches!(left, cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::String_(_))) {
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
                            let cmp_func = if is_alpha {
                                "cobolrt_compare_alphanumeric"
                            } else {
                                "cobolrt_compare_numeric"
                            };
                            instructions.push(MirInst::CallRuntime {
                                dest: Some(cmp_result),
                                func: cmp_func.to_string(),
                                args: vec![l_addr, l_len, r_addr, r_len],
                            });

                            let zero_val = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: zero_val,
                                value: MirConst::Int(0),
                            });

                            let bool_result = func.new_value();
                            let cmp_inst = match op {
                                cobol_hir::BinaryOp::Eq => MirInst::ICmpEq {
                                    dest: bool_result, left: cmp_result, right: zero_val,
                                },
                                cobol_hir::BinaryOp::Ne => MirInst::ICmpNe {
                                    dest: bool_result, left: cmp_result, right: zero_val,
                                },
                                cobol_hir::BinaryOp::Lt => MirInst::ICmpLt {
                                    dest: bool_result, left: cmp_result, right: zero_val,
                                },
                                cobol_hir::BinaryOp::Gt => MirInst::ICmpGt {
                                    dest: bool_result, left: cmp_result, right: zero_val,
                                },
                                cobol_hir::BinaryOp::Le => MirInst::ICmpLe {
                                    dest: bool_result, left: cmp_result, right: zero_val,
                                },
                                cobol_hir::BinaryOp::Ge => MirInst::ICmpGe {
                                    dest: bool_result, left: cmp_result, right: zero_val,
                                },
                                _ => MirInst::ICmpEq {
                                    dest: bool_result, left: cmp_result, right: zero_val,
                                },
                            };
                            instructions.push(cmp_inst);
                            bool_result
                        }
                        _ => {
                            let v = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v, value: MirConst::Bool(true),
                            });
                            v
                        }
                    }
                }
                cobol_hir::HirCondition::ConditionName(data_ref) => {
                    // 88-level condition name: resolve to comparison against parent
                    let cond_name = self.interner.resolve(data_ref.name).to_string();
                    if let Some((parent_id, ref cond_value)) = self.hir.condition_names.get(&cond_name) {
                        let parent_item = &self.hir.data_items[parent_id.into_raw()];
                        let parent_name = parent_item.name
                            .map(|n| self.interner.resolve(n).to_string())
                            .unwrap_or_default();
                        let parent_size = parent_item.storage.byte_size;

                        if self.global_map.contains_key(&parent_name) {
                            let parent_addr = func.new_value();
                            instructions.push(MirInst::GlobalAddr {
                                dest: parent_addr,
                                name: parent_name.clone(),
                            });
                            let parent_len = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: parent_len,
                                value: MirConst::Int(parent_size as i64),
                            });

                            let cond_str = match cond_value {
                                Some(cobol_hir::InitialValue::Numeric(n, _)) => {
                                    format!("{:0>width$}", n.unsigned_abs(), width = parent_size as usize)
                                }
                                Some(cobol_hir::InitialValue::String_(s)) => s.clone(),
                                Some(cobol_hir::InitialValue::Zero) => {
                                    "0".repeat(parent_size as usize)
                                }
                                Some(cobol_hir::InitialValue::Space) => {
                                    " ".repeat(parent_size as usize)
                                }
                                _ => "0".repeat(parent_size as usize),
                            };

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
                                args: vec![parent_addr, parent_len, cond_val, cond_len],
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
                        } else {
                            let v = func.new_value();
                            instructions.push(MirInst::Const {
                                dest: v, value: MirConst::Bool(true),
                            });
                            v
                        }
                    } else {
                        let v = func.new_value();
                        instructions.push(MirInst::Const {
                            dest: v, value: MirConst::Bool(true),
                        });
                        v
                    }
                }
                cobol_hir::HirCondition::And(left, right) => {
                    let left_cond = cobol_hir::HirExpr::Condition(Box::new(left.as_ref().clone()));
                    let right_cond = cobol_hir::HirExpr::Condition(Box::new(right.as_ref().clone()));
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
                    let right_cond = cobol_hir::HirExpr::Condition(Box::new(right.as_ref().clone()));
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
                    let inner_cond = cobol_hir::HirExpr::Condition(Box::new(inner.as_ref().clone()));
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
                _ => {
                    let v = func.new_value();
                    instructions.push(MirInst::Const {
                        dest: v, value: MirConst::Bool(true),
                    });
                    v
                }
            },
            _ => {
                let v = func.new_value();
                instructions.push(MirInst::Const {
                    dest: v, value: MirConst::Bool(true),
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
            cobol_hir::HirExpr::DataRef(_) => {
                self.emit_expr_addr(expr, func, instructions)
            }
            cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) => {
                // Create a string constant in display format for comparison
                let s = format!("{}", n.unsigned_abs());
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
                    cobol_hir::FigurativeConstant::HighValue => String::from_utf8(vec![0xFF]).unwrap_or_default(),
                    cobol_hir::FigurativeConstant::LowValue => String::from_utf8(vec![0x00]).unwrap_or_default(),
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
    /// WHEN OTHER (indicated by empty conditions) is the default fallthrough.
    /// After any body block, jump to the merge block.
    fn lower_evaluate(
        &mut self,
        _subject: &cobol_hir::HirExpr,
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
            // The condition in HIR is already a comparison (subject == value)
            let cond_val = if let Some(cond) = when_clause.conditions.first() {
                self.eval_condition(cond, func, instructions)
            } else {
                // Should not happen for regular whens, but handle gracefully
                let v = func.new_value();
                instructions.push(MirInst::Const { dest: v, value: MirConst::Bool(false) });
                v
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
            match action {
                cobol_hir::SetAction::To(expr) => {
                    let val = self.eval_set_expr(expr, func, instructions);
                    instructions.push(MirInst::Store {
                        addr: target_addr,
                        value: val,
                    });
                }
                cobol_hir::SetAction::UpBy(expr) => {
                    let add_val = self.eval_set_expr(expr, func, instructions);
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
                        right: add_val,
                    });
                    instructions.push(MirInst::Store {
                        addr: target_addr,
                        value: result,
                    });
                }
                cobol_hir::SetAction::DownBy(expr) => {
                    let sub_val = self.eval_set_expr(expr, func, instructions);
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
                        right: sub_val,
                    });
                    instructions.push(MirInst::Store {
                        addr: target_addr,
                        value: result,
                    });
                }
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
                                dest: result, left: current, right: delta,
                            });
                        }
                        cobol_hir::SetAction::DownBy(_) => {
                            instructions.push(MirInst::ISub {
                                dest: result, left: current, right: delta,
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
                if self.index_info.contains_key(&name) {
                    if self.global_map.contains_key(&name) {
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
                }
                if let Some(&g_idx) = self.global_map.get(&name) {
                    let size = match &self.module.globals[g_idx].ty {
                        MirType::Bytes(n) => *n,
                        _ => 1,
                    };
                    let addr = func.new_value();
                    instructions.push(MirInst::GlobalAddr {
                        dest: addr,
                        name,
                    });
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
        let mut check_insts = Vec::new();
        let idx_addr = func.new_value();
        check_insts.push(MirInst::GlobalAddr {
            dest: idx_addr,
            name: index_name.clone(),
        });
        let idx_val = func.new_value();
        check_insts.push(MirInst::Load {
            dest: idx_val,
            addr: idx_addr,
            ty: MirType::I64,
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
        let one = func.new_value();
        inc_insts.push(MirInst::Const {
            dest: one,
            value: MirConst::Int(1),
        });
        let new_val = func.new_value();
        inc_insts.push(MirInst::IAdd {
            dest: new_val,
            left: inc_val,
            right: one,
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
                self.lower_perform_out_of_line(*target, thru.as_ref().copied(),
                    func, current_block_id, instructions);
            }
            cobol_hir::PerformType::Times { target, thru, times } => {
                self.lower_perform_times(*target, thru.as_ref().copied(), times,
                    func, current_block_id, instructions);
            }
            cobol_hir::PerformType::Until { target, thru, condition, test_before } => {
                self.lower_perform_until(*target, thru.as_ref().copied(), condition, *test_before,
                    func, current_block_id, instructions);
            }
            cobol_hir::PerformType::Varying { varying, inline_body, .. } => {
                self.lower_perform_varying(varying, inline_body.as_deref(),
                    func, current_block_id, instructions);
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

    /// Emit MIR to jump to a paragraph block (out-of-line perform body).
    ///
    /// Pattern: push return address → goto paragraph → (paragraph ends with
    /// PerformReturn) → continuation block picks up here.
    ///
    /// When `thru` is Some, all paragraphs from `target` through `thru` should
    /// execute sequentially.  For now, we execute just the target paragraph
    /// (thru range refinement is left for a follow-up).
    fn lower_perform_out_of_line(
        &self,
        target: cobol_intern::Name,
        _thru: Option<cobol_intern::Name>,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let target_name = self.interner.resolve(target).to_string();
        if let Some(&target_block) = self.paragraph_block_map.get(&target_name) {
            let continue_block = func.new_block();

            // Push the return (continuation) block onto the perform stack
            instructions.push(MirInst::PerformPush {
                return_block: continue_block,
            });

            // Finalize current block with a Goto to the target paragraph
            let current_insts = std::mem::take(instructions);
            func.blocks.push(BasicBlock {
                id: *current_block_id,
                params: Vec::new(),
                instructions: current_insts,
                terminator: Terminator::Goto(target_block),
            });

            // Continue after the performed paragraph returns
            *current_block_id = continue_block;
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
        _thru: Option<cobol_intern::Name>,
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
            let offset = self.module.globals.iter().map(|g| g.offset + 8).max().unwrap_or(0);
            self.global_map.insert(counter_name.clone(), self.module.globals.len());
            self.module.globals.push(MirGlobal {
                name: counter_name.clone(),
                ty: MirType::Bytes(8),
                initial_value: None,
                offset,
                redefines: None,
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

        // Find and inline the target paragraph's statements
        let target_name = self.interner.resolve(target).to_string();
        let target_stmts: Vec<cobol_hir::HirStatement> = self
            .hir
            .paragraphs
            .iter()
            .find(|p| self.interner.resolve(p.name) == target_name)
            .map(|p| p.statements.clone())
            .unwrap_or_default();
        for stmt in &target_stmts {
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
    fn lower_perform_until(
        &self,
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
                    then_block: exit_block_id,  // condition met -> exit
                    else_block: body_block_id,  // condition not met -> body
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
                    then_block: exit_block_id,  // condition met -> exit
                    else_block: body_block_id,  // condition not met -> loop again
                },
            });

            *current_block_id = exit_block_id;
        }
    }

    /// Lower PERFORM VARYING with inline body into a loop.
    fn lower_perform_varying(
        &mut self,
        varying: &cobol_hir::VaryingClause,
        inline_body: Option<&[cobol_hir::HirStatement]>,
        func: &mut MirFunction,
        current_block_id: &mut BlockId,
        instructions: &mut Vec<MirInst>,
    ) {
        let cond_block_id = func.new_block();
        let body_block_id = func.new_block();
        let exit_block_id = func.new_block();

        let var_name = self.interner.resolve(varying.identifier.name).to_string();
        let var_size = self.global_map.get(&var_name)
            .and_then(|&idx| {
                let g = &self.module.globals[idx];
                match &g.ty { MirType::Bytes(n) => Some(*n), _ => Some(1) }
            })
            .unwrap_or(1);

        // Initialize loop variable with FROM value
        if let cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(n)) = &varying.from {
            let init_str = format!("{:0>width$}", n, width = var_size as usize);
            let addr = func.new_value();
            instructions.push(MirInst::GlobalAddr {
                dest: addr, name: var_name.clone(),
            });
            let init_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: init_val, value: MirConst::Str(init_str),
            });
            let len_val = func.new_value();
            instructions.push(MirInst::Const {
                dest: len_val, value: MirConst::Int(var_size as i64),
            });
            // cobolrt_move_alphanumeric(src, src_len, dest, dest_len)
            instructions.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_move_alphanumeric".to_string(),
                args: vec![init_val, len_val, addr, len_val],
            });
        }

        // Finalize current block → goto cond
        let current_insts = std::mem::take(instructions);
        func.blocks.push(BasicBlock {
            id: *current_block_id,
            params: Vec::new(),
            instructions: current_insts,
            terminator: Terminator::Goto(cond_block_id),
        });

        // Condition block: evaluate UNTIL, branch to exit or body
        let mut cond_insts = Vec::new();
        let cond_val = self.eval_condition(&varying.until, func, &mut cond_insts);
        func.blocks.push(BasicBlock {
            id: cond_block_id,
            params: Vec::new(),
            instructions: cond_insts,
            terminator: Terminator::Branch {
                cond: cond_val,
                then_block: exit_block_id,
                else_block: body_block_id,
            },
        });

        // Body block: inline statements + increment + goto cond
        let mut body_insts = Vec::new();
        let mut body_bid = body_block_id;
        if let Some(stmts) = inline_body {
            for stmt in stmts {
                self.lower_statement_to_blocks(stmt, func, &mut body_bid, &mut body_insts);
            }
        }

        // Increment: add BY value to loop variable
        if let cobol_hir::HirExpr::Literal(cobol_hir::LiteralValue::Integer(by_val)) = &varying.by {
            let by_str = format!("{:0>width$}", by_val, width = var_size as usize);
            let by_addr = func.new_value();
            body_insts.push(MirInst::Const {
                dest: by_addr, value: MirConst::Str(by_str),
            });
            let by_len = func.new_value();
            body_insts.push(MirInst::Const {
                dest: by_len, value: MirConst::Int(var_size as i64),
            });
            let var_addr = func.new_value();
            body_insts.push(MirInst::GlobalAddr {
                dest: var_addr, name: var_name.clone(),
            });
            let var_len = func.new_value();
            body_insts.push(MirInst::Const {
                dest: var_len, value: MirConst::Int(var_size as i64),
            });
            let var_addr2 = func.new_value();
            body_insts.push(MirInst::GlobalAddr {
                dest: var_addr2, name: var_name.clone(),
            });
            let var_len2 = func.new_value();
            body_insts.push(MirInst::Const {
                dest: var_len2, value: MirConst::Int(var_size as i64),
            });
            body_insts.push(MirInst::CallRuntime {
                dest: None,
                func: "cobolrt_add_numeric".to_string(),
                args: vec![by_addr, by_len, var_addr, var_len, var_addr2, var_len2],
            });
        }

        func.blocks.push(BasicBlock {
            id: body_bid,
            params: Vec::new(),
            instructions: body_insts,
            terminator: Terminator::Goto(cond_block_id),
        });

        *current_block_id = exit_block_id;
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
            instructions.push(MirInst::GlobalAddr { dest: addr, name: gname.clone() });
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
            MirType::Decimal { digits: 9, scale: 2 },
            MirType::Decimal { digits: 9, scale: 2 },
        );
        assert_ne!(
            MirType::Decimal { digits: 9, scale: 2 },
            MirType::Decimal { digits: 9, scale: 3 },
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

        if let MirInst::DecimalAdd { dest, result_scale, rounded, .. } = &inst {
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
            record_size: 80,
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
        if let Terminator::Branch { cond: c, then_block, else_block } = &term {
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
                    ty: MirType::Decimal { digits: 9, scale: 2 },
                    initial_value: Some(MirConst::Int(0)),
                    offset: 0,
                    redefines: None,
                },
                MirGlobal {
                    name: "WS-NAME".to_string(),
                    ty: MirType::Bytes(30),
                    initial_value: Some(MirConst::Str("SPACES".to_string())),
                    offset: 8,
                    redefines: None,
                },
            ],
            file_descriptors: vec![MirFileDescriptor {
                name: "EMPLOYEE-FILE".to_string(),
                organization: cobol_hir::FileOrganization::Indexed,
                record_size: 200,
            }],
        };

        assert_eq!(module.name, "PAYROLL");
        assert_eq!(module.globals.len(), 2);
        assert_eq!(module.file_descriptors.len(), 1);
        assert_eq!(
            module.file_descriptors[0].organization,
            cobol_hir::FileOrganization::Indexed
        );
    }
}
