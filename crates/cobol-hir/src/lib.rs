//! High-level IR for COBOL: semantic analysis, PIC types, data layout,
//! and name resolution.
//!
//! This crate lowers the untyped AST from [`cobol_ast`] into a fully typed,
//! semantically validated intermediate representation. Key responsibilities:
//!
//! - Parsing PIC strings into [`PictureType`] descriptors
//! - Computing storage sizes and byte offsets for every data item
//! - Resolving qualified data references
//! - Lowering procedure division statements into [`HirStatement`] trees

// ---------------------------------------------------------------------------
// PIC Type System
// ---------------------------------------------------------------------------

/// The broad category of a PIC string, determining what characters may be
/// stored and which operations are valid.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PictureCategory {
    Alphabetic,
    Alphanumeric,
    AlphanumericEdited,
    Numeric,
    NumericEdited,
    ExternalFloat,
    Dbcs,
    National,
}

/// Where the sign indicator lives relative to the data.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignPosition {
    Leading,
    Trailing,
    LeadingSeparate,
    TrailingSeparate,
    None,
}

/// Fully parsed representation of a COBOL PIC clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PictureType {
    /// The broad category deduced from the PIC string.
    pub category: PictureCategory,
    /// Total number of character or digit positions.
    pub size: u32,
    /// Decimal scale: positive = digits after V, negative = P positions.
    pub scale: i32,
    /// Where the sign is stored.
    pub sign: SignPosition,
    /// Original PIC string, retained for diagnostics.
    pub pic_string: String,
}

/// How a data item is physically stored.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsageType {
    Display,
    Comp,
    Comp1,
    Comp2,
    Comp3,
    Comp4,
    Comp5,
    Binary,
    PackedDecimal,
    Index,
    Pointer,
    FunctionPointer,
}

/// Low-level encoding of data bytes in storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataEncoding {
    Display,
    Binary,
    PackedDecimal,
    FloatSingle,
    FloatDouble,
    Index,
    Pointer,
}

/// Complete physical storage descriptor for a data item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StorageDescriptor {
    /// Exact storage size in bytes.
    pub byte_size: u32,
    /// Low-level encoding.
    pub encoding: DataEncoding,
    /// The parsed PIC clause, if any (group items have `None`).
    pub picture: Option<PictureType>,
    /// USAGE clause value.
    pub usage: UsageType,
}

// ---------------------------------------------------------------------------
// Data Item Hierarchy
// ---------------------------------------------------------------------------

/// Opaque handle to a data item inside a [`la_arena::Arena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DataItemId(la_arena::Idx<DataItemData>);

impl DataItemId {
    /// Creates a `DataItemId` from an arena index.
    pub fn from_raw(idx: la_arena::Idx<DataItemData>) -> Self {
        Self(idx)
    }

    /// Returns the underlying arena index.
    pub fn into_raw(self) -> la_arena::Idx<DataItemData> {
        self.0
    }
}

/// The full definition of a single data item (01-49, 66, 77, 88).
#[derive(Debug, Clone, PartialEq)]
pub struct DataItemData {
    /// Level number (01-49, 66, 77, 88).
    pub level: u8,
    /// Data name, or `None` for FILLER.
    pub name: Option<cobol_intern::Name>,
    /// Physical storage descriptor.
    pub storage: StorageDescriptor,
    /// Parent data item (group that contains this item).
    pub parent: Option<DataItemId>,
    /// Children of this group item (empty for elementary items).
    pub children: Vec<DataItemId>,
    /// Byte offset from the start of the containing record.
    pub offset: u32,
    /// If this item REDEFINES another, the target.
    pub redefines: Option<DataItemId>,
    /// OCCURS clause, if present.
    pub occurs: Option<OccursClause>,
    /// VALUE clause, if present.
    pub value: Option<InitialValue>,
    /// Source location.
    pub span: cobol_span::Span,
    /// Whether this is a group item (has subordinate items).
    pub is_group: bool,
}

/// OCCURS clause controlling table (array) dimensions.
#[derive(Debug, Clone, PartialEq)]
pub struct OccursClause {
    /// Minimum number of occurrences (for OCCURS DEPENDING ON).
    pub min: u32,
    /// Maximum number of occurrences.
    pub max: u32,
    /// DEPENDING ON identifier for variable-length tables.
    pub depending_on: Option<cobol_intern::Name>,
    /// KEY IS clauses for SEARCH.
    pub keys: Vec<SortKey>,
    /// INDEXED BY names.
    pub indexed_by: Vec<cobol_intern::Name>,
}

/// A single ASCENDING/DESCENDING KEY for OCCURS.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SortKey {
    /// The key data name.
    pub name: cobol_intern::Name,
    /// `true` for ASCENDING, `false` for DESCENDING.
    pub ascending: bool,
}

/// The initial value specified in a VALUE clause.
#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Numeric(i64, i32),
    String_(String),
    Figurative(FigurativeConstant),
    Zero,
    Space,
    HighValue,
    LowValue,
    Quote,
    All(String),
}

/// COBOL figurative constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FigurativeConstant {
    Zero,
    Space,
    HighValue,
    LowValue,
    Quote,
    All,
}

// ---------------------------------------------------------------------------
// HIR Module (top-level container)
// ---------------------------------------------------------------------------

/// The top-level container for a fully lowered COBOL program.
#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    /// The PROGRAM-ID.
    pub program_name: cobol_intern::Name,
    /// Arena that owns all data items.
    pub data_items: la_arena::Arena<DataItemData>,
    /// Top-level items in WORKING-STORAGE SECTION.
    pub working_storage: Vec<DataItemId>,
    /// Top-level items in LINKAGE SECTION.
    pub linkage_items: Vec<DataItemId>,
    /// PROCEDURE DIVISION USING parameter names.
    pub using_params: Vec<cobol_intern::Name>,
    /// File descriptions from FILE SECTION.
    pub file_items: Vec<FileDescriptor>,
    /// Paragraphs in the PROCEDURE DIVISION.
    pub paragraphs: Vec<HirParagraph>,
    /// Sections in the PROCEDURE DIVISION.
    pub sections: Vec<HirSection>,
    /// Diagnostics produced during lowering.
    pub diagnostics: Vec<HirDiagnostic>,
    /// Maps 88-level condition names to (parent DataItemId, condition value).
    pub condition_names: std::collections::HashMap<String, (DataItemId, Option<InitialValue>)>,
}

/// A FILE SECTION FD entry.
#[derive(Debug, Clone, PartialEq)]
pub struct FileDescriptor {
    /// The file name (SELECT name).
    pub name: cobol_intern::Name,
    /// Record-level data items belonging to this file.
    pub record_items: Vec<DataItemId>,
    /// File organisation.
    pub organization: FileOrganization,
    /// Access mode.
    pub access_mode: AccessMode,
    /// ASSIGN TO path (e.g. "SMOKE-FILE.DAT").
    pub assign_to: String,
    /// Source location.
    pub span: cobol_span::Span,
}

/// File organisation specified in SELECT ... ORGANIZATION IS ...
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileOrganization {
    Sequential,
    Relative,
    Indexed,
    LineSequential,
}

/// Access mode specified in SELECT ... ACCESS MODE IS ...
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessMode {
    Sequential,
    Random,
    Dynamic,
}

// ---------------------------------------------------------------------------
// Procedure Division: Paragraphs and Sections
// ---------------------------------------------------------------------------

/// A single paragraph in the PROCEDURE DIVISION.
#[derive(Debug, Clone, PartialEq)]
pub struct HirParagraph {
    /// Paragraph name.
    pub name: cobol_intern::Name,
    /// Statements in this paragraph.
    pub statements: Vec<HirStatement>,
    /// Source location.
    pub span: cobol_span::Span,
}

/// A section (group of paragraphs) in the PROCEDURE DIVISION.
#[derive(Debug, Clone, PartialEq)]
pub struct HirSection {
    /// Section name.
    pub name: cobol_intern::Name,
    /// Indices into [`HirModule::paragraphs`].
    pub paragraphs: Vec<usize>,
    /// Source location.
    pub span: cobol_span::Span,
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

/// A single COBOL statement, lowered to HIR form.
#[derive(Debug, Clone, PartialEq)]
pub enum HirStatement {
    Display {
        args: Vec<HirExpr>,
        no_advancing: bool,
    },
    Move {
        from: HirExpr,
        to: Vec<HirDataRef>,
    },
    Add {
        operands: Vec<HirExpr>,
        to: Vec<HirDataRef>,
        giving: Option<Vec<HirDataRef>>,
        on_size_error: Option<Vec<HirStatement>>,
        rounded: bool,
    },
    Subtract {
        operands: Vec<HirExpr>,
        from: Vec<HirDataRef>,
        giving: Option<Vec<HirDataRef>>,
        on_size_error: Option<Vec<HirStatement>>,
        rounded: bool,
    },
    Multiply {
        operand1: HirExpr,
        by: HirExpr,
        giving: Option<Vec<HirDataRef>>,
        on_size_error: Option<Vec<HirStatement>>,
        rounded: bool,
    },
    Divide {
        operand1: HirExpr,
        into_or_by: HirExpr,
        giving: Option<Vec<HirDataRef>>,
        remainder: Option<HirDataRef>,
        on_size_error: Option<Vec<HirStatement>>,
        rounded: bool,
    },
    Compute {
        targets: Vec<HirDataRef>,
        expr: HirExpr,
        on_size_error: Option<Vec<HirStatement>>,
        rounded: bool,
    },
    If {
        condition: HirExpr,
        then_branch: Vec<HirStatement>,
        else_branch: Option<Vec<HirStatement>>,
    },
    Evaluate {
        subject: HirExpr,
        whens: Vec<WhenClause>,
    },
    Perform(PerformType),
    GoTo {
        target: cobol_intern::Name,
    },
    Call {
        program: HirExpr,
        using: Vec<CallArg>,
        returning: Option<HirDataRef>,
    },
    StopRun,
    GoBack,
    Accept {
        target: HirDataRef,
        source: AcceptSource,
    },
    StringStmt {
        sources: Vec<HirExpr>,
        into: HirDataRef,
        pointer: Option<HirDataRef>,
    },
    Continue,
    ExitParagraph,
    ExitSection,
    ExitProgram,
    Open {
        file: cobol_intern::Name,
        mode: OpenMode,
    },
    Close {
        file: cobol_intern::Name,
    },
    Write {
        record: cobol_intern::Name,
    },
    Read {
        file: cobol_intern::Name,
        into: Option<cobol_intern::Name>,
        at_end: Vec<HirStatement>,
    },
    Inspect {
        target: HirDataRef,
        inspect_type: InspectType,
    },
    Unstring {
        source: HirDataRef,
        delimiters: Vec<UnstringDelimiter>,
        targets: Vec<UnstringTarget>,
        pointer: Option<HirDataRef>,
        tallying: Option<HirDataRef>,
    },
    Initialize {
        targets: Vec<HirDataRef>,
        /// Optional REPLACING clause: (category, replacement_value) pairs.
        /// Categories: "NUMERIC", "ALPHANUMERIC", "ALPHABETIC", etc.
        replacing: Vec<(String, HirExpr)>,
    },
    Search {
        table: HirDataRef,
        at_end: Vec<HirStatement>,
        whens: Vec<SearchWhen>,
    },
    Set {
        target: HirDataRef,
        action: SetAction,
    },
}

/// A WHEN clause inside a SEARCH statement.
#[derive(Debug, Clone, PartialEq)]
pub struct SearchWhen {
    /// The condition for this WHEN branch.
    pub condition: HirExpr,
    /// Statements executed when the condition matches.
    pub body: Vec<HirStatement>,
}

/// The action to take on the target index in a SET statement.
#[derive(Debug, Clone, PartialEq)]
pub enum SetAction {
    /// SET target TO value
    To(HirExpr),
    /// SET target UP BY value
    UpBy(HirExpr),
    /// SET target DOWN BY value
    DownBy(HirExpr),
}

/// The different forms of the INSPECT statement.
#[derive(Debug, Clone, PartialEq)]
pub enum InspectType {
    Tallying {
        tally_var: HirDataRef,
        mode: InspectTallyMode,
        search: Option<HirExpr>,
        before_initial: Option<HirExpr>,
        after_initial: Option<HirExpr>,
    },
    Replacing {
        mode: InspectReplaceMode,
        search: Option<HirExpr>,
        replacement: HirExpr,
        before_initial: Option<HirExpr>,
        after_initial: Option<HirExpr>,
    },
    Converting {
        from: HirExpr,
        to: HirExpr,
        before_initial: Option<HirExpr>,
        after_initial: Option<HirExpr>,
    },
}

/// Mode for INSPECT TALLYING.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InspectTallyMode {
    Characters,
    All,
    Leading,
}

/// Mode for INSPECT REPLACING.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InspectReplaceMode {
    Characters,
    All,
    Leading,
    First,
}

/// A delimiter in an UNSTRING statement.
#[derive(Debug, Clone, PartialEq)]
pub struct UnstringDelimiter {
    pub value: HirExpr,
    pub all: bool,
}

/// A target in an UNSTRING INTO clause.
#[derive(Debug, Clone, PartialEq)]
pub struct UnstringTarget {
    pub target: HirDataRef,
    pub delimiter_in: Option<HirDataRef>,
    pub count_in: Option<HirDataRef>,
}

/// Mode for OPEN statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpenMode {
    Input,
    Output,
    IoMode,
    Extend,
}

/// Source for ACCEPT statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AcceptSource {
    /// Read from stdin (console).
    Console,
    /// ACCEPT ... FROM DATE (YYMMDD).
    Date,
    /// ACCEPT ... FROM DAY (YYDDD).
    Day,
    /// ACCEPT ... FROM TIME (HHMMSSCC).
    Time,
    /// ACCEPT ... FROM DAY-OF-WEEK (1=Mon..7=Sun).
    DayOfWeek,
}

/// A WHEN clause inside an EVALUATE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct WhenClause {
    /// Conditions for this WHEN branch.
    pub conditions: Vec<HirExpr>,
    /// Statements executed when the conditions match.
    pub statements: Vec<HirStatement>,
}

/// The different forms of the PERFORM statement.
#[derive(Debug, Clone, PartialEq)]
pub enum PerformType {
    Inline {
        statements: Vec<HirStatement>,
    },
    OutOfLine {
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
    },
    Times {
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        times: HirExpr,
    },
    /// Inline PERFORM n TIMES ... END-PERFORM (no paragraph name)
    InlineTimes {
        times: HirExpr,
        statements: Vec<HirStatement>,
    },
    Until {
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        condition: HirExpr,
        test_before: bool,
    },
    /// Inline PERFORM UNTIL condition ... END-PERFORM (no paragraph name)
    InlineUntil {
        condition: HirExpr,
        test_before: bool,
        statements: Vec<HirStatement>,
    },
    Varying {
        target: cobol_intern::Name,
        thru: Option<cobol_intern::Name>,
        varying: VaryingClause,
        /// Inline body statements (for inline PERFORM VARYING ... END-PERFORM).
        inline_body: Option<Vec<HirStatement>>,
    },
}

/// The VARYING ... FROM ... BY ... UNTIL clause of PERFORM.
#[derive(Debug, Clone, PartialEq)]
pub struct VaryingClause {
    /// The loop variable.
    pub identifier: HirDataRef,
    /// Starting value.
    pub from: HirExpr,
    /// Increment value.
    pub by: HirExpr,
    /// Termination condition.
    pub until: HirExpr,
    /// Nested AFTER clauses.
    pub after: Vec<VaryingClause>,
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

/// An expression in HIR form.
#[derive(Debug, Clone, PartialEq)]
pub enum HirExpr {
    Literal(LiteralValue),
    DataRef(Box<HirDataRef>),
    BinaryOp {
        op: BinaryOp,
        left: Box<HirExpr>,
        right: Box<HirExpr>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<HirExpr>,
    },
    FunctionCall {
        name: cobol_intern::Name,
        args: Vec<HirExpr>,
    },
    Condition(Box<HirCondition>),
}

/// A literal value.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(i64),
    Decimal(String),
    String_(String),
    Figurative(FigurativeConstant),
}

/// Binary operators for arithmetic and logical expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// A reference to a data item, possibly qualified, subscripted, and/or
/// reference-modified.
#[derive(Debug, Clone, PartialEq)]
pub struct HirDataRef {
    /// The primary data name.
    pub name: cobol_intern::Name,
    /// OF/IN qualifier chain (innermost first).
    pub qualifiers: Vec<cobol_intern::Name>,
    /// Subscript expressions (for tables).
    pub subscripts: Vec<HirExpr>,
    /// Reference modification: (offset, optional length).
    pub ref_mod: Option<(Box<HirExpr>, Option<Box<HirExpr>>)>,
    /// Resolved data item, filled in during name resolution.
    pub resolved: Option<DataItemId>,
}

// ---------------------------------------------------------------------------
// Conditions
// ---------------------------------------------------------------------------

/// A condition expression (used in IF, EVALUATE, PERFORM UNTIL, etc.).
#[derive(Debug, Clone, PartialEq)]
pub enum HirCondition {
    Comparison {
        left: HirExpr,
        op: BinaryOp,
        right: HirExpr,
    },
    ClassCheck {
        operand: HirExpr,
        class: ClassType,
    },
    SignCheck {
        operand: HirExpr,
        sign: SignCheckType,
    },
    /// An 88-level condition name check.
    ConditionName(HirDataRef),
    And(Box<HirCondition>, Box<HirCondition>),
    Or(Box<HirCondition>, Box<HirCondition>),
    Not(Box<HirCondition>),
}

/// The class types for `IF identifier IS NUMERIC/ALPHABETIC` etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassType {
    Numeric,
    Alphabetic,
    AlphabeticLower,
    AlphabeticUpper,
}

/// The sign check types for `IF identifier IS POSITIVE/NEGATIVE/ZERO`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignCheckType {
    Positive,
    Negative,
    Zero,
}

// ---------------------------------------------------------------------------
// CALL arguments
// ---------------------------------------------------------------------------

/// A single argument to a CALL statement.
#[derive(Debug, Clone, PartialEq)]
pub struct CallArg {
    /// How the argument is passed.
    pub mode: CallArgMode,
    /// The value being passed.
    pub value: HirExpr,
}

/// The passing convention for a CALL argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallArgMode {
    ByReference,
    ByContent,
    ByValue,
}

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/// A diagnostic message produced during HIR lowering.
#[derive(Debug, Clone, PartialEq)]
pub struct HirDiagnostic {
    /// Human-readable message.
    pub message: String,
    /// Source location that triggered the diagnostic.
    pub span: cobol_span::Span,
    /// Severity level.
    pub severity: DiagnosticSeverity,
}

/// Severity of a diagnostic message.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Hint,
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Lower an AST SourceFile to HIR.
///
/// This is the main entry point for semantic analysis. It walks the typed
/// AST, resolves names, computes storage layouts, and produces a fully
/// validated [`HirModule`].
pub fn lower(ast: &cobol_ast::SourceFile, interner: &mut cobol_intern::Interner) -> HirModule {
    let mut lowerer = HirLowerer::new(interner);
    lowerer.lower_source_file(ast);
    lowerer.finish()
}

/// Parse a COBOL PIC string into a [`PictureType`].
///
/// The PIC string should contain only the picture characters (e.g.
/// `"9(5)V99"`, `"X(20)"`, `"S9(7)V9(2)"`). This function handles
/// repetition counts in parentheses and determines the category, size,
/// scale, and sign position.
pub fn parse_picture(pic_string: &str) -> Result<PictureType, String> {
    let upper = pic_string.to_ascii_uppercase();
    let chars: Vec<char> = upper.chars().collect();
    let len = chars.len();
    if len == 0 {
        return Err("empty PIC string".to_string());
    }

    let mut i = 0;
    let mut total_digits = 0u32;
    let mut scale = 0i32;
    let mut has_v = false;
    let mut _has_sign = false;
    let mut has_alpha = false;
    let mut has_numeric = false;
    let mut has_x = false;
    let mut has_edited = false;
    let mut sign_pos = SignPosition::None;

    while i < len {
        let ch = chars[i];
        let count = if i + 1 < len && chars[i + 1] == '(' {
            // Parse repetition count: X(nn)
            let start = i + 2;
            let end = chars[start..].iter().position(|&c| c == ')')
                .map(|p| start + p)
                .ok_or_else(|| format!("unclosed parenthesis in PIC string at position {}", start))?;
            let num: u32 = upper[start..end].parse()
                .map_err(|_| format!("invalid repeat count in PIC string: {}", &upper[start..end]))?;
            i = end + 1;
            num
        } else {
            i += 1;
            1
        };

        match ch {
            'S' => {
                _has_sign = true;
                sign_pos = SignPosition::Leading;
            }
            '9' => {
                has_numeric = true;
                total_digits += count;
                if has_v {
                    scale += count as i32;
                }
            }
            'X' => {
                has_x = true;
                total_digits += count;
            }
            'A' => {
                has_alpha = true;
                total_digits += count;
            }
            'V' => {
                has_v = true;
            }
            'P' => {
                total_digits += count;
                if has_v {
                    scale += count as i32;
                } else {
                    scale -= count as i32;
                }
            }
            '.' => {
                // Actual decimal point insertion character — takes storage
                // and marks the decimal position like V does.
                has_v = true;
                total_digits += count;
            }
            'Z' | '*' | '$' | '+' | '-' | ',' | '/' | 'B' | '0' | 'C' | 'R' | 'D' => {
                // Edited characters
                has_edited = true;
                total_digits += count;
                if has_v && matches!(ch, 'Z' | '*' | '9') {
                    scale += count as i32;
                }
            }
            _ => {
                return Err(format!("unknown PIC character: {}", ch));
            }
        }
    }

    let category = if has_x {
        if has_numeric || has_alpha {
            PictureCategory::Alphanumeric
        } else {
            PictureCategory::Alphanumeric
        }
    } else if has_edited && !has_alpha && !has_x {
        // Edited characters (Z, *, $, +, -, comma, etc.) with optional 9
        // indicate a numeric-edited picture.
        PictureCategory::NumericEdited
    } else if has_alpha && !has_numeric {
        PictureCategory::Alphabetic
    } else if has_numeric && !has_alpha {
        PictureCategory::Numeric
    } else {
        PictureCategory::Alphanumeric
    };

    Ok(PictureType {
        category,
        size: total_digits,
        scale,
        sign: sign_pos,
        pic_string: pic_string.to_string(),
    })
}

// ---------------------------------------------------------------------------
// HIR Lowerer
// ---------------------------------------------------------------------------

use cobol_ast::{AstNode, SyntaxKind};

/// Info about a SELECT entry extracted from the ENVIRONMENT DIVISION.
struct SelectInfo {
    /// The SELECT file name (e.g. TEST-FILE).
    file_name: String,
    /// The ASSIGN TO file path (e.g. "SMOKE-FILE.DAT").
    assign_to: String,
    /// File organization.
    organization: FileOrganization,
}

struct HirLowerer<'a> {
    interner: &'a mut cobol_intern::Interner,
    data_items: la_arena::Arena<DataItemData>,
    working_storage: Vec<DataItemId>,
    linkage_items: Vec<DataItemId>,
    using_params: Vec<cobol_intern::Name>,
    paragraphs: Vec<HirParagraph>,
    diagnostics: Vec<HirDiagnostic>,
    program_name: Option<cobol_intern::Name>,
    /// SELECT entries from the ENVIRONMENT DIVISION.
    select_entries: Vec<SelectInfo>,
    /// Maps FD file name to its record item names.
    fd_records: std::collections::HashMap<String, Vec<String>>,
    /// File section data items (record items under FD).
    file_data_items: Vec<DataItemId>,
    /// File descriptors built from SELECT + FD.
    file_items: Vec<FileDescriptor>,
    /// Maps 88-level condition names to (parent DataItemId, condition value).
    condition_names: std::collections::HashMap<String, (DataItemId, Option<InitialValue>)>,
    /// Tracks the last non-88 data item ID for parenting 88-level items.
    last_non_88_item: Option<DataItemId>,
}

impl<'a> HirLowerer<'a> {
    fn new(interner: &'a mut cobol_intern::Interner) -> Self {
        Self {
            interner,
            data_items: la_arena::Arena::new(),
            working_storage: Vec::new(),
            linkage_items: Vec::new(),
            using_params: Vec::new(),
            paragraphs: Vec::new(),
            diagnostics: Vec::new(),
            program_name: None,
            select_entries: Vec::new(),
            fd_records: std::collections::HashMap::new(),
            file_data_items: Vec::new(),
            file_items: Vec::new(),
            condition_names: std::collections::HashMap::new(),
            last_non_88_item: None,
        }
    }

    fn dummy_span(&self) -> cobol_span::Span {
        cobol_span::Span::new(
            cobol_span::FileId::new(0),
            cobol_span::TextRange::new(
                cobol_span::TextSize::from(0u32),
                cobol_span::TextSize::from(0u32),
            ),
            cobol_span::ExpansionId::ROOT,
        )
    }

    fn lower_source_file(&mut self, ast: &cobol_ast::SourceFile) {
        // Extract PROGRAM-ID
        if let Some(id_div) = ast.identification_division() {
            if let Some(pid) = id_div.program_id() {
                if let Some(name_tok) = pid.name() {
                    let name_text = name_tok.text().to_string();
                    self.program_name = Some(self.interner.intern(&name_text));
                }
            }
        }

        // Extract SELECT entries from ENVIRONMENT DIVISION
        if let Some(env_div) = ast.environment_division() {
            self.lower_environment_division(&env_div);
        }

        // Lower DATA DIVISION
        if let Some(data_div) = ast.data_division() {
            self.lower_data_division(&data_div);
        }

        // Compute group item sizes based on their children
        self.compute_group_sizes();

        // Build file descriptors from SELECT + FD info
        self.build_file_descriptors();

        // Lower PROCEDURE DIVISION
        if let Some(proc_div) = ast.procedure_division() {
            self.lower_procedure_division(&proc_div);
        }
    }

    fn lower_environment_division(&mut self, env_div: &cobol_ast::EnvironmentDivision) {
        // Scan for FILE_CONTROL_ENTRY nodes (SELECT entries)
        for child in env_div.syntax().children() {
            if child.kind() == SyntaxKind::FILE_CONTROL_ENTRY {
                self.extract_select_entry(&child);
            }
        }
    }

    fn extract_select_entry(&mut self, node: &cobol_ast::SyntaxNode) {
        // Collect all non-whitespace tokens
        let mut tokens: Vec<String> = Vec::new();
        for el in node.children_with_tokens() {
            if let Some(tok) = el.into_token() {
                let kind = tok.kind();
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                    || kind == SyntaxKind::COMMENT || kind == SyntaxKind::PERIOD
                {
                    continue;
                }
                tokens.push(tok.text().to_string());
            }
        }

        // Parse: SELECT file-name ASSIGN TO "path" ORGANIZATION IS LINE SEQUENTIAL
        let mut file_name = String::new();
        let mut assign_to = String::new();
        let mut organization = FileOrganization::Sequential;

        let mut i = 0;
        while i < tokens.len() {
            let upper = tokens[i].to_ascii_uppercase();
            match upper.as_str() {
                "SELECT" => {
                    i += 1;
                    // Next token should be the file name
                    if i < tokens.len() {
                        // Skip optional OPTIONAL keyword
                        let next_upper = tokens[i].to_ascii_uppercase();
                        if next_upper == "OPTIONAL" {
                            i += 1;
                        }
                        if i < tokens.len() {
                            file_name = tokens[i].to_ascii_uppercase();
                            i += 1;
                        }
                    }
                }
                "ASSIGN" => {
                    i += 1;
                    // Skip optional TO
                    if i < tokens.len() && tokens[i].eq_ignore_ascii_case("TO") {
                        i += 1;
                    }
                    if i < tokens.len() {
                        let text = &tokens[i];
                        // Strip quotes if present
                        if (text.starts_with('"') && text.ends_with('"'))
                            || (text.starts_with('\'') && text.ends_with('\''))
                        {
                            assign_to = text[1..text.len() - 1].to_string();
                        } else {
                            assign_to = text.clone();
                        }
                        i += 1;
                    }
                }
                "ORGANIZATION" => {
                    i += 1;
                    // Skip optional IS
                    if i < tokens.len() && tokens[i].eq_ignore_ascii_case("IS") {
                        i += 1;
                    }
                    // Check for LINE SEQUENTIAL or just SEQUENTIAL, etc.
                    if i < tokens.len() {
                        let org_upper = tokens[i].to_ascii_uppercase();
                        match org_upper.as_str() {
                            "LINE" => {
                                i += 1;
                                if i < tokens.len() && tokens[i].eq_ignore_ascii_case("SEQUENTIAL") {
                                    organization = FileOrganization::LineSequential;
                                    i += 1;
                                }
                            }
                            "SEQUENTIAL" => {
                                organization = FileOrganization::Sequential;
                                i += 1;
                            }
                            "INDEXED" => {
                                organization = FileOrganization::Indexed;
                                i += 1;
                            }
                            "RELATIVE" => {
                                organization = FileOrganization::Relative;
                                i += 1;
                            }
                            _ => { i += 1; }
                        }
                    }
                }
                _ => { i += 1; }
            }
        }

        if !file_name.is_empty() {
            self.select_entries.push(SelectInfo {
                file_name,
                assign_to,
                organization,
            });
        }
    }

    fn build_file_descriptors(&mut self) {
        for sel in &self.select_entries {
            let name = self.interner.intern(&sel.file_name);
            let mut record_items = Vec::new();
            if let Some(records) = self.fd_records.get(&sel.file_name) {
                for rec_name in records {
                    // Find the data item in file_data_items
                    for &item_id in &self.file_data_items {
                        let item = &self.data_items[item_id.into_raw()];
                        let item_name = item.name.map(|n| self.interner.resolve(n).to_string());
                        if item_name.as_deref() == Some(rec_name.as_str()) {
                            record_items.push(item_id);
                        }
                    }
                }
            }
            self.file_items.push(FileDescriptor {
                name,
                record_items,
                organization: sel.organization,
                access_mode: AccessMode::Sequential,
                assign_to: sel.assign_to.clone(),
                span: self.dummy_span(),
            });
        }
    }

    fn lower_data_division(&mut self, data_div: &cobol_ast::DataDivision) {
        // Process FILE SECTION: extract FD entries and their record items
        for child in data_div.syntax().children() {
            if child.kind() == SyntaxKind::FILE_SECTION {
                self.lower_file_section(&child);
            }
        }

        if let Some(ws) = data_div.working_storage_section() {
            for item in ws.items() {
                if let Some(id) = self.lower_data_item(&item) {
                    self.working_storage.push(id);
                }
            }
        }
        if let Some(ls) = data_div.linkage_section() {
            for item in ls.items() {
                if let Some(id) = self.lower_data_item(&item) {
                    self.linkage_items.push(id);
                }
            }
        }
    }

    /// Compute sizes for group items by summing their subordinate elementary items.
    /// Also accounts for OCCURS by multiplying the element size.
    fn compute_group_sizes(&mut self) {
        // Build a list of (level, index_into_data_items, byte_size, occurs_max)
        // for all working_storage items, then walk to compute group sizes.
        let ws_ids: Vec<DataItemId> = self.working_storage.clone();
        if ws_ids.is_empty() {
            return;
        }

        // Gather info
        let items: Vec<(u8, DataItemId, u32, u32)> = ws_ids
            .iter()
            .map(|&id| {
                let it = &self.data_items[id.into_raw()];
                let occurs = it.occurs.as_ref().map(|o| o.max).unwrap_or(1);
                (it.level, id, it.storage.byte_size, occurs)
            })
            .collect();

        // Walk bottom-up: for each group item (byte_size == 0 and level < 88),
        // compute its size as the sum of all immediate children's effective sizes.
        let n = items.len();
        let mut effective_sizes: Vec<u32> = items.iter().map(|&(_, _, sz, occ)| {
            if sz > 0 { sz * occ } else { 0 }
        }).collect();

        // Multiple passes: walk from the end to compute group sizes
        for _pass in 0..5 {
            for i in 0..n {
                let (level, _, sz, occurs) = items[i];
                if sz > 0 || level >= 88 {
                    continue; // elementary item or 88-level, skip
                }
                // Sum sizes of subordinate items (higher level numbers)
                let mut group_sum = 0u32;
                for j in (i + 1)..n {
                    let (child_level, _, _, _) = items[j];
                    if child_level <= level {
                        break; // no longer subordinate
                    }
                    // Only count direct children (next level down, skip grandchildren)
                    // Actually, for correct COBOL layout, we count all items at the
                    // next subordinate level, which accounts for nested groups.
                    // But the simplest correct approach: sum only items at the
                    // immediate child level (skip items nested deeper).
                    // We need to count direct children only:
                    let is_direct_child = {
                        let mut direct = true;
                        for k in (i + 1)..j {
                            let (intermediate_level, _, _, _) = items[k];
                            if intermediate_level <= level {
                                break;
                            }
                            if intermediate_level < child_level {
                                direct = false; // child_level is nested under an intermediate group
                                break;
                            }
                        }
                        direct
                    };
                    if is_direct_child {
                        group_sum += effective_sizes[j];
                    }
                }
                if group_sum > 0 {
                    effective_sizes[i] = group_sum * occurs;
                    // Update the data item's byte_size
                    self.data_items[items[i].1.into_raw()].storage.byte_size = group_sum;
                }
            }
        }
    }

    fn lower_file_section(&mut self, file_section: &cobol_ast::SyntaxNode) {
        let mut current_fd_name: Option<String> = None;

        for child in file_section.children() {
            if child.kind() == SyntaxKind::FD_ENTRY {
                // Extract FD file name from tokens
                let mut fd_name = None;
                let mut skip_fd = true;
                for el in child.children_with_tokens() {
                    if let Some(tok) = el.into_token() {
                        let kind = tok.kind();
                        if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                            || kind == SyntaxKind::COMMENT || kind == SyntaxKind::PERIOD
                        {
                            continue;
                        }
                        if skip_fd {
                            // First non-ws token is FD keyword
                            skip_fd = false;
                            continue;
                        }
                        // Next non-ws token is the file name
                        if fd_name.is_none() {
                            fd_name = Some(tok.text().to_ascii_uppercase());
                        }
                    }
                }
                current_fd_name = fd_name;
                if let Some(ref name) = current_fd_name {
                    self.fd_records.entry(name.clone()).or_insert_with(Vec::new);
                }
            } else if child.kind() == SyntaxKind::DATA_ITEM {
                // This is a record item under the current FD
                if let Some(data_item) = cobol_ast::DataItem::cast(child.clone()) {
                    if let Some(id) = self.lower_data_item(&data_item) {
                        self.file_data_items.push(id);
                        // Also add to working_storage so it gets a global variable
                        self.working_storage.push(id);
                        // Register this record name under the current FD
                        if let Some(ref fd_name) = current_fd_name {
                            let item = &self.data_items[id.into_raw()];
                            if let Some(rec_name) = item.name {
                                let rec_name_str = self.interner.resolve(rec_name).to_string();
                                self.fd_records.entry(fd_name.clone())
                                    .or_insert_with(Vec::new)
                                    .push(rec_name_str);
                            }
                        }
                    }
                }
            }
        }
    }

    fn lower_data_item(&mut self, item: &cobol_ast::DataItem) -> Option<DataItemId> {
        // Extract level number
        let level_tok = item.level_number()?;
        let level_text = level_tok.text().to_string().trim().to_string();
        let level: u8 = level_text.parse().unwrap_or(1);

        // Extract data name
        let name = item.data_name().map(|tok| {
            let name_text = tok.text().to_string();
            self.interner.intern(&name_text)
        });

        // Extract PIC clause
        // The PIC_CLAUSE CST node contains: PIC/PICTURE keyword, optional IS,
        // then the picture string tokens (which may be multiple: IntegerLiteral,
        // LeftParen, RightParen, WORD, etc.). We reconstruct the PIC string by
        // concatenating all non-keyword, non-whitespace tokens after PIC [IS].
        let (mut storage, pic_type) = if let Some(pic) = item.pic_clause() {
            let pic_str = {
                let mut parts = Vec::new();
                let mut skip_keyword = true; // skip PIC/PICTURE and IS
                for tok in pic.syntax().children_with_tokens().filter_map(|el| el.into_token()) {
                    let kind = tok.kind();
                    if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE {
                        if !parts.is_empty() {
                            // Whitespace after PIC string content means we're done
                            break;
                        }
                        continue;
                    }
                    if skip_keyword && kind == SyntaxKind::WORD {
                        let text_upper = tok.text().to_ascii_uppercase();
                        if text_upper == "PIC" || text_upper == "PICTURE" || text_upper == "IS" {
                            continue;
                        }
                    }
                    skip_keyword = false;
                    parts.push(tok.text().to_string());
                }
                parts.join("")
            };
            if !pic_str.is_empty() {
                match parse_picture(&pic_str) {
                    Ok(pt) => {
                        let usage = Self::extract_usage_type(item);
                        let encoding = Self::usage_to_encoding(usage);
                        let byte_size = Self::compute_byte_size(usage, &pt);
                        let sd = StorageDescriptor {
                            byte_size,
                            encoding,
                            picture: Some(pt.clone()),
                            usage,
                        };
                        (sd, Some(pt))
                    }
                    Err(msg) => {
                        self.diagnostics.push(HirDiagnostic {
                            message: format!("invalid PIC clause: {}", msg),
                            span: self.dummy_span(),
                            severity: DiagnosticSeverity::Error,
                        });
                        let usage = Self::extract_usage_type(item);
                        let encoding = Self::usage_to_encoding(usage);
                        let sd = StorageDescriptor {
                            byte_size: 0,
                            encoding,
                            picture: None,
                            usage,
                        };
                        (sd, None)
                    }
                }
            } else {
                let usage = Self::extract_usage_type(item);
                let encoding = Self::usage_to_encoding(usage);
                let sd = StorageDescriptor {
                    byte_size: 0,
                    encoding,
                    picture: None,
                    usage,
                };
                (sd, None)
            }
        } else {
            // Group item or item without PIC — check for USAGE clause anyway
            // (e.g. COMP-1, COMP-2 items have no PIC)
            let usage = Self::extract_usage_type(item);
            let encoding = Self::usage_to_encoding(usage);
            let byte_size = match usage {
                UsageType::Comp1 => 4,
                UsageType::Comp2 => 8,
                UsageType::Index => 4,
                UsageType::Pointer | UsageType::FunctionPointer => 4,
                _ => 0,
            };
            let sd = StorageDescriptor {
                byte_size,
                encoding,
                picture: None,
                usage,
            };
            (sd, None)
        };

        // Extract VALUE clause
        let value = self.lower_value_clause(item);

        // Extract OCCURS clause
        let occurs = self.lower_occurs_clause(item);

        // If OCCURS is present, multiply byte_size by occurs count for total storage
        let element_size = storage.byte_size;
        if let Some(ref occ) = occurs {
            if occ.max > 1 {
                storage.byte_size = element_size * occ.max;
            }
        }

        // Extract REDEFINES clause
        let redefines = self.lower_redefines_clause(item);

        let is_group = pic_type.is_none() && level < 77 && level != 88;

        let data = DataItemData {
            level,
            name,
            storage,
            parent: None,
            children: Vec::new(),
            offset: 0,
            redefines,
            occurs,
            value: value.clone(),
            span: self.dummy_span(),
            is_group,
        };

        let idx = self.data_items.alloc(data);
        let item_id = DataItemId::from_raw(idx);

        // Track the last non-88 item for parenting 88-level items
        if level != 88 {
            self.last_non_88_item = Some(item_id);
        }

        // If this is a level-88 item, register it as a condition name
        if level == 88 {
            if let Some(item_name) = name {
                let name_str = self.interner.resolve(item_name).to_string();
                let parent_id = self.find_last_non_88_item();
                if let Some(parent_id) = parent_id {
                    self.condition_names.insert(name_str, (parent_id, value));
                }
            }
        }

        Some(item_id)
    }

    /// Extract the USAGE type from a data item's USAGE clause CST node.
    ///
    /// Walks the USAGE_CLAUSE child node's tokens, skips USAGE and IS keywords,
    /// and parses the remaining WORD token as a usage type keyword.
    fn extract_usage_type(item: &cobol_ast::DataItem) -> UsageType {
        let uc = match item.usage_clause() {
            Some(uc) => uc,
            None => return UsageType::Display,
        };
        // Walk tokens in the USAGE_CLAUSE node, skip USAGE and IS keywords,
        // take the first non-keyword, non-whitespace WORD as the usage type.
        for tok in uc.syntax().children_with_tokens().filter_map(|el| el.into_token()) {
            let kind = tok.kind();
            if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                || kind == SyntaxKind::COMMENT
            {
                continue;
            }
            let text_upper = tok.text().to_ascii_uppercase();
            // Skip the USAGE and IS keywords
            if text_upper == "USAGE" || text_upper == "IS" {
                continue;
            }
            // Parse the usage type keyword
            return match text_upper.as_str() {
                "COMP" | "COMPUTATIONAL" => UsageType::Comp,
                "COMP-1" | "COMPUTATIONAL-1" => UsageType::Comp1,
                "COMP-2" | "COMPUTATIONAL-2" => UsageType::Comp2,
                "COMP-3" | "COMPUTATIONAL-3" => UsageType::Comp3,
                "COMP-4" | "COMPUTATIONAL-4" => UsageType::Comp4,
                "COMP-5" | "COMPUTATIONAL-5" => UsageType::Comp5,
                "BINARY" => UsageType::Binary,
                "PACKED-DECIMAL" => UsageType::PackedDecimal,
                "INDEX" => UsageType::Index,
                "POINTER" => UsageType::Pointer,
                "FUNCTION-POINTER" => UsageType::FunctionPointer,
                "DISPLAY" => UsageType::Display,
                _ => UsageType::Display, // unknown usage, default to Display
            };
        }
        UsageType::Display
    }

    /// Map a [`UsageType`] to its corresponding [`DataEncoding`].
    fn usage_to_encoding(usage: UsageType) -> DataEncoding {
        match usage {
            UsageType::Display => DataEncoding::Display,
            UsageType::Comp
            | UsageType::Comp4
            | UsageType::Comp5
            | UsageType::Binary => DataEncoding::Binary,
            UsageType::Comp1 => DataEncoding::FloatSingle,
            UsageType::Comp2 => DataEncoding::FloatDouble,
            UsageType::Comp3 | UsageType::PackedDecimal => DataEncoding::PackedDecimal,
            UsageType::Index => DataEncoding::Index,
            UsageType::Pointer | UsageType::FunctionPointer => DataEncoding::Pointer,
        }
    }

    /// Compute the byte size for a data item given its usage type and PIC info.
    ///
    /// - DISPLAY: 1 byte per character/digit position (= `PictureType::size`)
    /// - COMP / COMP-4 / COMP-5 / BINARY: sized by digit count
    ///   - 1..=4 digits  -> 2 bytes (i16)
    ///   - 5..=9 digits  -> 4 bytes (i32)
    ///   - 10..=18 digits -> 8 bytes (i64)
    /// - COMP-1: 4 bytes (f32), independent of PIC
    /// - COMP-2: 8 bytes (f64), independent of PIC
    /// - COMP-3 / PACKED-DECIMAL: (digits + 1) / 2 bytes (packed BCD)
    /// - INDEX: 4 bytes
    /// - POINTER / FUNCTION-POINTER: 4 bytes
    fn compute_byte_size(usage: UsageType, pic: &PictureType) -> u32 {
        match usage {
            UsageType::Display => {
                // 1 byte per character/digit position
                pic.size
            }
            UsageType::Comp | UsageType::Comp4 | UsageType::Comp5 | UsageType::Binary => {
                // Binary integer sized by digit count
                let digits = pic.size;
                if digits <= 4 {
                    2
                } else if digits <= 9 {
                    4
                } else {
                    8
                }
            }
            UsageType::Comp1 => 4,
            UsageType::Comp2 => 8,
            UsageType::Comp3 | UsageType::PackedDecimal => {
                // Packed BCD: each byte holds 2 digits, last nibble is sign
                (pic.size + 1) / 2
            }
            UsageType::Index => 4,
            UsageType::Pointer | UsageType::FunctionPointer => 4,
        }
    }

    fn lower_value_clause(&mut self, item: &cobol_ast::DataItem) -> Option<InitialValue> {
        let vc = item.value_clause()?;
        // Walk the value clause tokens to find the literal.
        // Track whether a MINUS sign preceded the numeric literal.
        let mut negate = false;
        for el in vc.syntax().children_with_tokens() {
            if let Some(tok) = el.into_token() {
                match tok.kind() {
                    SyntaxKind::MINUS => {
                        negate = true;
                    }
                    SyntaxKind::STRING_LITERAL => {
                        let text = tok.text().to_string();
                        // Strip quotes
                        let inner = if (text.starts_with('"') && text.ends_with('"'))
                            || (text.starts_with('\'') && text.ends_with('\''))
                        {
                            text[1..text.len() - 1].to_string()
                        } else {
                            text
                        };
                        return Some(InitialValue::String_(inner));
                    }
                    SyntaxKind::INTEGER_LITERAL => {
                        let text = tok.text().to_string();
                        if let Ok(n) = text.parse::<i64>() {
                            let value = if negate { -n } else { n };
                            return Some(InitialValue::Numeric(value, 0));
                        }
                    }
                    SyntaxKind::DECIMAL_LITERAL => {
                        let text = tok.text().to_string();
                        // Count decimal places
                        if let Some(dot_pos) = text.find('.') {
                            let scale = (text.len() - dot_pos - 1) as i32;
                            if let Ok(n) = text.replace('.', "").parse::<i64>() {
                                let value = if negate { -n } else { n };
                                return Some(InitialValue::Numeric(value, scale));
                            }
                        }
                    }
                    SyntaxKind::WORD => {
                        let upper = tok.text().to_ascii_uppercase();
                        match upper.as_str() {
                            "ZERO" | "ZEROS" | "ZEROES" => return Some(InitialValue::Zero),
                            "SPACE" | "SPACES" => return Some(InitialValue::Space),
                            "HIGH-VALUE" | "HIGH-VALUES" => return Some(InitialValue::HighValue),
                            "LOW-VALUE" | "LOW-VALUES" => return Some(InitialValue::LowValue),
                            "QUOTE" | "QUOTES" => return Some(InitialValue::Quote),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
        None
    }

    /// Extract an OCCURS clause from a data item CST node.
    fn lower_occurs_clause(&mut self, item: &cobol_ast::DataItem) -> Option<OccursClause> {
        // Look for OCCURS_CLAUSE child node
        for child in item.syntax().children() {
            if child.kind() == SyntaxKind::OCCURS_CLAUSE {
                let mut tokens: Vec<String> = Vec::new();
                for el in child.children_with_tokens() {
                    if let Some(tok) = el.into_token() {
                        let kind = tok.kind();
                        if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                            || kind == SyntaxKind::COMMENT
                        {
                            continue;
                        }
                        tokens.push(tok.text().to_string());
                    }
                }

                let mut max = 0u32;
                let mut min = 0u32;
                let mut depending_on = None;
                let mut indexed_by = Vec::new();

                let mut i = 0;
                if i < tokens.len() && tokens[i].eq_ignore_ascii_case("OCCURS") {
                    i += 1;
                }

                if i < tokens.len() {
                    if let Ok(n) = tokens[i].parse::<u32>() {
                        max = n;
                        min = n;
                        i += 1;
                    }
                }

                while i < tokens.len() {
                    let upper = tokens[i].to_ascii_uppercase();
                    match upper.as_str() {
                        "TO" => {
                            i += 1;
                            min = max;
                            if i < tokens.len() {
                                if let Ok(n) = tokens[i].parse::<u32>() {
                                    max = n;
                                    i += 1;
                                }
                            }
                        }
                        "TIMES" => { i += 1; }
                        "DEPENDING" => {
                            i += 1;
                            if i < tokens.len() && tokens[i].eq_ignore_ascii_case("ON") {
                                i += 1;
                            }
                            if i < tokens.len() {
                                let dep_name = tokens[i].to_ascii_uppercase();
                                depending_on = Some(self.interner.intern(&dep_name));
                                i += 1;
                            }
                        }
                        "INDEXED" => {
                            i += 1;
                            if i < tokens.len() && tokens[i].eq_ignore_ascii_case("BY") {
                                i += 1;
                            }
                            while i < tokens.len() {
                                let tok_upper = tokens[i].to_ascii_uppercase();
                                if tok_upper == "ASCENDING" || tok_upper == "DESCENDING"
                                    || tok_upper == "KEY" {
                                    break;
                                }
                                indexed_by.push(self.interner.intern(&tok_upper));
                                i += 1;
                            }
                        }
                        _ => { i += 1; }
                    }
                }

                if max > 0 {
                    return Some(OccursClause {
                        min,
                        max,
                        depending_on,
                        keys: Vec::new(),
                        indexed_by,
                    });
                }
            }
        }
        None
    }

    /// Extract a REDEFINES clause from a data item CST node.
    fn lower_redefines_clause(&self, item: &cobol_ast::DataItem) -> Option<DataItemId> {
        for child in item.syntax().children() {
            if child.kind() == SyntaxKind::REDEFINES_CLAUSE {
                let mut tokens: Vec<String> = Vec::new();
                for el in child.children_with_tokens() {
                    if let Some(tok) = el.into_token() {
                        let kind = tok.kind();
                        if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                            || kind == SyntaxKind::COMMENT
                        {
                            continue;
                        }
                        tokens.push(tok.text().to_string());
                    }
                }

                // tokens: [REDEFINES, target-name]
                if tokens.len() >= 2 {
                    let target_name = tokens[1].to_ascii_uppercase();
                    for (idx, item_data) in self.data_items.iter() {
                        if let Some(name) = item_data.name {
                            if self.interner.resolve(name) == target_name {
                                return Some(DataItemId::from_raw(idx));
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Find the last non-88 level data item that was added.
    fn find_last_non_88_item(&self) -> Option<DataItemId> {
        self.last_non_88_item
    }

    fn lower_procedure_division(&mut self, proc_div: &cobol_ast::ProcedureDivision) {
        // Extract USING parameters: scan direct token children for
        // "USING" keyword followed by parameter names until period.
        let mut saw_using = false;
        for tok in proc_div.syntax().children_with_tokens().filter_map(|el| el.into_token()) {
            let kind = tok.kind();
            let text = tok.text().to_ascii_uppercase();
            if text == "USING" {
                saw_using = true;
                continue;
            }
            if saw_using {
                if kind == SyntaxKind::PERIOD || kind == SyntaxKind::NEWLINE {
                    if kind == SyntaxKind::PERIOD { break; }
                    continue;
                }
                if kind == SyntaxKind::WHITESPACE { continue; }
                // Skip BY REFERENCE / BY CONTENT / BY VALUE keywords
                if matches!(text.as_str(), "BY" | "REFERENCE" | "CONTENT" | "VALUE") {
                    continue;
                }
                // This should be a parameter name
                if kind == SyntaxKind::WORD {
                    let name = self.interner.intern(&text);
                    self.using_params.push(name);
                }
            }
        }

        // First, handle "loose" sentences directly inside PROCEDURE DIVISION
        // (i.e., code without a named paragraph). Collect them into an
        // implicit main paragraph.
        let mut loose_stmts = Vec::new();
        for child in proc_div.syntax().children() {
            if child.kind() == SyntaxKind::SENTENCE {
                if let Some(sent) = cobol_ast::Sentence::cast(child) {
                    self.lower_sentence(&sent, &mut loose_stmts);
                }
            }
        }
        if !loose_stmts.is_empty() {
            let name = self.interner.intern("_MAIN");
            self.paragraphs.push(HirParagraph {
                name,
                statements: loose_stmts,
                span: self.dummy_span(),
            });
        }

        // Then handle named paragraphs
        for para in proc_div.paragraphs() {
            self.lower_paragraph(&para);
        }
    }

    fn lower_paragraph(&mut self, para: &cobol_ast::Paragraph) {
        let name = if let Some(tok) = para.name() {
            self.interner.intern(&tok.text().to_string())
        } else {
            self.interner.intern("UNNAMED")
        };

        let mut statements = Vec::new();
        for sentence in para.sentences() {
            self.lower_sentence(&sentence, &mut statements);
        }

        self.paragraphs.push(HirParagraph {
            name,
            statements,
            span: self.dummy_span(),
        });
    }

    fn lower_sentence(
        &mut self,
        sentence: &cobol_ast::Sentence,
        stmts: &mut Vec<HirStatement>,
    ) {
        // Sentences contain statement nodes as children.
        // Statement nodes can be DISPLAY_STMT, STOP_STMT, etc.
        for child in sentence.syntax().children() {
            match child.kind() {
                SyntaxKind::DISPLAY_STMT => {
                    stmts.push(self.lower_display_stmt(&child));
                }
                SyntaxKind::STOP_STMT => {
                    let tokens = self.collect_all_tokens(&child);
                    if tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("GOBACK")) {
                        stmts.push(HirStatement::GoBack);
                    } else {
                        stmts.push(HirStatement::StopRun);
                    }
                }
                SyntaxKind::MOVE_STMT => {
                    if let Some(s) = self.lower_move_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::ADD_STMT => {
                    if let Some(s) = self.lower_add_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::SUBTRACT_STMT => {
                    if let Some(s) = self.lower_subtract_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::MULTIPLY_STMT => {
                    if let Some(s) = self.lower_multiply_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::DIVIDE_STMT => {
                    if let Some(s) = self.lower_divide_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::COMPUTE_STMT => {
                    if let Some(s) = self.lower_compute_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::IF_STMT => {
                    if let Some(s) = self.lower_if_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::PERFORM_STMT => {
                    if let Some(s) = self.lower_perform_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::STRING_STMT => {
                    if let Some(s) = self.lower_string_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::CALL_STMT => {
                    if let Some(s) = self.lower_call_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::EXIT_STMT => {
                    stmts.push(self.lower_exit_stmt(&child));
                }
                SyntaxKind::OPEN_STMT => {
                    if let Some(s) = self.lower_open_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::CLOSE_STMT => {
                    if let Some(s) = self.lower_close_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::WRITE_STMT => {
                    if let Some(s) = self.lower_write_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::READ_STMT => {
                    if let Some(s) = self.lower_read_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::EVALUATE_STMT => {
                    if let Some(s) = self.lower_evaluate_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::GO_TO_STMT => {
                    if let Some(s) = self.lower_go_to_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::ACCEPT_STMT => {
                    if let Some(s) = self.lower_accept_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::INSPECT_STMT => {
                    if let Some(s) = self.lower_inspect_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::UNSTRING_STMT => {
                    if let Some(s) = self.lower_unstring_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::INITIALIZE_STMT => {
                    if let Some(s) = self.lower_initialize_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::SEARCH_STMT => {
                    if let Some(s) = self.lower_search_stmt(&child) {
                        stmts.push(s);
                    }
                }
                SyntaxKind::SET_STMT => {
                    if let Some(s) = self.lower_set_stmt(&child) {
                        stmts.push(s);
                    }
                }
                _ => {
                    // Other statements: not yet implemented
                }
            }
        }
    }

    fn lower_display_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> HirStatement {
        let tokens = self.collect_tokens(node);
        let mut args = Vec::new();
        let mut no_advancing = false;
        let mut i = 0;

        while i < tokens.len() {
            let (kind, ref text) = tokens[i];
            let upper = text.to_ascii_uppercase();

            match upper.as_str() {
                "DISPLAY" | "UPON" | "WITH" | "IS" | "STANDARD-1" | "STANDARD-2"
                | "CONSOLE" => {
                    i += 1;
                }
                "NO" => {
                    i += 1;
                }
                "ADVANCING" => {
                    no_advancing = true;
                    i += 1;
                }
                "(" | ")" | ":" | "," => {
                    i += 1; // skip stray punctuation
                }
                _ => {
                    match kind {
                        SyntaxKind::STRING_LITERAL => {
                            let inner = if (text.starts_with('"') && text.ends_with('"'))
                                || (text.starts_with('\'') && text.ends_with('\''))
                            {
                                text[1..text.len() - 1].to_string()
                            } else {
                                text.clone()
                            };
                            args.push(HirExpr::Literal(LiteralValue::String_(inner)));
                            i += 1;
                        }
                        SyntaxKind::INTEGER_LITERAL | SyntaxKind::DECIMAL_LITERAL => {
                            args.push(self.token_to_expr(kind, text));
                            i += 1;
                        }
                        SyntaxKind::WORD => {
                            // Use parse_expr_at to handle subscripts and ref-mod
                            let (expr, next) = self.parse_expr_at(&tokens, i);
                            args.push(expr);
                            i = next;
                        }
                        _ => {
                            i += 1;
                        }
                    }
                }
            }
        }

        HirStatement::Display { args, no_advancing }
    }

    fn lower_move_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);

        // Find TO separator
        let to_idx = tokens.iter().position(|(_, text)| text.eq_ignore_ascii_case("TO"))?;

        // Source: parse from position 1 (after MOVE) using parse_expr_at for
        // subscript/ref-mod awareness
        let source = if to_idx > 1 {
            let (expr, _) = self.parse_expr_at(&tokens, 1);
            expr
        } else {
            return None;
        };

        // Targets: parse after TO using parse_data_ref_at
        let mut targets = Vec::new();
        let mut t = to_idx + 1;
        while t < tokens.len() {
            let (kind, ref text) = tokens[t];
            if text == "(" || text == ")" || text == "," || text == ":" || kind == SyntaxKind::PERIOD {
                t += 1;
                continue;
            }
            if kind == SyntaxKind::WORD {
                let (dr, next) = self.parse_data_ref_at(&tokens, t);
                targets.push(dr);
                t = next;
            } else {
                t += 1;
            }
        }

        Some(HirStatement::Move {
            from: source,
            to: targets,
        })
    }

    fn lower_initialize_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);

        let mut targets = Vec::new();
        let mut replacing = Vec::new();
        let mut i = 1; // skip INITIALIZE keyword

        // Parse target identifiers until REPLACING or end
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            if upper == "REPLACING" {
                i += 1;
                break;
            }
            if tokens[i].0 == SyntaxKind::WORD || tokens[i].0 == SyntaxKind::KEYWORD {
                let (dr, next) = self.parse_data_ref_at(&tokens, i);
                targets.push(dr);
                i = next;
            } else {
                i += 1;
            }
        }

        // Parse REPLACING clauses: REPLACING {category} [DATA] BY {value} ...
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "NUMERIC" | "ALPHANUMERIC" | "ALPHABETIC"
                | "ALPHANUMERIC-EDITED" | "NUMERIC-EDITED" => {
                    let category = upper.clone();
                    i += 1;
                    // Skip optional "DATA"
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("DATA") {
                        i += 1;
                    }
                    // Expect "BY"
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("BY") {
                        i += 1;
                    }
                    // Parse the replacement value
                    if i < tokens.len() {
                        let (expr, next) = self.parse_expr_at(&tokens, i);
                        replacing.push((category, expr));
                        i = next;
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }

        if targets.is_empty() {
            return None;
        }

        Some(HirStatement::Initialize { targets, replacing })
    }

    /// Helper: collect non-whitespace tokens from a CST node.
    /// Recursively collect all tokens from a node and its descendants.
    fn collect_all_tokens(&self, node: &cobol_ast::SyntaxNode) -> Vec<(SyntaxKind, String)> {
        let mut tokens = Vec::new();
        for el in node.children_with_tokens() {
            if let Some(tok) = el.as_token() {
                let kind = tok.kind();
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                    || kind == SyntaxKind::COMMENT
                {
                    continue;
                }
                tokens.push((kind, tok.text().to_string()));
            } else if let Some(child) = el.as_node() {
                tokens.extend(self.collect_all_tokens(&child));
            }
        }
        tokens
    }

    fn collect_tokens(&self, node: &cobol_ast::SyntaxNode) -> Vec<(SyntaxKind, String)> {
        let mut tokens = Vec::new();
        for el in node.children_with_tokens() {
            if let Some(tok) = el.into_token() {
                let kind = tok.kind();
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                    || kind == SyntaxKind::COMMENT
                {
                    continue;
                }
                tokens.push((kind, tok.text().to_string()));
            }
        }
        tokens
    }

    /// Helper: parse a token as an expression (literal or data ref).
    fn token_to_expr(&mut self, kind: SyntaxKind, text: &str) -> HirExpr {
        match kind {
            SyntaxKind::INTEGER_LITERAL => {
                if let Ok(n) = text.parse::<i64>() {
                    HirExpr::Literal(LiteralValue::Integer(n))
                } else {
                    HirExpr::Literal(LiteralValue::Integer(0))
                }
            }
            SyntaxKind::DECIMAL_LITERAL => {
                HirExpr::Literal(LiteralValue::Decimal(text.to_string()))
            }
            SyntaxKind::STRING_LITERAL => {
                let inner = if (text.starts_with('"') && text.ends_with('"'))
                    || (text.starts_with('\'') && text.ends_with('\''))
                {
                    text[1..text.len() - 1].to_string()
                } else {
                    text.to_string()
                };
                HirExpr::Literal(LiteralValue::String_(inner))
            }
            _ => {
                let upper = text.to_ascii_uppercase();
                match upper.as_str() {
                    "ZERO" | "ZEROS" | "ZEROES" => {
                        HirExpr::Literal(LiteralValue::Figurative(FigurativeConstant::Zero))
                    }
                    "SPACE" | "SPACES" => {
                        HirExpr::Literal(LiteralValue::Figurative(FigurativeConstant::Space))
                    }
                    "HIGH-VALUE" | "HIGH-VALUES" => {
                        HirExpr::Literal(LiteralValue::Figurative(FigurativeConstant::HighValue))
                    }
                    "LOW-VALUE" | "LOW-VALUES" => {
                        HirExpr::Literal(LiteralValue::Figurative(FigurativeConstant::LowValue))
                    }
                    "QUOTE" | "QUOTES" => {
                        HirExpr::Literal(LiteralValue::Figurative(FigurativeConstant::Quote))
                    }
                    _ => {
                        let name = self.interner.intern(&upper);
                        HirExpr::DataRef(Box::new(HirDataRef {
                            name,
                            qualifiers: Vec::new(),
                            subscripts: Vec::new(),
                            ref_mod: None,
                            resolved: None,
                        }))
                    }
                }
            }
        }
    }

    /// Parse a single expression (possibly subscripted) from a token slice starting
    /// at position `pos`. Returns `(expr, next_pos)`.
    ///
    /// Handles patterns like:
    /// - `WS-SUM` → DataRef with no subscripts
    /// - `WS-ITEM(1)` → DataRef with literal subscript
    /// - `WS-ITEM(WS-I)` → DataRef with variable subscript
    /// - `42` → Integer literal
    fn parse_expr_at(&mut self, tokens: &[(SyntaxKind, String)], pos: usize) -> (HirExpr, usize) {
        if pos >= tokens.len() {
            return (HirExpr::Literal(LiteralValue::Integer(0)), pos);
        }

        let (kind, text) = &tokens[pos];
        let base_expr = self.token_to_expr(*kind, text);

        // Check if this is a DataRef followed by subscripts or ref-mod in parens
        if let HirExpr::DataRef(ref dr) = base_expr {
            if pos + 1 < tokens.len() && tokens[pos + 1].1 == "(" {
                // Peek inside parens to determine if this is ref-mod (has ':') or subscripts
                let has_colon = tokens[pos + 2..].iter()
                    .take_while(|(_, t)| t != ")")
                    .any(|(_, t)| t == ":");

                let name = dr.name;
                let mut i = pos + 2; // skip name and '('

                if has_colon {
                    // Reference modification: name(start:length)
                    let start_expr = if i < tokens.len() && tokens[i].1 != ":" {
                        let e = self.token_to_expr(tokens[i].0, &tokens[i].1);
                        i += 1;
                        e
                    } else {
                        HirExpr::Literal(LiteralValue::Integer(1))
                    };
                    // Skip ':'
                    if i < tokens.len() && tokens[i].1 == ":" { i += 1; }
                    // Optional length
                    let length_expr = if i < tokens.len() && tokens[i].1 != ")" {
                        let e = self.token_to_expr(tokens[i].0, &tokens[i].1);
                        i += 1;
                        Some(Box::new(e))
                    } else {
                        None
                    };
                    if i < tokens.len() && tokens[i].1 == ")" { i += 1; }

                    return (
                        HirExpr::DataRef(Box::new(HirDataRef {
                            name,
                            qualifiers: Vec::new(),
                            subscripts: Vec::new(),
                            ref_mod: Some((Box::new(start_expr), length_expr)),
                            resolved: None,
                        })),
                        i,
                    );
                } else {
                    // Subscripts: name(expr, expr, ...)
                    let mut subscripts = Vec::new();
                    while i < tokens.len() && tokens[i].1 != ")" {
                        let sub_text = &tokens[i].1;
                        if sub_text != "," {
                            subscripts.push(self.token_to_expr(tokens[i].0, sub_text));
                        }
                        i += 1;
                    }
                    if i < tokens.len() && tokens[i].1 == ")" { i += 1; }

                    return (
                        HirExpr::DataRef(Box::new(HirDataRef {
                            name,
                            qualifiers: Vec::new(),
                            subscripts,
                            ref_mod: None,
                            resolved: None,
                        })),
                        i,
                    );
                }
            }
        }

        (base_expr, pos + 1)
    }

    /// Parse a data reference (possibly subscripted) from a token slice at `pos`.
    /// Returns `(data_ref, next_pos)`.
    fn parse_data_ref_at(&mut self, tokens: &[(SyntaxKind, String)], pos: usize) -> (HirDataRef, usize) {
        if pos >= tokens.len() {
            let name = self.interner.intern("?");
            return (HirDataRef { name, qualifiers: Vec::new(), subscripts: Vec::new(), ref_mod: None, resolved: None }, pos);
        }

        let text = &tokens[pos].1;
        let upper = text.to_ascii_uppercase();
        let name = self.interner.intern(&upper);

        // Check for subscripts or ref-mod
        if pos + 1 < tokens.len() && tokens[pos + 1].1 == "(" {
            let has_colon = tokens[pos + 2..].iter()
                .take_while(|(_, t)| t != ")")
                .any(|(_, t)| t == ":");

            let mut i = pos + 2;

            if has_colon {
                // Reference modification: name(start:length)
                let start_expr = if i < tokens.len() && tokens[i].1 != ":" {
                    let e = self.token_to_expr(tokens[i].0, &tokens[i].1);
                    i += 1;
                    e
                } else {
                    HirExpr::Literal(LiteralValue::Integer(1))
                };
                if i < tokens.len() && tokens[i].1 == ":" { i += 1; }
                let length_expr = if i < tokens.len() && tokens[i].1 != ")" {
                    let e = self.token_to_expr(tokens[i].0, &tokens[i].1);
                    i += 1;
                    Some(Box::new(e))
                } else {
                    None
                };
                if i < tokens.len() && tokens[i].1 == ")" { i += 1; }
                return (
                    HirDataRef {
                        name, qualifiers: Vec::new(), subscripts: Vec::new(),
                        ref_mod: Some((Box::new(start_expr), length_expr)),
                        resolved: None,
                    },
                    i,
                );
            } else {
                // Subscripts
                let mut subscripts = Vec::new();
                while i < tokens.len() && tokens[i].1 != ")" {
                    let sub_text = &tokens[i].1;
                    if sub_text != "," {
                        subscripts.push(self.token_to_expr(tokens[i].0, sub_text));
                    }
                    i += 1;
                }
                if i < tokens.len() && tokens[i].1 == ")" { i += 1; }
                return (
                    HirDataRef { name, qualifiers: Vec::new(), subscripts, ref_mod: None, resolved: None },
                    i,
                );
            }
        }

        (HirDataRef { name, qualifiers: Vec::new(), subscripts: Vec::new(), ref_mod: None, resolved: None }, pos + 1)
    }

    /// Helper: make a data ref from a name string.
    fn make_data_ref(&mut self, text: &str) -> HirDataRef {
        let name = self.interner.intern(&text.to_ascii_uppercase());
        HirDataRef {
            name,
            qualifiers: Vec::new(),
            subscripts: Vec::new(),
            ref_mod: None,
            resolved: None,
        }
    }

    fn lower_open_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);
        // OPEN INPUT/OUTPUT/I-O/EXTEND file-name
        let mut mode = OpenMode::Input;
        let mut file_name = None;

        for (_, text) in &tokens[1..] { // Skip OPEN keyword
            let upper = text.to_ascii_uppercase();
            match upper.as_str() {
                "INPUT" => mode = OpenMode::Input,
                "OUTPUT" => mode = OpenMode::Output,
                "I-O" => mode = OpenMode::IoMode,
                "EXTEND" => mode = OpenMode::Extend,
                _ => {
                    if file_name.is_none() {
                        file_name = Some(self.interner.intern(&upper));
                    }
                }
            }
        }

        Some(HirStatement::Open {
            file: file_name?,
            mode,
        })
    }

    fn lower_close_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);
        // CLOSE file-name
        if tokens.len() < 2 { return None; }
        let file_name = self.interner.intern(&tokens[1].1.to_ascii_uppercase());
        Some(HirStatement::Close { file: file_name })
    }

    fn lower_write_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);
        // WRITE record-name
        if tokens.len() < 2 { return None; }
        let record = self.interner.intern(&tokens[1].1.to_ascii_uppercase());
        Some(HirStatement::Write { record })
    }

    fn lower_read_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // READ file-name [INTO ws-record] [AT END statements] [END-READ]
        let all_tokens = self.collect_all_tokens(node);
        let mut file_name = None;
        let mut into_name = None;
        let mut at_end_stmts = Vec::new();
        let mut i = 0;

        // Skip READ keyword
        if i < all_tokens.len() && all_tokens[i].1.eq_ignore_ascii_case("READ") {
            i += 1;
        }

        // File name
        if i < all_tokens.len() {
            file_name = Some(self.interner.intern(&all_tokens[i].1.to_ascii_uppercase()));
            i += 1;
        }

        // INTO clause and AT END handler
        while i < all_tokens.len() {
            let upper = all_tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "INTO" => {
                    i += 1;
                    if i < all_tokens.len() {
                        into_name = Some(self.interner.intern(&all_tokens[i].1.to_ascii_uppercase()));
                        i += 1;
                    }
                }
                "AT" => {
                    // AT [END] — stop parsing READ tokens here
                    break;
                }
                "END-READ" | "NOT" => break,
                _ => { i += 1; }
            }
        }

        // Lower AT END statements from child nodes
        for child in node.children() {
            let kind = child.kind();
            if kind == SyntaxKind::MOVE_STMT || kind == SyntaxKind::DISPLAY_STMT
                || kind == SyntaxKind::ADD_STMT || kind == SyntaxKind::STOP_STMT
                || kind == SyntaxKind::COMPUTE_STMT || kind == SyntaxKind::IF_STMT
                || kind == SyntaxKind::PERFORM_STMT
            {
                self.lower_child_statement(&child, &mut at_end_stmts);
            }
        }

        Some(HirStatement::Read {
            file: file_name?,
            into: into_name,
            at_end: at_end_stmts,
        })
    }

    fn lower_add_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);
        // ADD op1 [op2...] TO target / ADD op1 op2 GIVING target
        // Skip "ADD" keyword at index 0
        let has_giving = tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("GIVING"));
        let has_to = tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("TO"));

        let mut operands = Vec::new();
        let mut to_targets = Vec::new();
        let mut giving_targets = Vec::new();
        let mut rounded = false;

        let mut phase = 0; // 0=operands, 1=to-targets, 2=giving-targets
        let mut i = 1; // Skip ADD
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "TO" => { phase = 1; i += 1; continue; }
                "GIVING" => { phase = 2; i += 1; continue; }
                "ROUNDED" => { rounded = true; i += 1; continue; }
                "ON" | "SIZE" | "ERROR" | "NOT" | "END-ADD" => break,
                "(" | ")" | "," => { i += 1; continue; } // stray parens (shouldn't happen with parse_*_at)
                _ => {}
            }
            match phase {
                0 => {
                    let (expr, next) = self.parse_expr_at(&tokens, i);
                    operands.push(expr);
                    i = next;
                }
                1 => {
                    let (dr, next) = self.parse_data_ref_at(&tokens, i);
                    to_targets.push(dr);
                    i = next;
                }
                2 => {
                    let (dr, next) = self.parse_data_ref_at(&tokens, i);
                    giving_targets.push(dr);
                    i = next;
                }
                _ => { i += 1; }
            }
        }

        if has_giving {
            Some(HirStatement::Add {
                operands,
                to: Vec::new(),
                giving: Some(giving_targets),
                on_size_error: None,
                rounded,
            })
        } else if has_to {
            Some(HirStatement::Add {
                operands,
                to: to_targets,
                giving: None,
                on_size_error: None,
                rounded,
            })
        } else {
            None
        }
    }

    fn lower_subtract_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);
        let has_giving = tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("GIVING"));

        let mut operands = Vec::new();
        let mut from_targets = Vec::new();
        let mut giving_targets = Vec::new();
        let mut rounded = false;
        let mut phase = 0;

        let mut i = 1; // Skip SUBTRACT
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "FROM" => { phase = 1; i += 1; continue; }
                "GIVING" => { phase = 2; i += 1; continue; }
                "ROUNDED" => { rounded = true; i += 1; continue; }
                "ON" | "SIZE" | "ERROR" | "NOT" | "END-SUBTRACT" => break,
                "(" | ")" | "," => { i += 1; continue; }
                _ => {}
            }
            match phase {
                0 => {
                    let (expr, next) = self.parse_expr_at(&tokens, i);
                    operands.push(expr);
                    i = next;
                }
                1 => {
                    let (dr, next) = self.parse_data_ref_at(&tokens, i);
                    from_targets.push(dr);
                    i = next;
                }
                2 => {
                    let (dr, next) = self.parse_data_ref_at(&tokens, i);
                    giving_targets.push(dr);
                    i = next;
                }
                _ => { i += 1; }
            }
        }

        Some(HirStatement::Subtract {
            operands,
            from: from_targets,
            giving: if has_giving { Some(giving_targets) } else { None },
            on_size_error: None,
            rounded,
        })
    }

    fn lower_multiply_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);
        let mut operand1 = None;
        let mut by_operand = None;
        let mut giving_targets = Vec::new();
        let mut rounded = false;
        let mut phase = 0; // 0=operand1, 1=by, 2=giving

        let mut i = 1; // Skip MULTIPLY
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "BY" => { phase = 1; i += 1; continue; }
                "GIVING" => { phase = 2; i += 1; continue; }
                "ROUNDED" => { rounded = true; i += 1; continue; }
                "ON" | "SIZE" | "ERROR" | "NOT" | "END-MULTIPLY" => break,
                "(" | ")" | "," => { i += 1; continue; }
                _ => {}
            }
            match phase {
                0 => {
                    let (expr, next) = self.parse_expr_at(&tokens, i);
                    operand1 = Some(expr);
                    i = next;
                }
                1 => {
                    let (expr, next) = self.parse_expr_at(&tokens, i);
                    by_operand = Some(expr);
                    i = next;
                }
                2 => {
                    let (dr, next) = self.parse_data_ref_at(&tokens, i);
                    giving_targets.push(dr);
                    i = next;
                }
                _ => { i += 1; }
            }
        }

        Some(HirStatement::Multiply {
            operand1: operand1?,
            by: by_operand?,
            giving: if giving_targets.is_empty() { None } else { Some(giving_targets) },
            on_size_error: None,
            rounded,
        })
    }

    fn lower_divide_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_tokens(node);
        let mut operand1 = None;
        let mut into_or_by = None;
        let mut giving_targets = Vec::new();
        let mut remainder = None;
        let mut rounded = false;
        let mut phase = 0; // 0=operand1, 1=into/by, 2=giving, 3=remainder

        let mut i = 1; // Skip DIVIDE
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "INTO" | "BY" => { phase = 1; i += 1; continue; }
                "GIVING" => { phase = 2; i += 1; continue; }
                "REMAINDER" => { phase = 3; i += 1; continue; }
                "ROUNDED" => { rounded = true; i += 1; continue; }
                "ON" | "SIZE" | "ERROR" | "NOT" | "END-DIVIDE" => break,
                "(" | ")" | "," => { i += 1; continue; }
                _ => {}
            }
            match phase {
                0 => {
                    let (expr, next) = self.parse_expr_at(&tokens, i);
                    operand1 = Some(expr);
                    i = next;
                }
                1 => {
                    let (expr, next) = self.parse_expr_at(&tokens, i);
                    into_or_by = Some(expr);
                    i = next;
                }
                2 => {
                    let (dr, next) = self.parse_data_ref_at(&tokens, i);
                    giving_targets.push(dr);
                    i = next;
                }
                3 => {
                    let (dr, next) = self.parse_data_ref_at(&tokens, i);
                    remainder = Some(dr);
                    i = next;
                }
                _ => { i += 1; }
            }
        }

        Some(HirStatement::Divide {
            operand1: operand1?,
            into_or_by: into_or_by?,
            giving: if giving_targets.is_empty() { None } else { Some(giving_targets) },
            remainder,
            on_size_error: None,
            rounded,
        })
    }

    fn lower_compute_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // Use collect_all_tokens so we descend into ARITHMETIC_EXPR child nodes
        let tokens = self.collect_all_tokens(node);
        // COMPUTE target = expr
        let eq_idx = tokens.iter().position(|(_, t)| t == "=")?;
        let mut targets = Vec::new();
        for (_, text) in &tokens[1..eq_idx] {
            let upper = text.to_ascii_uppercase();
            if upper != "ROUNDED" {
                targets.push(self.make_data_ref(text));
            }
        }
        let rounded = tokens[1..eq_idx].iter().any(|(_, t)| t.eq_ignore_ascii_case("ROUNDED"));

        // Collect expression tokens after "=", filtering out termination keywords and period
        let expr_tokens: Vec<(SyntaxKind, String)> = tokens[eq_idx + 1..].iter()
            .filter(|(k, t)| {
                let u = t.to_ascii_uppercase();
                *k != SyntaxKind::PERIOD
                    && !matches!(u.as_str(), "END-COMPUTE" | "ON" | "SIZE" | "ERROR" | "NOT")
            })
            .cloned()
            .collect();

        if expr_tokens.is_empty() {
            return None;
        }

        // Parse the arithmetic expression using recursive descent
        let mut pos = 0;
        let expr = self.parse_arith_additive(&expr_tokens, &mut pos);

        Some(HirStatement::Compute {
            targets,
            expr,
            on_size_error: None,
            rounded,
        })
    }

    /// Recursive descent parser for arithmetic expressions from token lists.
    /// Handles: +, -, *, /, ** (exponentiation), parentheses, unary minus.
    /// Precedence (low to high): +/-, *//, **

    /// additive = multiplicative (('+' | '-') multiplicative)*
    fn parse_arith_additive(&mut self, tokens: &[(SyntaxKind, String)], pos: &mut usize) -> HirExpr {
        let mut left = self.parse_arith_multiplicative(tokens, pos);

        while *pos < tokens.len() {
            let op_text = &tokens[*pos].1;
            let op = match op_text.as_str() {
                "+" => BinaryOp::Add,
                "-" => BinaryOp::Sub,
                _ => break,
            };
            *pos += 1;
            let right = self.parse_arith_multiplicative(tokens, pos);
            left = HirExpr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    /// multiplicative = power (('*' | '/') power)*
    fn parse_arith_multiplicative(&mut self, tokens: &[(SyntaxKind, String)], pos: &mut usize) -> HirExpr {
        let mut left = self.parse_arith_power(tokens, pos);

        while *pos < tokens.len() {
            let op_text = &tokens[*pos].1;
            let op = match op_text.as_str() {
                "*" => BinaryOp::Mul,
                "/" => BinaryOp::Div,
                _ => break,
            };
            *pos += 1;
            let right = self.parse_arith_power(tokens, pos);
            left = HirExpr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    /// power = unary ('**' power)?    (right-associative)
    fn parse_arith_power(&mut self, tokens: &[(SyntaxKind, String)], pos: &mut usize) -> HirExpr {
        let base = self.parse_arith_unary(tokens, pos);

        if *pos < tokens.len() && tokens[*pos].1 == "**" {
            *pos += 1;
            let exponent = self.parse_arith_power(tokens, pos); // right-associative
            HirExpr::BinaryOp {
                op: BinaryOp::Pow,
                left: Box::new(base),
                right: Box::new(exponent),
            }
        } else {
            base
        }
    }

    /// unary = ('+' | '-')? atom
    fn parse_arith_unary(&mut self, tokens: &[(SyntaxKind, String)], pos: &mut usize) -> HirExpr {
        if *pos < tokens.len() {
            match tokens[*pos].1.as_str() {
                "-" => {
                    *pos += 1;
                    let operand = self.parse_arith_atom(tokens, pos);
                    HirExpr::UnaryOp {
                        op: UnaryOp::Neg,
                        operand: Box::new(operand),
                    }
                }
                "+" => {
                    *pos += 1;
                    self.parse_arith_atom(tokens, pos)
                }
                _ => self.parse_arith_atom(tokens, pos),
            }
        } else {
            HirExpr::Literal(LiteralValue::Integer(0))
        }
    }

    /// atom = '(' additive ')' | literal | data-ref
    fn parse_arith_atom(&mut self, tokens: &[(SyntaxKind, String)], pos: &mut usize) -> HirExpr {
        if *pos >= tokens.len() {
            return HirExpr::Literal(LiteralValue::Integer(0));
        }

        let (kind, text) = &tokens[*pos];

        // Parenthesized sub-expression
        if text == "(" {
            *pos += 1;
            let inner = self.parse_arith_additive(tokens, pos);
            // Skip closing paren
            if *pos < tokens.len() && tokens[*pos].1 == ")" {
                *pos += 1;
            }
            return inner;
        }

        // Use the existing token_to_expr helper for literals and data refs
        let expr = self.token_to_expr(*kind, text);
        *pos += 1;

        // Check for reference modification or subscripting: NAME ( ... )
        // e.g., WS-FIELD(1:5) or WS-TABLE(WS-IDX)
        if *pos < tokens.len() && tokens[*pos].1 == "(" {
            if let HirExpr::DataRef(mut dr) = expr {
                *pos += 1; // skip '('
                // Collect tokens inside parens
                let mut paren_depth = 1;
                let mut inner_tokens = Vec::new();
                while *pos < tokens.len() && paren_depth > 0 {
                    if tokens[*pos].1 == "(" {
                        paren_depth += 1;
                    } else if tokens[*pos].1 == ")" {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            *pos += 1;
                            break;
                        }
                    }
                    inner_tokens.push(tokens[*pos].clone());
                    *pos += 1;
                }

                // Check if this is reference modification (contains ':')
                let colon_pos = inner_tokens.iter().position(|(_, t)| t == ":");
                if let Some(cp) = colon_pos {
                    // Reference modification: (start : length)
                    let start_tokens: Vec<_> = inner_tokens[..cp].to_vec();
                    let len_tokens: Vec<_> = inner_tokens[cp + 1..].to_vec();

                    let mut sp = 0;
                    let start_expr = if !start_tokens.is_empty() {
                        self.parse_arith_additive(&start_tokens, &mut sp)
                    } else {
                        HirExpr::Literal(LiteralValue::Integer(1))
                    };

                    let len_expr = if !len_tokens.is_empty() {
                        let mut lp = 0;
                        Some(Box::new(self.parse_arith_additive(&len_tokens, &mut lp)))
                    } else {
                        None
                    };

                    dr.ref_mod = Some((Box::new(start_expr), len_expr));
                } else {
                    // Subscripts (table indexing)
                    let mut sp = 0;
                    let subscript = self.parse_arith_additive(&inner_tokens, &mut sp);
                    dr.subscripts.push(subscript);
                }

                return HirExpr::DataRef(dr);
            }
        }

        expr
    }

    fn lower_string_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // STRING source1 DELIMITED BY SIZE
        //        source2 DELIMITED BY SIZE
        //   INTO target
        //   WITH POINTER ptr.
        //
        // The parser creates a STRING_STMT node with all tokens as children.
        let tokens = self.collect_all_tokens(node);

        let mut sources: Vec<HirExpr> = Vec::new();
        let mut into_ref = None;
        let mut pointer_ref = None;
        let mut i = 0;

        // Skip the STRING keyword
        if i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            if upper == "STRING" {
                i += 1;
            }
        }

        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();

            // Check for INTO
            if upper == "INTO" {
                i += 1;
                if i < tokens.len() {
                    into_ref = Some(self.make_data_ref(&tokens[i].1));
                    i += 1;
                }
                continue;
            }

            // Check for WITH POINTER
            if upper == "WITH" || upper == "POINTER" {
                if upper == "WITH" { i += 1; }
                if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("POINTER") {
                    i += 1;
                }
                if i < tokens.len() {
                    pointer_ref = Some(self.make_data_ref(&tokens[i].1));
                    i += 1;
                }
                continue;
            }

            // Check for END-STRING
            if upper == "END-STRING" {
                break;
            }

            // Skip DELIMITED, BY, SIZE keywords
            if matches!(upper.as_str(), "DELIMITED" | "BY" | "SIZE") {
                i += 1;
                continue;
            }

            // This should be a source value
            let expr = self.token_to_expr(tokens[i].0, &tokens[i].1);
            sources.push(expr);
            i += 1;
        }

        let into = into_ref?;
        Some(HirStatement::StringStmt {
            sources,
            into,
            pointer: pointer_ref,
        })
    }

    fn lower_inspect_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_all_tokens(node);
        let mut i = 0;

        // Skip INSPECT keyword
        if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("INSPECT") {
            i += 1;
        }

        // Get the target identifier
        if i >= tokens.len() { return None; }
        let target = self.make_data_ref(&tokens[i].1);
        i += 1;

        // Determine the form: TALLYING, REPLACING, or CONVERTING
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "TALLYING" => {
                    i += 1;
                    // TALLYING tally_var FOR {ALL|LEADING|CHARACTERS} {literal} [BEFORE|AFTER INITIAL ...]
                    if i >= tokens.len() { return None; }
                    let tally_var = self.make_data_ref(&tokens[i].1);
                    i += 1;

                    // Skip FOR
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("FOR") {
                        i += 1;
                    }

                    let mut mode = InspectTallyMode::Characters;
                    let mut search = None;
                    let mut before_initial = None;
                    let mut after_initial = None;

                    while i < tokens.len() {
                        let u = tokens[i].1.to_ascii_uppercase();
                        match u.as_str() {
                            "ALL" => { mode = InspectTallyMode::All; i += 1; }
                            "LEADING" => { mode = InspectTallyMode::Leading; i += 1; }
                            "CHARACTERS" => { mode = InspectTallyMode::Characters; i += 1; }
                            "BEFORE" => {
                                i += 1;
                                // Skip optional INITIAL
                                if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("INITIAL") {
                                    i += 1;
                                }
                                if i < tokens.len() {
                                    before_initial = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                    i += 1;
                                }
                            }
                            "AFTER" => {
                                i += 1;
                                if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("INITIAL") {
                                    i += 1;
                                }
                                if i < tokens.len() {
                                    after_initial = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                    i += 1;
                                }
                            }
                            _ => {
                                // This should be the search literal/identifier
                                if search.is_none() && mode != InspectTallyMode::Characters {
                                    search = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                }
                                i += 1;
                            }
                        }
                    }

                    return Some(HirStatement::Inspect {
                        target,
                        inspect_type: InspectType::Tallying {
                            tally_var,
                            mode,
                            search,
                            before_initial,
                            after_initial,
                        },
                    });
                }
                "REPLACING" => {
                    i += 1;
                    let mut mode = InspectReplaceMode::All;
                    let mut search = None;
                    let mut replacement = None;
                    let mut before_initial = None;
                    let mut after_initial = None;

                    while i < tokens.len() {
                        let u = tokens[i].1.to_ascii_uppercase();
                        match u.as_str() {
                            "ALL" => { mode = InspectReplaceMode::All; i += 1; }
                            "LEADING" => { mode = InspectReplaceMode::Leading; i += 1; }
                            "FIRST" => { mode = InspectReplaceMode::First; i += 1; }
                            "CHARACTERS" => { mode = InspectReplaceMode::Characters; i += 1; }
                            "BY" => {
                                i += 1;
                                if i < tokens.len() {
                                    replacement = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                    i += 1;
                                }
                            }
                            "BEFORE" => {
                                i += 1;
                                if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("INITIAL") {
                                    i += 1;
                                }
                                if i < tokens.len() {
                                    before_initial = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                    i += 1;
                                }
                            }
                            "AFTER" => {
                                i += 1;
                                if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("INITIAL") {
                                    i += 1;
                                }
                                if i < tokens.len() {
                                    after_initial = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                    i += 1;
                                }
                            }
                            _ => {
                                // This should be the search pattern
                                if search.is_none() && mode != InspectReplaceMode::Characters {
                                    search = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                }
                                i += 1;
                            }
                        }
                    }

                    let repl = replacement?;
                    return Some(HirStatement::Inspect {
                        target,
                        inspect_type: InspectType::Replacing {
                            mode,
                            search,
                            replacement: repl,
                            before_initial,
                            after_initial,
                        },
                    });
                }
                "CONVERTING" => {
                    i += 1;
                    // CONVERTING from BY to [BEFORE|AFTER INITIAL ...]
                    if i >= tokens.len() { return None; }
                    let from = self.token_to_expr(tokens[i].0, &tokens[i].1);
                    i += 1;

                    // Skip BY
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("BY") {
                        i += 1;
                    }
                    // Skip TO (some dialects use TO)
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("TO") {
                        i += 1;
                    }

                    if i >= tokens.len() { return None; }
                    let to = self.token_to_expr(tokens[i].0, &tokens[i].1);
                    i += 1;

                    let mut before_initial = None;
                    let mut after_initial = None;

                    while i < tokens.len() {
                        let u = tokens[i].1.to_ascii_uppercase();
                        match u.as_str() {
                            "BEFORE" => {
                                i += 1;
                                if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("INITIAL") {
                                    i += 1;
                                }
                                if i < tokens.len() {
                                    before_initial = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                    i += 1;
                                }
                            }
                            "AFTER" => {
                                i += 1;
                                if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("INITIAL") {
                                    i += 1;
                                }
                                if i < tokens.len() {
                                    after_initial = Some(self.token_to_expr(tokens[i].0, &tokens[i].1));
                                    i += 1;
                                }
                            }
                            _ => { i += 1; }
                        }
                    }

                    return Some(HirStatement::Inspect {
                        target,
                        inspect_type: InspectType::Converting {
                            from,
                            to,
                            before_initial,
                            after_initial,
                        },
                    });
                }
                _ => { i += 1; }
            }
        }

        None
    }

    fn lower_unstring_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_all_tokens(node);
        let mut i = 0;

        // Skip UNSTRING keyword
        if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("UNSTRING") {
            i += 1;
        }

        // Get the source identifier
        if i >= tokens.len() { return None; }
        let source = self.make_data_ref(&tokens[i].1);
        i += 1;

        let mut delimiters = Vec::new();
        let mut targets: Vec<UnstringTarget> = Vec::new();
        let mut pointer = None;
        let mut tallying = None;

        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();

            match upper.as_str() {
                "DELIMITED" => {
                    i += 1;
                    // Skip BY
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("BY") {
                        i += 1;
                    }
                    // Parse delimiters: [ALL] delim [OR [ALL] delim]...
                    loop {
                        if i >= tokens.len() { break; }
                        let u = tokens[i].1.to_ascii_uppercase();
                        if matches!(u.as_str(), "INTO" | "WITH" | "POINTER" | "TALLYING"
                            | "ON" | "NOT" | "END-UNSTRING" | "OVERFLOW") {
                            break;
                        }
                        if u == "OR" {
                            i += 1;
                            continue;
                        }
                        let mut all = false;
                        if u == "ALL" {
                            all = true;
                            i += 1;
                            if i >= tokens.len() { break; }
                        }
                        let value = self.token_to_expr(tokens[i].0, &tokens[i].1);
                        delimiters.push(UnstringDelimiter { value, all });
                        i += 1;
                        // Check for OR
                        if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("OR") {
                            continue;
                        }
                        break;
                    }
                }
                "INTO" => {
                    i += 1;
                    // Parse targets: identifier [DELIMITER IN identifier] [COUNT IN identifier]
                    while i < tokens.len() {
                        let u = tokens[i].1.to_ascii_uppercase();
                        if matches!(u.as_str(), "WITH" | "POINTER" | "TALLYING"
                            | "ON" | "NOT" | "END-UNSTRING" | "OVERFLOW") {
                            break;
                        }
                        if u == "DELIMITER" {
                            i += 1;
                            // Skip IN
                            if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("IN") {
                                i += 1;
                            }
                            if i < tokens.len() {
                                if let Some(last) = targets.last_mut() {
                                    last.delimiter_in = Some(self.make_data_ref(&tokens[i].1));
                                }
                                i += 1;
                            }
                            continue;
                        }
                        if u == "COUNT" {
                            i += 1;
                            // Skip IN
                            if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("IN") {
                                i += 1;
                            }
                            if i < tokens.len() {
                                if let Some(last) = targets.last_mut() {
                                    last.count_in = Some(self.make_data_ref(&tokens[i].1));
                                }
                                i += 1;
                            }
                            continue;
                        }
                        // Skip IN keyword that may appear
                        if u == "IN" {
                            i += 1;
                            continue;
                        }
                        // This is a target identifier
                        let tgt = self.make_data_ref(&tokens[i].1);
                        targets.push(UnstringTarget {
                            target: tgt,
                            delimiter_in: None,
                            count_in: None,
                        });
                        i += 1;
                    }
                }
                "WITH" => {
                    i += 1;
                    // WITH POINTER
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("POINTER") {
                        i += 1;
                    }
                    if i < tokens.len() {
                        pointer = Some(self.make_data_ref(&tokens[i].1));
                        i += 1;
                    }
                }
                "POINTER" => {
                    i += 1;
                    if i < tokens.len() {
                        pointer = Some(self.make_data_ref(&tokens[i].1));
                        i += 1;
                    }
                }
                "TALLYING" => {
                    i += 1;
                    // Skip IN
                    if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("IN") {
                        i += 1;
                    }
                    if i < tokens.len() {
                        tallying = Some(self.make_data_ref(&tokens[i].1));
                        i += 1;
                    }
                }
                "END-UNSTRING" | "ON" | "NOT" | "OVERFLOW" => {
                    break;
                }
                _ => {
                    i += 1;
                }
            }
        }

        if targets.is_empty() {
            return None;
        }

        Some(HirStatement::Unstring {
            source,
            delimiters,
            targets,
            pointer,
            tallying,
        })
    }

    fn lower_call_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // CALL "program-name" USING arg1 arg2 ...
        let tokens = self.collect_all_tokens(node);
        let mut i = 0;

        // Skip CALL keyword
        if i < tokens.len() && tokens[i].1.eq_ignore_ascii_case("CALL") {
            i += 1;
        }

        // Get program name (string literal or identifier)
        if i >= tokens.len() { return None; }
        let program_expr = self.token_to_expr(tokens[i].0, &tokens[i].1);
        i += 1;

        // Parse USING clause
        let mut using_args = Vec::new();
        let mut mode = CallArgMode::ByReference; // default

        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            match upper.as_str() {
                "USING" | "BY" => { i += 1; continue; }
                "REFERENCE" => { mode = CallArgMode::ByReference; i += 1; continue; }
                "CONTENT" => { mode = CallArgMode::ByContent; i += 1; continue; }
                "VALUE" => { mode = CallArgMode::ByValue; i += 1; continue; }
                "RETURNING" | "END-CALL" | "ON" | "NOT" => break,
                _ => {
                    let expr = self.token_to_expr(tokens[i].0, &tokens[i].1);
                    using_args.push(CallArg { mode, value: expr });
                    i += 1;
                }
            }
        }

        Some(HirStatement::Call {
            program: program_expr,
            using: using_args,
            returning: None,
        })
    }

    fn lower_go_to_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // GO TO paragraph-name
        let tokens = self.collect_all_tokens(node);
        // Skip GO and TO keywords, then find the target name
        let mut target_name = None;
        for (kind, text) in &tokens {
            if *kind == SyntaxKind::PERIOD {
                continue;
            }
            let upper = text.to_ascii_uppercase();
            if upper == "GO" || upper == "TO" {
                continue;
            }
            // First non-keyword token is the target paragraph name
            target_name = Some(upper);
            break;
        }

        let target_str = target_name?;
        let target = self.interner.intern(&target_str);
        Some(HirStatement::GoTo { target })
    }

    fn lower_accept_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // ACCEPT identifier [FROM DATE | FROM DAY | FROM TIME | FROM DAY-OF-WEEK]
        let tokens = self.collect_all_tokens(node);

        let mut target_name = None;
        let mut source = AcceptSource::Console;
        let mut i = 0;

        // Skip ACCEPT keyword
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            if upper == "ACCEPT" {
                i += 1;
                break;
            }
            i += 1;
        }

        // Next non-period token is the target identifier
        while i < tokens.len() {
            if tokens[i].0 == SyntaxKind::PERIOD {
                i += 1;
                continue;
            }
            target_name = Some(tokens[i].1.to_ascii_uppercase());
            i += 1;
            break;
        }

        // Check for FROM clause
        while i < tokens.len() {
            let upper = tokens[i].1.to_ascii_uppercase();
            if upper == "FROM" {
                i += 1;
                // Next token is the source
                if i < tokens.len() {
                    let src_upper = tokens[i].1.to_ascii_uppercase();
                    match src_upper.as_str() {
                        "DATE" => source = AcceptSource::Date,
                        "DAY" => source = AcceptSource::Day,
                        "TIME" => source = AcceptSource::Time,
                        "DAY-OF-WEEK" => source = AcceptSource::DayOfWeek,
                        _ => {}
                    }
                }
                break;
            }
            i += 1;
        }

        let name_str = target_name?;
        let name = self.interner.intern(&name_str);
        Some(HirStatement::Accept {
            target: HirDataRef {
                name,
                qualifiers: Vec::new(),
                subscripts: Vec::new(),
                ref_mod: None,
                resolved: None,
            },
            source,
        })
    }

    fn lower_exit_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> HirStatement {
        // EXIT [PROGRAM | SECTION | PARAGRAPH]
        let tokens = self.collect_all_tokens(node);
        for (_, text) in &tokens {
            let upper = text.to_ascii_uppercase();
            if upper == "PROGRAM" {
                return HirStatement::ExitProgram;
            }
            if upper == "SECTION" {
                return HirStatement::ExitSection;
            }
            if upper == "PARAGRAPH" {
                return HirStatement::ExitParagraph;
            }
        }
        HirStatement::ExitParagraph
    }

    fn lower_if_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // The parser creates IF_STMT containing:
        //   Word("IF"), CONDITION_EXPR node, Word("THEN")?,
        //   statement nodes (then branch), Word("ELSE")?,
        //   statement nodes (else branch), Word("END-IF")
        let mut condition = None;
        let mut then_stmts = Vec::new();
        let mut else_stmts = Vec::new();
        let mut in_else = false;

        for el in node.children_with_tokens() {
            if let Some(tok) = el.as_token() {
                let kind = tok.kind();
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                    || kind == SyntaxKind::COMMENT || kind == SyntaxKind::PERIOD
                {
                    continue;
                }
                let text_upper = tok.text().to_ascii_uppercase();
                if text_upper == "ELSE" {
                    in_else = true;
                }
                // IF, THEN, END-IF — just skip
            } else if let Some(child) = el.as_node() {
                if child.kind() == SyntaxKind::CONDITION_EXPR {
                    // Extract condition tokens from the CONDITION_EXPR node
                    let cond_tokens = self.collect_all_tokens(&child);
                    condition = self.parse_condition_tokens(&cond_tokens);
                } else {
                    // Statement node
                    let mut stmts = Vec::new();
                    self.lower_child_statement(&child, &mut stmts);
                    if in_else {
                        else_stmts.extend(stmts);
                    } else {
                        then_stmts.extend(stmts);
                    }
                }
            }
        }

        Some(HirStatement::If {
            condition: condition.unwrap_or(HirExpr::Literal(LiteralValue::Integer(1))),
            then_branch: then_stmts,
            else_branch: if else_stmts.is_empty() { None } else { Some(else_stmts) },
        })
    }

    fn lower_evaluate_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // The parser creates EVALUATE_STMT with all children flattened:
        //   EVALUATE <subject> WHEN <val> <stmt-nodes>... WHEN OTHER <stmt-nodes>... END-EVALUATE
        //
        // Walk child tokens/nodes to find the subject, then build WhenClauses.

        // Phase 1: collect flat tokens to find subject and WHEN positions
        let tokens = self.collect_all_tokens(node);
        if tokens.is_empty() {
            return None;
        }

        // Skip "EVALUATE" keyword
        let mut idx = 0;
        if idx < tokens.len() && tokens[idx].1.to_ascii_uppercase() == "EVALUATE" {
            idx += 1;
        }

        // Collect subject tokens until "WHEN"
        let mut subject_tokens = Vec::new();
        while idx < tokens.len() {
            let upper = tokens[idx].1.to_ascii_uppercase();
            if upper == "WHEN" || upper == "END-EVALUATE" {
                break;
            }
            subject_tokens.push(tokens[idx].clone());
            idx += 1;
        }

        // Parse subject expression
        let is_evaluate_true = subject_tokens.len() == 1
            && subject_tokens[0].1.eq_ignore_ascii_case("TRUE");
        let is_evaluate_false = subject_tokens.len() == 1
            && subject_tokens[0].1.eq_ignore_ascii_case("FALSE");
        let subject = if subject_tokens.len() >= 1 {
            self.token_to_expr(subject_tokens[0].0, &subject_tokens[0].1)
        } else {
            return None;
        };

        // Phase 2: walk CST children directly to build WHEN clauses with
        // proper statement bodies. We track state to know which WHEN we are in.
        let mut whens: Vec<WhenClause> = Vec::new();
        let mut current_conditions: Vec<HirExpr> = Vec::new();
        let mut current_stmts: Vec<HirStatement> = Vec::new();
        let mut in_when = false;
        let mut seen_evaluate = false;
        let mut past_subject = false;
        let mut need_condition_value = false;
        let mut in_when_cond_tokens: Option<Vec<(SyntaxKind, String)>> = None;

        for el in node.children_with_tokens() {
            if let Some(tok) = el.as_token() {
                let kind = tok.kind();
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                    || kind == SyntaxKind::COMMENT || kind == SyntaxKind::PERIOD
                {
                    continue;
                }
                let text_upper = tok.text().to_ascii_uppercase();

                if !seen_evaluate && text_upper == "EVALUATE" {
                    seen_evaluate = true;
                    continue;
                }
                if text_upper == "END-EVALUATE" {
                    break;
                }
                if text_upper == "WHEN" {
                    // Flush any pending EVALUATE TRUE/FALSE condition tokens
                    if let Some(cond_toks) = in_when_cond_tokens.take() {
                        if !cond_toks.is_empty() {
                            if let Some(cond_expr) = self.parse_condition_tokens(&cond_toks) {
                                let final_cond = if is_evaluate_false {
                                    HirExpr::Condition(Box::new(HirCondition::Not(
                                        Box::new(match cond_expr {
                                            HirExpr::Condition(c) => *c,
                                            _ => HirCondition::Comparison {
                                                left: cond_expr, op: BinaryOp::Ne, right: HirExpr::Literal(LiteralValue::Integer(0)),
                                            },
                                        })
                                    )))
                                } else {
                                    cond_expr
                                };
                                current_conditions.push(final_cond);
                            }
                        }
                    }
                    // Save previous WHEN clause if any
                    if in_when {
                        whens.push(WhenClause {
                            conditions: std::mem::take(&mut current_conditions),
                            statements: std::mem::take(&mut current_stmts),
                        });
                    }
                    past_subject = true;
                    in_when = true;
                    need_condition_value = true;
                    continue;
                }
                if need_condition_value {
                    if text_upper == "OTHER" {
                        need_condition_value = false;
                        // WHEN OTHER: empty conditions vector signals default
                        continue;
                    }
                    if is_evaluate_true || is_evaluate_false {
                        // EVALUATE TRUE/FALSE: collect all tokens until
                        // the next statement node, then parse as condition.
                        // We accumulate condition tokens into a buffer and
                        // defer parsing until we see the first statement node.
                        if !in_when_cond_tokens.is_some() {
                            in_when_cond_tokens = Some(Vec::new());
                        }
                        if let Some(ref mut cond_toks) = in_when_cond_tokens {
                            cond_toks.push((kind, tok.text().to_string()));
                        }
                        continue;
                    }
                    need_condition_value = false;
                    // This is the WHEN value — build an equality comparison
                    let cond_expr = self.token_to_expr(kind, tok.text());
                    current_conditions.push(HirExpr::Condition(Box::new(
                        HirCondition::Comparison {
                            left: subject.clone(),
                            op: BinaryOp::Eq,
                            right: cond_expr,
                        },
                    )));
                    continue;
                }
                if !past_subject {
                    // Subject tokens — skip
                    continue;
                }
                // Other tokens inside WHEN body — skip (statements are child nodes)
            } else if let Some(child) = el.as_node() {
                if !past_subject {
                    // Could be part of subject; skip
                    continue;
                }
                // Flush any pending EVALUATE TRUE/FALSE condition tokens
                if let Some(cond_toks) = in_when_cond_tokens.take() {
                    if !cond_toks.is_empty() {
                        if let Some(cond_expr) = self.parse_condition_tokens(&cond_toks) {
                            let final_cond = if is_evaluate_false {
                                HirExpr::Condition(Box::new(HirCondition::Not(
                                    Box::new(match cond_expr {
                                        HirExpr::Condition(c) => *c,
                                        _ => HirCondition::Comparison {
                                            left: cond_expr, op: BinaryOp::Ne, right: HirExpr::Literal(LiteralValue::Integer(0)),
                                        },
                                    })
                                )))
                            } else {
                                cond_expr
                            };
                            current_conditions.push(final_cond);
                        }
                        need_condition_value = false;
                    }
                }
                if in_when {
                    let mut child_stmts = Vec::new();
                    self.lower_child_statement(&child, &mut child_stmts);
                    current_stmts.extend(child_stmts);
                }
            }
        }

        // Flush any pending EVALUATE TRUE/FALSE condition tokens
        if let Some(cond_toks) = in_when_cond_tokens.take() {
            if !cond_toks.is_empty() {
                if let Some(cond_expr) = self.parse_condition_tokens(&cond_toks) {
                    let final_cond = if is_evaluate_false {
                        HirExpr::Condition(Box::new(HirCondition::Not(
                            Box::new(match cond_expr {
                                HirExpr::Condition(c) => *c,
                                _ => HirCondition::Comparison {
                                    left: cond_expr, op: BinaryOp::Ne, right: HirExpr::Literal(LiteralValue::Integer(0)),
                                },
                            })
                        )))
                    } else {
                        cond_expr
                    };
                    current_conditions.push(final_cond);
                }
            }
        }
        // Push the last WHEN clause
        if in_when {
            whens.push(WhenClause {
                conditions: std::mem::take(&mut current_conditions),
                statements: std::mem::take(&mut current_stmts),
            });
        }

        Some(HirStatement::Evaluate { subject, whens })
    }

    fn lower_search_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // SEARCH table-name AT END stmts... WHEN condition stmts... END-SEARCH
        let tokens = self.collect_all_tokens(node);
        if tokens.is_empty() {
            return None;
        }

        let mut idx = 0;
        // Skip "SEARCH" keyword
        if idx < tokens.len() && tokens[idx].1.to_ascii_uppercase() == "SEARCH" {
            idx += 1;
        }

        // Get the table name
        if idx >= tokens.len() {
            return None;
        }
        let table_name_str = tokens[idx].1.to_ascii_uppercase();
        let table_ref = self.make_data_ref(&table_name_str);

        // Walk CST children to build AT END / WHEN clauses with proper statement bodies
        let mut at_end_stmts: Vec<HirStatement> = Vec::new();
        let mut search_whens: Vec<SearchWhen> = Vec::new();
        let mut in_at_end = false;
        let mut in_when = false;
        let mut current_when_cond: Option<HirExpr> = None;
        let mut current_when_stmts: Vec<HirStatement> = Vec::new();
        let mut seen_search = false;
        let mut seen_table = false;
        let mut collecting_condition = false;
        let mut condition_tokens: Vec<(SyntaxKind, String)> = Vec::new();

        for el in node.children_with_tokens() {
            if let Some(tok) = el.as_token() {
                let kind = tok.kind();
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE
                    || kind == SyntaxKind::COMMENT || kind == SyntaxKind::PERIOD
                {
                    continue;
                }
                let text_upper = tok.text().to_ascii_uppercase();

                if !seen_search && text_upper == "SEARCH" {
                    seen_search = true;
                    continue;
                }
                if seen_search && !seen_table {
                    seen_table = true;
                    continue;
                }
                if text_upper == "END-SEARCH" {
                    break;
                }
                if text_upper == "AT" {
                    if in_when {
                        if let Some(cond) = current_when_cond.take() {
                            search_whens.push(SearchWhen {
                                condition: cond,
                                body: std::mem::take(&mut current_when_stmts),
                            });
                        }
                        in_when = false;
                    }
                    in_at_end = true;
                    continue;
                }
                if text_upper == "END" && in_at_end {
                    continue;
                }
                if text_upper == "WHEN" {
                    if in_when {
                        if let Some(cond) = current_when_cond.take() {
                            search_whens.push(SearchWhen {
                                condition: cond,
                                body: std::mem::take(&mut current_when_stmts),
                            });
                        }
                    }
                    in_at_end = false;
                    in_when = true;
                    collecting_condition = true;
                    condition_tokens.clear();
                    continue;
                }
                if collecting_condition {
                    condition_tokens.push((kind, tok.text().to_string()));
                    continue;
                }
            } else if let Some(child) = el.as_node() {
                if !seen_table {
                    continue;
                }
                if collecting_condition && !condition_tokens.is_empty() {
                    current_when_cond = self.parse_condition_tokens(&condition_tokens);
                    condition_tokens.clear();
                    collecting_condition = false;
                }
                if in_at_end {
                    let mut child_stmts = Vec::new();
                    self.lower_child_statement(&child, &mut child_stmts);
                    at_end_stmts.extend(child_stmts);
                } else if in_when {
                    let mut child_stmts = Vec::new();
                    self.lower_child_statement(&child, &mut child_stmts);
                    current_when_stmts.extend(child_stmts);
                }
            }
        }

        // Push the last WHEN clause
        if in_when {
            if collecting_condition && !condition_tokens.is_empty() {
                current_when_cond = self.parse_condition_tokens(&condition_tokens);
            }
            if let Some(cond) = current_when_cond.take() {
                search_whens.push(SearchWhen {
                    condition: cond,
                    body: std::mem::take(&mut current_when_stmts),
                });
            }
        }

        Some(HirStatement::Search {
            table: table_ref,
            at_end: at_end_stmts,
            whens: search_whens,
        })
    }

    fn lower_set_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        let tokens = self.collect_all_tokens(node);
        if tokens.len() < 3 {
            return None;
        }

        let mut idx = 0;
        if idx < tokens.len() && tokens[idx].1.to_ascii_uppercase() == "SET" {
            idx += 1;
        }

        if idx >= tokens.len() {
            return None;
        }
        let target = self.make_data_ref(&tokens[idx].1);
        idx += 1;

        if idx >= tokens.len() {
            return None;
        }

        let action_word = tokens[idx].1.to_ascii_uppercase();
        match action_word.as_str() {
            "TO" => {
                idx += 1;
                if idx >= tokens.len() {
                    return None;
                }
                let value = self.token_to_expr(tokens[idx].0, &tokens[idx].1);
                Some(HirStatement::Set {
                    target,
                    action: SetAction::To(value),
                })
            }
            "UP" => {
                idx += 1;
                if idx < tokens.len() && tokens[idx].1.eq_ignore_ascii_case("BY") {
                    idx += 1;
                }
                if idx >= tokens.len() {
                    return None;
                }
                let value = self.token_to_expr(tokens[idx].0, &tokens[idx].1);
                Some(HirStatement::Set {
                    target,
                    action: SetAction::UpBy(value),
                })
            }
            "DOWN" => {
                idx += 1;
                if idx < tokens.len() && tokens[idx].1.eq_ignore_ascii_case("BY") {
                    idx += 1;
                }
                if idx >= tokens.len() {
                    return None;
                }
                let value = self.token_to_expr(tokens[idx].0, &tokens[idx].1);
                Some(HirStatement::Set {
                    target,
                    action: SetAction::DownBy(value),
                })
            }
            _ => None,
        }
    }

    fn lower_child_statement(
        &mut self,
        node: &cobol_ast::SyntaxNode,
        stmts: &mut Vec<HirStatement>,
    ) {
        match node.kind() {
            SyntaxKind::DISPLAY_STMT => stmts.push(self.lower_display_stmt(node)),
            SyntaxKind::STOP_STMT => {
                let tokens = self.collect_all_tokens(node);
                if tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("GOBACK")) {
                    stmts.push(HirStatement::GoBack);
                } else {
                    stmts.push(HirStatement::StopRun);
                }
            }
            SyntaxKind::MOVE_STMT => {
                if let Some(s) = self.lower_move_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::ADD_STMT => {
                if let Some(s) = self.lower_add_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::SUBTRACT_STMT => {
                if let Some(s) = self.lower_subtract_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::MULTIPLY_STMT => {
                if let Some(s) = self.lower_multiply_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::DIVIDE_STMT => {
                if let Some(s) = self.lower_divide_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::COMPUTE_STMT => {
                if let Some(s) = self.lower_compute_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::IF_STMT => {
                if let Some(s) = self.lower_if_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::PERFORM_STMT => {
                if let Some(s) = self.lower_perform_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::STRING_STMT => {
                if let Some(s) = self.lower_string_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::CALL_STMT => {
                if let Some(s) = self.lower_call_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::EXIT_STMT => {
                stmts.push(self.lower_exit_stmt(node));
            }
            SyntaxKind::OPEN_STMT => {
                if let Some(s) = self.lower_open_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::CLOSE_STMT => {
                if let Some(s) = self.lower_close_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::WRITE_STMT => {
                if let Some(s) = self.lower_write_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::READ_STMT => {
                if let Some(s) = self.lower_read_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::EVALUATE_STMT => {
                if let Some(s) = self.lower_evaluate_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::GO_TO_STMT => {
                if let Some(s) = self.lower_go_to_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::ACCEPT_STMT => {
                if let Some(s) = self.lower_accept_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::INSPECT_STMT => {
                if let Some(s) = self.lower_inspect_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::UNSTRING_STMT => {
                if let Some(s) = self.lower_unstring_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::INITIALIZE_STMT => {
                if let Some(s) = self.lower_initialize_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::SEARCH_STMT => {
                if let Some(s) = self.lower_search_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::SET_STMT => {
                if let Some(s) = self.lower_set_stmt(node) { stmts.push(s); }
            }
            SyntaxKind::SENTENCE => {
                // Sentence wrapping statements
                for child in node.children() {
                    self.lower_child_statement(&child, stmts);
                }
            }
            _ => {}
        }
    }

    fn parse_condition_tokens(&mut self, tokens: &[(SyntaxKind, String)]) -> Option<HirExpr> {
        // Handle compound conditions with AND/OR by splitting at logical operators.
        // Precedence: AND binds tighter than OR.

        if tokens.len() < 3 {
            // Check for single-token condition name (level 88)
            if tokens.len() == 1 {
                let name = self.interner.intern(&tokens[0].1.to_ascii_uppercase());
                return Some(HirExpr::Condition(Box::new(HirCondition::ConditionName(HirDataRef {
                    name,
                    qualifiers: Vec::new(),
                    subscripts: Vec::new(),
                    ref_mod: None,
                    resolved: None,
                }))));
            }
            return None;
        }

        // Split at OR (lowest precedence) respecting parentheses
        let or_parts = self.split_condition_at_keyword(tokens, "OR");
        if or_parts.len() > 1 {
            let mut result: Option<HirExpr> = None;
            for part in &or_parts {
                if let Some(cond) = self.parse_condition_tokens(part) {
                    result = Some(match result {
                        None => cond,
                        Some(left) => {
                            let left_cond = self.expr_to_condition(left);
                            let right_cond = self.expr_to_condition(cond);
                            HirExpr::Condition(Box::new(HirCondition::Or(
                                Box::new(left_cond),
                                Box::new(right_cond),
                            )))
                        }
                    });
                }
            }
            return result;
        }

        // Split at AND (higher precedence than OR)
        let and_parts = self.split_condition_at_keyword(tokens, "AND");
        if and_parts.len() > 1 {
            let mut result: Option<HirExpr> = None;
            for part in &and_parts {
                if let Some(cond) = self.parse_condition_tokens(part) {
                    result = Some(match result {
                        None => cond,
                        Some(left) => {
                            let left_cond = self.expr_to_condition(left);
                            let right_cond = self.expr_to_condition(cond);
                            HirExpr::Condition(Box::new(HirCondition::And(
                                Box::new(left_cond),
                                Box::new(right_cond),
                            )))
                        }
                    });
                }
            }
            return result;
        }

        // No AND/OR: parse as simple comparison
        self.parse_simple_condition(tokens)
    }

    /// Split a condition token stream at a logical keyword (AND/OR),
    /// respecting parentheses.
    fn split_condition_at_keyword<'b>(
        &self,
        tokens: &'b [(SyntaxKind, String)],
        keyword: &str,
    ) -> Vec<&'b [(SyntaxKind, String)]> {
        let mut parts = Vec::new();
        let mut start = 0;
        let mut paren_depth = 0;
        for (i, (_, t)) in tokens.iter().enumerate() {
            if t == "(" { paren_depth += 1; }
            if t == ")" { paren_depth -= 1; }
            if paren_depth == 0 && t.eq_ignore_ascii_case(keyword) {
                if i > start {
                    parts.push(&tokens[start..i]);
                }
                start = i + 1;
            }
        }
        if start < tokens.len() {
            parts.push(&tokens[start..]);
        }
        parts
    }

    /// Convert an HirExpr to HirCondition (extract from Condition wrapper
    /// or wrap a comparison).
    fn expr_to_condition(&self, expr: HirExpr) -> HirCondition {
        match expr {
            HirExpr::Condition(c) => *c,
            other => HirCondition::Comparison {
                left: other,
                op: BinaryOp::Ne,
                right: HirExpr::Literal(LiteralValue::Integer(0)),
            },
        }
    }

    /// Parse a simple condition (no AND/OR): expr OP expr
    fn parse_simple_condition(&mut self, tokens: &[(SyntaxKind, String)]) -> Option<HirExpr> {
        if tokens.len() < 3 {
            // Single-token condition name
            if tokens.len() == 1 {
                let name = self.interner.intern(&tokens[0].1.to_ascii_uppercase());
                return Some(HirExpr::Condition(Box::new(HirCondition::ConditionName(HirDataRef {
                    name,
                    qualifiers: Vec::new(),
                    subscripts: Vec::new(),
                    ref_mod: None,
                    resolved: None,
                }))));
            }
            return None;
        }

        // Find the comparison operator, skipping tokens inside parentheses
        let mut paren_depth = 0;
        let mut op_idx = None;
        for (i, (_, t)) in tokens.iter().enumerate() {
            if t == "(" { paren_depth += 1; continue; }
            if t == ")" { paren_depth -= 1; continue; }
            if paren_depth > 0 { continue; }

            let u = t.to_ascii_uppercase();
            if matches!(u.as_str(), "=" | ">" | "<" | ">=" | "<=" | "<>"
                | "EQUAL" | "GREATER" | "LESS" | "NOT")
            {
                op_idx = Some(i);
                break;
            }
        }
        let op_idx = op_idx?;

        // Build left-side expression from tokens[0..op_idx]
        // Check for subscript or reference modification: NAME(subscript) or NAME(offset:length)
        let left = if op_idx >= 4
            && tokens[1].1 == "("
            && tokens[op_idx - 1].1 == ")"
        {
            let inner_tokens = &tokens[2..op_idx - 1];
            let has_colon = inner_tokens.iter().any(|(_, t)| t == ":");
            if has_colon {
                // Reference modification: NAME(offset:length)
                let name = self.interner.intern(&tokens[0].1.to_ascii_uppercase());
                let colon_pos = inner_tokens.iter()
                    .position(|(_, t)| t == ":")
                    .unwrap();
                let offset_expr = if colon_pos > 0 {
                    self.token_to_expr(inner_tokens[0].0, &inner_tokens[0].1)
                } else {
                    HirExpr::Literal(LiteralValue::Integer(1))
                };
                let length_expr = if colon_pos + 1 < inner_tokens.len() {
                    Some(Box::new(self.token_to_expr(inner_tokens[colon_pos + 1].0, &inner_tokens[colon_pos + 1].1)))
                } else {
                    None
                };
                HirExpr::DataRef(Box::new(HirDataRef {
                    name,
                    qualifiers: Vec::new(),
                    subscripts: Vec::new(),
                    ref_mod: Some((Box::new(offset_expr), length_expr)),
                    resolved: None,
                }))
            } else {
                // Subscript: NAME(subscript-expr)
                let name = self.interner.intern(&tokens[0].1.to_ascii_uppercase());
                let subscript = if inner_tokens.len() == 1 {
                    self.token_to_expr(inner_tokens[0].0, &inner_tokens[0].1)
                } else {
                    self.token_to_expr(inner_tokens[0].0, &inner_tokens[0].1)
                };
                HirExpr::DataRef(Box::new(HirDataRef {
                    name,
                    qualifiers: Vec::new(),
                    subscripts: vec![subscript],
                    ref_mod: None,
                    resolved: None,
                }))
            }
        } else {
            self.token_to_expr(tokens[0].0, &tokens[0].1)
        };

        // Parse operator (may span multiple tokens like "NOT EQUAL")
        let (op, right_start) = {
            let op_text = tokens[op_idx].1.to_ascii_uppercase();
            match op_text.as_str() {
                "=" | "EQUAL" => (BinaryOp::Eq, op_idx + 1),
                ">" | "GREATER" => {
                    if op_idx + 1 < tokens.len() && tokens[op_idx + 1].1.eq_ignore_ascii_case("THAN") {
                        (BinaryOp::Gt, op_idx + 2)
                    } else {
                        (BinaryOp::Gt, op_idx + 1)
                    }
                }
                "<" | "LESS" => {
                    if op_idx + 1 < tokens.len() && tokens[op_idx + 1].1.eq_ignore_ascii_case("THAN") {
                        (BinaryOp::Lt, op_idx + 2)
                    } else {
                        (BinaryOp::Lt, op_idx + 1)
                    }
                }
                ">=" => (BinaryOp::Ge, op_idx + 1),
                "<=" => (BinaryOp::Le, op_idx + 1),
                "<>" => (BinaryOp::Ne, op_idx + 1),
                "NOT" => {
                    if op_idx + 1 < tokens.len() {
                        let next = tokens[op_idx + 1].1.to_ascii_uppercase();
                        match next.as_str() {
                            "=" | "EQUAL" => (BinaryOp::Ne, op_idx + 2),
                            ">" | "GREATER" => (BinaryOp::Le, op_idx + 2),
                            "<" | "LESS" => (BinaryOp::Ge, op_idx + 2),
                            _ => (BinaryOp::Ne, op_idx + 1),
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        };

        if right_start >= tokens.len() {
            return None;
        }

        let right = self.token_to_expr(tokens[right_start].0, &tokens[right_start].1);

        Some(HirExpr::Condition(Box::new(HirCondition::Comparison {
            left,
            op,
            right,
        })))
    }

    fn lower_perform_stmt(&mut self, node: &cobol_ast::SyntaxNode) -> Option<HirStatement> {
        // Walk the PERFORM_STMT CST node. The structure is:
        //   PERFORM_STMT {
        //     Word("PERFORM"), Word("VARYING"), Word("WS-I"), Word("FROM"),
        //     IntegerLiteral("1"), Word("BY"), IntegerLiteral("1"),
        //     CONDITION_EXPR { ... },   ← UNTIL condition
        //     ADD_STMT { ... },          ← inline body
        //     Word("END-PERFORM")
        //   }
        let tokens = self.collect_tokens(node);

        let has_varying = tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("VARYING"));

        if has_varying {
            let varying_idx = tokens.iter().position(|(_, t)| t.eq_ignore_ascii_case("VARYING"))?;
            let from_idx = tokens.iter().position(|(_, t)| t.eq_ignore_ascii_case("FROM"))?;
            let by_idx = tokens.iter().position(|(_, t)| t.eq_ignore_ascii_case("BY"))?;

            let var_name = &tokens[varying_idx + 1].1;
            let var_ref = self.make_data_ref(var_name);

            let from_expr = self.token_to_expr(tokens[from_idx + 1].0, &tokens[from_idx + 1].1);
            let by_expr = self.token_to_expr(tokens[by_idx + 1].0, &tokens[by_idx + 1].1);

            // Parse AFTER clauses: AFTER var FROM expr BY expr [AFTER ...]
            let mut after_clauses = Vec::new();
            let after_positions: Vec<usize> = tokens.iter().enumerate()
                .filter(|(_, (_, t))| t.eq_ignore_ascii_case("AFTER"))
                .map(|(i, _)| i)
                .collect();

            for &after_pos in &after_positions {
                // AFTER var FROM expr BY expr
                if after_pos + 1 >= tokens.len() { continue; }
                let after_var = self.make_data_ref(&tokens[after_pos + 1].1);
                // Find FROM and BY after this AFTER
                let mut after_from = None;
                let mut after_by = None;
                let mut j = after_pos + 2;
                while j < tokens.len() {
                    let u = tokens[j].1.to_ascii_uppercase();
                    if u == "FROM" && j + 1 < tokens.len() {
                        after_from = Some(self.token_to_expr(tokens[j + 1].0, &tokens[j + 1].1));
                        j += 2;
                    } else if u == "BY" && j + 1 < tokens.len() {
                        after_by = Some(self.token_to_expr(tokens[j + 1].0, &tokens[j + 1].1));
                        j += 2;
                    } else if u == "AFTER" || u == "END-PERFORM" {
                        break;
                    } else {
                        j += 1;
                    }
                }
                after_clauses.push(VaryingClause {
                    identifier: after_var,
                    from: after_from.unwrap_or(HirExpr::Literal(LiteralValue::Integer(1))),
                    by: after_by.unwrap_or(HirExpr::Literal(LiteralValue::Integer(1))),
                    until: HirExpr::Literal(LiteralValue::Integer(0)), // filled below
                    after: Vec::new(),
                });
            }

            // Extract UNTIL conditions from CONDITION_EXPR child nodes.
            // First CONDITION_EXPR is for the outer VARYING, subsequent ones
            // are for each AFTER clause in order.
            let mut condition_exprs = Vec::new();
            let mut inline_stmts = Vec::new();

            for child in node.children() {
                if child.kind() == SyntaxKind::CONDITION_EXPR {
                    let cond_tokens = self.collect_all_tokens(&child);
                    if let Some(cond) = self.parse_condition_tokens(&cond_tokens) {
                        condition_exprs.push(cond);
                    }
                } else {
                    // Statement node — inline body
                    self.lower_child_statement(&child, &mut inline_stmts);
                }
            }

            let until_expr = condition_exprs.first()
                .cloned()
                .unwrap_or(HirExpr::Literal(LiteralValue::Integer(0)));

            // Assign UNTIL conditions to AFTER clauses
            for (i, ac) in after_clauses.iter_mut().enumerate() {
                if i + 1 < condition_exprs.len() {
                    ac.until = condition_exprs[i + 1].clone();
                }
            }

            let body = if inline_stmts.is_empty() { None } else { Some(inline_stmts) };

            Some(HirStatement::Perform(PerformType::Varying {
                target: self.interner.intern("_INLINE"),
                thru: None,
                varying: VaryingClause {
                    identifier: var_ref,
                    from: from_expr,
                    by: by_expr,
                    until: until_expr,
                    after: after_clauses,
                },
                inline_body: body,
            }))
        } else {
            // Non-VARYING: could be TIMES, UNTIL, THRU, or simple out-of-line/inline
            // Skip the initial "PERFORM" token
            let mut idx = 0;
            if idx < tokens.len() && tokens[idx].1.eq_ignore_ascii_case("PERFORM") {
                idx += 1;
            }

            // Check if we have inline body (END-PERFORM present or statement children)
            let has_end_perform = tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("END-PERFORM"));
            let has_times = tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("TIMES"));
            let has_until = tokens.iter().any(|(_, t)| t.eq_ignore_ascii_case("UNTIL"));

            if has_times && idx < tokens.len() {
                // Check if this is inline PERFORM n TIMES (first token is a literal)
                let first_is_literal = tokens[idx].0 == SyntaxKind::INTEGER_LITERAL
                    || tokens[idx].0 == SyntaxKind::DECIMAL_LITERAL;
                let inline_times = first_is_literal && has_end_perform;

                if inline_times {
                    // Inline PERFORM n TIMES ... END-PERFORM
                    let times_expr = self.token_to_expr(tokens[idx].0, &tokens[idx].1);
                    // Collect inline body statements
                    let mut inline_stmts = Vec::new();
                    for child in node.children() {
                        self.lower_child_statement(&child, &mut inline_stmts);
                    }
                    Some(HirStatement::Perform(PerformType::InlineTimes {
                        times: times_expr,
                        statements: inline_stmts,
                    }))
                } else {
                    // Out-of-line PERFORM target [THRU thru] expr TIMES
                    let target_name = tokens[idx].1.to_ascii_uppercase();
                    let target = self.interner.intern(&target_name);
                    idx += 1;

                    // Optional THRU
                    let mut thru = None;
                    if idx < tokens.len() && (tokens[idx].1.eq_ignore_ascii_case("THRU")
                        || tokens[idx].1.eq_ignore_ascii_case("THROUGH")) {
                        idx += 1;
                        if idx < tokens.len() {
                            let thru_name = tokens[idx].1.to_ascii_uppercase();
                            thru = Some(self.interner.intern(&thru_name));
                            idx += 1;
                        }
                    }

                    // Parse the TIMES expression (number or data reference before TIMES keyword)
                    let times_expr = if idx < tokens.len() {
                        self.token_to_expr(tokens[idx].0, &tokens[idx].1)
                    } else {
                        HirExpr::Literal(LiteralValue::Integer(1))
                    };

                    Some(HirStatement::Perform(PerformType::Times {
                        target,
                        thru,
                        times: times_expr,
                    }))
                }
            } else if has_until && idx < tokens.len() {
                // Check if this is inline PERFORM UNTIL (first token is UNTIL)
                let inline_until = tokens[idx].1.eq_ignore_ascii_case("UNTIL") && has_end_perform;

                if inline_until {
                    // Inline PERFORM UNTIL condition ... END-PERFORM
                    // Parse condition from CONDITION_EXPR child
                    let mut condition = HirExpr::Literal(LiteralValue::Integer(0));
                    let mut inline_stmts = Vec::new();
                    for child in node.children() {
                        if child.kind() == SyntaxKind::CONDITION_EXPR {
                            let cond_tokens = self.collect_all_tokens(&child);
                            if let Some(cond) = self.parse_condition_tokens(&cond_tokens) {
                                condition = cond;
                            }
                        } else {
                            self.lower_child_statement(&child, &mut inline_stmts);
                        }
                    }
                    // Also try from flat tokens (skip UNTIL keyword)
                    let mut cond_idx = idx + 1;
                    if cond_idx < tokens.len() {
                        // Collect tokens until END-PERFORM or a statement keyword
                        let cond_end = tokens[cond_idx..].iter()
                            .position(|(_, t)| t.eq_ignore_ascii_case("END-PERFORM"))
                            .map(|p| cond_idx + p)
                            .unwrap_or(tokens.len());
                        let cond_tokens: Vec<(SyntaxKind, String)> = tokens[cond_idx..cond_end].to_vec();
                        if let Some(cond) = self.parse_condition_tokens(&cond_tokens) {
                            condition = cond;
                        }
                    }
                    Some(HirStatement::Perform(PerformType::InlineUntil {
                        condition,
                        test_before: true,
                        statements: inline_stmts,
                    }))
                } else {
                    // Out-of-line PERFORM target [THRU thru] [WITH TEST BEFORE|AFTER] UNTIL condition
                    let target_name = tokens[idx].1.to_ascii_uppercase();
                    let target = self.interner.intern(&target_name);
                    idx += 1;

                    // Optional THRU
                    let mut thru = None;
                    if idx < tokens.len() && (tokens[idx].1.eq_ignore_ascii_case("THRU")
                        || tokens[idx].1.eq_ignore_ascii_case("THROUGH")) {
                        idx += 1;
                        if idx < tokens.len() {
                            let thru_name = tokens[idx].1.to_ascii_uppercase();
                            thru = Some(self.interner.intern(&thru_name));
                            idx += 1;
                        }
                    }

                    // Optional WITH TEST BEFORE|AFTER
                    let mut test_before = true;
                    if idx < tokens.len() && tokens[idx].1.eq_ignore_ascii_case("WITH") {
                        idx += 1;
                        if idx < tokens.len() && tokens[idx].1.eq_ignore_ascii_case("TEST") {
                            idx += 1;
                        }
                        if idx < tokens.len() {
                            if tokens[idx].1.eq_ignore_ascii_case("AFTER") {
                                test_before = false;
                            }
                            idx += 1;
                        }
                    }

                    // Skip UNTIL keyword
                    if idx < tokens.len() && tokens[idx].1.eq_ignore_ascii_case("UNTIL") {
                        idx += 1;
                    }

                    // Parse condition from remaining tokens or CONDITION_EXPR child
                    let mut condition = HirExpr::Literal(LiteralValue::Integer(0));
                    for child in node.children() {
                        if child.kind() == SyntaxKind::CONDITION_EXPR {
                            let cond_tokens = self.collect_all_tokens(&child);
                            if let Some(cond) = self.parse_condition_tokens(&cond_tokens) {
                                condition = cond;
                            }
                        }
                    }
                    // Also try to parse condition from the remaining flat tokens
                    if idx < tokens.len() {
                        let cond_tokens: Vec<(SyntaxKind, String)> = tokens[idx..].to_vec();
                        if let Some(cond) = self.parse_condition_tokens(&cond_tokens) {
                            condition = cond;
                        }
                    }

                    Some(HirStatement::Perform(PerformType::Until {
                        target,
                        thru,
                        condition,
                        test_before,
                    }))
                }
            } else if idx < tokens.len() && !has_end_perform {
                // Simple out-of-line PERFORM: PERFORM target [THRU thru]
                let target_name = tokens[idx].1.to_ascii_uppercase();
                let target = self.interner.intern(&target_name);
                idx += 1;

                let mut thru = None;
                if idx < tokens.len() && (tokens[idx].1.eq_ignore_ascii_case("THRU")
                    || tokens[idx].1.eq_ignore_ascii_case("THROUGH")) {
                    idx += 1;
                    if idx < tokens.len() {
                        let thru_name = tokens[idx].1.to_ascii_uppercase();
                        thru = Some(self.interner.intern(&thru_name));
                    }
                }

                Some(HirStatement::Perform(PerformType::OutOfLine {
                    target,
                    thru,
                }))
            } else if has_end_perform {
                // Inline PERFORM: PERFORM ... END-PERFORM
                let mut inline_stmts = Vec::new();
                for child in node.children() {
                    self.lower_child_statement(&child, &mut inline_stmts);
                }
                Some(HirStatement::Perform(PerformType::Inline {
                    statements: inline_stmts,
                }))
            } else {
                None
            }
        }
    }

    #[allow(unused_mut)]
    fn finish(mut self) -> HirModule {
        let program_name = match self.program_name {
            Some(name) => name,
            None => self.interner.intern("UNNAMED"),
        };

        HirModule {
            program_name,
            data_items: self.data_items,
            working_storage: self.working_storage,
            linkage_items: self.linkage_items,
            using_params: self.using_params,
            file_items: self.file_items,
            paragraphs: self.paragraphs,
            sections: Vec::new(),
            diagnostics: self.diagnostics,
            condition_names: self.condition_names,
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn picture_category_equality() {
        assert_eq!(PictureCategory::Numeric, PictureCategory::Numeric);
        assert_ne!(PictureCategory::Numeric, PictureCategory::Alphabetic);
        assert_ne!(
            PictureCategory::Alphanumeric,
            PictureCategory::AlphanumericEdited
        );
    }

    #[test]
    fn data_encoding_equality() {
        assert_eq!(DataEncoding::Display, DataEncoding::Display);
        assert_ne!(DataEncoding::Binary, DataEncoding::PackedDecimal);
        assert_ne!(DataEncoding::FloatSingle, DataEncoding::FloatDouble);
    }

    #[test]
    fn picture_category_is_copy() {
        let a = PictureCategory::Numeric;
        let b = a; // Copy
        assert_eq!(a, b);
    }

    #[test]
    fn data_encoding_is_copy() {
        let a = DataEncoding::Binary;
        let b = a; // Copy
        assert_eq!(a, b);
    }

    #[test]
    fn usage_type_variants() {
        let usages = [
            UsageType::Display,
            UsageType::Comp,
            UsageType::Comp1,
            UsageType::Comp2,
            UsageType::Comp3,
            UsageType::Comp4,
            UsageType::Comp5,
            UsageType::Binary,
            UsageType::PackedDecimal,
            UsageType::Index,
            UsageType::Pointer,
            UsageType::FunctionPointer,
        ];
        // All variants should be distinct.
        for (i, a) in usages.iter().enumerate() {
            for (j, b) in usages.iter().enumerate() {
                if i == j {
                    assert_eq!(a, b);
                } else {
                    assert_ne!(a, b);
                }
            }
        }
    }

    #[test]
    fn sign_position_variants() {
        assert_ne!(SignPosition::Leading, SignPosition::Trailing);
        assert_ne!(SignPosition::LeadingSeparate, SignPosition::TrailingSeparate);
        assert_ne!(SignPosition::None, SignPosition::Leading);
    }

    #[test]
    fn storage_descriptor_construction() {
        let sd = StorageDescriptor {
            byte_size: 5,
            encoding: DataEncoding::Display,
            picture: Some(PictureType {
                category: PictureCategory::Numeric,
                size: 5,
                scale: 0,
                sign: SignPosition::None,
                pic_string: "9(5)".to_string(),
            }),
            usage: UsageType::Display,
        };
        assert_eq!(sd.byte_size, 5);
        assert_eq!(sd.encoding, DataEncoding::Display);
        assert!(sd.picture.is_some());
    }

    #[test]
    fn initial_value_variants() {
        let v1 = InitialValue::Zero;
        let v2 = InitialValue::Space;
        let v3 = InitialValue::Numeric(42, 0);
        let v4 = InitialValue::String_("HELLO".to_string());
        let v5 = InitialValue::All("*".to_string());
        assert_ne!(v1, v2);
        assert_ne!(v3, v4);
        assert_eq!(v5, InitialValue::All("*".to_string()));
    }

    #[test]
    fn parse_picture_numeric() {
        let result = parse_picture("9(5)V99").unwrap();
        assert_eq!(result.category, PictureCategory::Numeric);
        assert_eq!(result.size, 7);
        assert_eq!(result.scale, 2);
    }

    #[test]
    fn parse_picture_alphanumeric() {
        let result = parse_picture("X(20)").unwrap();
        assert_eq!(result.category, PictureCategory::Alphanumeric);
        assert_eq!(result.size, 20);
        assert_eq!(result.scale, 0);
    }

    #[test]
    fn parse_picture_signed() {
        let result = parse_picture("S9(7)V9(2)").unwrap();
        assert_eq!(result.category, PictureCategory::Numeric);
        assert_eq!(result.size, 9);
        assert_eq!(result.scale, 2);
        assert_eq!(result.sign, SignPosition::Leading);
    }

    #[test]
    fn parse_picture_empty_returns_error() {
        let result = parse_picture("");
        assert!(result.is_err());
    }

    #[test]
    fn file_organization_variants() {
        assert_eq!(FileOrganization::Sequential, FileOrganization::Sequential);
        assert_ne!(FileOrganization::Sequential, FileOrganization::Indexed);
    }

    #[test]
    fn access_mode_variants() {
        assert_eq!(AccessMode::Sequential, AccessMode::Sequential);
        assert_ne!(AccessMode::Random, AccessMode::Dynamic);
    }

    #[test]
    fn binary_op_copy() {
        let op = BinaryOp::Add;
        let op2 = op;
        assert_eq!(op, op2);
    }

    #[test]
    fn diagnostic_severity_variants() {
        assert_ne!(DiagnosticSeverity::Error, DiagnosticSeverity::Warning);
        assert_ne!(DiagnosticSeverity::Info, DiagnosticSeverity::Hint);
    }

    #[test]
    fn call_arg_mode_copy() {
        let mode = CallArgMode::ByReference;
        let mode2 = mode;
        assert_eq!(mode, mode2);
    }

    // ------------------------------------------------------------------
    // USAGE clause lowering helpers (unit tests)
    // ------------------------------------------------------------------

    #[test]
    fn compute_byte_size_display() {
        let pic = parse_picture("9(5)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Display, &pic), 5);
    }

    #[test]
    fn compute_byte_size_comp_small() {
        // PIC 9(4) COMP -> 2 bytes (i16)
        let pic = parse_picture("9(4)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp, &pic), 2);
    }

    #[test]
    fn compute_byte_size_comp_medium() {
        // PIC 9(5) COMP -> 4 bytes (i32)
        let pic = parse_picture("9(5)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp, &pic), 4);
    }

    #[test]
    fn compute_byte_size_comp_large() {
        // PIC 9(10) COMP -> 8 bytes (i64)
        let pic = parse_picture("9(10)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp, &pic), 8);
    }

    #[test]
    fn compute_byte_size_binary() {
        let pic = parse_picture("9(7)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Binary, &pic), 4);
    }

    #[test]
    fn compute_byte_size_comp4() {
        let pic = parse_picture("9(3)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp4, &pic), 2);
    }

    #[test]
    fn compute_byte_size_comp5() {
        let pic = parse_picture("9(18)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp5, &pic), 8);
    }

    #[test]
    fn compute_byte_size_comp1() {
        // COMP-1 is always 4 bytes regardless of PIC
        let pic = parse_picture("9(5)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp1, &pic), 4);
    }

    #[test]
    fn compute_byte_size_comp2() {
        // COMP-2 is always 8 bytes regardless of PIC
        let pic = parse_picture("9(5)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp2, &pic), 8);
    }

    #[test]
    fn compute_byte_size_comp3() {
        // COMP-3: (digits + 1) / 2
        // PIC 9(5) -> (5 + 1) / 2 = 3 bytes
        let pic = parse_picture("9(5)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp3, &pic), 3);
    }

    #[test]
    fn compute_byte_size_packed_decimal() {
        // PACKED-DECIMAL: (digits + 1) / 2
        // PIC 9(7) -> (7 + 1) / 2 = 4 bytes
        let pic = parse_picture("9(7)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::PackedDecimal, &pic), 4);
    }

    #[test]
    fn compute_byte_size_comp3_even_digits() {
        // COMP-3: (digits + 1) / 2
        // PIC 9(4) -> (4 + 1) / 2 = 2 bytes (integer division)
        let pic = parse_picture("9(4)").unwrap();
        assert_eq!(HirLowerer::compute_byte_size(UsageType::Comp3, &pic), 2);
    }

    #[test]
    fn usage_to_encoding_mappings() {
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Display), DataEncoding::Display);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Comp), DataEncoding::Binary);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Comp1), DataEncoding::FloatSingle);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Comp2), DataEncoding::FloatDouble);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Comp3), DataEncoding::PackedDecimal);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Comp4), DataEncoding::Binary);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Comp5), DataEncoding::Binary);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Binary), DataEncoding::Binary);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::PackedDecimal), DataEncoding::PackedDecimal);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Index), DataEncoding::Index);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::Pointer), DataEncoding::Pointer);
        assert_eq!(HirLowerer::usage_to_encoding(UsageType::FunctionPointer), DataEncoding::Pointer);
    }

    // ------------------------------------------------------------------
    // End-to-end USAGE clause lowering (parse -> lower -> check)
    // ------------------------------------------------------------------

    /// Helper: find the first working-storage data item by name (case-insensitive).
    fn find_ws_item<'a>(module: &'a HirModule, interner: &cobol_intern::Interner, name: &str) -> &'a DataItemData {
        for &id in &module.working_storage {
            let item = &module.data_items[id.into_raw()];
            if let Some(n) = item.name {
                if interner.resolve(n).eq_ignore_ascii_case(name) {
                    return item;
                }
            }
        }
        panic!("data item '{}' not found in working storage", name);
    }

    /// Builds a minimal COBOL program with a single WORKING-STORAGE item.
    fn make_cobol_source(data_entry: &str) -> String {
        format!(
            "IDENTIFICATION DIVISION.\n\
             PROGRAM-ID. TEST-USAGE.\n\
             DATA DIVISION.\n\
             WORKING-STORAGE SECTION.\n\
             {}\n\
             PROCEDURE DIVISION.\n\
             STOP RUN.\n",
            data_entry
        )
    }

    #[test]
    fn lower_usage_display_explicit() {
        let src = make_cobol_source("01 WS-ITEM PIC 9(5) USAGE DISPLAY.");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-ITEM");
        assert_eq!(item.storage.usage, UsageType::Display);
        assert_eq!(item.storage.encoding, DataEncoding::Display);
        assert_eq!(item.storage.byte_size, 5);
    }

    #[test]
    fn lower_usage_comp() {
        let src = make_cobol_source("01 WS-COMP PIC 9(5) USAGE COMP.");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-COMP");
        assert_eq!(item.storage.usage, UsageType::Comp);
        assert_eq!(item.storage.encoding, DataEncoding::Binary);
        assert_eq!(item.storage.byte_size, 4); // 5 digits -> 4 bytes
    }

    #[test]
    fn lower_usage_comp3() {
        let src = make_cobol_source("01 WS-PACKED PIC 9(7) USAGE COMP-3.");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-PACKED");
        assert_eq!(item.storage.usage, UsageType::Comp3);
        assert_eq!(item.storage.encoding, DataEncoding::PackedDecimal);
        assert_eq!(item.storage.byte_size, 4); // (7 + 1) / 2 = 4
    }

    #[test]
    fn lower_usage_binary() {
        let src = make_cobol_source("01 WS-BIN PIC 9(4) USAGE BINARY.");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-BIN");
        assert_eq!(item.storage.usage, UsageType::Binary);
        assert_eq!(item.storage.encoding, DataEncoding::Binary);
        assert_eq!(item.storage.byte_size, 2); // 4 digits -> 2 bytes
    }

    #[test]
    fn lower_usage_packed_decimal() {
        let src = make_cobol_source("01 WS-PKD PIC 9(5) USAGE PACKED-DECIMAL.");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-PKD");
        assert_eq!(item.storage.usage, UsageType::PackedDecimal);
        assert_eq!(item.storage.encoding, DataEncoding::PackedDecimal);
        assert_eq!(item.storage.byte_size, 3); // (5 + 1) / 2 = 3
    }

    #[test]
    fn lower_usage_comp_large_pic() {
        let src = make_cobol_source("01 WS-BIG PIC 9(18) USAGE COMP.");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-BIG");
        assert_eq!(item.storage.usage, UsageType::Comp);
        assert_eq!(item.storage.encoding, DataEncoding::Binary);
        assert_eq!(item.storage.byte_size, 8); // 18 digits -> 8 bytes
    }

    #[test]
    fn lower_usage_no_clause_defaults_display() {
        let src = make_cobol_source("01 WS-DEFAULT PIC X(10).");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-DEFAULT");
        assert_eq!(item.storage.usage, UsageType::Display);
        assert_eq!(item.storage.encoding, DataEncoding::Display);
        assert_eq!(item.storage.byte_size, 10);
    }

    #[test]
    fn lower_usage_is_comp() {
        // USAGE IS COMP (with IS keyword)
        let src = make_cobol_source("01 WS-ISCOMP PIC 9(9) USAGE IS COMP.");
        let file_id = cobol_span::FileId::new(0);
        let tokens = cobol_lexer::lex(&src, file_id, cobol_lexer::SourceFormat::Free);
        let parse_result = cobol_parser::parse(&tokens);
        let root = parse_result.syntax();
        let sf = cobol_ast::SourceFile::cast(root).unwrap();
        let mut interner = cobol_intern::Interner::default();
        let module = lower(&sf, &mut interner);

        let item = find_ws_item(&module, &interner, "WS-ISCOMP");
        assert_eq!(item.storage.usage, UsageType::Comp);
        assert_eq!(item.storage.encoding, DataEncoding::Binary);
        assert_eq!(item.storage.byte_size, 4); // 9 digits -> 4 bytes
    }
}
