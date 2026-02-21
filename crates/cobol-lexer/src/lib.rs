//! COBOL tokenizer supporting fixed and free format.
//!
//! This crate provides the first stage of the compilation pipeline: breaking
//! raw COBOL source text into a flat sequence of [`Token`]s.
//!
//! ## Architecture
//!
//! The lexer uses a two-layer approach:
//!
//! 1. **Line Normalizer** - Processes raw source into logical lines by handling
//!    the fixed-format column layout (sequence numbers, indicators, continuation
//!    lines, and the identification area).
//!
//! 2. **Tokenizer** - Walks each logical line character-by-character to produce
//!    tokens. All identifier/keyword words are emitted as [`TokenKind::Word`];
//!    the parser handles keyword recognition by context.

pub use smol_str::SmolStr;

use cobol_span::{ExpansionId, FileId, Span, TextRange, TextSize};

// ---------------------------------------------------------------------------
// TokenKind
// ---------------------------------------------------------------------------

/// Every distinct token the COBOL lexer can produce.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum TokenKind {
    // -- Literals & Identifiers --
    Word,
    IntegerLiteral,
    DecimalLiteral,
    StringLiteral,
    HexLiteral,

    // -- Punctuation --
    Period,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    Colon,
    EqualSign,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    NotEqual,
    Plus,
    Minus,
    Star,
    Slash,
    DoubleStar,
    Ampersand,

    // -- Division headers --
    IdentificationDivision,
    EnvironmentDivision,
    DataDivision,
    ProcedureDivision,

    // -- Section / paragraph headers --
    ProgramId,
    Author,
    DateWritten,
    DateCompiled,
    Security,
    ConfigurationSection,
    InputOutputSection,
    FileSection,
    WorkingStorageSection,
    LinkageSection,
    LocalStorageSection,
    ScreenSection,
    ReportSection,
    CommunicationSection,
    FileControl,
    IoControl,

    // -- Data description --
    Level,
    Level66,
    Level77,
    Level88,
    Pic,
    Picture,
    Usage,
    Value,
    Values,
    Redefines,
    Occurs,
    Depending,
    Ascending,
    Descending,
    Key,
    Indexed,
    Comp,
    Comp1,
    Comp2,
    Comp3,
    Comp4,
    Comp5,
    Binary,
    PackedDecimal,
    Display_,
    Index_,
    Pointer_,
    Filler,
    Renames,
    Blank,
    When,
    Zero,
    Zeroes,
    Zeros,
    Justified,
    Just,
    Right_,
    Sign_,
    Leading,
    Trailing,
    Separate,
    Sync,
    Synchronized,
    Left_,
    Global,
    External,

    // -- Procedure division verbs --
    Accept,
    Add,
    Alter,
    Call,
    Cancel,
    Close,
    Compute,
    Continue,
    Delete,
    Display,
    Divide,
    Enter,
    Evaluate,
    ExecSql,
    Exit,
    GoTo,
    GoBack,
    If,
    Initialize,
    Inspect,
    Merge,
    Move,
    Multiply,
    Open,
    Perform,
    Read,
    Release,
    Return_,
    Rewrite,
    Search,
    Set,
    Sort,
    Start,
    Stop,
    String_,
    Subtract,
    Unstring,
    Write_,

    // -- Clauses & phrases --
    Also,
    Any,
    Are,
    At,
    Before,
    After,
    By,
    Corresponding,
    Corr,
    Delimited,
    Delimiter,
    End,
    EndAdd,
    EndCall,
    EndCompute,
    EndDelete,
    EndDivide,
    EndEvaluate,
    EndIf,
    EndMultiply,
    EndPerform,
    EndRead,
    EndReturn,
    EndRewrite,
    EndSearch,
    EndStart,
    EndString,
    EndSubtract,
    EndUnstring,
    EndWrite,
    Else,
    From,
    Giving,
    In,
    Of,
    Into,
    Not,
    On,
    Or,
    And,
    Other,
    Overflow_,
    Pointer_Kw,
    Remainder,
    Replacing,
    Rounded,
    SizeError,
    OnSizeError,
    NotOnSizeError,
    Tallying,
    Test,
    Than,
    Then,
    Through,
    Thru,
    Times,
    To,
    Until,
    Up,
    Down,
    Using,
    Varying,
    With,
    True_,
    False_,
    Null_,
    Nulls,

    // -- File I/O --
    Fd,
    Sd,
    Select,
    Assign,
    Organization,
    Sequential,
    Relative_,
    LineSequential,
    Record_,
    Contains,
    Block,
    AccessMode,
    Random_,
    Dynamic_,
    Status_,
    FileStatus,
    Linage,
    Footing,
    Input,
    Output,
    IoMode,
    Extend,
    InvalidKey,
    NotInvalidKey,
    AtEnd,
    NotAtEnd,
    Lock,
    Unlock,
    Optional_,

    // -- PERFORM --
    PerformInline,
    EndPerform_Kw,

    // -- Misc --
    Copy,
    Replacing_Kw,
    Replace,
    SectionKw,
    ParagraphKw,
    Run,
    Returning,
    AlphabeticLower,
    AlphabeticUpper,
    Alphabetic_Kw,
    Numeric,
    NumericEdited,
    Alphanumeric,
    AlphanumericEdited,
    Positive,
    Negative,
    Class_,
    Spaces,
    Space,
    HighValues,
    HighValue,
    LowValues,
    LowValue,
    Quotes,
    Quote,
    All,
    Reference,
    Content,
    Length_,
    Address_,

    // -- Special --
    Whitespace,
    Comment,
    Newline,
    Eof,
    Error,
}

// ---------------------------------------------------------------------------
// Token
// ---------------------------------------------------------------------------

/// A single lexical token produced by the COBOL tokenizer.
#[derive(Clone, PartialEq)]
pub struct Token {
    /// What kind of token this is.
    pub kind: TokenKind,
    /// The original source text of the token.
    pub text: SmolStr,
    /// Where the token appears in the source.
    pub span: Span,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("kind", &self.kind)
            .field("text", &self.text)
            .field("span", &self.span)
            .finish()
    }
}

// ---------------------------------------------------------------------------
// SourceFormat
// ---------------------------------------------------------------------------

/// Whether the COBOL source is in traditional fixed-column format or modern
/// free format.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SourceFormat {
    /// Traditional fixed format (columns 1-6 sequence, 7 indicator, 8-72 area A/B).
    Fixed,
    /// Free format (no column restrictions).
    Free,
}

/// COBOL dialect controlling language extensions and vendor-specific behavior.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub enum Dialect {
    /// Strict COBOL-85 standard (ANSI X3.23-1985).
    #[default]
    Cobol85,
    /// IBM Enterprise COBOL extensions (GOBACK, EXEC SQL/CICS, relaxed reserved words).
    Ibm,
    /// Micro Focus COBOL extensions (COMP-X, relaxed syntax).
    MicroFocus,
    /// GnuCOBOL extensions (most permissive, COMP-5, many non-standard features).
    GnuCobol,
}

// ---------------------------------------------------------------------------
// Layer 1: Line Normalizer
// ---------------------------------------------------------------------------

/// A logical line of COBOL source code after fixed-format normalization.
///
/// Continuation lines are merged into their predecessor, so each `LogicalLine`
/// may span multiple physical lines in the original source.
#[derive(Debug, Clone)]
struct LogicalLine {
    /// The extracted source text (columns 8-72 of all contributing physical
    /// lines, joined for continuations).
    text: String,
    /// Byte offset in the original source where this logical line's content
    /// begins (i.e. the offset of column 8 of the first physical line).
    start_offset: u32,
}

/// Represents a single physical line after extracting fixed-format fields.
#[derive(Debug)]
struct PhysicalLine<'a> {
    /// The indicator character from column 7, or space if the line is too short.
    indicator: char,
    /// The source content from columns 8-72, trimmed to available length.
    content: &'a str,
    /// Byte offset in the original source where column 8 begins.
    content_offset: u32,
}

/// Extract the fixed-format fields from a single physical line.
///
/// Handles short lines gracefully: if the line is shorter than 7 characters,
/// the indicator defaults to space and the content is empty.
fn parse_physical_line(line: &str, line_start_offset: u32) -> PhysicalLine<'_> {
    let line_bytes = line.as_bytes();
    let len = line_bytes.len();

    // Column 7 is at byte index 6 (0-based).
    let indicator = if len > 6 { line_bytes[6] as char } else { ' ' };

    // Columns 8-72 are at byte indices 7..72 (0-based).
    let content_start = 7.min(len);
    let content_end = 72.min(len);
    let content = if content_start <= content_end {
        &line[content_start..content_end]
    } else {
        ""
    };

    let content_offset = line_start_offset + content_start as u32;

    PhysicalLine {
        indicator,
        content,
        content_offset,
    }
}

/// Process raw fixed-format COBOL source into logical lines.
///
/// This implements the line-normalization layer:
/// 1. Splits input into physical lines.
/// 2. Extracts columns 7 (indicator) and 8-72 (content).
/// 3. Skips comment lines (indicator `*`, `/`, `D`, `d`).
/// 4. Merges continuation lines (indicator `-`) into the preceding logical line.
fn normalize_fixed_format(source: &str) -> Vec<LogicalLine> {
    let mut logical_lines: Vec<LogicalLine> = Vec::new();
    let mut offset: u32 = 0;

    for raw_line in source.split('\n') {
        // Compute the byte length of this line in the original source,
        // including the newline delimiter (if present).
        let line_start = offset;

        // Strip trailing \r for Windows line endings.
        let line = raw_line.strip_suffix('\r').unwrap_or(raw_line);

        let physical = parse_physical_line(line, line_start);

        // Advance offset past this line plus its newline.
        offset += raw_line.len() as u32 + 1; // +1 for the \n

        match physical.indicator {
            // Comment lines: skip entirely.
            '*' | '/' | 'D' | 'd' => continue,

            // Continuation line: append to previous logical line.
            '-' => {
                if let Some(prev) = logical_lines.last_mut() {
                    let cont_content = physical.content;
                    // For continuation, skip leading spaces up to first
                    // non-space character. For continued string literals, the
                    // continuation starts at the opening quote.
                    let trimmed = cont_content.trim_start();
                    // If the continuation starts with a quote, that quote is
                    // the continuation of the previous string literal. We
                    // include it as-is. Otherwise, we just append the trimmed
                    // content.
                    prev.text.push_str(trimmed);
                } else {
                    // Continuation with no preceding line -- treat as regular.
                    logical_lines.push(LogicalLine {
                        text: physical.content.to_string(),
                        start_offset: physical.content_offset,
                    });
                }
            }

            // Normal source line (space or any other indicator).
            _ => {
                logical_lines.push(LogicalLine {
                    text: physical.content.to_string(),
                    start_offset: physical.content_offset,
                });
            }
        }
    }

    logical_lines
}

// ---------------------------------------------------------------------------
// Layer 2: Tokenizer
// ---------------------------------------------------------------------------

/// Tokenize a set of logical lines into a flat token stream.
fn tokenize_lines(lines: &[LogicalLine], file_id: FileId, tokens: &mut Vec<Token>) {
    for line in lines {
        tokenize_line(&line.text, line.start_offset, file_id, tokens);
        // Emit a newline token at the end of each logical line so the parser
        // can detect line boundaries if needed.
        let end_offset = line.start_offset + line.text.len() as u32;
        tokens.push(Token {
            kind: TokenKind::Newline,
            text: SmolStr::new("\n"),
            span: Span::new(
                file_id,
                TextRange::new(TextSize::from(end_offset), TextSize::from(end_offset)),
                ExpansionId::ROOT,
            ),
        });
    }
}

/// Create a [`Span`] from a base offset and a local start/end position.
#[inline]
fn make_span(file_id: FileId, base: u32, start: u32, end: u32) -> Span {
    Span::new(
        file_id,
        TextRange::new(TextSize::from(base + start), TextSize::from(base + end)),
        ExpansionId::ROOT,
    )
}

/// Tokenize a single logical line.
fn tokenize_line(text: &str, base_offset: u32, file_id: FileId, tokens: &mut Vec<Token>) {
    let bytes = text.as_bytes();
    let len = bytes.len();
    let mut pos: usize = 0;

    while pos < len {
        let ch = bytes[pos] as char;

        match ch {
            // -----------------------------------------------------------
            // Whitespace
            // -----------------------------------------------------------
            ' ' | '\t' => {
                let start = pos;
                while pos < len && (bytes[pos] == b' ' || bytes[pos] == b'\t') {
                    pos += 1;
                }
                tokens.push(Token {
                    kind: TokenKind::Whitespace,
                    text: SmolStr::new(&text[start..pos]),
                    span: make_span(file_id, base_offset, start as u32, pos as u32),
                });
            }

            // -----------------------------------------------------------
            // String literals (single-quoted and double-quoted)
            // Also handles hex literals: X"..." or X'...'
            // -----------------------------------------------------------
            '"' | '\'' => {
                let start = pos;
                let quote = bytes[pos];
                pos += 1;
                while pos < len && bytes[pos] != quote {
                    pos += 1;
                }
                if pos < len {
                    pos += 1; // consume closing quote
                }
                tokens.push(Token {
                    kind: TokenKind::StringLiteral,
                    text: SmolStr::new(&text[start..pos]),
                    span: make_span(file_id, base_offset, start as u32, pos as u32),
                });
            }

            // -----------------------------------------------------------
            // Numeric literals (digits)
            // -----------------------------------------------------------
            '0'..='9' => {
                let start = pos;
                let mut has_dot = false;
                while pos < len {
                    let b = bytes[pos];
                    if b.is_ascii_digit() {
                        pos += 1;
                    } else if b == b'.' && !has_dot {
                        // Check if this is a decimal point in a number or a
                        // sentence-ending period. A period is a decimal point
                        // if the next character is a digit.
                        if pos + 1 < len && bytes[pos + 1] >= b'0' && bytes[pos + 1] <= b'9' {
                            has_dot = true;
                            pos += 1;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                let kind = if has_dot {
                    TokenKind::DecimalLiteral
                } else {
                    TokenKind::IntegerLiteral
                };
                tokens.push(Token {
                    kind,
                    text: SmolStr::new(&text[start..pos]),
                    span: make_span(file_id, base_offset, start as u32, pos as u32),
                });
            }

            // -----------------------------------------------------------
            // Words (identifiers/keywords): start with letter
            // Also handles hex literal prefix X" / X'
            // -----------------------------------------------------------
            'A'..='Z' | 'a'..='z' => {
                let start = pos;

                // Special case: X"..." or X'...' hex literals
                if (ch == 'X' || ch == 'x')
                    && pos + 1 < len
                    && (bytes[pos + 1] == b'"' || bytes[pos + 1] == b'\'')
                {
                    pos += 1; // skip 'X'
                    let quote = bytes[pos];
                    pos += 1; // skip opening quote
                    while pos < len && bytes[pos] != quote {
                        pos += 1;
                    }
                    if pos < len {
                        pos += 1; // skip closing quote
                    }
                    tokens.push(Token {
                        kind: TokenKind::HexLiteral,
                        text: SmolStr::new(&text[start..pos]),
                        span: make_span(file_id, base_offset, start as u32, pos as u32),
                    });
                    continue;
                }

                // Regular word: letters, digits, hyphens.
                // COBOL words can contain hyphens (e.g., WORKING-STORAGE,
                // END-IF, PROGRAM-ID). They must start with a letter.
                while pos < len {
                    let b = bytes[pos];
                    if b.is_ascii_uppercase()
                        || b.is_ascii_lowercase()
                        || b.is_ascii_digit()
                        || b == b'-'
                    {
                        pos += 1;
                    } else {
                        break;
                    }
                }

                // COBOL words should not end with a hyphen if followed by a
                // space/punctuation, but we allow it since legacy code does
                // this. The parser can handle validation.

                let word_text = &text[start..pos];

                // All words are emitted as Word -- the parser promotes to
                // keywords based on context.
                tokens.push(Token {
                    kind: TokenKind::Word,
                    text: SmolStr::new(word_text),
                    span: make_span(file_id, base_offset, start as u32, pos as u32),
                });
            }

            // -----------------------------------------------------------
            // Period
            // -----------------------------------------------------------
            '.' => {
                // Check if this starts a decimal literal: .NNN
                if pos + 1 < len && bytes[pos + 1] >= b'0' && bytes[pos + 1] <= b'9' {
                    let start = pos;
                    pos += 1; // skip '.'
                    while pos < len && bytes[pos] >= b'0' && bytes[pos] <= b'9' {
                        pos += 1;
                    }
                    tokens.push(Token {
                        kind: TokenKind::DecimalLiteral,
                        text: SmolStr::new(&text[start..pos]),
                        span: make_span(file_id, base_offset, start as u32, pos as u32),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Period,
                        text: SmolStr::new("."),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                    });
                    pos += 1;
                }
            }

            // -----------------------------------------------------------
            // Comma
            // -----------------------------------------------------------
            ',' => {
                tokens.push(Token {
                    kind: TokenKind::Comma,
                    text: SmolStr::new(","),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Semicolon
            // -----------------------------------------------------------
            ';' => {
                tokens.push(Token {
                    kind: TokenKind::Semicolon,
                    text: SmolStr::new(";"),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Left paren
            // -----------------------------------------------------------
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::LeftParen,
                    text: SmolStr::new("("),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Right paren
            // -----------------------------------------------------------
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::RightParen,
                    text: SmolStr::new(")"),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Colon
            // -----------------------------------------------------------
            ':' => {
                tokens.push(Token {
                    kind: TokenKind::Colon,
                    text: SmolStr::new(":"),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Equal sign
            // -----------------------------------------------------------
            '=' => {
                tokens.push(Token {
                    kind: TokenKind::EqualSign,
                    text: SmolStr::new("="),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Greater than / Greater equal
            // -----------------------------------------------------------
            '>' => {
                if pos + 1 < len && bytes[pos + 1] == b'=' {
                    tokens.push(Token {
                        kind: TokenKind::GreaterEqual,
                        text: SmolStr::new(">="),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 2) as u32),
                    });
                    pos += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::GreaterThan,
                        text: SmolStr::new(">"),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                    });
                    pos += 1;
                }
            }

            // -----------------------------------------------------------
            // Less than / Less equal
            // -----------------------------------------------------------
            '<' => {
                if pos + 1 < len && bytes[pos + 1] == b'=' {
                    tokens.push(Token {
                        kind: TokenKind::LessEqual,
                        text: SmolStr::new("<="),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 2) as u32),
                    });
                    pos += 2;
                } else if pos + 1 < len && bytes[pos + 1] == b'>' {
                    tokens.push(Token {
                        kind: TokenKind::NotEqual,
                        text: SmolStr::new("<>"),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 2) as u32),
                    });
                    pos += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::LessThan,
                        text: SmolStr::new("<"),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                    });
                    pos += 1;
                }
            }

            // -----------------------------------------------------------
            // Plus
            // -----------------------------------------------------------
            '+' => {
                // A '+' before digits could be a signed numeric literal,
                // but for simplicity we always emit Plus and let the parser
                // handle sign-as-prefix.
                tokens.push(Token {
                    kind: TokenKind::Plus,
                    text: SmolStr::new("+"),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Minus
            // -----------------------------------------------------------
            '-' => {
                tokens.push(Token {
                    kind: TokenKind::Minus,
                    text: SmolStr::new("-"),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Star / DoubleStar / Inline comment (*>)
            // -----------------------------------------------------------
            '*' => {
                if pos + 1 < len && bytes[pos + 1] == b'>' {
                    // Inline comment: consume the rest of the line.
                    let start = pos;
                    pos = len;
                    tokens.push(Token {
                        kind: TokenKind::Comment,
                        text: SmolStr::new(&text[start..pos]),
                        span: make_span(file_id, base_offset, start as u32, pos as u32),
                    });
                } else if pos + 1 < len && bytes[pos + 1] == b'*' {
                    tokens.push(Token {
                        kind: TokenKind::DoubleStar,
                        text: SmolStr::new("**"),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 2) as u32),
                    });
                    pos += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Star,
                        text: SmolStr::new("*"),
                        span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                    });
                    pos += 1;
                }
            }

            // -----------------------------------------------------------
            // Slash
            // -----------------------------------------------------------
            '/' => {
                tokens.push(Token {
                    kind: TokenKind::Slash,
                    text: SmolStr::new("/"),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Ampersand
            // -----------------------------------------------------------
            '&' => {
                tokens.push(Token {
                    kind: TokenKind::Ampersand,
                    text: SmolStr::new("&"),
                    span: make_span(file_id, base_offset, pos as u32, (pos + 1) as u32),
                });
                pos += 1;
            }

            // -----------------------------------------------------------
            // Any other character: emit Error token
            // -----------------------------------------------------------
            _ => {
                let start = pos;
                pos += ch.len_utf8();
                tokens.push(Token {
                    kind: TokenKind::Error,
                    text: SmolStr::new(&text[start..pos]),
                    span: make_span(file_id, base_offset, start as u32, pos as u32),
                });
            }
        }
    }
}

// ---------------------------------------------------------------------------
// lex() - Public entry point
// ---------------------------------------------------------------------------

/// Tokenizes COBOL `source` text belonging to `file_id` using the given
/// `format`.
///
/// Returns a flat vector of [`Token`]s. The last token is always
/// [`TokenKind::Eof`]. All identifier and keyword words are emitted as
/// [`TokenKind::Word`]; the parser handles keyword recognition by context.
pub fn lex(source: &str, file_id: FileId, format: SourceFormat) -> Vec<Token> {
    match format {
        SourceFormat::Fixed => lex_fixed(source, file_id),
        SourceFormat::Free => lex_free(source, file_id),
    }
}

/// Tokenize fixed-format COBOL source.
fn lex_fixed(source: &str, file_id: FileId) -> Vec<Token> {
    let lines = normalize_fixed_format(source);
    let mut tokens = Vec::new();
    tokenize_lines(&lines, file_id, &mut tokens);

    // Compute the EOF position.
    let eof_offset = source.len() as u32;
    tokens.push(Token {
        kind: TokenKind::Eof,
        text: SmolStr::default(),
        span: Span::new(
            file_id,
            TextRange::new(TextSize::from(eof_offset), TextSize::from(eof_offset)),
            ExpansionId::ROOT,
        ),
    });

    tokens
}

/// Tokenize free-format COBOL source.
///
/// Free format has no column restrictions: the entire line is source code,
/// and `*>` introduces inline comments anywhere on a line. Lines starting
/// with `*>` are full-line comments.
fn lex_free(source: &str, file_id: FileId) -> Vec<Token> {
    let mut logical_lines: Vec<LogicalLine> = Vec::new();
    let mut offset: u32 = 0;

    for raw_line in source.split('\n') {
        let line_start = offset;
        let line = raw_line.strip_suffix('\r').unwrap_or(raw_line);
        let trimmed = line.trim_start();

        offset += raw_line.len() as u32 + 1;

        // Full-line comment: line starts with *>
        if trimmed.starts_with("*>") {
            continue;
        }

        logical_lines.push(LogicalLine {
            text: line.to_string(),
            start_offset: line_start,
        });
    }

    let mut tokens = Vec::new();
    tokenize_lines(&logical_lines, file_id, &mut tokens);

    let eof_offset = source.len() as u32;
    tokens.push(Token {
        kind: TokenKind::Eof,
        text: SmolStr::default(),
        span: Span::new(
            file_id,
            TextRange::new(TextSize::from(eof_offset), TextSize::from(eof_offset)),
            ExpansionId::ROOT,
        ),
    });

    tokens
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: lex fixed-format source and return non-whitespace,
    /// non-newline, non-eof tokens.
    fn lex_fixed_significant(source: &str) -> Vec<Token> {
        let tokens = lex(source, FileId::new(0), SourceFormat::Fixed);
        tokens
            .into_iter()
            .filter(|t| {
                t.kind != TokenKind::Whitespace
                    && t.kind != TokenKind::Newline
                    && t.kind != TokenKind::Eof
            })
            .collect()
    }

    /// Helper: lex free-format source and return non-whitespace,
    /// non-newline, non-eof tokens.
    fn lex_free_significant(source: &str) -> Vec<Token> {
        let tokens = lex(source, FileId::new(0), SourceFormat::Free);
        tokens
            .into_iter()
            .filter(|t| {
                t.kind != TokenKind::Whitespace
                    && t.kind != TokenKind::Newline
                    && t.kind != TokenKind::Eof
            })
            .collect()
    }

    /// Build a fixed-format line with the given indicator and content in
    /// columns 8-72. Columns 1-6 are filled with spaces, and the line is
    /// padded to exactly 80 characters (columns 73-80 are identification area).
    fn fixed_line(indicator: char, content: &str) -> String {
        // 6 spaces for sequence area + indicator + content (padded to 65 chars for cols 8-72)
        let padded_content = format!("{:<65}", content);
        // Truncate to exactly 65 characters for columns 8-72
        let cols8_72 = &padded_content[..65.min(padded_content.len())];
        format!("      {}{}{}", indicator, cols8_72, "        ")
    }

    // -------------------------------------------------------------------
    // Existing tests (preserved)
    // -------------------------------------------------------------------

    #[test]
    fn lex_returns_eof_token() {
        let file_id = FileId::new(0);
        let tokens = lex("", file_id, SourceFormat::Fixed);
        assert!(!tokens.is_empty(), "lex should return at least one token");
        assert_eq!(tokens.last().unwrap().kind, TokenKind::Eof);
    }

    #[test]
    fn lex_nonempty_source_still_returns_eof() {
        let file_id = FileId::new(1);
        let tokens = lex("IDENTIFICATION DIVISION.", file_id, SourceFormat::Free);
        assert!(
            tokens.iter().any(|t| t.kind == TokenKind::Eof),
            "token stream must contain Eof"
        );
    }

    #[test]
    fn source_format_equality() {
        assert_eq!(SourceFormat::Fixed, SourceFormat::Fixed);
        assert_ne!(SourceFormat::Fixed, SourceFormat::Free);
    }

    #[test]
    fn token_kind_is_copy() {
        let kind = TokenKind::Move;
        let copy = kind;
        assert_eq!(kind, copy);
    }

    // -------------------------------------------------------------------
    // New tests
    // -------------------------------------------------------------------

    #[test]
    fn test_lex_hello_world() {
        // A minimal COBOL program in fixed format.
        let source = [
            fixed_line(' ', "IDENTIFICATION DIVISION."),
            fixed_line(' ', "PROGRAM-ID. HELLO."),
            fixed_line(' ', "PROCEDURE DIVISION."),
            fixed_line(' ', "    DISPLAY \"HELLO WORLD\"."),
            fixed_line(' ', "    STOP RUN."),
        ]
        .join("\n");

        let tokens = lex_fixed_significant(&source);

        // Check that we get the expected word tokens.
        let words: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Word)
            .map(|t| t.text.as_str())
            .collect();

        assert!(
            words.contains(&"IDENTIFICATION"),
            "should contain IDENTIFICATION, got: {:?}",
            words
        );
        assert!(
            words.contains(&"DIVISION"),
            "should contain DIVISION, got: {:?}",
            words
        );
        assert!(
            words.contains(&"PROGRAM-ID"),
            "should contain PROGRAM-ID, got: {:?}",
            words
        );
        assert!(
            words.contains(&"HELLO"),
            "should contain HELLO, got: {:?}",
            words
        );
        assert!(
            words.contains(&"PROCEDURE"),
            "should contain PROCEDURE, got: {:?}",
            words
        );
        assert!(
            words.contains(&"DISPLAY"),
            "should contain DISPLAY, got: {:?}",
            words
        );
        assert!(
            words.contains(&"STOP"),
            "should contain STOP, got: {:?}",
            words
        );
        assert!(
            words.contains(&"RUN"),
            "should contain RUN, got: {:?}",
            words
        );

        // Check that we have the string literal.
        let strings: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::StringLiteral)
            .map(|t| t.text.as_str())
            .collect();
        assert_eq!(strings, vec!["\"HELLO WORLD\""]);

        // Check that periods are present.
        let periods = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Period)
            .count();
        assert!(
            periods >= 4,
            "should have at least 4 periods, got: {}",
            periods
        );
    }

    #[test]
    fn test_lex_string_literal() {
        // Single-quoted.
        let tokens = lex_free_significant("'hello world'");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[0].text.as_str(), "'hello world'");

        // Double-quoted.
        let tokens = lex_free_significant("\"hello world\"");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[0].text.as_str(), "\"hello world\"");

        // Empty string.
        let tokens = lex_free_significant("''");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[0].text.as_str(), "''");
    }

    #[test]
    fn test_lex_numeric_literals() {
        // Simple integer.
        let tokens = lex_free_significant("42");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::IntegerLiteral);
        assert_eq!(tokens[0].text.as_str(), "42");

        // Decimal.
        let tokens = lex_free_significant("3.14");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::DecimalLiteral);
        assert_eq!(tokens[0].text.as_str(), "3.14");

        // Leading-dot decimal.
        let tokens = lex_free_significant(".5");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::DecimalLiteral);
        assert_eq!(tokens[0].text.as_str(), ".5");

        // Integer followed by period (sentence terminator).
        let tokens = lex_free_significant("100.");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, TokenKind::IntegerLiteral);
        assert_eq!(tokens[0].text.as_str(), "100");
        assert_eq!(tokens[1].kind, TokenKind::Period);
    }

    #[test]
    fn test_lex_comment_lines() {
        let source = [
            fixed_line('*', "This is a comment"),
            fixed_line(' ', "DISPLAY \"HI\"."),
            fixed_line('/', "This is also a comment"),
        ]
        .join("\n");

        let tokens = lex_fixed_significant(&source);

        // Only the DISPLAY line should produce tokens.
        let words: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Word)
            .map(|t| t.text.as_str())
            .collect();
        assert_eq!(words, vec!["DISPLAY"]);

        // The comment text should NOT appear in tokens.
        assert!(
            !tokens.iter().any(|t| t.text.as_str().contains("comment")),
            "comment lines should be skipped"
        );
    }

    #[test]
    fn test_lex_continuation() {
        // First line has an unclosed string, continuation line continues it.
        // In fixed format, the continuation appends the trimmed content.
        let line1 = fixed_line(' ', "DISPLAY \"HELLO ");
        let line2 = fixed_line('-', "    \"WORLD\".");

        let source = format!("{}\n{}", line1, line2);
        let tokens = lex_fixed_significant(&source);

        // The continuation should merge the two physical lines.
        // After merging, the logical line text is:
        //   DISPLAY "HELLO "WORLD".
        // The tokenizer then processes this merged line.
        let words: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Word)
            .map(|t| t.text.as_str())
            .collect();
        assert!(words.contains(&"DISPLAY"), "should contain DISPLAY");

        // We should have at least one string literal.
        let strings: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::StringLiteral)
            .map(|t| t.text.as_str())
            .collect();
        assert!(
            !strings.is_empty(),
            "should have string literals after continuation"
        );
    }

    #[test]
    fn test_lex_punctuation() {
        let tokens = lex_free_significant("( ) , ; : = .");
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::LeftParen,
                TokenKind::RightParen,
                TokenKind::Comma,
                TokenKind::Semicolon,
                TokenKind::Colon,
                TokenKind::EqualSign,
                TokenKind::Period,
            ]
        );
    }

    #[test]
    fn test_lex_compound_operators() {
        let tokens = lex_free_significant(">= <= ** <>");
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::GreaterEqual,
                TokenKind::LessEqual,
                TokenKind::DoubleStar,
                TokenKind::NotEqual,
            ]
        );
    }

    #[test]
    fn test_lex_inline_comment() {
        let tokens = lex_free_significant("MOVE A TO B *> this is a comment");

        // The inline comment should be a Comment token.
        let comment_tokens: Vec<&Token> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Comment)
            .collect();
        assert_eq!(comment_tokens.len(), 1);
        assert!(comment_tokens[0].text.starts_with("*>"));

        // The words before the comment should still be present.
        let words: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Word)
            .map(|t| t.text.as_str())
            .collect();
        assert_eq!(words, vec!["MOVE", "A", "TO", "B"]);
    }

    #[test]
    fn test_lex_pic_string() {
        // PIC X(10) should tokenize into Word, Whitespace (filtered), Word, LeftParen, IntegerLiteral, RightParen
        let tokens = lex_free_significant("PIC X(10)");
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Word,           // PIC
                TokenKind::Word,           // X
                TokenKind::LeftParen,      // (
                TokenKind::IntegerLiteral, // 10
                TokenKind::RightParen,     // )
            ]
        );
        assert_eq!(tokens[0].text.as_str(), "PIC");
        assert_eq!(tokens[1].text.as_str(), "X");
    }

    #[test]
    fn test_lex_fixed_format_columns() {
        // Columns 1-6: sequence number (ignored)
        // Column 7: indicator (space = normal)
        // Columns 8-72: source code
        // Columns 73+: identification area (ignored)
        let line = "000100 IDENTIFICATION DIVISION.                                         HELLO";
        let tokens = lex_fixed_significant(line);

        let words: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Word)
            .map(|t| t.text.as_str())
            .collect();

        // "IDENTIFICATION" and "DIVISION" should be present.
        assert!(words.contains(&"IDENTIFICATION"));
        assert!(words.contains(&"DIVISION"));

        // "000100" (sequence number) and "HELLO" (identification area) should NOT appear.
        assert!(
            !words.contains(&"000100"),
            "sequence number should be ignored"
        );
        assert!(
            !words.contains(&"HELLO"),
            "identification area should be ignored"
        );
    }

    #[test]
    fn test_lex_hex_literal() {
        let tokens = lex_free_significant("X\"FF\" X'0A'");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, TokenKind::HexLiteral);
        assert_eq!(tokens[0].text.as_str(), "X\"FF\"");
        assert_eq!(tokens[1].kind, TokenKind::HexLiteral);
        assert_eq!(tokens[1].text.as_str(), "X'0A'");
    }

    #[test]
    fn test_lex_debug_lines_skipped() {
        let source = [
            fixed_line('D', "DISPLAY \"DEBUG INFO\""),
            fixed_line(' ', "DISPLAY \"NORMAL\"."),
            fixed_line('d', "DISPLAY \"MORE DEBUG\""),
        ]
        .join("\n");

        let tokens = lex_fixed_significant(&source);

        // Only the normal line should produce tokens.
        let strings: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::StringLiteral)
            .map(|t| t.text.as_str())
            .collect();
        assert_eq!(strings, vec!["\"NORMAL\""]);
    }

    #[test]
    fn test_lex_arithmetic_operators() {
        let tokens = lex_free_significant("A + B - C * D / E");
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Word,  // A
                TokenKind::Plus,  // +
                TokenKind::Word,  // B
                TokenKind::Minus, // -
                TokenKind::Word,  // C
                TokenKind::Star,  // *
                TokenKind::Word,  // D
                TokenKind::Slash, // /
                TokenKind::Word,  // E
            ]
        );
    }

    #[test]
    fn test_lex_empty_source() {
        let tokens = lex("", FileId::new(0), SourceFormat::Fixed);
        // Should have at least the EOF token plus possibly a Newline.
        assert!(tokens.last().unwrap().kind == TokenKind::Eof);
    }

    #[test]
    fn test_lex_words_preserve_case() {
        let tokens = lex_free_significant("Display move Perform");
        assert_eq!(tokens.len(), 3);
        // All should be Word tokens with original casing preserved.
        assert_eq!(tokens[0].kind, TokenKind::Word);
        assert_eq!(tokens[0].text.as_str(), "Display");
        assert_eq!(tokens[1].text.as_str(), "move");
        assert_eq!(tokens[2].text.as_str(), "Perform");
    }

    #[test]
    fn test_lex_hyphenated_words() {
        let tokens = lex_free_significant("WORKING-STORAGE END-IF PROGRAM-ID");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].kind, TokenKind::Word);
        assert_eq!(tokens[0].text.as_str(), "WORKING-STORAGE");
        assert_eq!(tokens[1].text.as_str(), "END-IF");
        assert_eq!(tokens[2].text.as_str(), "PROGRAM-ID");
    }

    #[test]
    fn test_lex_short_lines() {
        // Lines shorter than 7 characters should be handled gracefully.
        let tokens = lex("ABC\n", FileId::new(0), SourceFormat::Fixed);
        // Short lines have empty content (columns 8-72 don't exist), so
        // we should just get Newline + EOF.
        let significant: Vec<&Token> = tokens
            .iter()
            .filter(|t| {
                t.kind != TokenKind::Whitespace
                    && t.kind != TokenKind::Newline
                    && t.kind != TokenKind::Eof
            })
            .collect();
        assert!(
            significant.is_empty(),
            "short line should produce no significant tokens, got: {:?}",
            significant
        );
    }

    #[test]
    fn test_lex_multiple_logical_lines() {
        let source = [
            fixed_line(' ', "MOVE A TO B."),
            fixed_line(' ', "MOVE C TO D."),
        ]
        .join("\n");

        let tokens = lex_fixed_significant(&source);
        let words: Vec<&str> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Word)
            .map(|t| t.text.as_str())
            .collect();
        assert_eq!(words, vec!["MOVE", "A", "TO", "B", "MOVE", "C", "TO", "D"]);
    }

    #[test]
    fn test_lex_ampersand() {
        let tokens = lex_free_significant("A & B");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].kind, TokenKind::Word);
        assert_eq!(tokens[1].kind, TokenKind::Ampersand);
        assert_eq!(tokens[2].kind, TokenKind::Word);
    }

    #[test]
    fn test_lex_comparison_operators() {
        let tokens = lex_free_significant("> < >= <= = <>");
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::GreaterThan,
                TokenKind::LessThan,
                TokenKind::GreaterEqual,
                TokenKind::LessEqual,
                TokenKind::EqualSign,
                TokenKind::NotEqual,
            ]
        );
    }

    #[test]
    fn test_lex_newlines_between_lines() {
        let source = [fixed_line(' ', "MOVE A"), fixed_line(' ', "TO B.")].join("\n");

        let all_tokens = lex(&source, FileId::new(0), SourceFormat::Fixed);
        let newlines = all_tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Newline)
            .count();
        // Each logical line gets a trailing Newline token.
        assert!(
            newlines >= 2,
            "should have at least 2 newline tokens, got: {}",
            newlines
        );
    }

    #[test]
    fn test_lex_mixed_content() {
        // A line mixing words, numbers, strings, and punctuation.
        let tokens = lex_free_significant("MOVE 42 TO WS-FIELD. DISPLAY \"RESULT\" 3.14");

        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Word,           // MOVE
                TokenKind::IntegerLiteral, // 42
                TokenKind::Word,           // TO
                TokenKind::Word,           // WS-FIELD
                TokenKind::Period,         // .
                TokenKind::Word,           // DISPLAY
                TokenKind::StringLiteral,  // "RESULT"
                TokenKind::DecimalLiteral, // 3.14
            ]
        );
    }
}
