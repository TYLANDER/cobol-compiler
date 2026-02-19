//! Error-tolerant COBOL parser producing a lossless CST via rowan.
//!
//! This crate takes a flat token stream from [`cobol_lexer`] and builds a
//! concrete syntax tree (CST) using the [`rowan`] library. The CST is
//! *lossless* -- every byte of the original source (including whitespace and
//! comments) is preserved in the tree, enabling precise refactoring and
//! formatting tools.

use cobol_span::TextRange;
#[allow(unused_imports)]
use cobol_span::TextSize;
use rowan::Language;

// ---------------------------------------------------------------------------
// SyntaxKind
// ---------------------------------------------------------------------------

/// Node and token kinds for the COBOL CST.
///
/// Token-level kinds (e.g. `WORD`, `PERIOD`) correspond directly to lexer
/// tokens, while composite kinds (e.g. `SOURCE_FILE`, `IF_STMT`) represent
/// interior CST nodes that group tokens together.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    // -- Token kinds (mirroring TokenKind) --
    WORD = 0,
    INTEGER_LITERAL,
    DECIMAL_LITERAL,
    STRING_LITERAL,
    PERIOD,
    COMMA,
    LEFT_PAREN,
    RIGHT_PAREN,
    EQUAL_SIGN,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    DOUBLE_STAR,
    GREATER_THAN,
    LESS_THAN,
    GREATER_EQUAL,
    LESS_EQUAL,
    NOT_EQUAL,
    WHITESPACE,
    COMMENT,
    NEWLINE,
    ERROR_TOKEN,
    EOF,

    /// Catch-all for keyword tokens that do not have their own dedicated
    /// SyntaxKind yet. As the parser matures, individual keywords will be
    /// promoted to their own variants.
    KEYWORD,

    // -- Composite nodes (CST interior nodes) --
    SOURCE_FILE,
    IDENTIFICATION_DIVISION,
    ENVIRONMENT_DIVISION,
    DATA_DIVISION,
    PROCEDURE_DIVISION,
    PROGRAM_ID_CLAUSE,
    WORKING_STORAGE_SECTION,
    LINKAGE_SECTION,
    FILE_SECTION,
    DATA_ITEM,
    LEVEL_NUMBER,
    DATA_NAME,
    PIC_CLAUSE,
    USAGE_CLAUSE,
    VALUE_CLAUSE,
    OCCURS_CLAUSE,
    REDEFINES_CLAUSE,
    PARAGRAPH,
    SECTION,
    SENTENCE,
    STATEMENT,

    // -- Statement nodes --
    DISPLAY_STMT,
    MOVE_STMT,
    ADD_STMT,
    SUBTRACT_STMT,
    MULTIPLY_STMT,
    DIVIDE_STMT,
    COMPUTE_STMT,
    IF_STMT,
    EVALUATE_STMT,
    PERFORM_STMT,
    GO_TO_STMT,
    CALL_STMT,
    STOP_STMT,
    ACCEPT_STMT,
    READ_STMT,
    WRITE_STMT,
    OPEN_STMT,
    CLOSE_STMT,
    EXIT_STMT,
    STRING_STMT,
    UNSTRING_STMT,
    INSPECT_STMT,
    SEARCH_STMT,
    SET_STMT,
    INITIALIZE_STMT,
    SORT_STMT,
    MERGE_STMT,
    DELETE_STMT,
    RETURN_STMT,
    RELEASE_STMT,
    REWRITE_STMT,
    START_STMT,
    ALTER_STMT,
    CONTINUE_STMT,

    // -- Expressions --
    ARITHMETIC_EXPR,
    CONDITION_EXPR,
    IDENTIFIER_REF,
    QUALIFIED_NAME,
    SUBSCRIPT,
    LITERAL,
    FIGURATIVE_CONSTANT,

    // -- Misc --
    COPY_STMT,
    REPLACE_STMT,
    FD_ENTRY,
    SD_ENTRY,
    SELECT_CLAUSE,
    FILE_CONTROL_ENTRY,

    // -- Error recovery --
    ERROR_NODE,
    TOMBSTONE,
}

impl SyntaxKind {
    /// Converts a raw `u16` discriminant back into a [`SyntaxKind`].
    ///
    /// # Safety
    ///
    /// This uses `transmute` which is the standard pattern for rowan language
    /// implementations. The caller must ensure `raw` is a valid discriminant.
    #[inline]
    pub fn from_raw(raw: u16) -> Self {
        assert!(raw <= SyntaxKind::TOMBSTONE as u16, "invalid SyntaxKind raw value: {raw}");
        unsafe { std::mem::transmute(raw) }
    }

    /// Returns the `u16` discriminant of this kind.
    #[inline]
    pub fn into_raw(self) -> u16 {
        self as u16
    }
}

// ---------------------------------------------------------------------------
// CobolLanguage (rowan integration)
// ---------------------------------------------------------------------------

/// Zero-sized type that implements [`rowan::Language`] for COBOL, tying
/// [`SyntaxKind`] into rowan's generic tree machinery.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CobolLanguage {}

impl rowan::Language for CobolLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from_raw(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into_raw())
    }
}

// ---------------------------------------------------------------------------
// Type aliases
// ---------------------------------------------------------------------------

/// A strongly-typed CST node for COBOL.
pub type SyntaxNode = rowan::SyntaxNode<CobolLanguage>;
/// A strongly-typed CST token for COBOL.
pub type SyntaxToken = rowan::SyntaxToken<CobolLanguage>;
/// Either a [`SyntaxNode`] or a [`SyntaxToken`].
pub type SyntaxElement = rowan::SyntaxElement<CobolLanguage>;

// ---------------------------------------------------------------------------
// ParseError
// ---------------------------------------------------------------------------

/// A parse error with a human-readable message and source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// A human-readable description of what went wrong.
    pub message: String,
    /// The source range where the error was detected.
    pub range: TextRange,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "error at {}..{}: {}",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            self.message,
        )
    }
}

// ---------------------------------------------------------------------------
// ParseResult
// ---------------------------------------------------------------------------

/// The output of [`parse`]: a green (immutable, thread-safe) CST root plus
/// any errors encountered during parsing.
#[derive(Debug, Clone)]
pub struct ParseResult {
    /// The root green node of the CST.
    pub green: rowan::GreenNode,
    /// Parse errors collected during tree construction.
    pub errors: Vec<ParseError>,
}

impl ParseResult {
    /// Convenience: wraps the green node in a typed [`SyntaxNode`].
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }
}

// ---------------------------------------------------------------------------
// parse()
// ---------------------------------------------------------------------------

/// Parses a token stream into a lossless COBOL CST.
///
/// This is the main entry point for the recursive-descent parser. It produces
/// a lossless CST where every token (including whitespace and comments) is
/// preserved. On error the parser recovers at the next period (`.`) and wraps
/// skipped tokens in an `ERROR_NODE`.
pub fn parse(tokens: &[cobol_lexer::Token]) -> ParseResult {
    let mut parser = Parser::new(tokens);
    parser.parse_source_file();
    parser.finish()
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

use rowan::GreenNodeBuilder;

/// Recursive-descent parser that builds a lossless CST via rowan.
struct Parser<'t> {
    tokens: &'t [cobol_lexer::Token],
    pos: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
}

#[allow(dead_code)]
impl<'t> Parser<'t> {
    fn new(tokens: &'t [cobol_lexer::Token]) -> Self {
        Self {
            tokens,
            pos: 0,
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    fn finish(self) -> ParseResult {
        ParseResult {
            green: self.builder.finish(),
            errors: self.errors,
        }
    }

    // ------------------------------------------------------------------
    // Token inspection
    // ------------------------------------------------------------------

    fn current(&self) -> &cobol_lexer::Token {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else if !self.tokens.is_empty() {
            // Should not normally be called past the end; callers check at_end().
            // Return last token (should be EOF) as a fallback.
            &self.tokens[self.tokens.len() - 1]
        } else {
            // Empty token list -- should not happen in normal usage since
            // parse_source_file guards against it, but avoid panicking.
            panic!("current() called on empty token stream");
        }
    }

    fn current_kind(&self) -> cobol_lexer::TokenKind {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].kind
        } else {
            cobol_lexer::TokenKind::Eof
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len() || self.current_kind() == cobol_lexer::TokenKind::Eof
    }

    /// Returns the upper-cased text of the current token.
    fn current_text_upper(&self) -> String {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].text.to_ascii_uppercase()
        } else {
            String::new()
        }
    }

    /// Check if current token is a Word (or keyword mapped from Word) with
    /// specific text (case-insensitive).
    fn at_word(&self, text: &str) -> bool {
        if self.at_end() {
            return false;
        }
        self.current().text.eq_ignore_ascii_case(text)
            && self.current_is_word_like()
    }

    /// Returns true if the current token is word-like (an identifier or a
    /// keyword that the lexer recognized).
    fn current_is_word_like(&self) -> bool {
        use cobol_lexer::TokenKind;
        !matches!(
            self.current_kind(),
            TokenKind::Period
                | TokenKind::Comma
                | TokenKind::Semicolon
                | TokenKind::LeftParen
                | TokenKind::RightParen
                | TokenKind::Colon
                | TokenKind::EqualSign
                | TokenKind::GreaterThan
                | TokenKind::LessThan
                | TokenKind::GreaterEqual
                | TokenKind::LessEqual
                | TokenKind::NotEqual
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::DoubleStar
                | TokenKind::Ampersand
                | TokenKind::IntegerLiteral
                | TokenKind::DecimalLiteral
                | TokenKind::StringLiteral
                | TokenKind::HexLiteral
                | TokenKind::Whitespace
                | TokenKind::Comment
                | TokenKind::Newline
                | TokenKind::Eof
                | TokenKind::Error
        )
    }

    /// Check if current token matches any of the given words.
    fn at_any_word(&self, texts: &[&str]) -> bool {
        texts.iter().any(|t| self.at_word(t))
    }

    /// Check if current token is a specific TokenKind.
    fn at_kind(&self, kind: cobol_lexer::TokenKind) -> bool {
        self.current_kind() == kind
    }

    /// Check if the current token is a literal (integer, decimal, or string).
    fn at_literal(&self) -> bool {
        use cobol_lexer::TokenKind;
        matches!(
            self.current_kind(),
            TokenKind::IntegerLiteral
                | TokenKind::DecimalLiteral
                | TokenKind::StringLiteral
                | TokenKind::HexLiteral
        )
    }

    /// Check if the current token is a figurative constant (ZERO, ZEROS,
    /// ZEROES, SPACE, SPACES, HIGH-VALUE, HIGH-VALUES, LOW-VALUE, LOW-VALUES,
    /// QUOTE, QUOTES, ALL).
    fn at_figurative(&self) -> bool {
        use cobol_lexer::TokenKind;
        matches!(
            self.current_kind(),
            TokenKind::Zero
                | TokenKind::Zeros
                | TokenKind::Zeroes
                | TokenKind::Space
                | TokenKind::Spaces
                | TokenKind::HighValue
                | TokenKind::HighValues
                | TokenKind::LowValue
                | TokenKind::LowValues
                | TokenKind::Quote
                | TokenKind::Quotes
        )
    }

    /// Check if the current token is an identifier (Word) or something that
    /// can serve as a data reference.
    fn at_identifier(&self) -> bool {
        if self.at_end() {
            return false;
        }
        self.current_kind() == cobol_lexer::TokenKind::Word
    }

    /// Check if the current token is a value: literal, figurative, or
    /// identifier.
    fn at_value(&self) -> bool {
        self.at_literal() || self.at_figurative() || self.at_identifier()
    }

    /// Look ahead past whitespace tokens. `offset` counts non-whitespace
    /// tokens (0 = current non-ws token).
    fn peek_word_at(&self, offset: usize, text: &str) -> bool {
        let mut i = self.pos;
        let mut seen = 0usize;
        while i < self.tokens.len() {
            let tk = self.tokens[i].kind;
            if !matches!(
                tk,
                cobol_lexer::TokenKind::Whitespace
                    | cobol_lexer::TokenKind::Newline
                    | cobol_lexer::TokenKind::Comment
            ) {
                if seen == offset {
                    return self.tokens[i].text.eq_ignore_ascii_case(text);
                }
                seen += 1;
            }
            i += 1;
        }
        false
    }

    /// Peek at the TokenKind of the nth non-whitespace token from the current
    /// position.
    fn peek_kind_at(&self, offset: usize) -> cobol_lexer::TokenKind {
        let mut i = self.pos;
        let mut seen = 0usize;
        while i < self.tokens.len() {
            let tk = self.tokens[i].kind;
            if !matches!(
                tk,
                cobol_lexer::TokenKind::Whitespace
                    | cobol_lexer::TokenKind::Newline
                    | cobol_lexer::TokenKind::Comment
            ) {
                if seen == offset {
                    return tk;
                }
                seen += 1;
            }
            i += 1;
        }
        cobol_lexer::TokenKind::Eof
    }

    // ------------------------------------------------------------------
    // Token consumption
    // ------------------------------------------------------------------

    /// Add the current token to the tree and advance.
    fn bump(&mut self) {
        if self.pos >= self.tokens.len() {
            return;
        }
        let token = &self.tokens[self.pos];
        let syntax_kind = token_kind_to_syntax_kind(token.kind);
        self.builder
            .token(CobolLanguage::kind_to_raw(syntax_kind), &token.text);
        self.pos += 1;
    }

    /// Add the current token with a specific SyntaxKind override.
    fn bump_as(&mut self, kind: SyntaxKind) {
        if self.pos >= self.tokens.len() {
            return;
        }
        let token = &self.tokens[self.pos];
        self.builder
            .token(CobolLanguage::kind_to_raw(kind), &token.text);
        self.pos += 1;
    }

    /// Skip whitespace, newline, and comment tokens (but still add them to
    /// the tree for losslessness).
    fn skip_ws(&mut self) {
        while !self.at_end()
            && matches!(
                self.current_kind(),
                cobol_lexer::TokenKind::Whitespace
                    | cobol_lexer::TokenKind::Newline
                    | cobol_lexer::TokenKind::Comment
            )
        {
            self.bump();
        }
    }

    /// Expect a word with specific text (case-insensitive). Emits error if
    /// not found.
    fn expect_word(&mut self, text: &str) -> bool {
        self.skip_ws();
        if self.at_word(text) {
            self.bump();
            true
        } else {
            self.error(&format!("expected '{}'", text));
            false
        }
    }

    /// Expect a period token. Emits error if not found.
    fn expect_period(&mut self) -> bool {
        self.skip_ws();
        if self.current_kind() == cobol_lexer::TokenKind::Period {
            self.bump();
            true
        } else {
            self.error("expected '.'");
            false
        }
    }

    /// Error recovery: skip tokens until the next period (inclusive), wrapping
    /// the skipped tokens in an ERROR_NODE.
    fn recover_to_period(&mut self) {
        if self.at_end() {
            return;
        }
        self.builder
            .start_node(CobolLanguage::kind_to_raw(SyntaxKind::ERROR_NODE));
        while !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            self.bump();
        }
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
            self.bump();
        }
        self.builder.finish_node();
    }

    /// Record a parse error at the current position.
    fn error(&mut self, message: &str) {
        let range = if self.at_end() || self.tokens.is_empty() {
            TextRange::empty(TextSize::from(0))
        } else {
            self.current().span.range
        };
        self.errors.push(ParseError {
            message: message.to_string(),
            range,
        });
    }

    // ------------------------------------------------------------------
    // Node building helpers
    // ------------------------------------------------------------------

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder
            .start_node(CobolLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    // ------------------------------------------------------------------
    // Division / section boundary detection
    // ------------------------------------------------------------------

    fn at_division(&self) -> bool {
        if self.at_end() {
            return false;
        }
        self.at_any_word(&["IDENTIFICATION", "ID", "ENVIRONMENT", "DATA", "PROCEDURE"])
            && self.peek_word_at(1, "DIVISION")
    }

    fn at_section_header(&self) -> bool {
        if self.at_end() {
            return false;
        }
        // Data division sections
        if self.at_any_word(&["WORKING-STORAGE", "LINKAGE", "FILE", "LOCAL-STORAGE",
            "SCREEN", "REPORT", "COMMUNICATION"]) && self.peek_word_at(1, "SECTION") {
            return true;
        }
        // Environment division sections
        if self.at_any_word(&["CONFIGURATION", "INPUT-OUTPUT"]) && self.peek_word_at(1, "SECTION") {
            return true;
        }
        false
    }

    /// In the PROCEDURE DIVISION, detect WORD SECTION (user-defined section).
    fn at_procedure_section(&self) -> bool {
        if self.at_end() {
            return false;
        }
        if !self.current_is_word_like() {
            return false;
        }
        self.peek_word_at(1, "SECTION")
    }

    /// Detect if the current position is at a paragraph header: a Word token
    /// followed (skipping whitespace) by a period, that is not a statement
    /// keyword.
    fn at_paragraph_header(&self) -> bool {
        if self.at_end() {
            return false;
        }
        if self.current_kind() != cobol_lexer::TokenKind::Word {
            return false;
        }
        // Must not be a statement keyword
        if self.at_statement_start() {
            return false;
        }
        // The next non-ws token must be a period
        self.peek_kind_at(1) == cobol_lexer::TokenKind::Period
    }

    fn at_statement_start(&self) -> bool {
        if self.at_end() {
            return false;
        }
        use cobol_lexer::TokenKind;
        matches!(
            self.current_kind(),
            TokenKind::Accept
                | TokenKind::Add
                | TokenKind::Alter
                | TokenKind::Call
                | TokenKind::Cancel
                | TokenKind::Close
                | TokenKind::Compute
                | TokenKind::Continue
                | TokenKind::Delete
                | TokenKind::Display
                | TokenKind::Divide
                | TokenKind::Enter
                | TokenKind::Evaluate
                | TokenKind::Exit
                | TokenKind::GoTo
                | TokenKind::GoBack
                | TokenKind::If
                | TokenKind::Initialize
                | TokenKind::Inspect
                | TokenKind::Merge
                | TokenKind::Move
                | TokenKind::Multiply
                | TokenKind::Open
                | TokenKind::Perform
                | TokenKind::Read
                | TokenKind::Release
                | TokenKind::Return_
                | TokenKind::Rewrite
                | TokenKind::Search
                | TokenKind::Set
                | TokenKind::Sort
                | TokenKind::Start
                | TokenKind::Stop
                | TokenKind::String_
                | TokenKind::Subtract
                | TokenKind::Unstring
                | TokenKind::Write_
        ) || (self.current_kind() == TokenKind::Word && {
            let t = self.current_text_upper();
            matches!(
                t.as_str(),
                "ACCEPT" | "ADD" | "ALTER" | "CALL" | "CANCEL" | "CLOSE"
                    | "COMPUTE" | "CONTINUE" | "DELETE" | "DISPLAY" | "DIVIDE"
                    | "ENTER" | "EVALUATE" | "EXIT" | "GO" | "GOBACK" | "IF"
                    | "INITIALIZE" | "INSPECT" | "MERGE" | "MOVE" | "MULTIPLY"
                    | "OPEN" | "PERFORM" | "READ" | "RELEASE" | "RETURN"
                    | "REWRITE" | "SEARCH" | "SET" | "SORT" | "START" | "STOP"
                    | "STRING" | "SUBTRACT" | "UNSTRING" | "WRITE"
            )
        })
    }

    /// Check if we are at a scope terminator (END-IF, END-PERFORM, etc.)
    fn at_scope_terminator(&self) -> bool {
        use cobol_lexer::TokenKind;
        matches!(
            self.current_kind(),
            TokenKind::EndIf
                | TokenKind::EndPerform
                | TokenKind::EndAdd
                | TokenKind::EndSubtract
                | TokenKind::EndMultiply
                | TokenKind::EndDivide
                | TokenKind::EndCompute
                | TokenKind::EndCall
                | TokenKind::EndEvaluate
                | TokenKind::EndRead
                | TokenKind::EndReturn
                | TokenKind::EndRewrite
                | TokenKind::EndSearch
                | TokenKind::EndStart
                | TokenKind::EndString
                | TokenKind::EndUnstring
                | TokenKind::EndWrite
                | TokenKind::EndDelete
                | TokenKind::When
        ) || (self.current_is_word_like() && {
            let t = self.current_text_upper();
            matches!(
                t.as_str(),
                "END-IF" | "END-PERFORM" | "END-ADD" | "END-SUBTRACT"
                    | "END-MULTIPLY" | "END-DIVIDE" | "END-COMPUTE"
                    | "END-CALL" | "END-EVALUATE" | "END-READ"
                    | "END-RETURN" | "END-REWRITE" | "END-SEARCH"
                    | "END-START" | "END-STRING" | "END-UNSTRING"
                    | "END-WRITE" | "END-DELETE"
                    | "WHEN"
            )
        })
    }

    /// Check if at ELSE keyword.
    fn at_else(&self) -> bool {
        self.at_kind(cobol_lexer::TokenKind::Else) || self.at_word("ELSE")
    }

    // ------------------------------------------------------------------
    // Top-level: SOURCE_FILE
    // ------------------------------------------------------------------

    fn parse_source_file(&mut self) {
        self.start_node(SyntaxKind::SOURCE_FILE);

        self.skip_ws();

        // IDENTIFICATION DIVISION
        if !self.at_end() && self.at_any_word(&["IDENTIFICATION", "ID"]) && self.peek_word_at(1, "DIVISION") {
            self.parse_identification_division();
        }

        self.skip_ws();

        // ENVIRONMENT DIVISION
        if !self.at_end() && self.at_word("ENVIRONMENT") && self.peek_word_at(1, "DIVISION") {
            self.parse_environment_division();
        }

        self.skip_ws();

        // DATA DIVISION
        if !self.at_end() && self.at_word("DATA") && self.peek_word_at(1, "DIVISION") {
            self.parse_data_division();
        }

        self.skip_ws();

        // PROCEDURE DIVISION
        if !self.at_end() && self.at_word("PROCEDURE") && self.peek_word_at(1, "DIVISION") {
            self.parse_procedure_division();
        }

        // Consume any remaining tokens (including EOF).
        // If there are non-whitespace, non-EOF tokens left, the source has
        // content that didn't match any division — report an error.
        if !self.at_end() && !self.at_kind(cobol_lexer::TokenKind::Eof) {
            self.error("unexpected tokens outside of any division");
            while !self.at_end() {
                self.bump();
            }
        } else {
            while !self.at_end() {
                self.bump();
            }
        }
        // Consume the EOF token itself if present.
        if self.pos < self.tokens.len() {
            self.bump();
        }

        // If there were no tokens at all, insert a synthetic EOF.
        if self.tokens.is_empty() {
            self.builder.token(
                CobolLanguage::kind_to_raw(SyntaxKind::EOF),
                "",
            );
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // IDENTIFICATION DIVISION
    // ------------------------------------------------------------------

    fn parse_identification_division(&mut self) {
        self.start_node(SyntaxKind::IDENTIFICATION_DIVISION);

        // "IDENTIFICATION" or "ID"
        self.skip_ws();
        self.bump(); // IDENTIFICATION / ID
        // "DIVISION"
        self.skip_ws();
        self.expect_word("DIVISION");
        // Period
        self.skip_ws();
        self.expect_period();

        self.skip_ws();

        // PROGRAM-ID clause
        if !self.at_end() && self.at_word("PROGRAM-ID") {
            self.parse_program_id_clause();
        }

        // Skip remaining identification clauses (AUTHOR, DATE-WRITTEN, etc.)
        // until the next division or end of file.
        self.skip_ws();
        while !self.at_end() && !self.at_division() {
            self.bump();
        }

        self.finish_node();
    }

    fn parse_program_id_clause(&mut self) {
        self.start_node(SyntaxKind::PROGRAM_ID_CLAUSE);

        // "PROGRAM-ID"
        self.skip_ws();
        self.bump(); // PROGRAM-ID keyword

        // Period after PROGRAM-ID
        self.skip_ws();
        self.expect_period();

        // Program name (a Word token)
        self.skip_ws();
        if !self.at_end() && (self.at_identifier() || self.current_is_word_like()) {
            self.bump_as(SyntaxKind::WORD);
        } else {
            self.error("expected program name");
        }

        // Optional period after program name
        self.skip_ws();
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // ENVIRONMENT DIVISION
    // ------------------------------------------------------------------

    fn parse_environment_division(&mut self) {
        self.start_node(SyntaxKind::ENVIRONMENT_DIVISION);

        // "ENVIRONMENT"
        self.skip_ws();
        self.bump();
        // "DIVISION"
        self.skip_ws();
        self.expect_word("DIVISION");
        // Period
        self.skip_ws();
        self.expect_period();

        // Parse environment division content until next division.
        self.skip_ws();
        while !self.at_end() && !self.at_division() {
            self.skip_ws();
            if self.at_end() || self.at_division() {
                break;
            }

            // INPUT-OUTPUT SECTION
            if self.at_word("INPUT-OUTPUT") && self.peek_word_at(1, "SECTION") {
                self.bump(); // INPUT-OUTPUT
                self.skip_ws();
                self.bump(); // SECTION
                self.skip_ws();
                if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
                    self.bump();
                }
                self.skip_ws();
                continue;
            }

            // FILE-CONTROL paragraph
            if self.at_word("FILE-CONTROL") || self.at_kind(cobol_lexer::TokenKind::Word) && self.current_text_upper() == "FILE-CONTROL" {
                self.bump(); // FILE-CONTROL
                self.skip_ws();
                if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
                    self.bump();
                }
                self.skip_ws();
                // Parse SELECT entries
                while !self.at_end() && !self.at_division() && !self.at_section_header()
                    && !self.at_word("I-O-CONTROL")
                {
                    self.skip_ws();
                    if self.at_end() || self.at_division() || self.at_section_header() {
                        break;
                    }
                    if self.at_word("SELECT") || self.at_kind(cobol_lexer::TokenKind::Select) {
                        self.parse_select_entry();
                    } else {
                        self.bump();
                    }
                }
                continue;
            }

            // Skip other content
            self.bump();
        }

        self.finish_node();
    }

    fn parse_select_entry(&mut self) {
        self.start_node(SyntaxKind::FILE_CONTROL_ENTRY);

        // Consume all tokens until period (inclusive)
        while !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            self.bump();
        }
        // Consume the period
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // DATA DIVISION
    // ------------------------------------------------------------------

    fn parse_data_division(&mut self) {
        self.start_node(SyntaxKind::DATA_DIVISION);

        // "DATA"
        self.skip_ws();
        self.bump();
        // "DIVISION"
        self.skip_ws();
        self.expect_word("DIVISION");
        // Period
        self.skip_ws();
        self.expect_period();

        self.skip_ws();

        // Parse sections within the DATA DIVISION
        while !self.at_end() && !self.at_division() {
            self.skip_ws();
            if self.at_end() || self.at_division() {
                break;
            }

            if self.at_word("WORKING-STORAGE") && self.peek_word_at(1, "SECTION") {
                self.parse_working_storage_section();
            } else if self.at_word("LINKAGE") && self.peek_word_at(1, "SECTION") {
                self.parse_linkage_section();
            } else if self.at_word("FILE") && self.peek_word_at(1, "SECTION") {
                self.parse_file_section();
            } else if self.at_section_header() {
                // Skip other sections (LOCAL-STORAGE, SCREEN, etc.)
                self.bump(); // section name
                self.skip_ws();
                self.bump(); // SECTION
                self.skip_ws();
                if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
                    self.bump();
                }
                // Skip content until next section or division
                self.skip_ws();
                while !self.at_end() && !self.at_division() && !self.at_section_header() {
                    self.bump();
                }
            } else {
                // Unknown content, skip a token
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_working_storage_section(&mut self) {
        self.start_node(SyntaxKind::WORKING_STORAGE_SECTION);

        // "WORKING-STORAGE"
        self.skip_ws();
        self.bump();
        // "SECTION"
        self.skip_ws();
        self.expect_word("SECTION");
        // Period
        self.skip_ws();
        self.expect_period();

        // Parse data items
        self.skip_ws();
        while !self.at_end() && !self.at_division() && !self.at_section_header() {
            self.skip_ws();
            if self.at_end() || self.at_division() || self.at_section_header() {
                break;
            }
            if self.at_level_number() {
                self.parse_data_item();
            } else {
                // Skip unexpected tokens
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_linkage_section(&mut self) {
        self.start_node(SyntaxKind::LINKAGE_SECTION);

        // "LINKAGE"
        self.skip_ws();
        self.bump();
        // "SECTION"
        self.skip_ws();
        self.expect_word("SECTION");
        // Period
        self.skip_ws();
        self.expect_period();

        // Parse data items
        self.skip_ws();
        while !self.at_end() && !self.at_division() && !self.at_section_header() {
            self.skip_ws();
            if self.at_end() || self.at_division() || self.at_section_header() {
                break;
            }
            if self.at_level_number() {
                self.parse_data_item();
            } else {
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_file_section(&mut self) {
        self.start_node(SyntaxKind::FILE_SECTION);

        // "FILE"
        self.skip_ws();
        self.bump();
        // "SECTION"
        self.skip_ws();
        self.expect_word("SECTION");
        // Period
        self.skip_ws();
        self.expect_period();

        // Parse FD entries and their record items
        self.skip_ws();
        while !self.at_end() && !self.at_division() && !self.at_section_header() {
            self.skip_ws();
            if self.at_end() || self.at_division() || self.at_section_header() {
                break;
            }
            if self.at_word("FD") || self.at_kind(cobol_lexer::TokenKind::Fd) {
                self.parse_fd_entry();
            } else if self.at_level_number() {
                self.parse_data_item();
            } else {
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_fd_entry(&mut self) {
        self.start_node(SyntaxKind::FD_ENTRY);

        // FD keyword
        self.skip_ws();
        self.bump(); // FD

        // File name and clauses until period
        self.skip_ws();
        while !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            self.bump();
        }
        // Period
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
            self.bump();
        }

        self.finish_node();
    }

    /// Check if current token is a COBOL level number (01-49, 66, 77, 88) or
    /// the lexer recognized it as Level/Level66/Level77/Level88.
    fn at_level_number(&self) -> bool {
        if self.at_end() {
            return false;
        }
        use cobol_lexer::TokenKind;
        match self.current_kind() {
            TokenKind::Level | TokenKind::Level66 | TokenKind::Level77 | TokenKind::Level88 => {
                true
            }
            TokenKind::IntegerLiteral => {
                // Check if text is a valid level number
                let text = self.current().text.trim();
                if let Ok(n) = text.parse::<u32>() {
                    (1..=49).contains(&n) || n == 66 || n == 77 || n == 88
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn parse_data_item(&mut self) {
        self.start_node(SyntaxKind::DATA_ITEM);

        // Level number
        self.skip_ws();
        self.bump(); // level number token

        // Data name or FILLER
        self.skip_ws();
        if !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_data_clause_start()
        {
            // Could be a data name (Word), FILLER keyword, or a level-88 value
            if self.at_word("FILLER") || self.at_identifier() || self.current_is_word_like() {
                self.bump_as(SyntaxKind::WORD);
            }
        }

        // Parse data clauses (PIC, VALUE, USAGE, OCCURS, REDEFINES, etc.)
        self.skip_ws();
        while !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            self.skip_ws();
            if self.at_end() || self.current_kind() == cobol_lexer::TokenKind::Period {
                break;
            }

            if self.at_any_word(&["PIC", "PICTURE"]) || self.at_kind(cobol_lexer::TokenKind::Pic)
                || self.at_kind(cobol_lexer::TokenKind::Picture)
            {
                self.parse_pic_clause();
            } else if self.at_word("VALUE") || self.at_word("VALUES")
                || self.at_kind(cobol_lexer::TokenKind::Value)
                || self.at_kind(cobol_lexer::TokenKind::Values)
            {
                self.parse_value_clause();
            } else if self.at_word("USAGE") || self.at_kind(cobol_lexer::TokenKind::Usage) {
                self.parse_usage_clause();
            } else if self.at_word("OCCURS") || self.at_kind(cobol_lexer::TokenKind::Occurs) {
                self.parse_occurs_clause();
            } else if self.at_word("REDEFINES") || self.at_kind(cobol_lexer::TokenKind::Redefines)
            {
                self.parse_redefines_clause();
            } else {
                // Unknown clause content, consume the token
                self.bump();
            }
        }

        // Trailing period
        self.skip_ws();
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
            self.bump();
        }

        self.finish_node();
    }

    fn at_data_clause_start(&self) -> bool {
        if self.at_end() {
            return false;
        }
        use cobol_lexer::TokenKind;
        matches!(
            self.current_kind(),
            TokenKind::Pic
                | TokenKind::Picture
                | TokenKind::Usage
                | TokenKind::Value
                | TokenKind::Values
                | TokenKind::Occurs
                | TokenKind::Redefines
                | TokenKind::Blank
                | TokenKind::Justified
                | TokenKind::Just
                | TokenKind::Sign_
                | TokenKind::Sync
                | TokenKind::Synchronized
                | TokenKind::Global
                | TokenKind::External
        ) || self.at_any_word(&[
            "PIC", "PICTURE", "USAGE", "VALUE", "VALUES", "OCCURS",
            "REDEFINES", "BLANK", "JUSTIFIED", "JUST", "SIGN", "SYNC",
            "SYNCHRONIZED", "GLOBAL", "EXTERNAL",
        ])
    }

    fn parse_pic_clause(&mut self) {
        self.start_node(SyntaxKind::PIC_CLAUSE);

        // PIC or PICTURE keyword
        self.skip_ws();
        self.bump();

        // Optional IS
        self.skip_ws();
        if !self.at_end() && self.at_word("IS") {
            self.bump();
        }

        // Consume the picture string tokens. The picture string can be
        // composed of multiple tokens: Word, LeftParen, IntegerLiteral,
        // RightParen, etc. We collect tokens until we hit whitespace followed
        // by a clause keyword, or a period, or a newline followed by
        // non-PIC-string content.
        self.skip_ws();
        while !self.at_end() {
            let k = self.current_kind();
            if k == cobol_lexer::TokenKind::Period {
                break;
            }
            if matches!(
                k,
                cobol_lexer::TokenKind::Whitespace
                    | cobol_lexer::TokenKind::Newline
                    | cobol_lexer::TokenKind::Comment
            ) {
                break;
            }
            self.bump();
        }

        self.finish_node();
    }

    fn parse_value_clause(&mut self) {
        self.start_node(SyntaxKind::VALUE_CLAUSE);

        // VALUE or VALUES keyword
        self.skip_ws();
        self.bump();

        // Optional IS / ARE
        self.skip_ws();
        if !self.at_end() && (self.at_word("IS") || self.at_word("ARE")) {
            self.bump();
        }

        // The value literal(s)
        self.skip_ws();
        if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            // Consume the value expression (could be literal, figurative, identifier)
            self.bump();
        }

        self.finish_node();
    }

    fn parse_usage_clause(&mut self) {
        self.start_node(SyntaxKind::USAGE_CLAUSE);

        // USAGE keyword
        self.skip_ws();
        self.bump();

        // Optional IS
        self.skip_ws();
        if !self.at_end() && self.at_word("IS") {
            self.bump();
        }

        // Usage type (COMP, COMP-3, BINARY, DISPLAY, INDEX, POINTER, etc.)
        self.skip_ws();
        if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            self.bump();
        }

        self.finish_node();
    }

    fn parse_occurs_clause(&mut self) {
        self.start_node(SyntaxKind::OCCURS_CLAUSE);

        // OCCURS keyword
        self.skip_ws();
        self.bump();

        // Consume the OCCURS clause content until we hit a period or another
        // data clause keyword.
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_data_clause_start()
        {
            self.bump();
            // skip_ws within the clause
            while !self.at_end()
                && matches!(
                    self.current_kind(),
                    cobol_lexer::TokenKind::Whitespace
                        | cobol_lexer::TokenKind::Newline
                        | cobol_lexer::TokenKind::Comment
                )
                && !self.at_data_clause_start()
            {
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_redefines_clause(&mut self) {
        self.start_node(SyntaxKind::REDEFINES_CLAUSE);

        // REDEFINES keyword
        self.skip_ws();
        self.bump();

        // The data name being redefined
        self.skip_ws();
        if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // PROCEDURE DIVISION
    // ------------------------------------------------------------------

    fn parse_procedure_division(&mut self) {
        self.start_node(SyntaxKind::PROCEDURE_DIVISION);

        // "PROCEDURE"
        self.skip_ws();
        self.bump();
        // "DIVISION"
        self.skip_ws();
        self.expect_word("DIVISION");

        // Optional USING clause
        self.skip_ws();
        if !self.at_end() && (self.at_word("USING") || self.at_kind(cobol_lexer::TokenKind::Using)) {
            self.bump(); // USING
            self.skip_ws();
            // Consume parameter list until period
            while !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                self.bump();
            }
        }

        // Optional RETURNING clause
        self.skip_ws();
        if !self.at_end() && (self.at_word("RETURNING") || self.at_kind(cobol_lexer::TokenKind::Returning)) {
            self.bump(); // RETURNING
            self.skip_ws();
            if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                self.bump(); // return identifier
            }
        }

        // Period after PROCEDURE DIVISION header
        self.skip_ws();
        self.expect_period();

        // Parse paragraphs and sections
        self.skip_ws();
        while !self.at_end() && !self.at_division() {
            self.skip_ws();
            if self.at_end() || self.at_division() {
                break;
            }

            if self.at_procedure_section() {
                self.parse_section();
            } else if self.at_paragraph_header() {
                self.parse_paragraph();
            } else if self.at_statement_start() {
                // Statements directly in the procedure division (no paragraph)
                self.parse_sentence();
            } else {
                // Unknown content, skip
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_section(&mut self) {
        self.start_node(SyntaxKind::SECTION);

        // Section name (Word)
        self.skip_ws();
        self.bump_as(SyntaxKind::WORD);

        // "SECTION"
        self.skip_ws();
        self.bump(); // SECTION keyword

        // Period
        self.skip_ws();
        self.expect_period();

        // Parse paragraphs within the section
        self.skip_ws();
        while !self.at_end() && !self.at_division() && !self.at_procedure_section() {
            self.skip_ws();
            if self.at_end() || self.at_division() || self.at_procedure_section() {
                break;
            }

            if self.at_paragraph_header() {
                self.parse_paragraph();
            } else if self.at_statement_start() {
                self.parse_sentence();
            } else {
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_paragraph(&mut self) {
        self.start_node(SyntaxKind::PARAGRAPH);

        // Paragraph name
        self.skip_ws();
        self.bump_as(SyntaxKind::WORD);

        // Period after paragraph name
        self.skip_ws();
        self.expect_period();

        // Parse sentences
        self.skip_ws();
        while !self.at_end()
            && !self.at_division()
            && !self.at_procedure_section()
            && !self.at_paragraph_header()
        {
            self.skip_ws();
            if self.at_end()
                || self.at_division()
                || self.at_procedure_section()
                || self.at_paragraph_header()
            {
                break;
            }

            if self.at_statement_start() {
                self.parse_sentence();
            } else {
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_sentence(&mut self) {
        self.start_node(SyntaxKind::SENTENCE);

        // Parse statements until we hit a period.
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_division()
            && !self.at_paragraph_header()
            && !self.at_procedure_section()
        {
            self.skip_ws();
            if self.at_end()
                || self.current_kind() == cobol_lexer::TokenKind::Period
                || self.at_division()
                || self.at_paragraph_header()
                || self.at_procedure_section()
            {
                break;
            }

            if self.at_statement_start() {
                self.parse_statement();
            } else {
                // Unexpected token in sentence. Record error and skip to
                // the next period.
                self.error("unexpected token in sentence");
                self.recover_to_period();
                self.finish_node(); // finish SENTENCE
                return;
            }
        }

        // Consume trailing period
        self.skip_ws();
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::Period {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // Statement dispatch
    // ------------------------------------------------------------------

    fn parse_statement(&mut self) {
        use cobol_lexer::TokenKind;
        let text = self.current_text_upper();
        let kind = self.current_kind();

        match kind {
            TokenKind::Display => self.parse_display_stmt(),
            TokenKind::Move => self.parse_move_stmt(),
            TokenKind::Add => self.parse_add_stmt(),
            TokenKind::Subtract => self.parse_subtract_stmt(),
            TokenKind::Multiply => self.parse_multiply_stmt(),
            TokenKind::Divide => self.parse_divide_stmt(),
            TokenKind::Compute => self.parse_compute_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::Perform => self.parse_perform_stmt(),
            TokenKind::Stop => self.parse_stop_stmt(),
            TokenKind::GoTo => self.parse_go_to_stmt(),
            TokenKind::Exit => self.parse_exit_stmt(),
            TokenKind::Call => self.parse_call_stmt(),
            TokenKind::Accept => self.parse_accept_stmt(),
            TokenKind::Continue => self.parse_continue_stmt(),
            TokenKind::Evaluate => self.parse_evaluate_stmt(),
            TokenKind::Initialize => self.parse_initialize_stmt(),
            TokenKind::Set => self.parse_set_stmt(),
            TokenKind::GoBack => self.parse_stop_stmt(), // GOBACK is like STOP
            TokenKind::Open => self.parse_open_stmt(),
            TokenKind::Close => self.parse_close_stmt(),
            TokenKind::Read => self.parse_read_stmt(),
            TokenKind::Write_ => self.parse_write_stmt(),
            TokenKind::Delete => self.parse_generic_stmt(SyntaxKind::DELETE_STMT),
            TokenKind::Rewrite => self.parse_generic_stmt(SyntaxKind::REWRITE_STMT),
            TokenKind::Release => self.parse_generic_stmt(SyntaxKind::RELEASE_STMT),
            TokenKind::Return_ => self.parse_generic_stmt(SyntaxKind::RETURN_STMT),
            TokenKind::Sort => self.parse_generic_stmt(SyntaxKind::SORT_STMT),
            TokenKind::Merge => self.parse_generic_stmt(SyntaxKind::MERGE_STMT),
            TokenKind::Search => self.parse_generic_stmt(SyntaxKind::SEARCH_STMT),
            TokenKind::Inspect => self.parse_inspect_stmt(),
            TokenKind::String_ => self.parse_generic_stmt(SyntaxKind::STRING_STMT),
            TokenKind::Unstring => self.parse_unstring_stmt(),
            TokenKind::Start => self.parse_generic_stmt(SyntaxKind::START_STMT),
            TokenKind::Alter => self.parse_generic_stmt(SyntaxKind::ALTER_STMT),
            TokenKind::Enter => self.parse_generic_stmt(SyntaxKind::STATEMENT),
            _ if kind == TokenKind::Word => {
                // Dispatch by text content for Word tokens
                match text.as_str() {
                    "DISPLAY" => self.parse_display_stmt(),
                    "MOVE" => self.parse_move_stmt(),
                    "ADD" => self.parse_add_stmt(),
                    "SUBTRACT" => self.parse_subtract_stmt(),
                    "MULTIPLY" => self.parse_multiply_stmt(),
                    "DIVIDE" => self.parse_divide_stmt(),
                    "COMPUTE" => self.parse_compute_stmt(),
                    "IF" => self.parse_if_stmt(),
                    "PERFORM" => self.parse_perform_stmt(),
                    "STOP" => self.parse_stop_stmt(),
                    "GO" => self.parse_go_to_stmt(),
                    "EXIT" => self.parse_exit_stmt(),
                    "CALL" => self.parse_call_stmt(),
                    "ACCEPT" => self.parse_accept_stmt(),
                    "CONTINUE" => self.parse_continue_stmt(),
                    "EVALUATE" => self.parse_evaluate_stmt(),
                    "GOBACK" => self.parse_stop_stmt(),
                    "INITIALIZE" => self.parse_initialize_stmt(),
                    "SET" => self.parse_set_stmt(),
                    "OPEN" => self.parse_open_stmt(),
                    "CLOSE" => self.parse_close_stmt(),
                    "READ" => self.parse_read_stmt(),
                    "WRITE" => self.parse_write_stmt(),
                    "STRING" => self.parse_generic_stmt(SyntaxKind::STRING_STMT),
                    "UNSTRING" => self.parse_unstring_stmt(),
                    "INSPECT" => self.parse_inspect_stmt(),
                    _ => self.parse_generic_stmt(SyntaxKind::STATEMENT),
                }
            }
            _ => self.parse_generic_stmt(SyntaxKind::STATEMENT),
        }
    }

    // ------------------------------------------------------------------
    // INSPECT statement
    // ------------------------------------------------------------------

    fn parse_inspect_stmt(&mut self) {
        self.start_node(SyntaxKind::INSPECT_STMT);
        self.bump(); // INSPECT
        self.skip_ws();

        // Consume all tokens until period, next statement start, scope terminator,
        // ELSE, or division boundary. The generic approach works well here because
        // the HIR lowerer parses the token stream semantically.
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
            && !self.at_division()
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // UNSTRING statement
    // ------------------------------------------------------------------

    fn parse_unstring_stmt(&mut self) {
        self.start_node(SyntaxKind::UNSTRING_STMT);
        self.bump(); // UNSTRING
        self.skip_ws();

        // Consume tokens, handling ON OVERFLOW / NOT ON OVERFLOW sub-statements
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_word("END-UNSTRING")
            && !self.at_kind(cobol_lexer::TokenKind::EndUnstring)
            && !self.at_division()
        {
            // Handle ON OVERFLOW
            if self.at_word("ON") && self.peek_word_at(1, "OVERFLOW") {
                self.bump(); // ON
                self.skip_ws();
                self.bump(); // OVERFLOW
                self.skip_ws();
                // Parse imperative statements in the ON OVERFLOW handler
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::Period
                    && !self.at_word("END-UNSTRING")
                    && !self.at_kind(cobol_lexer::TokenKind::EndUnstring)
                    && !self.at_word("NOT")
                    && !self.at_division()
                {
                    if self.at_statement_start() {
                        self.parse_statement();
                    } else {
                        self.bump();
                    }
                    self.skip_ws();
                }
                continue;
            }

            // Handle NOT ON OVERFLOW
            if self.at_word("NOT") && self.peek_word_at(1, "ON") {
                self.bump(); // NOT
                self.skip_ws();
                self.bump(); // ON
                self.skip_ws();
                if !self.at_end() && self.at_word("OVERFLOW") {
                    self.bump(); // OVERFLOW
                }
                self.skip_ws();
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::Period
                    && !self.at_word("END-UNSTRING")
                    && !self.at_kind(cobol_lexer::TokenKind::EndUnstring)
                    && !self.at_division()
                {
                    if self.at_statement_start() {
                        self.parse_statement();
                    } else {
                        self.bump();
                    }
                    self.skip_ws();
                }
                continue;
            }

            // Handle OVERFLOW without ON prefix
            if self.at_word("OVERFLOW") {
                self.bump(); // OVERFLOW
                self.skip_ws();
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::Period
                    && !self.at_word("END-UNSTRING")
                    && !self.at_kind(cobol_lexer::TokenKind::EndUnstring)
                    && !self.at_word("NOT")
                    && !self.at_division()
                {
                    if self.at_statement_start() {
                        self.parse_statement();
                    } else {
                        self.bump();
                    }
                    self.skip_ws();
                }
                continue;
            }

            if self.at_statement_start() {
                break;
            }

            self.bump();
        }

        // Optional END-UNSTRING
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-UNSTRING")
                || self.at_kind(cobol_lexer::TokenKind::EndUnstring))
        {
            self.bump();
        }

        self.finish_node();
    }

    /// Generic statement parser: wraps tokens from the statement keyword until
    /// the next statement start, scope terminator, period, ELSE, or division
    /// boundary.
    fn parse_generic_stmt(&mut self, kind: SyntaxKind) {
        self.start_node(kind);
        self.bump(); // statement keyword
        self.skip_ws();

        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
            && !self.at_division()
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // File I/O statements
    // ------------------------------------------------------------------

    fn parse_open_stmt(&mut self) {
        self.start_node(SyntaxKind::OPEN_STMT);
        self.bump(); // OPEN
        self.skip_ws();

        // Consume tokens until period, next statement, or scope terminator
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
            && !self.at_division()
        {
            self.bump();
        }

        self.finish_node();
    }

    fn parse_close_stmt(&mut self) {
        self.start_node(SyntaxKind::CLOSE_STMT);
        self.bump(); // CLOSE
        self.skip_ws();

        // Consume tokens until period, next statement, or scope terminator
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
            && !self.at_division()
        {
            self.bump();
        }

        self.finish_node();
    }

    fn parse_write_stmt(&mut self) {
        self.start_node(SyntaxKind::WRITE_STMT);
        self.bump(); // WRITE
        self.skip_ws();

        // Consume tokens until period, next statement, scope terminator, or END-WRITE
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
            && !self.at_division()
        {
            self.bump();
        }

        // Optional END-WRITE
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-WRITE") || self.at_kind(cobol_lexer::TokenKind::EndWrite))
        {
            self.bump();
        }

        self.finish_node();
    }

    fn parse_read_stmt(&mut self) {
        self.start_node(SyntaxKind::READ_STMT);
        self.bump(); // READ
        self.skip_ws();

        // Consume tokens until AT END, NOT AT END, END-READ, period, or next statement
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_word("END-READ")
            && !self.at_kind(cobol_lexer::TokenKind::EndRead)
            && !self.at_division()
        {
            // Check for AT END
            if self.at_word("AT") {
                if self.peek_word_at(1, "END") {
                    self.bump(); // AT
                    self.skip_ws();
                    self.bump(); // END
                    self.skip_ws();
                    // Parse statements in the AT END handler
                    while !self.at_end()
                        && self.current_kind() != cobol_lexer::TokenKind::Period
                        && !self.at_word("END-READ")
                        && !self.at_kind(cobol_lexer::TokenKind::EndRead)
                        && !self.at_word("NOT")
                        && !self.at_division()
                    {
                        if self.at_statement_start() {
                            self.parse_statement();
                        } else {
                            self.bump();
                        }
                        self.skip_ws();
                    }
                    continue;
                }
            }

            // Check for NOT AT END
            if self.at_word("NOT") && self.peek_word_at(1, "AT") {
                self.bump(); // NOT
                self.skip_ws();
                self.bump(); // AT
                self.skip_ws();
                if !self.at_end() && self.at_word("END") {
                    self.bump(); // END
                }
                self.skip_ws();
                // Parse statements in NOT AT END handler
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::Period
                    && !self.at_word("END-READ")
                    && !self.at_kind(cobol_lexer::TokenKind::EndRead)
                    && !self.at_division()
                {
                    if self.at_statement_start() {
                        self.parse_statement();
                    } else {
                        self.bump();
                    }
                    self.skip_ws();
                }
                continue;
            }

            if self.at_statement_start() {
                break;
            }

            self.bump();
        }

        // Optional END-READ
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-READ") || self.at_kind(cobol_lexer::TokenKind::EndRead))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // DISPLAY statement
    // ------------------------------------------------------------------

    fn parse_display_stmt(&mut self) {
        self.start_node(SyntaxKind::DISPLAY_STMT);

        // DISPLAY keyword
        self.skip_ws();
        self.bump();

        // Operands: (identifier | literal)+
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
        {
            self.skip_ws();
            if self.at_end()
                || self.current_kind() == cobol_lexer::TokenKind::Period
                || self.at_statement_start()
                || self.at_scope_terminator()
                || self.at_else()
            {
                break;
            }

            // UPON clause
            if self.at_word("UPON") || self.at_kind(cobol_lexer::TokenKind::Word) && self.current_text_upper() == "UPON" {
                self.bump(); // UPON
                self.skip_ws();
                if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                    self.bump(); // device name
                }
                continue;
            }

            // WITH NO ADVANCING
            if self.at_word("WITH") {
                self.bump();
                self.skip_ws();
                if self.at_word("NO") {
                    self.bump();
                    self.skip_ws();
                    if self.at_word("ADVANCING") {
                        self.bump();
                    }
                }
                continue;
            }
            if self.at_word("NO") {
                self.bump();
                self.skip_ws();
                if self.at_word("ADVANCING") {
                    self.bump();
                }
                continue;
            }

            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // MOVE statement
    // ------------------------------------------------------------------

    fn parse_move_stmt(&mut self) {
        self.start_node(SyntaxKind::MOVE_STMT);

        // MOVE keyword
        self.skip_ws();
        self.bump();

        self.skip_ws();

        // MOVE CORRESPONDING / CORR
        if !self.at_end()
            && (self.at_word("CORRESPONDING")
                || self.at_word("CORR")
                || self.at_kind(cobol_lexer::TokenKind::Corresponding)
                || self.at_kind(cobol_lexer::TokenKind::Corr))
        {
            self.bump(); // CORRESPONDING/CORR
        }

        // Source operand
        self.skip_ws();
        if !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_word("TO")
            && !self.at_kind(cobol_lexer::TokenKind::To)
        {
            self.bump();
        }

        // TO keyword
        self.skip_ws();
        if !self.at_end() && (self.at_word("TO") || self.at_kind(cobol_lexer::TokenKind::To)) {
            self.bump();
        }

        // Destination operand(s)
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // ADD statement
    // ------------------------------------------------------------------

    fn parse_add_stmt(&mut self) {
        self.start_node(SyntaxKind::ADD_STMT);

        // ADD keyword
        self.skip_ws();
        self.bump();

        // Consume everything until period, next statement, or scope terminator
        self.consume_arithmetic_stmt_body(&["END-ADD"]);

        // Optional END-ADD
        self.skip_ws();
        if !self.at_end() && (self.at_word("END-ADD") || self.at_kind(cobol_lexer::TokenKind::EndAdd)) {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // SUBTRACT statement
    // ------------------------------------------------------------------

    fn parse_subtract_stmt(&mut self) {
        self.start_node(SyntaxKind::SUBTRACT_STMT);

        self.skip_ws();
        self.bump(); // SUBTRACT

        self.consume_arithmetic_stmt_body(&["END-SUBTRACT"]);

        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-SUBTRACT")
                || self.at_kind(cobol_lexer::TokenKind::EndSubtract))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // MULTIPLY statement
    // ------------------------------------------------------------------

    fn parse_multiply_stmt(&mut self) {
        self.start_node(SyntaxKind::MULTIPLY_STMT);

        self.skip_ws();
        self.bump(); // MULTIPLY

        self.consume_arithmetic_stmt_body(&["END-MULTIPLY"]);

        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-MULTIPLY")
                || self.at_kind(cobol_lexer::TokenKind::EndMultiply))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // DIVIDE statement
    // ------------------------------------------------------------------

    fn parse_divide_stmt(&mut self) {
        self.start_node(SyntaxKind::DIVIDE_STMT);

        self.skip_ws();
        self.bump(); // DIVIDE

        self.consume_arithmetic_stmt_body(&["END-DIVIDE"]);

        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-DIVIDE")
                || self.at_kind(cobol_lexer::TokenKind::EndDivide))
        {
            self.bump();
        }

        self.finish_node();
    }

    /// Helper for arithmetic statements: consume body tokens until we see
    /// the period, a next statement, a scope terminator, or the specific
    /// END-xxx keyword. This handles ON SIZE ERROR / NOT ON SIZE ERROR
    /// sub-clauses by consuming them as part of the body.
    fn consume_arithmetic_stmt_body(&mut self, end_keywords: &[&str]) {
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_division()
        {
            // Check for our specific END-xxx keyword
            if end_keywords.iter().any(|kw| self.at_word(kw)) || self.at_scope_terminator() {
                break;
            }
            // If we are at a new statement that is NOT preceded by SIZE ERROR
            // context, break. But ON SIZE ERROR / NOT ON SIZE ERROR may contain
            // statements, so we handle those by consuming them inline.
            if self.at_statement_start() {
                // Check if this is a SIZE ERROR handler by looking at what
                // preceded. This is hard without backtracking, so we use a
                // simple heuristic: if we previously saw SIZE ERROR, these
                // statements are part of the handler. For simplicity we just
                // consume the statement here.
                // Actually we just let statements flow for now.
                break;
            }
            self.bump();
        }
    }

    // ------------------------------------------------------------------
    // COMPUTE statement
    // ------------------------------------------------------------------

    fn parse_compute_stmt(&mut self) {
        self.start_node(SyntaxKind::COMPUTE_STMT);

        self.skip_ws();
        self.bump(); // COMPUTE

        // Target identifiers before "="
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::EqualSign
            && self.current_kind() != cobol_lexer::TokenKind::Period
        {
            self.bump();
        }

        // "="
        self.skip_ws();
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::EqualSign {
            self.bump();
        }

        // Arithmetic expression
        self.skip_ws();
        self.parse_arithmetic_expr();

        // Optional ON SIZE ERROR / NOT ON SIZE ERROR
        // Only enter handler if we see ON/SIZE/NOT keywords.  Otherwise the
        // next statement (e.g. IF) should be parsed at sentence level.
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("ON") || self.at_word("SIZE") || self.at_word("NOT"))
        {
            while !self.at_end()
                && self.current_kind() != cobol_lexer::TokenKind::Period
                && !self.at_word("END-COMPUTE")
                && !self.at_kind(cobol_lexer::TokenKind::EndCompute)
                && !self.at_scope_terminator()
                && !self.at_division()
            {
                if self.at_statement_start() {
                    // Statements inside SIZE ERROR handler
                    self.parse_statement();
                } else {
                    self.bump();
                }
                self.skip_ws();
            }
        }

        // Optional END-COMPUTE
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-COMPUTE")
                || self.at_kind(cobol_lexer::TokenKind::EndCompute))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // Arithmetic expression
    // ------------------------------------------------------------------

    fn parse_arithmetic_expr(&mut self) {
        self.start_node(SyntaxKind::ARITHMETIC_EXPR);
        self.parse_expr_additive();
        self.finish_node();
    }

    /// expr = term (('+' | '-') term)*
    fn parse_expr_additive(&mut self) {
        self.parse_expr_multiplicative();

        self.skip_ws();
        while !self.at_end()
            && (self.current_kind() == cobol_lexer::TokenKind::Plus
                || self.current_kind() == cobol_lexer::TokenKind::Minus)
        {
            self.bump(); // operator
            self.skip_ws();
            self.parse_expr_multiplicative();
            self.skip_ws();
        }
    }

    /// term = factor (('*' | '/') factor)*
    fn parse_expr_multiplicative(&mut self) {
        self.parse_expr_power();

        self.skip_ws();
        while !self.at_end()
            && (self.current_kind() == cobol_lexer::TokenKind::Star
                || self.current_kind() == cobol_lexer::TokenKind::Slash)
        {
            self.bump(); // operator
            self.skip_ws();
            self.parse_expr_power();
            self.skip_ws();
        }
    }

    /// factor = ('+' | '-')? atom ('**' factor)?
    fn parse_expr_power(&mut self) {
        self.skip_ws();

        // Optional unary sign
        if !self.at_end()
            && (self.current_kind() == cobol_lexer::TokenKind::Plus
                || self.current_kind() == cobol_lexer::TokenKind::Minus)
        {
            self.bump();
            self.skip_ws();
        }

        self.parse_expr_atom();

        // Optional ** (exponentiation)
        self.skip_ws();
        if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::DoubleStar {
            self.bump();
            self.skip_ws();
            self.parse_expr_power(); // right-associative
        }
    }

    /// atom = identifier | literal | '(' expr ')'
    fn parse_expr_atom(&mut self) {
        self.skip_ws();
        if self.at_end() {
            return;
        }

        if self.current_kind() == cobol_lexer::TokenKind::LeftParen {
            self.bump(); // (
            self.skip_ws();
            self.parse_expr_additive();
            self.skip_ws();
            if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::RightParen {
                self.bump(); // )
            } else {
                self.error("expected ')'");
            }
        } else if self.at_literal() || self.at_figurative() || self.at_identifier()
            || self.current_is_word_like()
        {
            self.bump();
            // Handle qualified names (identifier OF/IN identifier) and subscripts
            self.skip_ws();
            while !self.at_end() && (self.at_word("OF") || self.at_word("IN")
                || self.at_kind(cobol_lexer::TokenKind::Of)
                || self.at_kind(cobol_lexer::TokenKind::In))
            {
                self.bump(); // OF/IN
                self.skip_ws();
                if !self.at_end() && (self.at_identifier() || self.current_is_word_like()) {
                    self.bump();
                }
                self.skip_ws();
            }
            // Subscript
            if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::LeftParen {
                self.bump(); // (
                self.skip_ws();
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::RightParen
                {
                    self.bump();
                }
                if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::RightParen {
                    self.bump(); // )
                }
            }
        } else {
            // Not a valid atom, but do not panic. Just report an error.
            self.error("expected expression");
        }
    }

    // ------------------------------------------------------------------
    // IF statement
    // ------------------------------------------------------------------

    fn parse_if_stmt(&mut self) {
        self.start_node(SyntaxKind::IF_STMT);

        // IF keyword
        self.skip_ws();
        self.bump();

        // Condition
        self.skip_ws();
        self.parse_condition();

        // Optional THEN
        self.skip_ws();
        if !self.at_end() && (self.at_word("THEN") || self.at_kind(cobol_lexer::TokenKind::Then)) {
            self.bump();
        }

        // Then-branch statements
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_else()
            && !self.at_word("END-IF")
            && !self.at_kind(cobol_lexer::TokenKind::EndIf)
            && !self.at_division()
        {
            self.skip_ws();
            if self.at_end()
                || self.current_kind() == cobol_lexer::TokenKind::Period
                || self.at_else()
                || self.at_word("END-IF")
                || self.at_kind(cobol_lexer::TokenKind::EndIf)
                || self.at_division()
            {
                break;
            }
            if self.at_statement_start() {
                self.parse_statement();
            } else {
                break;
            }
            self.skip_ws();
        }

        // Optional ELSE branch
        self.skip_ws();
        if !self.at_end() && self.at_else() {
            self.bump(); // ELSE

            self.skip_ws();
            while !self.at_end()
                && self.current_kind() != cobol_lexer::TokenKind::Period
                && !self.at_word("END-IF")
                && !self.at_kind(cobol_lexer::TokenKind::EndIf)
                && !self.at_division()
            {
                self.skip_ws();
                if self.at_end()
                    || self.current_kind() == cobol_lexer::TokenKind::Period
                    || self.at_word("END-IF")
                    || self.at_kind(cobol_lexer::TokenKind::EndIf)
                    || self.at_division()
                {
                    break;
                }
                if self.at_statement_start() {
                    self.parse_statement();
                } else {
                    break;
                }
                self.skip_ws();
            }
        }

        // Optional END-IF
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-IF") || self.at_kind(cobol_lexer::TokenKind::EndIf))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // Condition parsing
    // ------------------------------------------------------------------

    fn parse_condition(&mut self) {
        self.start_node(SyntaxKind::CONDITION_EXPR);
        self.parse_condition_or();
        self.finish_node();
    }

    /// condition = simple_condition (("AND" | "OR") simple_condition)*
    fn parse_condition_or(&mut self) {
        self.parse_condition_simple();

        self.skip_ws();
        while !self.at_end()
            && (self.at_word("AND")
                || self.at_word("OR")
                || self.at_kind(cobol_lexer::TokenKind::And)
                || self.at_kind(cobol_lexer::TokenKind::Or))
        {
            self.bump(); // AND / OR
            self.skip_ws();
            self.parse_condition_simple();
            self.skip_ws();
        }
    }

    /// simple_condition = "NOT"? comparison
    /// comparison = expr relop expr
    fn parse_condition_simple(&mut self) {
        self.skip_ws();
        if self.at_end() {
            return;
        }

        // Optional NOT prefix
        if self.at_word("NOT") || self.at_kind(cobol_lexer::TokenKind::Not) {
            self.bump();
            self.skip_ws();
        }

        // Left side: an atom (identifier, literal, figurative)
        if self.at_literal() || self.at_figurative() || self.at_identifier()
            || self.current_is_word_like()
        {
            self.bump();
            // Handle qualified names
            self.skip_ws();
            while !self.at_end() && (self.at_word("OF") || self.at_word("IN")
                || self.at_kind(cobol_lexer::TokenKind::Of)
                || self.at_kind(cobol_lexer::TokenKind::In))
            {
                self.bump();
                self.skip_ws();
                if !self.at_end() && (self.at_identifier() || self.current_is_word_like()) {
                    self.bump();
                }
                self.skip_ws();
            }
            // Subscript
            if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::LeftParen {
                self.bump();
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::RightParen
                {
                    self.bump();
                }
                if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::RightParen {
                    self.bump();
                }
            }
        } else if self.current_kind() == cobol_lexer::TokenKind::LeftParen {
            // Parenthesized condition
            self.bump(); // (
            self.skip_ws();
            self.parse_condition_or();
            self.skip_ws();
            if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::RightParen {
                self.bump(); // )
            }
            return;
        } else {
            return;
        }

        // Check for relational operator
        self.skip_ws();
        if self.at_end() {
            return;
        }

        if self.at_relop() {
            self.consume_relop();

            // Right side
            self.skip_ws();
            if self.at_literal() || self.at_figurative() || self.at_identifier()
                || self.current_is_word_like()
            {
                self.bump();
                // Handle qualified names on right side
                self.skip_ws();
                while !self.at_end() && (self.at_word("OF") || self.at_word("IN")
                    || self.at_kind(cobol_lexer::TokenKind::Of)
                    || self.at_kind(cobol_lexer::TokenKind::In))
                {
                    self.bump();
                    self.skip_ws();
                    if !self.at_end() && (self.at_identifier() || self.current_is_word_like()) {
                        self.bump();
                    }
                    self.skip_ws();
                }
            } else if self.current_kind() == cobol_lexer::TokenKind::LeftParen {
                self.bump();
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::RightParen
                {
                    self.bump();
                }
                if !self.at_end() && self.current_kind() == cobol_lexer::TokenKind::RightParen {
                    self.bump();
                }
            }
        }
        // If no relop, this is just a truth-value condition (e.g. identifier alone or level-88)
    }

    fn at_relop(&self) -> bool {
        if self.at_end() {
            return false;
        }
        use cobol_lexer::TokenKind;
        matches!(
            self.current_kind(),
            TokenKind::EqualSign
                | TokenKind::GreaterThan
                | TokenKind::LessThan
                | TokenKind::GreaterEqual
                | TokenKind::LessEqual
                | TokenKind::NotEqual
        ) || self.at_any_word(&["EQUAL", "EQUALS", "GREATER", "LESS", "NOT"])
            || self.at_kind(TokenKind::Not)
    }

    fn consume_relop(&mut self) {
        self.skip_ws();
        use cobol_lexer::TokenKind;

        match self.current_kind() {
            TokenKind::EqualSign
            | TokenKind::GreaterThan
            | TokenKind::LessThan
            | TokenKind::GreaterEqual
            | TokenKind::LessEqual
            | TokenKind::NotEqual => {
                self.bump();
            }
            _ => {
                // Word-form relational operators
                if self.at_word("NOT") || self.at_kind(TokenKind::Not) {
                    self.bump(); // NOT
                    self.skip_ws();
                    // NOT EQUAL, NOT GREATER, NOT LESS, NOT =, NOT >, NOT <
                    if !self.at_end() {
                        self.bump(); // the actual comparison word/operator
                        self.skip_ws();
                        // Optional THAN/TO
                        if !self.at_end() && self.at_any_word(&["THAN", "TO"]) {
                            self.bump();
                        }
                    }
                } else if self.at_any_word(&["EQUAL", "EQUALS"]) {
                    self.bump();
                    self.skip_ws();
                    if !self.at_end() && self.at_word("TO") {
                        self.bump();
                    }
                } else if self.at_word("GREATER") {
                    self.bump();
                    self.skip_ws();
                    if !self.at_end() && self.at_word("THAN") {
                        self.bump();
                        self.skip_ws();
                    }
                    if !self.at_end() && self.at_word("OR") {
                        self.bump();
                        self.skip_ws();
                        if !self.at_end() && self.at_word("EQUAL") {
                            self.bump();
                            self.skip_ws();
                            if !self.at_end() && self.at_word("TO") {
                                self.bump();
                            }
                        }
                    }
                } else if self.at_word("LESS") {
                    self.bump();
                    self.skip_ws();
                    if !self.at_end() && self.at_word("THAN") {
                        self.bump();
                        self.skip_ws();
                    }
                    if !self.at_end() && self.at_word("OR") {
                        self.bump();
                        self.skip_ws();
                        if !self.at_end() && self.at_word("EQUAL") {
                            self.bump();
                            self.skip_ws();
                            if !self.at_end() && self.at_word("TO") {
                                self.bump();
                            }
                        }
                    }
                } else {
                    self.bump(); // consume whatever operator-like token
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // PERFORM statement
    // ------------------------------------------------------------------

    fn parse_perform_stmt(&mut self) {
        self.start_node(SyntaxKind::PERFORM_STMT);

        // PERFORM keyword
        self.skip_ws();
        self.bump();

        self.skip_ws();
        if self.at_end() || self.current_kind() == cobol_lexer::TokenKind::Period {
            self.finish_node();
            return;
        }

        // Determine the form of PERFORM:
        // 1. PERFORM paragraph-name [THRU paragraph-name]
        // 2. PERFORM paragraph-name VARYING ...
        // 3. PERFORM paragraph-name n TIMES
        // 4. PERFORM ... END-PERFORM (inline)

        // If the next non-ws token after PERFORM is a statement keyword, this
        // is an inline PERFORM.
        if self.at_statement_start() {
            // Inline PERFORM: statements until END-PERFORM
            while !self.at_end()
                && self.current_kind() != cobol_lexer::TokenKind::Period
                && !self.at_word("END-PERFORM")
                && !self.at_kind(cobol_lexer::TokenKind::EndPerform)
                && !self.at_division()
            {
                self.skip_ws();
                if self.at_end()
                    || self.current_kind() == cobol_lexer::TokenKind::Period
                    || self.at_word("END-PERFORM")
                    || self.at_kind(cobol_lexer::TokenKind::EndPerform)
                    || self.at_division()
                {
                    break;
                }
                if self.at_statement_start() {
                    self.parse_statement();
                } else {
                    self.bump();
                }
                self.skip_ws();
            }

            self.skip_ws();
            if !self.at_end()
                && (self.at_word("END-PERFORM")
                    || self.at_kind(cobol_lexer::TokenKind::EndPerform))
            {
                self.bump();
            }

            self.finish_node();
            return;
        }

        // Paragraph/section reference
        if self.at_identifier() || self.current_is_word_like() {
            // Check if this is VARYING (inline PERFORM VARYING without a target)
            if self.at_word("VARYING") || self.at_kind(cobol_lexer::TokenKind::Varying) {
                self.parse_perform_varying_clause();
                // Inline body
                self.skip_ws();
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::Period
                    && !self.at_word("END-PERFORM")
                    && !self.at_kind(cobol_lexer::TokenKind::EndPerform)
                    && !self.at_division()
                {
                    self.skip_ws();
                    if self.at_end()
                        || self.current_kind() == cobol_lexer::TokenKind::Period
                        || self.at_word("END-PERFORM")
                        || self.at_kind(cobol_lexer::TokenKind::EndPerform)
                    {
                        break;
                    }
                    if self.at_statement_start() {
                        self.parse_statement();
                    } else {
                        self.bump();
                    }
                    self.skip_ws();
                }

                self.skip_ws();
                if !self.at_end()
                    && (self.at_word("END-PERFORM")
                        || self.at_kind(cobol_lexer::TokenKind::EndPerform))
                {
                    self.bump();
                }

                self.finish_node();
                return;
            }

            // Not VARYING, so this is a paragraph/section name
            self.bump(); // paragraph name
        }

        // Optional THRU/THROUGH
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("THRU")
                || self.at_word("THROUGH")
                || self.at_kind(cobol_lexer::TokenKind::Thru)
                || self.at_kind(cobol_lexer::TokenKind::Through))
        {
            self.bump(); // THRU/THROUGH
            self.skip_ws();
            if !self.at_end()
                && self.current_kind() != cobol_lexer::TokenKind::Period
                && (self.at_identifier() || self.current_is_word_like())
            {
                self.bump(); // end paragraph name
            }
        }

        // Optional VARYING clause
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("VARYING") || self.at_kind(cobol_lexer::TokenKind::Varying))
        {
            self.parse_perform_varying_clause();
        }

        // Optional n TIMES
        self.skip_ws();
        if !self.at_end() && self.at_literal() {
            self.bump(); // n
            self.skip_ws();
            if !self.at_end() && (self.at_word("TIMES") || self.at_kind(cobol_lexer::TokenKind::Times)) {
                self.bump(); // TIMES
            }
        } else if !self.at_end() && self.at_identifier() {
            // Could be identifier TIMES
            // Peek: if next non-ws is TIMES, consume both
            if self.peek_word_at(1, "TIMES") {
                self.bump(); // identifier
                self.skip_ws();
                self.bump(); // TIMES
            }
            // Otherwise it might be UNTIL
        }

        // Optional UNTIL
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("UNTIL") || self.at_kind(cobol_lexer::TokenKind::Until))
        {
            self.bump(); // UNTIL
            self.skip_ws();
            self.parse_condition();
        }

        // If followed by statements and END-PERFORM, consume inline body
        self.skip_ws();
        if !self.at_end() && self.at_statement_start() {
            while !self.at_end()
                && self.current_kind() != cobol_lexer::TokenKind::Period
                && !self.at_word("END-PERFORM")
                && !self.at_kind(cobol_lexer::TokenKind::EndPerform)
                && !self.at_division()
            {
                self.skip_ws();
                if self.at_end()
                    || self.current_kind() == cobol_lexer::TokenKind::Period
                    || self.at_word("END-PERFORM")
                    || self.at_kind(cobol_lexer::TokenKind::EndPerform)
                {
                    break;
                }
                if self.at_statement_start() {
                    self.parse_statement();
                } else {
                    self.bump();
                }
                self.skip_ws();
            }

            self.skip_ws();
            if !self.at_end()
                && (self.at_word("END-PERFORM")
                    || self.at_kind(cobol_lexer::TokenKind::EndPerform))
            {
                self.bump();
            }
        }

        self.finish_node();
    }

    fn parse_perform_varying_clause(&mut self) {
        // VARYING identifier FROM expr BY expr UNTIL condition
        self.skip_ws();
        self.bump(); // VARYING

        self.skip_ws();
        if !self.at_end() && (self.at_identifier() || self.current_is_word_like()) {
            self.bump(); // loop variable
        }

        self.skip_ws();
        if !self.at_end() && (self.at_word("FROM") || self.at_kind(cobol_lexer::TokenKind::From)) {
            self.bump(); // FROM
            self.skip_ws();
            // FROM value
            if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                self.bump();
            }
        }

        self.skip_ws();
        if !self.at_end() && (self.at_word("BY") || self.at_kind(cobol_lexer::TokenKind::By)) {
            self.bump(); // BY
            self.skip_ws();
            // BY value
            if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                self.bump();
            }
        }

        self.skip_ws();
        if !self.at_end()
            && (self.at_word("UNTIL") || self.at_kind(cobol_lexer::TokenKind::Until))
        {
            self.bump(); // UNTIL
            self.skip_ws();
            self.parse_condition();
        }

        // Optional AFTER clauses for nested VARYING
        self.skip_ws();
        while !self.at_end()
            && (self.at_word("AFTER") || self.at_kind(cobol_lexer::TokenKind::After))
        {
            self.bump(); // AFTER
            self.skip_ws();
            if !self.at_end() && (self.at_identifier() || self.current_is_word_like()) {
                self.bump(); // loop variable
            }
            self.skip_ws();
            if !self.at_end() && (self.at_word("FROM") || self.at_kind(cobol_lexer::TokenKind::From)) {
                self.bump();
                self.skip_ws();
                if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                    self.bump();
                }
            }
            self.skip_ws();
            if !self.at_end() && (self.at_word("BY") || self.at_kind(cobol_lexer::TokenKind::By)) {
                self.bump();
                self.skip_ws();
                if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                    self.bump();
                }
            }
            self.skip_ws();
            if !self.at_end()
                && (self.at_word("UNTIL") || self.at_kind(cobol_lexer::TokenKind::Until))
            {
                self.bump();
                self.skip_ws();
                self.parse_condition();
            }
            self.skip_ws();
        }
    }

    // ------------------------------------------------------------------
    // STOP statement
    // ------------------------------------------------------------------

    fn parse_stop_stmt(&mut self) {
        self.start_node(SyntaxKind::STOP_STMT);

        self.skip_ws();
        self.bump(); // STOP or GOBACK

        self.skip_ws();
        // Optional RUN
        if !self.at_end()
            && (self.at_word("RUN") || self.at_kind(cobol_lexer::TokenKind::Run))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // GO TO statement
    // ------------------------------------------------------------------

    fn parse_go_to_stmt(&mut self) {
        self.start_node(SyntaxKind::GO_TO_STMT);

        self.skip_ws();
        self.bump(); // GO (or GO TO as a single token)

        // If the lexer emitted GO as one token, we need TO separately
        self.skip_ws();
        if !self.at_end() && (self.at_word("TO") || self.at_kind(cobol_lexer::TokenKind::To)) {
            self.bump(); // TO
        }

        // Target paragraph/section name
        self.skip_ws();
        if !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && (self.at_identifier() || self.current_is_word_like())
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // EXIT statement
    // ------------------------------------------------------------------

    fn parse_exit_stmt(&mut self) {
        self.start_node(SyntaxKind::EXIT_STMT);

        self.skip_ws();
        self.bump(); // EXIT

        self.skip_ws();
        // Optional PROGRAM, SECTION, PARAGRAPH
        if !self.at_end()
            && self.at_any_word(&["PROGRAM", "SECTION", "PARAGRAPH"])
        {
            self.bump();
        } else if !self.at_end() && matches!(
            self.current_kind(),
            cobol_lexer::TokenKind::SectionKw | cobol_lexer::TokenKind::ParagraphKw
        ) {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // CALL statement
    // ------------------------------------------------------------------

    fn parse_call_stmt(&mut self) {
        self.start_node(SyntaxKind::CALL_STMT);

        self.skip_ws();
        self.bump(); // CALL

        // Called program (literal or identifier)
        self.skip_ws();
        if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
            self.bump();
        }

        // Optional USING clause
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("USING") || self.at_kind(cobol_lexer::TokenKind::Using))
        {
            self.bump(); // USING
            self.skip_ws();
            while !self.at_end()
                && self.current_kind() != cobol_lexer::TokenKind::Period
                && !self.at_word("END-CALL")
                && !self.at_kind(cobol_lexer::TokenKind::EndCall)
                && !self.at_word("RETURNING")
                && !self.at_kind(cobol_lexer::TokenKind::Returning)
                && !self.at_statement_start()
                && !self.at_scope_terminator()
                && !self.at_else()
                && !self.at_word("ON")
                && !self.at_word("NOT")
            {
                self.bump();
            }
        }

        // Optional RETURNING clause
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("RETURNING") || self.at_kind(cobol_lexer::TokenKind::Returning))
        {
            self.bump(); // RETURNING
            self.skip_ws();
            if !self.at_end() && self.current_kind() != cobol_lexer::TokenKind::Period {
                self.bump();
            }
        }

        // ON EXCEPTION / NOT ON EXCEPTION handlers - consume until END-CALL
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_word("END-CALL")
            && !self.at_kind(cobol_lexer::TokenKind::EndCall)
            && !self.at_scope_terminator()
            && !self.at_division()
        {
            if self.at_statement_start() {
                self.parse_statement();
            } else {
                self.bump();
            }
            self.skip_ws();
        }

        // Optional END-CALL
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-CALL") || self.at_kind(cobol_lexer::TokenKind::EndCall))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // ACCEPT statement
    // ------------------------------------------------------------------

    fn parse_accept_stmt(&mut self) {
        self.start_node(SyntaxKind::ACCEPT_STMT);

        self.skip_ws();
        self.bump(); // ACCEPT

        // Identifier
        self.skip_ws();
        if !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && (self.at_identifier() || self.current_is_word_like())
        {
            self.bump();
        }

        // Optional FROM clause
        self.skip_ws();
        if !self.at_end()
            && (self.at_word("FROM") || self.at_kind(cobol_lexer::TokenKind::From))
        {
            self.bump(); // FROM
            self.skip_ws();
            while !self.at_end()
                && self.current_kind() != cobol_lexer::TokenKind::Period
                && !self.at_statement_start()
                && !self.at_scope_terminator()
            {
                self.bump();
            }
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // CONTINUE statement
    // ------------------------------------------------------------------

    fn parse_continue_stmt(&mut self) {
        self.start_node(SyntaxKind::CONTINUE_STMT);
        self.skip_ws();
        self.bump(); // CONTINUE
        self.finish_node();
    }

    // ------------------------------------------------------------------
    // EVALUATE statement (simplified)
    // ------------------------------------------------------------------

    fn parse_evaluate_stmt(&mut self) {
        self.start_node(SyntaxKind::EVALUATE_STMT);

        self.skip_ws();
        self.bump(); // EVALUATE

        // Consume the subject expression (tokens until WHEN)
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_word("WHEN")
            && !self.at_word("END-EVALUATE")
            && !self.at_kind(cobol_lexer::TokenKind::EndEvaluate)
            && !self.at_division()
        {
            self.bump();
            self.skip_ws();
        }

        // Parse WHEN clauses
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_word("END-EVALUATE")
            && !self.at_kind(cobol_lexer::TokenKind::EndEvaluate)
            && !self.at_division()
        {
            if self.at_word("WHEN") {
                self.bump(); // WHEN
                self.skip_ws();

                // Consume condition value(s) until statement start, next WHEN,
                // or END-EVALUATE
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::Period
                    && !self.at_statement_start()
                    && !self.at_word("WHEN")
                    && !self.at_word("END-EVALUATE")
                    && !self.at_kind(cobol_lexer::TokenKind::EndEvaluate)
                    && !self.at_division()
                {
                    self.bump();
                    self.skip_ws();
                }

                // Parse statements in this WHEN clause body, stopping at
                // WHEN or END-EVALUATE
                while !self.at_end()
                    && self.current_kind() != cobol_lexer::TokenKind::Period
                    && !self.at_word("WHEN")
                    && !self.at_word("END-EVALUATE")
                    && !self.at_kind(cobol_lexer::TokenKind::EndEvaluate)
                    && !self.at_division()
                {
                    if self.at_statement_start() {
                        self.parse_statement();
                    } else {
                        self.bump();
                    }
                    self.skip_ws();
                }
            } else {
                self.bump();
                self.skip_ws();
            }
        }

        self.skip_ws();
        if !self.at_end()
            && (self.at_word("END-EVALUATE")
                || self.at_kind(cobol_lexer::TokenKind::EndEvaluate))
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // INITIALIZE statement
    // ------------------------------------------------------------------

    fn parse_initialize_stmt(&mut self) {
        self.start_node(SyntaxKind::INITIALIZE_STMT);
        self.skip_ws();
        self.bump(); // INITIALIZE

        // Consume operands
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
        {
            self.bump();
        }

        self.finish_node();
    }

    // ------------------------------------------------------------------
    // SET statement
    // ------------------------------------------------------------------

    fn parse_set_stmt(&mut self) {
        self.start_node(SyntaxKind::SET_STMT);
        self.skip_ws();
        self.bump(); // SET

        // Consume operands
        self.skip_ws();
        while !self.at_end()
            && self.current_kind() != cobol_lexer::TokenKind::Period
            && !self.at_statement_start()
            && !self.at_scope_terminator()
            && !self.at_else()
        {
            self.bump();
        }

        self.finish_node();
    }
}

/// Maps a lexer [`cobol_lexer::TokenKind`] to the corresponding [`SyntaxKind`].
///
/// Keyword tokens that do not yet have a dedicated `SyntaxKind` variant are
/// mapped to [`SyntaxKind::KEYWORD`].
fn token_kind_to_syntax_kind(kind: cobol_lexer::TokenKind) -> SyntaxKind {
    use cobol_lexer::TokenKind;
    match kind {
        TokenKind::Word => SyntaxKind::WORD,
        TokenKind::IntegerLiteral => SyntaxKind::INTEGER_LITERAL,
        TokenKind::DecimalLiteral => SyntaxKind::DECIMAL_LITERAL,
        TokenKind::StringLiteral | TokenKind::HexLiteral => SyntaxKind::STRING_LITERAL,
        TokenKind::Period => SyntaxKind::PERIOD,
        TokenKind::Comma => SyntaxKind::COMMA,
        TokenKind::LeftParen => SyntaxKind::LEFT_PAREN,
        TokenKind::RightParen => SyntaxKind::RIGHT_PAREN,
        TokenKind::EqualSign => SyntaxKind::EQUAL_SIGN,
        TokenKind::Plus => SyntaxKind::PLUS,
        TokenKind::Minus => SyntaxKind::MINUS,
        TokenKind::Star => SyntaxKind::STAR,
        TokenKind::Slash => SyntaxKind::SLASH,
        TokenKind::DoubleStar => SyntaxKind::DOUBLE_STAR,
        TokenKind::GreaterThan => SyntaxKind::GREATER_THAN,
        TokenKind::LessThan => SyntaxKind::LESS_THAN,
        TokenKind::GreaterEqual => SyntaxKind::GREATER_EQUAL,
        TokenKind::LessEqual => SyntaxKind::LESS_EQUAL,
        TokenKind::NotEqual => SyntaxKind::NOT_EQUAL,
        TokenKind::Whitespace => SyntaxKind::WHITESPACE,
        TokenKind::Comment => SyntaxKind::COMMENT,
        TokenKind::Newline => SyntaxKind::NEWLINE,
        TokenKind::Eof => SyntaxKind::EOF,
        TokenKind::Error => SyntaxKind::ERROR_TOKEN,
        // All keyword and misc tokens map to the KEYWORD catch-all for now.
        _ => SyntaxKind::KEYWORD,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use cobol_lexer::{SmolStr, Token, TokenKind};
    use cobol_span::{ExpansionId, FileId, Span};

    /// Helper: create a token at a dummy span.
    fn tok(kind: TokenKind, text: &str) -> Token {
        let start = TextSize::from(0);
        let end = TextSize::from(text.len() as u32);
        Token {
            kind,
            text: SmolStr::new(text),
            span: Span::new(FileId::new(0), TextRange::new(start, end), ExpansionId::ROOT),
        }
    }

    fn ws() -> Token {
        tok(TokenKind::Whitespace, " ")
    }

    fn nl() -> Token {
        tok(TokenKind::Newline, "\n")
    }

    fn eof() -> Token {
        tok(TokenKind::Eof, "")
    }

    fn period() -> Token {
        tok(TokenKind::Period, ".")
    }

    fn word(text: &str) -> Token {
        tok(TokenKind::Word, text)
    }

    #[test]
    fn test_parse_empty() {
        let result = parse(&[]);
        let syntax = result.syntax();
        assert_eq!(syntax.kind(), SyntaxKind::SOURCE_FILE);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_single_eof_token() {
        let result = parse(&[eof()]);
        let syntax = result.syntax();
        assert_eq!(syntax.kind(), SyntaxKind::SOURCE_FILE);
        assert_eq!(syntax.children_with_tokens().count(), 1);
    }

    #[test]
    fn syntax_kind_round_trip() {
        let kind = SyntaxKind::DISPLAY_STMT;
        let raw = kind.into_raw();
        let back = SyntaxKind::from_raw(raw);
        assert_eq!(kind, back);
    }

    #[test]
    fn parse_error_display() {
        let err = ParseError {
            message: "unexpected token".to_string(),
            range: TextRange::new(TextSize::from(10), TextSize::from(15)),
        };
        let s = format!("{err}");
        assert_eq!(s, "error at 10..15: unexpected token");
    }

    #[test]
    fn cobol_language_kind_round_trip() {
        use rowan::Language;
        let kind = SyntaxKind::WORD;
        let raw = CobolLanguage::kind_to_raw(kind);
        let back = CobolLanguage::kind_from_raw(raw);
        assert_eq!(kind, back);
    }

    #[test]
    fn test_parse_hello_world() {
        // Simulates tokens for a minimal HELLO-WORLD.cob:
        //   IDENTIFICATION DIVISION.
        //   PROGRAM-ID. HELLO-WORLD.
        //   PROCEDURE DIVISION.
        //   MAIN-PARA.
        //       DISPLAY "HELLO, WORLD!".
        //       STOP RUN.
        let tokens = vec![
            tok(TokenKind::IdentificationDivision, "IDENTIFICATION"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            tok(TokenKind::ProgramId, "PROGRAM-ID"),
            period(),
            ws(),
            word("HELLO-WORLD"),
            period(),
            nl(),
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("MAIN-PARA"),
            period(),
            nl(),
            ws(),
            tok(TokenKind::Display, "DISPLAY"),
            ws(),
            tok(TokenKind::StringLiteral, "\"HELLO, WORLD!\""),
            period(),
            nl(),
            ws(),
            tok(TokenKind::Stop, "STOP"),
            ws(),
            tok(TokenKind::Run, "RUN"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        assert_eq!(syntax.kind(), SyntaxKind::SOURCE_FILE);

        // Should have IDENTIFICATION_DIVISION and PROCEDURE_DIVISION children
        let children: Vec<_> = syntax.children().collect();
        let has_id_div = children
            .iter()
            .any(|c| c.kind() == SyntaxKind::IDENTIFICATION_DIVISION);
        let has_proc_div = children
            .iter()
            .any(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION);
        assert!(has_id_div, "expected IDENTIFICATION_DIVISION node");
        assert!(has_proc_div, "expected PROCEDURE_DIVISION node");

        // Inside PROCEDURE_DIVISION, find the PARAGRAPH
        let proc_div = children
            .iter()
            .find(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION)
            .unwrap();
        let para: Vec<_> = proc_div
            .children()
            .filter(|c| c.kind() == SyntaxKind::PARAGRAPH)
            .collect();
        assert!(!para.is_empty(), "expected at least one PARAGRAPH");

        // Inside the paragraph, find SENTENCE nodes
        let sentences: Vec<_> = para[0]
            .children()
            .filter(|c| c.kind() == SyntaxKind::SENTENCE)
            .collect();
        assert!(!sentences.is_empty(), "expected at least one SENTENCE");

        // The first sentence should contain a DISPLAY_STMT
        let stmts: Vec<_> = sentences[0]
            .children()
            .filter(|c| c.kind() == SyntaxKind::DISPLAY_STMT)
            .collect();
        assert!(
            !stmts.is_empty(),
            "expected DISPLAY_STMT in first sentence"
        );
    }

    #[test]
    fn test_parse_data_division() {
        // DATA DIVISION.
        // WORKING-STORAGE SECTION.
        // 01  WS-NAME PIC X(20) VALUE "TEST".
        // 01  WS-NUM  PIC 9(5).
        let tokens = vec![
            tok(TokenKind::DataDivision, "DATA"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            tok(TokenKind::WorkingStorageSection, "WORKING-STORAGE"),
            ws(),
            word("SECTION"),
            period(),
            nl(),
            tok(TokenKind::Level, "01"),
            ws(),
            word("WS-NAME"),
            ws(),
            tok(TokenKind::Pic, "PIC"),
            ws(),
            word("X(20)"),
            ws(),
            tok(TokenKind::Value, "VALUE"),
            ws(),
            tok(TokenKind::StringLiteral, "\"TEST\""),
            period(),
            nl(),
            tok(TokenKind::Level, "01"),
            ws(),
            word("WS-NUM"),
            ws(),
            tok(TokenKind::Pic, "PIC"),
            ws(),
            word("9(5)"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        assert_eq!(syntax.kind(), SyntaxKind::SOURCE_FILE);

        // Find DATA_DIVISION
        let data_div = syntax
            .children()
            .find(|c| c.kind() == SyntaxKind::DATA_DIVISION);
        assert!(data_div.is_some(), "expected DATA_DIVISION node");
        let data_div = data_div.unwrap();

        // Find WORKING_STORAGE_SECTION
        let ws_sec = data_div
            .children()
            .find(|c| c.kind() == SyntaxKind::WORKING_STORAGE_SECTION);
        assert!(ws_sec.is_some(), "expected WORKING_STORAGE_SECTION node");
        let ws_sec = ws_sec.unwrap();

        // Should have 2 DATA_ITEM children
        let items: Vec<_> = ws_sec
            .children()
            .filter(|c| c.kind() == SyntaxKind::DATA_ITEM)
            .collect();
        assert_eq!(items.len(), 2, "expected 2 DATA_ITEM nodes");

        // First item should have a PIC_CLAUSE and VALUE_CLAUSE
        let pic = items[0]
            .children()
            .find(|c| c.kind() == SyntaxKind::PIC_CLAUSE);
        assert!(pic.is_some(), "expected PIC_CLAUSE in first data item");
        let val = items[0]
            .children()
            .find(|c| c.kind() == SyntaxKind::VALUE_CLAUSE);
        assert!(val.is_some(), "expected VALUE_CLAUSE in first data item");
    }

    #[test]
    fn test_parse_if_stmt() {
        // PROCEDURE DIVISION.
        // TEST-PARA.
        //   IF WS-X = 1
        //     DISPLAY "ONE"
        //   ELSE
        //     DISPLAY "OTHER"
        //   END-IF.
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("TEST-PARA"),
            period(),
            nl(),
            tok(TokenKind::If, "IF"),
            ws(),
            word("WS-X"),
            ws(),
            tok(TokenKind::EqualSign, "="),
            ws(),
            tok(TokenKind::IntegerLiteral, "1"),
            nl(),
            ws(),
            tok(TokenKind::Display, "DISPLAY"),
            ws(),
            tok(TokenKind::StringLiteral, "\"ONE\""),
            nl(),
            tok(TokenKind::Else, "ELSE"),
            nl(),
            ws(),
            tok(TokenKind::Display, "DISPLAY"),
            ws(),
            tok(TokenKind::StringLiteral, "\"OTHER\""),
            nl(),
            tok(TokenKind::EndIf, "END-IF"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        assert_eq!(syntax.kind(), SyntaxKind::SOURCE_FILE);

        let proc_div = syntax
            .children()
            .find(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION)
            .unwrap();
        let para = proc_div
            .children()
            .find(|c| c.kind() == SyntaxKind::PARAGRAPH)
            .unwrap();
        let sentence = para
            .children()
            .find(|c| c.kind() == SyntaxKind::SENTENCE)
            .unwrap();
        let if_stmt = sentence
            .children()
            .find(|c| c.kind() == SyntaxKind::IF_STMT);
        assert!(if_stmt.is_some(), "expected IF_STMT node");
    }

    #[test]
    fn test_parse_perform_varying() {
        // PROCEDURE DIVISION.
        // TEST-PARA.
        //   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
        //     DISPLAY WS-I
        //   END-PERFORM.
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("TEST-PARA"),
            period(),
            nl(),
            tok(TokenKind::Perform, "PERFORM"),
            ws(),
            tok(TokenKind::Varying, "VARYING"),
            ws(),
            word("WS-I"),
            ws(),
            tok(TokenKind::From, "FROM"),
            ws(),
            tok(TokenKind::IntegerLiteral, "1"),
            ws(),
            tok(TokenKind::By, "BY"),
            ws(),
            tok(TokenKind::IntegerLiteral, "1"),
            ws(),
            tok(TokenKind::Until, "UNTIL"),
            ws(),
            word("WS-I"),
            ws(),
            tok(TokenKind::GreaterThan, ">"),
            ws(),
            tok(TokenKind::IntegerLiteral, "10"),
            nl(),
            ws(),
            tok(TokenKind::Display, "DISPLAY"),
            ws(),
            word("WS-I"),
            nl(),
            tok(TokenKind::EndPerform, "END-PERFORM"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        assert_eq!(syntax.kind(), SyntaxKind::SOURCE_FILE);

        let proc_div = syntax
            .children()
            .find(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION)
            .unwrap();
        let para = proc_div
            .children()
            .find(|c| c.kind() == SyntaxKind::PARAGRAPH)
            .unwrap();
        let sentence = para
            .children()
            .find(|c| c.kind() == SyntaxKind::SENTENCE)
            .unwrap();
        let perform = sentence
            .children()
            .find(|c| c.kind() == SyntaxKind::PERFORM_STMT);
        assert!(perform.is_some(), "expected PERFORM_STMT node");
    }

    #[test]
    fn test_parse_error_recovery() {
        // Intentionally malformed: missing DIVISION after PROCEDURE.
        // Parser should produce an error.
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            period(),
            nl(),
            word("MAIN-PARA"),
            period(),
            nl(),
            tok(TokenKind::Display, "DISPLAY"),
            ws(),
            tok(TokenKind::StringLiteral, "\"HELLO\""),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        assert_eq!(syntax.kind(), SyntaxKind::SOURCE_FILE);
        // Should have errors since DIVISION was missing
        assert!(
            !result.errors.is_empty(),
            "expected parse errors due to missing DIVISION"
        );
    }

    #[test]
    fn test_parse_move_stmt() {
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("MAIN-PARA"),
            period(),
            nl(),
            tok(TokenKind::Move, "MOVE"),
            ws(),
            tok(TokenKind::StringLiteral, "\"HELLO\""),
            ws(),
            tok(TokenKind::To, "TO"),
            ws(),
            word("WS-NAME"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        let proc_div = syntax
            .children()
            .find(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION)
            .unwrap();
        let para = proc_div
            .children()
            .find(|c| c.kind() == SyntaxKind::PARAGRAPH)
            .unwrap();
        let sentence = para
            .children()
            .find(|c| c.kind() == SyntaxKind::SENTENCE)
            .unwrap();
        let move_stmt = sentence
            .children()
            .find(|c| c.kind() == SyntaxKind::MOVE_STMT);
        assert!(move_stmt.is_some(), "expected MOVE_STMT node");
    }

    #[test]
    fn test_parse_add_stmt() {
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("TEST-PARA"),
            period(),
            nl(),
            tok(TokenKind::Add, "ADD"),
            ws(),
            tok(TokenKind::IntegerLiteral, "1"),
            ws(),
            tok(TokenKind::To, "TO"),
            ws(),
            word("WS-COUNT"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        let proc_div = syntax
            .children()
            .find(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION)
            .unwrap();
        let para = proc_div
            .children()
            .find(|c| c.kind() == SyntaxKind::PARAGRAPH)
            .unwrap();
        let sentence = para
            .children()
            .find(|c| c.kind() == SyntaxKind::SENTENCE)
            .unwrap();
        let add_stmt = sentence
            .children()
            .find(|c| c.kind() == SyntaxKind::ADD_STMT);
        assert!(add_stmt.is_some(), "expected ADD_STMT node");
    }

    #[test]
    fn test_lossless_round_trip() {
        // Verify that every token appears in the tree (losslessness)
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("MAIN-PARA"),
            period(),
            nl(),
            tok(TokenKind::Display, "DISPLAY"),
            ws(),
            tok(TokenKind::StringLiteral, "\"HI\""),
            period(),
            nl(),
            eof(),
        ];

        let original_text: String = tokens.iter().map(|t| t.text.as_str()).collect();
        let result = parse(&tokens);
        let syntax = result.syntax();
        let tree_text = syntax.text().to_string();
        assert_eq!(
            tree_text, original_text,
            "CST text must match original tokens"
        );
    }

    #[test]
    fn test_parse_compute_stmt() {
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("TEST-PARA"),
            period(),
            nl(),
            tok(TokenKind::Compute, "COMPUTE"),
            ws(),
            word("WS-RESULT"),
            ws(),
            tok(TokenKind::EqualSign, "="),
            ws(),
            word("WS-A"),
            ws(),
            tok(TokenKind::Plus, "+"),
            ws(),
            word("WS-B"),
            ws(),
            tok(TokenKind::Star, "*"),
            ws(),
            tok(TokenKind::IntegerLiteral, "2"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        let proc_div = syntax
            .children()
            .find(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION)
            .unwrap();
        let para = proc_div
            .children()
            .find(|c| c.kind() == SyntaxKind::PARAGRAPH)
            .unwrap();
        let sentence = para
            .children()
            .find(|c| c.kind() == SyntaxKind::SENTENCE)
            .unwrap();
        let compute = sentence
            .children()
            .find(|c| c.kind() == SyntaxKind::COMPUTE_STMT);
        assert!(compute.is_some(), "expected COMPUTE_STMT node");

        // Verify there is an ARITHMETIC_EXPR inside
        let compute = compute.unwrap();
        let arith = compute
            .children()
            .find(|c| c.kind() == SyntaxKind::ARITHMETIC_EXPR);
        assert!(
            arith.is_some(),
            "expected ARITHMETIC_EXPR inside COMPUTE_STMT"
        );
    }

    #[test]
    fn test_parse_stop_run() {
        let tokens = vec![
            tok(TokenKind::ProcedureDivision, "PROCEDURE"),
            ws(),
            word("DIVISION"),
            period(),
            nl(),
            word("MAIN-PARA"),
            period(),
            nl(),
            tok(TokenKind::Stop, "STOP"),
            ws(),
            tok(TokenKind::Run, "RUN"),
            period(),
            nl(),
            eof(),
        ];

        let result = parse(&tokens);
        let syntax = result.syntax();
        let proc_div = syntax
            .children()
            .find(|c| c.kind() == SyntaxKind::PROCEDURE_DIVISION)
            .unwrap();
        let para = proc_div
            .children()
            .find(|c| c.kind() == SyntaxKind::PARAGRAPH)
            .unwrap();
        let sentence = para
            .children()
            .find(|c| c.kind() == SyntaxKind::SENTENCE)
            .unwrap();
        let stop = sentence
            .children()
            .find(|c| c.kind() == SyntaxKind::STOP_STMT);
        assert!(stop.is_some(), "expected STOP_STMT node");
    }
}
