//! Typed AST view over the COBOL CST (zero-cost wrappers).
//!
//! This crate follows the rust-analyzer pattern of providing zero-cost newtype
//! wrappers around rowan [`SyntaxNode`] and [`SyntaxToken`] values.  Each AST
//! type corresponds to a specific [`SyntaxKind`] and provides convenient,
//! typed accessor methods for navigating the tree.
//!
//! Because the wrappers are newtypes with no additional data, casting between
//! the raw CST and the typed AST is free at runtime.

pub use cobol_parser::{CobolLanguage, SyntaxKind, SyntaxNode, SyntaxToken};

// ---------------------------------------------------------------------------
// Core traits
// ---------------------------------------------------------------------------

/// Trait for AST nodes that wrap CST nodes.
pub trait AstNode: Sized {
    /// Returns `true` if a CST node of the given kind can be cast to this type.
    fn can_cast(kind: SyntaxKind) -> bool;

    /// Attempts to cast a CST node to this AST type.  Returns `None` if the
    /// node's kind does not match.
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Returns a reference to the underlying CST node.
    fn syntax(&self) -> &SyntaxNode;
}

/// Trait for AST tokens that wrap CST tokens.
pub trait AstToken: Sized {
    /// Returns `true` if a CST token of the given kind can be cast to this type.
    fn can_cast(kind: SyntaxKind) -> bool;

    /// Attempts to cast a CST token to this AST type.  Returns `None` if the
    /// token's kind does not match.
    fn cast(token: SyntaxToken) -> Option<Self>;

    /// Returns a reference to the underlying CST token.
    fn syntax(&self) -> &SyntaxToken;
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Returns an iterator over the children of `parent` that can be cast to `N`.
fn children<'a, N: AstNode + 'a>(parent: &'a SyntaxNode) -> impl Iterator<Item = N> + 'a {
    parent.children().filter_map(N::cast)
}

/// Finds the first child token of `parent` with the given `kind`.
fn child_token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| tok.kind() == kind)
}

/// Finds the first child token of `parent` matching any of the given kinds.
fn child_token_any(parent: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| kinds.contains(&tok.kind()))
}

// ---------------------------------------------------------------------------
// ast_node! macro
// ---------------------------------------------------------------------------

/// Generates a zero-cost newtype wrapper around [`SyntaxNode`] that implements
/// [`AstNode`] for the given [`SyntaxKind`] variant.
macro_rules! ast_node {
    ($name:ident, $kind:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            syntax: SyntaxNode,
        }

        impl AstNode for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self { syntax: node })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }
    };
}

// ---------------------------------------------------------------------------
// AST node definitions
// ---------------------------------------------------------------------------

ast_node!(SourceFile, SOURCE_FILE);
ast_node!(Program, PROGRAM);
ast_node!(IdentificationDivision, IDENTIFICATION_DIVISION);
ast_node!(ProgramIdClause, PROGRAM_ID_CLAUSE);
ast_node!(EnvironmentDivision, ENVIRONMENT_DIVISION);
ast_node!(DataDivision, DATA_DIVISION);
ast_node!(WorkingStorageSection, WORKING_STORAGE_SECTION);
ast_node!(LinkageSection, LINKAGE_SECTION);
ast_node!(DataItem, DATA_ITEM);
ast_node!(PicClause, PIC_CLAUSE);
ast_node!(ValueClause, VALUE_CLAUSE);
ast_node!(UsageClause, USAGE_CLAUSE);
ast_node!(SignClause, SIGN_CLAUSE);
ast_node!(JustifiedClause, JUSTIFIED_CLAUSE);
ast_node!(BlankClause, BLANK_CLAUSE);
ast_node!(ProcedureDivision, PROCEDURE_DIVISION);
ast_node!(Paragraph, PARAGRAPH);
ast_node!(Section, SECTION);
ast_node!(Sentence, SENTENCE);
ast_node!(Statement, STATEMENT);

// Statement-specific nodes
ast_node!(DisplayStmt, DISPLAY_STMT);
ast_node!(MoveStmt, MOVE_STMT);
ast_node!(AddStmt, ADD_STMT);
ast_node!(SubtractStmt, SUBTRACT_STMT);
ast_node!(MultiplyStmt, MULTIPLY_STMT);
ast_node!(DivideStmt, DIVIDE_STMT);
ast_node!(ComputeStmt, COMPUTE_STMT);
ast_node!(IfStmt, IF_STMT);
ast_node!(PerformStmt, PERFORM_STMT);
ast_node!(StopStmt, STOP_STMT);
ast_node!(CallStmt, CALL_STMT);
ast_node!(GoToStmt, GO_TO_STMT);
ast_node!(AcceptStmt, ACCEPT_STMT);

// ---------------------------------------------------------------------------
// SourceFile methods
// ---------------------------------------------------------------------------

impl SourceFile {
    /// Returns all program units in this source file.
    pub fn programs(&self) -> impl Iterator<Item = Program> + '_ {
        children::<Program>(&self.syntax)
    }

    /// Returns the first (or only) IDENTIFICATION DIVISION.
    /// Checks both direct children and inside the first PROGRAM node
    /// for backward compatibility.
    pub fn identification_division(&self) -> Option<IdentificationDivision> {
        children::<IdentificationDivision>(&self.syntax)
            .next()
            .or_else(|| {
                self.programs()
                    .next()
                    .and_then(|p| p.identification_division())
            })
    }

    /// Returns the first ENVIRONMENT DIVISION.
    pub fn environment_division(&self) -> Option<EnvironmentDivision> {
        children::<EnvironmentDivision>(&self.syntax)
            .next()
            .or_else(|| {
                self.programs()
                    .next()
                    .and_then(|p| p.environment_division())
            })
    }

    /// Returns the first DATA DIVISION.
    pub fn data_division(&self) -> Option<DataDivision> {
        children::<DataDivision>(&self.syntax)
            .next()
            .or_else(|| self.programs().next().and_then(|p| p.data_division()))
    }

    /// Returns the first PROCEDURE DIVISION.
    pub fn procedure_division(&self) -> Option<ProcedureDivision> {
        children::<ProcedureDivision>(&self.syntax)
            .next()
            .or_else(|| self.programs().next().and_then(|p| p.procedure_division()))
    }
}

impl Program {
    /// Returns the IDENTIFICATION DIVISION of this program.
    pub fn identification_division(&self) -> Option<IdentificationDivision> {
        children::<IdentificationDivision>(&self.syntax).next()
    }

    /// Returns the ENVIRONMENT DIVISION of this program.
    pub fn environment_division(&self) -> Option<EnvironmentDivision> {
        children::<EnvironmentDivision>(&self.syntax).next()
    }

    /// Returns the DATA DIVISION of this program.
    pub fn data_division(&self) -> Option<DataDivision> {
        children::<DataDivision>(&self.syntax).next()
    }

    /// Returns the PROCEDURE DIVISION of this program.
    pub fn procedure_division(&self) -> Option<ProcedureDivision> {
        children::<ProcedureDivision>(&self.syntax).next()
    }

    /// Returns nested programs inside this program.
    pub fn nested_programs(&self) -> impl Iterator<Item = Program> + '_ {
        children::<Program>(&self.syntax)
    }
}

// ---------------------------------------------------------------------------
// IdentificationDivision methods
// ---------------------------------------------------------------------------

impl IdentificationDivision {
    /// Returns the PROGRAM-ID clause, if present.
    pub fn program_id(&self) -> Option<ProgramIdClause> {
        children::<ProgramIdClause>(&self.syntax).next()
    }
}

// ---------------------------------------------------------------------------
// ProgramIdClause methods
// ---------------------------------------------------------------------------

impl ProgramIdClause {
    /// Returns the program name token.
    ///
    /// The first WORD child is typically the "PROGRAM-ID" keyword itself,
    /// so we skip it and return the second WORD, which is the actual name.
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .filter(|tok| tok.kind() == SyntaxKind::WORD)
            .nth(1) // Skip "PROGRAM-ID", get the name
            .or_else(|| child_token(&self.syntax, SyntaxKind::WORD)) // Fallback
    }
}

// ---------------------------------------------------------------------------
// DataDivision methods
// ---------------------------------------------------------------------------

impl DataDivision {
    /// Returns the WORKING-STORAGE SECTION, if present.
    pub fn working_storage_section(&self) -> Option<WorkingStorageSection> {
        children::<WorkingStorageSection>(&self.syntax).next()
    }

    /// Returns the LINKAGE SECTION, if present.
    pub fn linkage_section(&self) -> Option<LinkageSection> {
        children::<LinkageSection>(&self.syntax).next()
    }
}

// ---------------------------------------------------------------------------
// WorkingStorageSection methods
// ---------------------------------------------------------------------------

impl WorkingStorageSection {
    /// Returns an iterator over all data item children.
    pub fn items(&self) -> impl Iterator<Item = DataItem> + '_ {
        children::<DataItem>(&self.syntax)
    }
}

// ---------------------------------------------------------------------------
// LinkageSection methods
// ---------------------------------------------------------------------------

impl LinkageSection {
    /// Returns an iterator over all data item children.
    pub fn items(&self) -> impl Iterator<Item = DataItem> + '_ {
        children::<DataItem>(&self.syntax)
    }
}

// ---------------------------------------------------------------------------
// DataItem methods
// ---------------------------------------------------------------------------

impl DataItem {
    /// Returns the level number token (first INTEGER_LITERAL or KEYWORD child).
    ///
    /// Level numbers in COBOL (01, 05, 77, 88, etc.) may appear as integer
    /// literal tokens or as keyword tokens depending on how the lexer/parser
    /// categorises them.
    pub fn level_number(&self) -> Option<SyntaxToken> {
        child_token_any(
            &self.syntax,
            &[SyntaxKind::INTEGER_LITERAL, SyntaxKind::KEYWORD],
        )
    }

    /// Returns the data name token (first WORD child).
    pub fn data_name(&self) -> Option<SyntaxToken> {
        child_token(&self.syntax, SyntaxKind::WORD)
    }

    /// Returns the PIC / PICTURE clause, if present.
    pub fn pic_clause(&self) -> Option<PicClause> {
        children::<PicClause>(&self.syntax).next()
    }

    /// Returns the VALUE clause, if present.
    pub fn value_clause(&self) -> Option<ValueClause> {
        children::<ValueClause>(&self.syntax).next()
    }

    /// Returns the USAGE clause, if present.
    pub fn usage_clause(&self) -> Option<UsageClause> {
        children::<UsageClause>(&self.syntax).next()
    }

    /// Returns the SIGN clause, if present.
    pub fn sign_clause(&self) -> Option<SignClause> {
        children::<SignClause>(&self.syntax).next()
    }

    /// Returns the JUSTIFIED clause, if present.
    pub fn justified_clause(&self) -> Option<JustifiedClause> {
        children::<JustifiedClause>(&self.syntax).next()
    }

    /// Returns the BLANK WHEN ZERO clause, if present.
    pub fn blank_clause(&self) -> Option<BlankClause> {
        children::<BlankClause>(&self.syntax).next()
    }
}

// ---------------------------------------------------------------------------
// PicClause methods
// ---------------------------------------------------------------------------

impl PicClause {
    /// Returns the picture string token.
    ///
    /// In the CST the picture string is typically stored as a WORD token
    /// following the PIC/PICTURE keyword.
    pub fn picture_string(&self) -> Option<SyntaxToken> {
        child_token(&self.syntax, SyntaxKind::WORD)
    }
}

// ---------------------------------------------------------------------------
// ProcedureDivision methods
// ---------------------------------------------------------------------------

impl ProcedureDivision {
    /// Returns an iterator over all paragraphs in the procedure division.
    pub fn paragraphs(&self) -> impl Iterator<Item = Paragraph> + '_ {
        children::<Paragraph>(&self.syntax)
    }

    /// Returns an iterator over all sections in the procedure division.
    pub fn sections(&self) -> impl Iterator<Item = Section> + '_ {
        children::<Section>(&self.syntax)
    }
}

// ---------------------------------------------------------------------------
// Paragraph methods
// ---------------------------------------------------------------------------

impl Paragraph {
    /// Returns the paragraph name token (first WORD child).
    pub fn name(&self) -> Option<SyntaxToken> {
        child_token(&self.syntax, SyntaxKind::WORD)
    }

    /// Returns an iterator over all sentences in the paragraph.
    pub fn sentences(&self) -> impl Iterator<Item = Sentence> + '_ {
        children::<Sentence>(&self.syntax)
    }
}

// ---------------------------------------------------------------------------
// Section methods
// ---------------------------------------------------------------------------

impl Section {
    /// Returns the section name token (first WORD child).
    pub fn name(&self) -> Option<SyntaxToken> {
        child_token(&self.syntax, SyntaxKind::WORD)
    }

    /// Returns an iterator over all paragraphs in this section.
    pub fn paragraphs(&self) -> impl Iterator<Item = Paragraph> + '_ {
        children::<Paragraph>(&self.syntax)
    }

    /// Returns an iterator over all sentences directly in this section
    /// (not inside a named paragraph).
    pub fn sentences(&self) -> impl Iterator<Item = Sentence> + '_ {
        children::<Sentence>(&self.syntax)
    }
}

// ---------------------------------------------------------------------------
// Sentence methods
// ---------------------------------------------------------------------------

impl Sentence {
    /// Returns an iterator over all statements in the sentence.
    pub fn statements(&self) -> impl Iterator<Item = Statement> + '_ {
        children::<Statement>(&self.syntax)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_file_can_cast_returns_correct_values() {
        assert!(SourceFile::can_cast(SyntaxKind::SOURCE_FILE));
        assert!(!SourceFile::can_cast(SyntaxKind::DATA_DIVISION));
        assert!(!SourceFile::can_cast(SyntaxKind::WORD));
    }

    #[test]
    fn all_node_types_can_cast_their_own_kind() {
        assert!(IdentificationDivision::can_cast(
            SyntaxKind::IDENTIFICATION_DIVISION
        ));
        assert!(ProgramIdClause::can_cast(SyntaxKind::PROGRAM_ID_CLAUSE));
        assert!(EnvironmentDivision::can_cast(
            SyntaxKind::ENVIRONMENT_DIVISION
        ));
        assert!(DataDivision::can_cast(SyntaxKind::DATA_DIVISION));
        assert!(WorkingStorageSection::can_cast(
            SyntaxKind::WORKING_STORAGE_SECTION
        ));
        assert!(LinkageSection::can_cast(SyntaxKind::LINKAGE_SECTION));
        assert!(DataItem::can_cast(SyntaxKind::DATA_ITEM));
        assert!(PicClause::can_cast(SyntaxKind::PIC_CLAUSE));
        assert!(ValueClause::can_cast(SyntaxKind::VALUE_CLAUSE));
        assert!(UsageClause::can_cast(SyntaxKind::USAGE_CLAUSE));
        assert!(ProcedureDivision::can_cast(SyntaxKind::PROCEDURE_DIVISION));
        assert!(Paragraph::can_cast(SyntaxKind::PARAGRAPH));
        assert!(Section::can_cast(SyntaxKind::SECTION));
        assert!(Sentence::can_cast(SyntaxKind::SENTENCE));
        assert!(Statement::can_cast(SyntaxKind::STATEMENT));
        assert!(DisplayStmt::can_cast(SyntaxKind::DISPLAY_STMT));
        assert!(MoveStmt::can_cast(SyntaxKind::MOVE_STMT));
        assert!(AddStmt::can_cast(SyntaxKind::ADD_STMT));
        assert!(SubtractStmt::can_cast(SyntaxKind::SUBTRACT_STMT));
        assert!(MultiplyStmt::can_cast(SyntaxKind::MULTIPLY_STMT));
        assert!(DivideStmt::can_cast(SyntaxKind::DIVIDE_STMT));
        assert!(ComputeStmt::can_cast(SyntaxKind::COMPUTE_STMT));
        assert!(IfStmt::can_cast(SyntaxKind::IF_STMT));
        assert!(PerformStmt::can_cast(SyntaxKind::PERFORM_STMT));
        assert!(StopStmt::can_cast(SyntaxKind::STOP_STMT));
        assert!(CallStmt::can_cast(SyntaxKind::CALL_STMT));
        assert!(GoToStmt::can_cast(SyntaxKind::GO_TO_STMT));
        assert!(AcceptStmt::can_cast(SyntaxKind::ACCEPT_STMT));
    }

    #[test]
    fn node_types_reject_wrong_kinds() {
        assert!(!SourceFile::can_cast(SyntaxKind::PARAGRAPH));
        assert!(!Paragraph::can_cast(SyntaxKind::SOURCE_FILE));
        assert!(!DisplayStmt::can_cast(SyntaxKind::MOVE_STMT));
        assert!(!DataItem::can_cast(SyntaxKind::SENTENCE));
    }

    #[test]
    fn cast_source_file_from_parsed_tree() {
        // Build a minimal CST with a SOURCE_FILE root.
        let result = cobol_parser::parse(&[]);
        let root = result.syntax();
        assert_eq!(root.kind(), SyntaxKind::SOURCE_FILE);

        let source_file = SourceFile::cast(root).expect("should cast to SourceFile");
        assert_eq!(source_file.syntax().kind(), SyntaxKind::SOURCE_FILE);

        // Divisions are absent in an empty tree.
        assert!(source_file.identification_division().is_none());
        assert!(source_file.environment_division().is_none());
        assert!(source_file.data_division().is_none());
        assert!(source_file.procedure_division().is_none());
    }

    #[test]
    fn cast_wrong_kind_returns_none() {
        let result = cobol_parser::parse(&[]);
        let root = result.syntax();

        // SOURCE_FILE node should not cast to Paragraph.
        assert!(Paragraph::cast(root).is_none());
    }
}
