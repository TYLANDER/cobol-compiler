use cobol_span::{FileId, Span, TextRange};

/// Severity level for diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// A single diagnostic message
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub primary_span: Span,
    pub primary_label: Option<String>,
    pub secondary_labels: Vec<SecondaryLabel>,
    pub notes: Vec<String>,
}

/// A secondary label pointing to related code
#[derive(Debug, Clone)]
pub struct SecondaryLabel {
    pub span: Span,
    pub message: String,
}

impl Diagnostic {
    /// Create a new error diagnostic
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            code: None,
            message: message.into(),
            primary_span: span,
            primary_label: None,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a new warning diagnostic
    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            code: None,
            message: message.into(),
            primary_span: span,
            primary_label: None,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Set the error code (e.g., "E0001")
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Set the primary label
    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.primary_label = Some(label.into());
        self
    }

    /// Add a secondary label
    pub fn with_secondary(mut self, span: Span, message: impl Into<String>) -> Self {
        self.secondary_labels.push(SecondaryLabel {
            span,
            message: message.into(),
        });
        self
    }

    /// Add a help note
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

/// Trait for rendering diagnostics to different outputs
pub trait DiagnosticRenderer {
    fn render(&self, diagnostic: &Diagnostic, files: &dyn DiagnosticFileSource) -> String;
}

/// Trait for providing file source code to the renderer
pub trait DiagnosticFileSource {
    fn file_name(&self, file_id: FileId) -> Option<&str>;
    fn file_source(&self, file_id: FileId) -> Option<&str>;
}

/// Terminal renderer using codespan-reporting
pub struct TerminalRenderer {
    color: bool,
}

impl TerminalRenderer {
    pub fn new(color: bool) -> Self {
        Self { color }
    }
}

impl DiagnosticRenderer for TerminalRenderer {
    fn render(&self, diagnostic: &Diagnostic, files: &dyn DiagnosticFileSource) -> String {
        // TODO: implement using codespan-reporting
        // For now, simple text format
        let severity_str = match diagnostic.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
            Severity::Hint => "hint",
        };

        let code_str = diagnostic.code.as_deref().unwrap_or("");
        let file_name = files
            .file_name(diagnostic.primary_span.file)
            .unwrap_or("<unknown>");

        let _ = self.color; // TODO: use for terminal colors

        format!(
            "{}[{}]: {}\n  --> {}",
            severity_str, code_str, diagnostic.message, file_name
        )
    }
}

/// LSP diagnostic converter
pub struct LspDiagnosticConverter;

impl LspDiagnosticConverter {
    pub fn new() -> Self {
        Self
    }

    /// Convert our Diagnostic to a serializable LSP diagnostic
    pub fn convert(&self, diagnostic: &Diagnostic) -> LspDiagnostic {
        LspDiagnostic {
            range: diagnostic.primary_span.range,
            severity: match diagnostic.severity {
                Severity::Error => 1,
                Severity::Warning => 2,
                Severity::Info => 3,
                Severity::Hint => 4,
            },
            message: diagnostic.message.clone(),
            code: diagnostic.code.clone(),
        }
    }
}

impl Default for LspDiagnosticConverter {
    fn default() -> Self {
        Self::new()
    }
}

/// Simplified LSP diagnostic (actual LSP types will come from tower-lsp)
#[derive(Debug, Clone)]
pub struct LspDiagnostic {
    pub range: TextRange,
    pub severity: u8,
    pub message: String,
    pub code: Option<String>,
}

/// Diagnostic accumulator - collects diagnostics during compilation
#[derive(Debug, Clone, Default)]
pub struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn error(&mut self, message: impl Into<String>, span: Span) {
        self.push(Diagnostic::error(message, span));
    }

    pub fn warning(&mut self, message: impl Into<String>, span: Span) {
        self.push(Diagnostic::warning(message, span));
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .count()
    }

    pub fn warning_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .count()
    }

    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cobol_span::{ExpansionId, TextSize};

    fn test_span() -> Span {
        Span::new(
            FileId::new(0),
            TextRange::new(TextSize::from(0), TextSize::from(10)),
            ExpansionId::ROOT,
        )
    }

    #[test]
    fn diagnostic_error_creation() {
        let diag = Diagnostic::error("something went wrong", test_span());
        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.message, "something went wrong");
        assert!(diag.code.is_none());
        assert!(diag.primary_label.is_none());
        assert!(diag.secondary_labels.is_empty());
        assert!(diag.notes.is_empty());
    }

    #[test]
    fn diagnostic_warning_creation() {
        let diag = Diagnostic::warning("unused variable", test_span());
        assert_eq!(diag.severity, Severity::Warning);
        assert_eq!(diag.message, "unused variable");
    }

    #[test]
    fn diagnostic_builder_chain() {
        let secondary_span = Span::new(
            FileId::new(1),
            TextRange::new(TextSize::from(20), TextSize::from(30)),
            ExpansionId::ROOT,
        );

        let diag = Diagnostic::error("type mismatch", test_span())
            .with_code("E0001")
            .with_label("expected NUMERIC, found ALPHANUMERIC")
            .with_secondary(secondary_span, "declared here")
            .with_note("COBOL requires matching data types for arithmetic");

        assert_eq!(diag.code.as_deref(), Some("E0001"));
        assert_eq!(
            diag.primary_label.as_deref(),
            Some("expected NUMERIC, found ALPHANUMERIC")
        );
        assert_eq!(diag.secondary_labels.len(), 1);
        assert_eq!(diag.secondary_labels[0].message, "declared here");
        assert_eq!(diag.notes.len(), 1);
        assert_eq!(
            diag.notes[0],
            "COBOL requires matching data types for arithmetic"
        );
    }

    #[test]
    fn diagnostic_bag_empty() {
        let bag = DiagnosticBag::new();
        assert!(!bag.has_errors());
        assert_eq!(bag.error_count(), 0);
        assert_eq!(bag.warning_count(), 0);
        assert!(bag.diagnostics().is_empty());
    }

    #[test]
    fn diagnostic_bag_push_error() {
        let mut bag = DiagnosticBag::new();
        bag.error("something went wrong", test_span());
        assert!(bag.has_errors());
        assert_eq!(bag.error_count(), 1);
        assert_eq!(bag.warning_count(), 0);
        assert_eq!(bag.diagnostics().len(), 1);
    }

    #[test]
    fn diagnostic_bag_push_warning() {
        let mut bag = DiagnosticBag::new();
        bag.warning("unused paragraph", test_span());
        assert!(!bag.has_errors());
        assert_eq!(bag.error_count(), 0);
        assert_eq!(bag.warning_count(), 1);
    }

    #[test]
    fn diagnostic_bag_mixed() {
        let mut bag = DiagnosticBag::new();
        bag.error("error one", test_span());
        bag.warning("warning one", test_span());
        bag.error("error two", test_span());
        bag.warning("warning two", test_span());
        bag.warning("warning three", test_span());

        assert!(bag.has_errors());
        assert_eq!(bag.error_count(), 2);
        assert_eq!(bag.warning_count(), 3);
        assert_eq!(bag.diagnostics().len(), 5);
    }

    #[test]
    fn diagnostic_bag_into_diagnostics() {
        let mut bag = DiagnosticBag::new();
        bag.error("err", test_span());
        bag.warning("warn", test_span());

        let diags = bag.into_diagnostics();
        assert_eq!(diags.len(), 2);
        assert_eq!(diags[0].severity, Severity::Error);
        assert_eq!(diags[1].severity, Severity::Warning);
    }

    #[test]
    fn severity_ordering() {
        assert!(Severity::Error < Severity::Warning);
        assert!(Severity::Warning < Severity::Info);
        assert!(Severity::Info < Severity::Hint);
    }

    #[test]
    fn terminal_renderer_basic() {
        struct TestFileSource;
        impl DiagnosticFileSource for TestFileSource {
            fn file_name(&self, _file_id: FileId) -> Option<&str> {
                Some("HELLO.cob")
            }
            fn file_source(&self, _file_id: FileId) -> Option<&str> {
                Some("       IDENTIFICATION DIVISION.")
            }
        }

        let renderer = TerminalRenderer::new(false);
        let diag = Diagnostic::error("unexpected token", test_span()).with_code("E0042");

        let output = renderer.render(&diag, &TestFileSource);
        assert!(output.contains("error"));
        assert!(output.contains("E0042"));
        assert!(output.contains("unexpected token"));
        assert!(output.contains("HELLO.cob"));
    }

    #[test]
    fn terminal_renderer_unknown_file() {
        struct EmptyFileSource;
        impl DiagnosticFileSource for EmptyFileSource {
            fn file_name(&self, _file_id: FileId) -> Option<&str> {
                None
            }
            fn file_source(&self, _file_id: FileId) -> Option<&str> {
                None
            }
        }

        let renderer = TerminalRenderer::new(false);
        let diag = Diagnostic::error("bad thing", test_span());

        let output = renderer.render(&diag, &EmptyFileSource);
        assert!(output.contains("<unknown>"));
    }

    #[test]
    fn lsp_converter_error() {
        let converter = LspDiagnosticConverter::new();
        let diag = Diagnostic::error("type mismatch", test_span()).with_code("E0001");

        let lsp_diag = converter.convert(&diag);
        assert_eq!(lsp_diag.severity, 1);
        assert_eq!(lsp_diag.message, "type mismatch");
        assert_eq!(lsp_diag.code.as_deref(), Some("E0001"));
        assert_eq!(
            lsp_diag.range,
            TextRange::new(TextSize::from(0), TextSize::from(10))
        );
    }

    #[test]
    fn lsp_converter_warning() {
        let converter = LspDiagnosticConverter;
        let diag = Diagnostic::warning("unused", test_span());

        let lsp_diag = converter.convert(&diag);
        assert_eq!(lsp_diag.severity, 2);
    }

    #[test]
    fn lsp_converter_info_and_hint() {
        let converter = LspDiagnosticConverter::new();

        let info = Diagnostic {
            severity: Severity::Info,
            code: None,
            message: "info msg".into(),
            primary_span: test_span(),
            primary_label: None,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        };
        assert_eq!(converter.convert(&info).severity, 3);

        let hint = Diagnostic {
            severity: Severity::Hint,
            code: None,
            message: "hint msg".into(),
            primary_span: test_span(),
            primary_label: None,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
        };
        assert_eq!(converter.convert(&hint).severity, 4);
    }
}
