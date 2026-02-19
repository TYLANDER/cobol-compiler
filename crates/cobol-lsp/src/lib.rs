//! COBOL Language Server Protocol implementation.
//!
//! Provides IDE features via the Language Server Protocol:
//! - Diagnostics (errors and warnings)
//! - Completion (data names, paragraphs, verbs, copybooks)
//! - Go to definition (data items, paragraphs, copybooks)
//! - Find references
//! - Hover information (PIC types, data layout)
//! - Rename
//! - Formatting
//! - Semantic tokens (for syntax highlighting)
//! - Code actions (quick fixes)
//! - Code lens
//!
//! Uses the Cranelift backend for "Run" code lens actions.
//! Built on tower-lsp (to be added in Phase 4).

/// LSP server capabilities that will be supported
#[derive(Debug, Clone)]
pub struct ServerCapabilities {
    pub diagnostics: bool,
    pub completion: bool,
    pub hover: bool,
    pub goto_definition: bool,
    pub find_references: bool,
    pub rename: bool,
    pub formatting: bool,
    pub semantic_tokens: bool,
    pub code_actions: bool,
    pub code_lens: bool,
}

impl Default for ServerCapabilities {
    fn default() -> Self {
        Self {
            diagnostics: true,
            completion: true,
            hover: true,
            goto_definition: true,
            find_references: true,
            rename: true,
            formatting: true,
            semantic_tokens: true,
            code_actions: true,
            code_lens: true,
        }
    }
}

/// Placeholder for the LSP server
pub struct CobolLanguageServer {
    capabilities: ServerCapabilities,
}

impl CobolLanguageServer {
    pub fn new() -> Self {
        Self {
            capabilities: ServerCapabilities::default(),
        }
    }

    pub fn capabilities(&self) -> &ServerCapabilities {
        &self.capabilities
    }
}

impl Default for CobolLanguageServer {
    fn default() -> Self {
        Self::new()
    }
}

// TODO: Implement tower-lsp traits in Phase 4

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cobol_language_server_new() {
        let server = CobolLanguageServer::new();
        let caps = server.capabilities();
        assert!(caps.diagnostics);
        assert!(caps.completion);
        assert!(caps.hover);
        assert!(caps.goto_definition);
        assert!(caps.find_references);
        assert!(caps.rename);
        assert!(caps.formatting);
        assert!(caps.semantic_tokens);
        assert!(caps.code_actions);
        assert!(caps.code_lens);
    }

    #[test]
    fn cobol_language_server_default() {
        let server = CobolLanguageServer::default();
        assert!(server.capabilities().diagnostics);
    }

    #[test]
    fn server_capabilities_default() {
        let caps = ServerCapabilities::default();
        assert!(caps.diagnostics);
        assert!(caps.completion);
        assert!(caps.hover);
        assert!(caps.goto_definition);
        assert!(caps.find_references);
        assert!(caps.rename);
        assert!(caps.formatting);
        assert!(caps.semantic_tokens);
        assert!(caps.code_actions);
        assert!(caps.code_lens);
    }

    #[test]
    fn server_capabilities_clone() {
        let caps = ServerCapabilities::default();
        let caps2 = caps.clone();
        assert_eq!(caps.diagnostics, caps2.diagnostics);
        assert_eq!(caps.completion, caps2.completion);
    }
}
