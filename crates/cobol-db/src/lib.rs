//! Incremental computation database for the cobolc compiler.
//!
//! This crate defines the query interface that connects all compilation phases.
//! In CLI mode, queries run once. In LSP mode, queries are re-evaluated
//! incrementally when source files change.
//!
//! Will be backed by Salsa once the full pipeline is connected.

use cobol_span::FileId;
use cobol_diag::Diagnostic;

/// Input queries -- set by the driver (CLI or LSP)
pub trait InputDatabase {
    /// Set/get the source text for a file
    fn set_file_text(&mut self, file_id: FileId, text: String);
    fn file_text(&self, file_id: FileId) -> Option<&str>;

    /// Set/get the source format for a file
    fn set_source_format(&mut self, file_id: FileId, format: cobol_lexer::SourceFormat);
    fn source_format(&self, file_id: FileId) -> cobol_lexer::SourceFormat;
}

/// Derived queries -- computed from inputs, cached and invalidated automatically
pub trait CompilerDatabase: InputDatabase {
    /// Preprocess a file (expand COPY/REPLACE)
    fn preprocessed_text(&self, file_id: FileId) -> &cobol_pp::PreprocessResult;

    /// Tokenize preprocessed text
    fn tokens(&self, file_id: FileId) -> &[cobol_lexer::Token];

    /// Parse tokens into CST
    fn parse(&self, file_id: FileId) -> &cobol_parser::ParseResult;

    /// Collect all diagnostics for a file
    fn diagnostics(&self, file_id: FileId) -> Vec<Diagnostic>;
}

/// Simple non-incremental implementation for CLI use
pub struct SimpleDatabase {
    files: std::collections::HashMap<FileId, FileEntry>,
    interner: cobol_intern::Interner,
    vfs: cobol_vfs::Vfs,
}

struct FileEntry {
    text: String,
    format: cobol_lexer::SourceFormat,
    // Cached results (lazily computed)
    _preprocessed: Option<cobol_pp::PreprocessResult>,
    _tokens: Option<Vec<cobol_lexer::Token>>,
    _parsed: Option<cobol_parser::ParseResult>,
}

impl SimpleDatabase {
    pub fn new() -> Self {
        Self {
            files: std::collections::HashMap::new(),
            interner: cobol_intern::Interner::new(),
            vfs: cobol_vfs::Vfs::new(),
        }
    }

    pub fn interner(&self) -> &cobol_intern::Interner {
        &self.interner
    }

    pub fn interner_mut(&mut self) -> &mut cobol_intern::Interner {
        &mut self.interner
    }

    pub fn vfs(&self) -> &cobol_vfs::Vfs {
        &self.vfs
    }

    pub fn vfs_mut(&mut self) -> &mut cobol_vfs::Vfs {
        &mut self.vfs
    }
}

impl Default for SimpleDatabase {
    fn default() -> Self {
        Self::new()
    }
}

impl InputDatabase for SimpleDatabase {
    fn set_file_text(&mut self, file_id: FileId, text: String) {
        let entry = self.files.entry(file_id).or_insert_with(|| FileEntry {
            text: String::new(),
            format: cobol_lexer::SourceFormat::Fixed,
            _preprocessed: None,
            _tokens: None,
            _parsed: None,
        });
        entry.text = text;
        // Invalidate caches
        entry._preprocessed = None;
        entry._tokens = None;
        entry._parsed = None;
    }

    fn file_text(&self, file_id: FileId) -> Option<&str> {
        self.files.get(&file_id).map(|e| e.text.as_str())
    }

    fn set_source_format(&mut self, file_id: FileId, format: cobol_lexer::SourceFormat) {
        if let Some(entry) = self.files.get_mut(&file_id) {
            entry.format = format;
            entry._tokens = None;
            entry._parsed = None;
        }
    }

    fn source_format(&self, file_id: FileId) -> cobol_lexer::SourceFormat {
        self.files
            .get(&file_id)
            .map(|e| e.format)
            .unwrap_or(cobol_lexer::SourceFormat::Fixed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_database_set_and_get_file_text() {
        let mut db = SimpleDatabase::new();
        let file_id = FileId::new(0);
        db.set_file_text(file_id, "IDENTIFICATION DIVISION.".to_string());
        assert_eq!(db.file_text(file_id), Some("IDENTIFICATION DIVISION."));
    }

    #[test]
    fn simple_database_unknown_file_returns_none() {
        let db = SimpleDatabase::new();
        assert_eq!(db.file_text(FileId::new(99)), None);
    }

    #[test]
    fn simple_database_overwrite_invalidates_cache() {
        let mut db = SimpleDatabase::new();
        let file_id = FileId::new(0);
        db.set_file_text(file_id, "v1".to_string());
        assert_eq!(db.file_text(file_id), Some("v1"));
        db.set_file_text(file_id, "v2".to_string());
        assert_eq!(db.file_text(file_id), Some("v2"));
    }

    #[test]
    fn simple_database_default_source_format() {
        let db = SimpleDatabase::new();
        assert_eq!(
            db.source_format(FileId::new(0)),
            cobol_lexer::SourceFormat::Fixed
        );
    }

    #[test]
    fn simple_database_set_source_format() {
        let mut db = SimpleDatabase::new();
        let file_id = FileId::new(0);
        db.set_file_text(file_id, "source".to_string());
        db.set_source_format(file_id, cobol_lexer::SourceFormat::Free);
        assert_eq!(db.source_format(file_id), cobol_lexer::SourceFormat::Free);
    }

    #[test]
    fn simple_database_interner_access() {
        let mut db = SimpleDatabase::new();
        let name = db.interner_mut().intern("HELLO");
        assert_eq!(db.interner().resolve(name), "HELLO");
    }

    #[test]
    fn simple_database_default() {
        let db = SimpleDatabase::default();
        assert!(db.interner().is_empty());
    }
}
