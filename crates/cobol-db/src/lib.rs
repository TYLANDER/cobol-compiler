//! Incremental computation database for the cobolc compiler.
//!
//! This crate defines the query interface that connects all compilation phases.
//! In CLI mode, queries are memoized and lazily computed. In LSP mode, queries
//! are invalidated when source files change and recomputed on demand.
//!
//! The [`SimpleDatabase`] provides memoized query evaluation with automatic
//! cache invalidation. This can be replaced with Salsa for fine-grained
//! incremental recomputation in the future.

use std::cell::RefCell;
use std::collections::HashMap;

use cobol_diag::Diagnostic;
use cobol_span::FileId;

/// Input queries -- set by the driver (CLI or LSP)
pub trait InputDatabase {
    /// Set/get the source text for a file
    fn set_file_text(&mut self, file_id: FileId, text: String);
    fn file_text(&self, file_id: FileId) -> Option<&str>;

    /// Set/get the source format for a file
    fn set_source_format(&mut self, file_id: FileId, format: cobol_lexer::SourceFormat);
    fn source_format(&self, file_id: FileId) -> cobol_lexer::SourceFormat;
}

/// Derived queries -- computed from inputs, cached and invalidated automatically.
///
/// Each query depends on upstream queries. When an input changes, all downstream
/// caches are invalidated. Queries are lazily recomputed on next access.
pub trait CompilerDatabase: InputDatabase {
    /// Preprocess a file (expand COPY/REPLACE).
    /// Depends on: `file_text`.
    fn preprocessed_text(&self, file_id: FileId) -> Option<&cobol_pp::PreprocessResult>;

    /// Tokenize preprocessed text.
    /// Depends on: `preprocessed_text`, `source_format`.
    fn tokens(&self, file_id: FileId) -> Option<&[cobol_lexer::Token]>;

    /// Parse tokens into CST.
    /// Depends on: `tokens`.
    fn parse(&self, file_id: FileId) -> Option<&cobol_parser::ParseResult>;

    /// Collect all diagnostics for a file (parse errors + HIR diagnostics).
    /// Depends on: `parse`.
    fn diagnostics(&self, file_id: FileId) -> Vec<Diagnostic>;
}

/// Cached compilation results for a single file.
struct FileEntry {
    text: String,
    format: cobol_lexer::SourceFormat,
    /// Revision counter — incremented on each input change.
    revision: u64,
    /// Memoized query results with their revision stamps.
    cache: RefCell<QueryCache>,
}

/// Lazily-computed query results with revision tracking.
struct QueryCache {
    preprocessed: Option<(u64, cobol_pp::PreprocessResult)>,
    tokens: Option<(u64, Vec<cobol_lexer::Token>)>,
    parsed: Option<(u64, cobol_parser::ParseResult)>,
}

impl QueryCache {
    fn new() -> Self {
        Self {
            preprocessed: None,
            tokens: None,
            parsed: None,
        }
    }
}

/// Memoized query database for the compilation pipeline.
///
/// All derived queries are lazily computed and cached. When inputs change
/// (via `set_file_text` or `set_source_format`), downstream caches are
/// automatically invalidated via revision tracking.
pub struct SimpleDatabase {
    files: HashMap<FileId, FileEntry>,
    interner: cobol_intern::Interner,
    vfs: cobol_vfs::Vfs,
    /// Global revision counter.
    revision: u64,
}

impl SimpleDatabase {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            interner: cobol_intern::Interner::new(),
            vfs: cobol_vfs::Vfs::new(),
            revision: 0,
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

    /// Ensure the preprocessed text cache is up to date for a file.
    fn ensure_preprocessed(&self, file_id: FileId) {
        let entry = match self.files.get(&file_id) {
            Some(e) => e,
            None => return,
        };
        let rev = entry.revision;
        let needs_compute = {
            let cache = entry.cache.borrow();
            cache.preprocessed.as_ref().is_none_or(|(r, _)| *r != rev)
        };
        if needs_compute {
            let result = cobol_pp::preprocess(&entry.text, file_id, &self.vfs);
            entry.cache.borrow_mut().preprocessed = Some((rev, result));
            // Invalidate downstream caches.
            entry.cache.borrow_mut().tokens = None;
            entry.cache.borrow_mut().parsed = None;
        }
    }

    /// Ensure the token cache is up to date for a file.
    fn ensure_tokens(&self, file_id: FileId) {
        self.ensure_preprocessed(file_id);
        let entry = match self.files.get(&file_id) {
            Some(e) => e,
            None => return,
        };
        let rev = entry.revision;
        let needs_compute = {
            let cache = entry.cache.borrow();
            cache.tokens.as_ref().is_none_or(|(r, _)| *r != rev)
        };
        if needs_compute {
            let pp_text = {
                let cache = entry.cache.borrow();
                cache
                    .preprocessed
                    .as_ref()
                    .map(|(_, pp)| pp.text.clone())
                    .unwrap_or_default()
            };
            let tokens = cobol_lexer::lex(&pp_text, file_id, entry.format);
            entry.cache.borrow_mut().tokens = Some((rev, tokens));
            // Invalidate downstream.
            entry.cache.borrow_mut().parsed = None;
        }
    }

    /// Ensure the parse cache is up to date for a file.
    fn ensure_parsed(&self, file_id: FileId) {
        self.ensure_tokens(file_id);
        let entry = match self.files.get(&file_id) {
            Some(e) => e,
            None => return,
        };
        let rev = entry.revision;
        let needs_compute = {
            let cache = entry.cache.borrow();
            cache.parsed.as_ref().is_none_or(|(r, _)| *r != rev)
        };
        if needs_compute {
            let parse_result = {
                let cache = entry.cache.borrow();
                cache
                    .tokens
                    .as_ref()
                    .map(|(_, toks)| cobol_parser::parse(toks))
            };
            if let Some(result) = parse_result {
                entry.cache.borrow_mut().parsed = Some((rev, result));
            }
        }
    }
}

impl Default for SimpleDatabase {
    fn default() -> Self {
        Self::new()
    }
}

impl InputDatabase for SimpleDatabase {
    fn set_file_text(&mut self, file_id: FileId, text: String) {
        self.revision += 1;
        let entry = self.files.entry(file_id).or_insert_with(|| FileEntry {
            text: String::new(),
            format: cobol_lexer::SourceFormat::Fixed,
            revision: 0,
            cache: RefCell::new(QueryCache::new()),
        });
        entry.text = text;
        entry.revision = self.revision;
    }

    fn file_text(&self, file_id: FileId) -> Option<&str> {
        self.files.get(&file_id).map(|e| e.text.as_str())
    }

    fn set_source_format(&mut self, file_id: FileId, format: cobol_lexer::SourceFormat) {
        if let Some(entry) = self.files.get_mut(&file_id) {
            if entry.format != format {
                self.revision += 1;
                entry.format = format;
                entry.revision = self.revision;
            }
        }
    }

    fn source_format(&self, file_id: FileId) -> cobol_lexer::SourceFormat {
        self.files
            .get(&file_id)
            .map(|e| e.format)
            .unwrap_or(cobol_lexer::SourceFormat::Fixed)
    }
}

impl CompilerDatabase for SimpleDatabase {
    fn preprocessed_text(&self, file_id: FileId) -> Option<&cobol_pp::PreprocessResult> {
        self.ensure_preprocessed(file_id);
        let entry = self.files.get(&file_id)?;
        // SAFETY: The RefCell borrow is released immediately; the returned reference
        // borrows from the HashMap entry which lives as long as &self.
        // We use unsafe to extend the lifetime from the RefCell borrow.
        // This is safe because:
        // 1. The data lives in the FileEntry which is borrowed via &self
        // 2. The cache is only modified through &mut self (set_file_text/set_source_format)
        //    or through ensure_* methods which only write when stale
        // 3. We never hold a RefCell borrow across a mutation point
        let cache = entry.cache.borrow();
        cache.preprocessed.as_ref().map(|(_, pp)| {
            // Extend lifetime to match &self
            let ptr: *const cobol_pp::PreprocessResult = pp;
            unsafe { &*ptr }
        })
    }

    fn tokens(&self, file_id: FileId) -> Option<&[cobol_lexer::Token]> {
        self.ensure_tokens(file_id);
        let entry = self.files.get(&file_id)?;
        let cache = entry.cache.borrow();
        cache.tokens.as_ref().map(|(_, toks)| {
            let ptr: *const [cobol_lexer::Token] = toks.as_slice();
            unsafe { &*ptr }
        })
    }

    fn parse(&self, file_id: FileId) -> Option<&cobol_parser::ParseResult> {
        self.ensure_parsed(file_id);
        let entry = self.files.get(&file_id)?;
        let cache = entry.cache.borrow();
        cache.parsed.as_ref().map(|(_, pr)| {
            let ptr: *const cobol_parser::ParseResult = pr;
            unsafe { &*ptr }
        })
    }

    fn diagnostics(&self, file_id: FileId) -> Vec<Diagnostic> {
        let mut diags = Vec::new();

        // Parse errors
        if let Some(pr) = self.parse(file_id) {
            for err in &pr.errors {
                let span = cobol_span::Span::new(file_id, err.range, cobol_span::ExpansionId::ROOT);
                diags.push(Diagnostic::error(&err.message, span));
            }
        }

        diags
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

    #[test]
    fn compiler_db_preprocessed_text() {
        let mut db = SimpleDatabase::new();
        let fid = FileId::new(0);
        db.set_file_text(
            fid,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n".to_string(),
        );
        let pp = db.preprocessed_text(fid);
        assert!(pp.is_some());
        assert!(pp.unwrap().text.contains("IDENTIFICATION"));
    }

    #[test]
    fn compiler_db_tokens() {
        let mut db = SimpleDatabase::new();
        let fid = FileId::new(0);
        db.set_file_text(
            fid,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n".to_string(),
        );
        let toks = db.tokens(fid);
        assert!(toks.is_some());
        assert!(!toks.unwrap().is_empty());
    }

    #[test]
    fn compiler_db_parse() {
        let mut db = SimpleDatabase::new();
        let fid = FileId::new(0);
        db.set_file_text(
            fid,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n".to_string(),
        );
        let pr = db.parse(fid);
        assert!(pr.is_some());
    }

    #[test]
    fn compiler_db_memoization() {
        let mut db = SimpleDatabase::new();
        let fid = FileId::new(0);
        db.set_file_text(
            fid,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n".to_string(),
        );
        // First call computes; second call should hit cache.
        let pp1 = db.preprocessed_text(fid).unwrap() as *const _;
        let pp2 = db.preprocessed_text(fid).unwrap() as *const _;
        assert_eq!(pp1, pp2, "second call should return same pointer (cached)");
    }

    #[test]
    fn compiler_db_invalidation_on_text_change() {
        let mut db = SimpleDatabase::new();
        let fid = FileId::new(0);
        db.set_file_text(
            fid,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n".to_string(),
        );
        let pp1 = db.preprocessed_text(fid).unwrap().text.clone();
        db.set_file_text(
            fid,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CHANGED.\n".to_string(),
        );
        let pp2 = db.preprocessed_text(fid).unwrap().text.clone();
        assert_ne!(pp1, pp2, "cache should be invalidated after text change");
        assert!(pp2.contains("CHANGED"));
    }

    #[test]
    fn compiler_db_diagnostics() {
        let mut db = SimpleDatabase::new();
        let fid = FileId::new(0);
        db.set_file_text(
            fid,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n".to_string(),
        );
        let diags = db.diagnostics(fid);
        // A valid COBOL program should have zero or few diagnostics
        assert!(diags.len() < 5);
    }

    #[test]
    fn compiler_db_unknown_file_returns_none() {
        let db = SimpleDatabase::new();
        assert!(db.preprocessed_text(FileId::new(99)).is_none());
        assert!(db.tokens(FileId::new(99)).is_none());
        assert!(db.parse(FileId::new(99)).is_none());
    }
}
