//! COBOL Language Server Protocol implementation.
//!
//! Provides IDE features via the Language Server Protocol:
//! - Diagnostics (parse errors + HIR semantic errors)
//! - Hover (PIC type, byte size, offset, encoding)
//! - Go to definition (data items, paragraphs, sections)
//! - Completion (data names, paragraphs, COBOL verbs)

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use cobol_ast::AstNode;
use cobol_hir::HirModule;
use cobol_intern::Interner;
use cobol_span::FileId;

// ---------------------------------------------------------------------------
// LineIndex — byte offset ↔ LSP Position
// ---------------------------------------------------------------------------

/// Maps byte offsets in source text to LSP line/column positions.
struct LineIndex {
    /// Byte offset of the start of each line.
    line_starts: Vec<u32>,
}

impl LineIndex {
    fn new(text: &str) -> Self {
        let mut line_starts = vec![0u32];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push((i + 1) as u32);
            }
        }
        Self { line_starts }
    }

    /// Converts a byte offset to an LSP Position (0-based line & character).
    fn position(&self, offset: u32) -> Position {
        let line = self
            .line_starts
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        let col = offset - self.line_starts[line];
        Position::new(line as u32, col)
    }

    /// Converts a `TextRange` to an LSP `Range`.
    fn range(&self, range: cobol_span::TextRange) -> Range {
        Range::new(
            self.position(range.start().into()),
            self.position(range.end().into()),
        )
    }

    /// Converts an LSP Position back to a byte offset.
    fn offset(&self, pos: Position) -> u32 {
        let line = pos.line as usize;
        if line < self.line_starts.len() {
            self.line_starts[line] + pos.character
        } else {
            *self.line_starts.last().unwrap_or(&0)
        }
    }
}

// ---------------------------------------------------------------------------
// FileState — per-file compilation state
// ---------------------------------------------------------------------------

/// Cached compilation results for a single open file.
struct FileState {
    #[allow(dead_code)] // used in multi-file / cross-reference scenarios
    file_id: FileId,
    /// Original source text (as the editor sees it).
    source: String,
    /// LineIndex over the preprocessed text (spans from parser/HIR are offsets into this).
    pp_line_index: LineIndex,
    /// Preprocessed text — the text that was actually parsed.
    #[allow(dead_code)] // stored for future source-map integration
    pp_text: String,
    hir: Option<HirModule>,
    interner: Interner,
}

// ---------------------------------------------------------------------------
// ServerState — shared mutable state
// ---------------------------------------------------------------------------

struct ServerState {
    vfs: cobol_vfs::Vfs,
    files: HashMap<Url, FileState>,
}

impl ServerState {
    fn new() -> Self {
        Self {
            vfs: cobol_vfs::Vfs::new(),
            files: HashMap::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// CobolLanguageServer
// ---------------------------------------------------------------------------

/// The COBOL language server.
pub struct CobolLanguageServer {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

impl CobolLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(Mutex::new(ServerState::new())),
        }
    }

    /// Runs the full compile pipeline and publishes diagnostics.
    async fn on_change(&self, uri: Url, text: String) {
        let mut state = self.state.lock().await;

        let path = uri_to_path(&uri);
        let file_id = state.vfs.set_file_content(path, text.clone());

        // Run pipeline: preprocess → lex → parse → HIR lower
        let pp_result = cobol_pp::preprocess(&text, file_id, &state.vfs);
        let pp_text = pp_result.text.clone();
        let pp_line_index = LineIndex::new(&pp_text);
        let tokens = cobol_lexer::lex(&pp_text, file_id, cobol_lexer::SourceFormat::Fixed);
        let parse_result = cobol_parser::parse(&tokens);

        let mut interner = Interner::new();
        let hir = cobol_ast::SourceFile::cast(parse_result.syntax())
            .map(|ast| cobol_hir::lower(&ast, &mut interner, file_id));

        // Collect diagnostics
        let mut diagnostics = Vec::new();

        // Preprocessor warnings
        for err in &pp_result.errors {
            diagnostics.push(tower_lsp::lsp_types::Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                severity: Some(DiagnosticSeverity::WARNING),
                message: format!("{}", err),
                ..Default::default()
            });
        }

        // Parse errors
        for err in &parse_result.errors {
            diagnostics.push(tower_lsp::lsp_types::Diagnostic {
                range: pp_line_index.range(err.range),
                severity: Some(DiagnosticSeverity::ERROR),
                message: err.message.clone(),
                source: Some("cobolc".into()),
                ..Default::default()
            });
        }

        // HIR diagnostics
        if let Some(ref hir) = hir {
            for diag in &hir.diagnostics {
                let severity = match diag.severity {
                    cobol_hir::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
                    cobol_hir::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
                    cobol_hir::DiagnosticSeverity::Info => DiagnosticSeverity::INFORMATION,
                    cobol_hir::DiagnosticSeverity::Hint => DiagnosticSeverity::HINT,
                };
                diagnostics.push(tower_lsp::lsp_types::Diagnostic {
                    range: pp_line_index.range(diag.span.range),
                    severity: Some(severity),
                    message: diag.message.clone(),
                    source: Some("cobolc".into()),
                    ..Default::default()
                });
            }
        }

        // Cache file state for hover/goto-def/completion
        state.files.insert(
            uri.clone(),
            FileState {
                file_id,
                source: text,
                pp_line_index,
                pp_text,
                hir,
                interner,
            },
        );

        // Drop the lock before async publish
        drop(state);

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    /// Finds the word under the cursor in the source text.
    fn word_at_position(text: &str, offset: u32) -> Option<&str> {
        let offset = offset as usize;
        if offset > text.len() {
            return None;
        }

        let bytes = text.as_bytes();
        let mut start = offset;
        while start > 0 && is_cobol_name_char(bytes[start - 1]) {
            start -= 1;
        }
        let mut end = offset;
        while end < bytes.len() && is_cobol_name_char(bytes[end]) {
            end += 1;
        }

        if start == end {
            None
        } else {
            Some(&text[start..end])
        }
    }

    /// Finds a data item by name in the HIR module.
    fn find_data_item<'a>(
        hir: &'a HirModule,
        interner: &Interner,
        name: &str,
    ) -> Option<&'a cobol_hir::DataItemData> {
        let upper = name.to_uppercase();
        for (_, item) in hir.data_items.iter() {
            if let Some(n) = item.name {
                if interner.resolve(n).eq_ignore_ascii_case(&upper) {
                    return Some(item);
                }
            }
        }
        None
    }

    /// Finds a paragraph by name in the HIR module.
    fn find_paragraph<'a>(
        hir: &'a HirModule,
        interner: &Interner,
        name: &str,
    ) -> Option<&'a cobol_hir::HirParagraph> {
        let upper = name.to_uppercase();
        hir.paragraphs
            .iter()
            .find(|p| interner.resolve(p.name).eq_ignore_ascii_case(&upper))
    }

    /// Finds a section by name in the HIR module.
    fn find_section<'a>(
        hir: &'a HirModule,
        interner: &Interner,
        name: &str,
    ) -> Option<&'a cobol_hir::HirSection> {
        let upper = name.to_uppercase();
        hir.sections
            .iter()
            .find(|s| interner.resolve(s.name).eq_ignore_ascii_case(&upper))
    }

    /// Formats hover info for a data item.
    fn format_data_item_hover(item: &cobol_hir::DataItemData, interner: &Interner) -> String {
        let name = item
            .name
            .map(|n| interner.resolve(n).to_string())
            .unwrap_or_else(|| "FILLER".to_string());

        let mut lines = vec![format!("**{}**  (level {:02})", name, item.level)];

        if let Some(ref pic) = item.storage.picture {
            lines.push(format!("PIC {}", pic.pic_string));
            lines.push(format!(
                "Category: {:?} | Size: {} digits",
                pic.category, pic.size
            ));
            if pic.scale != 0 {
                lines.push(format!("Scale: {}", pic.scale));
            }
        }

        lines.push(format!("Byte size: {}", item.storage.byte_size));
        lines.push(format!("Offset: {}", item.offset));
        lines.push(format!("Usage: {:?}", item.storage.usage));
        lines.push(format!("Encoding: {:?}", item.storage.encoding));

        if item.is_group {
            lines.push("Group item".to_string());
        }

        if let Some(ref occurs) = item.occurs {
            if occurs.min == occurs.max {
                lines.push(format!("OCCURS {}", occurs.max));
            } else {
                lines.push(format!("OCCURS {} TO {}", occurs.min, occurs.max));
            }
        }

        lines.join("  \n")
    }
}

fn is_cobol_name_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'-' || b == b'_'
}

fn uri_to_path(uri: &Url) -> PathBuf {
    uri.to_file_path()
        .unwrap_or_else(|_| PathBuf::from(uri.path()))
}

// ---------------------------------------------------------------------------
// COBOL keywords for completion
// ---------------------------------------------------------------------------

const COBOL_VERBS: &[&str] = &[
    "ACCEPT",
    "ADD",
    "ALTER",
    "CALL",
    "CANCEL",
    "CLOSE",
    "COMPUTE",
    "CONTINUE",
    "DELETE",
    "DISPLAY",
    "DIVIDE",
    "EVALUATE",
    "EXIT",
    "GO",
    "GOBACK",
    "IF",
    "INITIALIZE",
    "INSPECT",
    "MERGE",
    "MOVE",
    "MULTIPLY",
    "OPEN",
    "PERFORM",
    "READ",
    "RELEASE",
    "RETURN",
    "REWRITE",
    "SEARCH",
    "SET",
    "SORT",
    "START",
    "STOP",
    "STRING",
    "SUBTRACT",
    "UNSTRING",
    "WRITE",
];

// ---------------------------------------------------------------------------
// LanguageServer trait implementation
// ---------------------------------------------------------------------------

#[tower_lsp::async_trait]
impl LanguageServer for CobolLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![" ".into(), ".".into()]),
                    ..Default::default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "cobolc-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "cobolc LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // With FULL sync, the last content change has the complete text.
        if let Some(change) = params.content_changes.into_iter().last() {
            self.on_change(params.text_document.uri, change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut state = self.state.lock().await;
        state.files.remove(&params.text_document.uri);
        drop(state);

        // Clear diagnostics for closed file
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let state = self.state.lock().await;

        let file = match state.files.get(uri) {
            Some(f) => f,
            None => return Ok(None),
        };

        let hir = match &file.hir {
            Some(h) => h,
            None => return Ok(None),
        };

        let offset = file.pp_line_index.offset(pos);
        let word = match Self::word_at_position(&file.source, offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Try data item first
        if let Some(item) = Self::find_data_item(hir, &file.interner, word) {
            let hover_text = Self::format_data_item_hover(item, &file.interner);
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_text,
                }),
                range: None,
            }));
        }

        // Try paragraph
        if let Some(para) = Self::find_paragraph(hir, &file.interner, word) {
            let name = file.interner.resolve(para.name);
            let text = format!(
                "**{}** (paragraph)  \n{} statements",
                name,
                para.statements.len()
            );
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: text,
                }),
                range: None,
            }));
        }

        // Try section
        if let Some(sec) = Self::find_section(hir, &file.interner, word) {
            let name = file.interner.resolve(sec.name);
            let text = format!(
                "**{}** (section)  \n{} paragraphs",
                name,
                sec.paragraphs.len()
            );
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: text,
                }),
                range: None,
            }));
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let state = self.state.lock().await;

        let file = match state.files.get(uri) {
            Some(f) => f,
            None => return Ok(None),
        };

        let hir = match &file.hir {
            Some(h) => h,
            None => return Ok(None),
        };

        let offset = file.pp_line_index.offset(pos);
        let word = match Self::word_at_position(&file.source, offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Try data item
        if let Some(item) = Self::find_data_item(hir, &file.interner, word) {
            let range = file.pp_line_index.range(item.span.range);
            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                uri.clone(),
                range,
            ))));
        }

        // Try paragraph
        if let Some(para) = Self::find_paragraph(hir, &file.interner, word) {
            let range = file.pp_line_index.range(para.span.range);
            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                uri.clone(),
                range,
            ))));
        }

        // Try section
        if let Some(sec) = Self::find_section(hir, &file.interner, word) {
            let range = file.pp_line_index.range(sec.span.range);
            return Ok(Some(GotoDefinitionResponse::Scalar(Location::new(
                uri.clone(),
                range,
            ))));
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let state = self.state.lock().await;

        let file = match state.files.get(uri) {
            Some(f) => f,
            None => return Ok(None),
        };

        let offset = file.pp_line_index.offset(pos) as usize;
        let source = &file.source;

        // Get the text on the current line up to the cursor
        let line_start = source[..offset.min(source.len())]
            .rfind('\n')
            .map(|i| i + 1)
            .unwrap_or(0);
        let line_prefix = &source[line_start..offset.min(source.len())];
        let prefix_upper = line_prefix.trim().to_uppercase();

        // Extract the partial word the user is typing (for filtering)
        let typing_prefix = {
            let bytes = source.as_bytes();
            let mut start = offset.min(source.len());
            while start > 0 && is_cobol_name_char(bytes[start - 1]) {
                start -= 1;
            }
            source[start..offset.min(source.len())].to_uppercase()
        };

        let mut items = Vec::new();

        // Helper: build a data item CompletionItem with consistent detail
        let make_data_item =
            |di: &cobol_hir::DataItemData, interner: &Interner| -> CompletionItem {
                let name = di
                    .name
                    .map(|n| interner.resolve(n).to_string())
                    .unwrap_or_default();
                let detail = di
                    .storage
                    .picture
                    .as_ref()
                    .map(|p| format!("PIC {} ({} bytes)", p.pic_string, di.storage.byte_size))
                    .or_else(|| {
                        if di.is_group {
                            Some(format!("Group ({} bytes)", di.storage.byte_size))
                        } else {
                            Some(format!(
                                "{:?} ({} bytes)",
                                di.storage.usage, di.storage.byte_size
                            ))
                        }
                    });
                CompletionItem {
                    label: name,
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail,
                    ..Default::default()
                }
            };

        // Context-aware completion
        if let Some(ref hir) = file.hir {
            let after_perform =
                prefix_upper.ends_with("PERFORM") || prefix_upper.ends_with("PERFORM ");
            let after_verb = is_after_verb(&prefix_upper);

            if after_perform {
                // After PERFORM: suggest paragraph and section names
                for para in &hir.paragraphs {
                    let name = file.interner.resolve(para.name).to_string();
                    items.push(CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(format!("Paragraph ({} stmts)", para.statements.len())),
                        ..Default::default()
                    });
                }
                for sec in &hir.sections {
                    let name = file.interner.resolve(sec.name).to_string();
                    items.push(CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::MODULE),
                        detail: Some(format!("Section ({} paras)", sec.paragraphs.len())),
                        ..Default::default()
                    });
                }
            } else if after_verb {
                // After a verb (MOVE, ADD, TO, FROM, etc.): suggest data item names
                for (_, di) in hir.data_items.iter() {
                    if di.name.is_some() {
                        items.push(make_data_item(di, &file.interner));
                    }
                }
            } else {
                // Start of statement: suggest COBOL verbs + data names
                for &verb in COBOL_VERBS {
                    items.push(CompletionItem {
                        label: verb.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        detail: Some("COBOL verb".into()),
                        ..Default::default()
                    });
                }
                for (_, di) in hir.data_items.iter() {
                    if di.name.is_some() {
                        items.push(make_data_item(di, &file.interner));
                    }
                }
            }
        } else {
            // No HIR yet — just suggest verbs
            for &verb in COBOL_VERBS {
                items.push(CompletionItem {
                    label: verb.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("COBOL verb".into()),
                    ..Default::default()
                });
            }
        }

        // Filter by typed prefix
        if !typing_prefix.is_empty() {
            items.retain(|item| item.label.to_uppercase().starts_with(&typing_prefix));
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let state = self.state.lock().await;

        let file = match state.files.get(uri) {
            Some(f) => f,
            None => return Ok(None),
        };

        let hir = match &file.hir {
            Some(h) => h,
            None => return Ok(None),
        };

        let mut symbols = Vec::new();

        // Data Division symbols
        let mut data_children = Vec::new();
        for (_, item) in hir.data_items.iter() {
            let name = match item.name {
                Some(n) => file.interner.resolve(n).to_string(),
                None => continue,
            };
            let range = file.pp_line_index.range(item.span.range);
            let detail = item
                .storage
                .picture
                .as_ref()
                .map(|p| format!("PIC {} ({} bytes)", p.pic_string, item.storage.byte_size));

            #[allow(deprecated)] // DocumentSymbol requires deprecated `deprecated` field
            data_children.push(DocumentSymbol {
                name,
                detail,
                kind: if item.is_group {
                    SymbolKind::STRUCT
                } else {
                    SymbolKind::FIELD
                },
                range,
                selection_range: range,
                children: None,
                tags: None,
                deprecated: None,
            });
        }

        if !data_children.is_empty() {
            let first_range = data_children.first().map(|s| s.range).unwrap_or_default();
            let last_range = data_children.last().map(|s| s.range).unwrap_or_default();
            let enclosing = Range::new(first_range.start, last_range.end);

            #[allow(deprecated)]
            symbols.push(DocumentSymbol {
                name: "DATA DIVISION".into(),
                detail: Some(format!("{} items", data_children.len())),
                kind: SymbolKind::NAMESPACE,
                range: enclosing,
                selection_range: enclosing,
                children: Some(data_children),
                tags: None,
                deprecated: None,
            });
        }

        // Procedure Division symbols — sections with paragraph children
        let mut proc_children = Vec::new();

        for sec in &hir.sections {
            let sec_name = file.interner.resolve(sec.name).to_string();
            let sec_range = file.pp_line_index.range(sec.span.range);

            let mut para_symbols = Vec::new();
            for &para_idx in &sec.paragraphs {
                if let Some(para) = hir.paragraphs.get(para_idx) {
                    let para_name = file.interner.resolve(para.name).to_string();
                    let para_range = file.pp_line_index.range(para.span.range);
                    #[allow(deprecated)]
                    para_symbols.push(DocumentSymbol {
                        name: para_name,
                        detail: Some(format!("{} stmts", para.statements.len())),
                        kind: SymbolKind::FUNCTION,
                        range: para_range,
                        selection_range: para_range,
                        children: None,
                        tags: None,
                        deprecated: None,
                    });
                }
            }

            #[allow(deprecated)]
            proc_children.push(DocumentSymbol {
                name: sec_name,
                detail: Some(format!("{} paragraphs", sec.paragraphs.len())),
                kind: SymbolKind::MODULE,
                range: sec_range,
                selection_range: sec_range,
                children: if para_symbols.is_empty() {
                    None
                } else {
                    Some(para_symbols)
                },
                tags: None,
                deprecated: None,
            });
        }

        // Standalone paragraphs (not in any section)
        let section_para_indices: std::collections::HashSet<usize> = hir
            .sections
            .iter()
            .flat_map(|s| s.paragraphs.iter().copied())
            .collect();

        for (idx, para) in hir.paragraphs.iter().enumerate() {
            if section_para_indices.contains(&idx) {
                continue; // already nested under a section
            }
            let para_name = file.interner.resolve(para.name).to_string();
            let para_range = file.pp_line_index.range(para.span.range);
            #[allow(deprecated)]
            proc_children.push(DocumentSymbol {
                name: para_name,
                detail: Some(format!("{} stmts", para.statements.len())),
                kind: SymbolKind::FUNCTION,
                range: para_range,
                selection_range: para_range,
                children: None,
                tags: None,
                deprecated: None,
            });
        }

        if !proc_children.is_empty() {
            let first_range = proc_children.first().map(|s| s.range).unwrap_or_default();
            let last_range = proc_children.last().map(|s| s.range).unwrap_or_default();
            let enclosing = Range::new(first_range.start, last_range.end);

            #[allow(deprecated)]
            symbols.push(DocumentSymbol {
                name: "PROCEDURE DIVISION".into(),
                detail: Some(format!("{} entries", proc_children.len())),
                kind: SymbolKind::NAMESPACE,
                range: enclosing,
                selection_range: enclosing,
                children: Some(proc_children),
                tags: None,
                deprecated: None,
            });
        }

        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let state = self.state.lock().await;

        let file = match state.files.get(uri) {
            Some(f) => f,
            None => return Ok(None),
        };

        let offset = file.pp_line_index.offset(pos);
        let word = match Self::word_at_position(&file.source, offset) {
            Some(w) => w.to_uppercase(),
            None => return Ok(None),
        };

        // Find all occurrences of this name in the preprocessed text
        let pp_text_upper = file.pp_text.to_uppercase();
        let mut locations = Vec::new();

        let mut search_from = 0;
        while let Some(pos) = pp_text_upper[search_from..].find(&word) {
            let abs_pos = search_from + pos;
            let end_pos = abs_pos + word.len();

            // Verify it's a whole word match (not a substring of a larger identifier)
            let before_ok =
                abs_pos == 0 || !is_cobol_name_char(file.pp_text.as_bytes()[abs_pos - 1]);
            let after_ok = end_pos >= file.pp_text.len()
                || !is_cobol_name_char(file.pp_text.as_bytes()[end_pos]);

            if before_ok && after_ok {
                let start = file.pp_line_index.position(abs_pos as u32);
                let end = file.pp_line_index.position(end_pos as u32);
                locations.push(Location::new(uri.clone(), Range::new(start, end)));
            }

            search_from = abs_pos + 1;
        }

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }
}

/// Checks if the line prefix ends with a COBOL verb (indicating data names should follow).
fn is_after_verb(prefix: &str) -> bool {
    let verbs = [
        "MOVE",
        "ADD",
        "SUBTRACT",
        "MULTIPLY",
        "DIVIDE",
        "COMPUTE",
        "DISPLAY",
        "ACCEPT",
        "SET",
        "INITIALIZE",
        "INSPECT",
        "STRING",
        "UNSTRING",
        "IF",
        "EVALUATE",
        "WHEN",
        "UNTIL",
        "TO",
        "FROM",
        "INTO",
        "GIVING",
        "BY",
        "OF",
        "IN",
    ];
    let trimmed = prefix.trim();
    verbs
        .iter()
        .any(|v| trimmed.ends_with(v) || trimmed.ends_with(&format!("{} ", v)))
}

// ---------------------------------------------------------------------------
// Public API — start the server
// ---------------------------------------------------------------------------

/// Starts the LSP server on stdin/stdout using a tokio runtime.
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(CobolLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_index_single_line() {
        let idx = LineIndex::new("HELLO WORLD");
        assert_eq!(idx.position(0), Position::new(0, 0));
        assert_eq!(idx.position(6), Position::new(0, 6));
    }

    #[test]
    fn line_index_multi_line() {
        let idx = LineIndex::new("LINE1\nLINE2\nLINE3");
        assert_eq!(idx.position(0), Position::new(0, 0));
        assert_eq!(idx.position(6), Position::new(1, 0));
        assert_eq!(idx.position(12), Position::new(2, 0));
        assert_eq!(idx.position(14), Position::new(2, 2));
    }

    #[test]
    fn line_index_roundtrip() {
        let idx = LineIndex::new("ABC\nDEF\nGHI");
        let pos = Position::new(1, 2);
        let off = idx.offset(pos);
        assert_eq!(idx.position(off), pos);
    }

    #[test]
    fn word_at_position_basic() {
        let text = "       MOVE WS-COUNT TO WS-TOTAL";
        assert_eq!(
            CobolLanguageServer::word_at_position(text, 12),
            Some("WS-COUNT")
        );
        assert_eq!(
            CobolLanguageServer::word_at_position(text, 24),
            Some("WS-TOTAL")
        );
    }

    #[test]
    fn word_at_position_start_of_word() {
        let text = "HELLO WORLD";
        assert_eq!(
            CobolLanguageServer::word_at_position(text, 0),
            Some("HELLO")
        );
    }

    #[test]
    fn word_at_position_spaces() {
        let text = "   ";
        assert_eq!(CobolLanguageServer::word_at_position(text, 1), None);
    }

    #[test]
    fn is_after_verb_detection() {
        assert!(is_after_verb("       MOVE"));
        assert!(is_after_verb("       MOVE "));
        assert!(is_after_verb("           ADD"));
        assert!(is_after_verb("           TO"));
        assert!(!is_after_verb("       01"));
        assert!(!is_after_verb(""));
    }

    #[test]
    fn uri_to_path_file_scheme() {
        let uri = Url::parse("file:///tmp/test.cbl").unwrap();
        assert_eq!(uri_to_path(&uri), PathBuf::from("/tmp/test.cbl"));
    }
}
