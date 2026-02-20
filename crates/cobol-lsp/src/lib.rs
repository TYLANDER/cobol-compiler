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
    file_id: FileId,
    /// Original source text (as the editor sees it).
    source: String,
    /// LineIndex over the original source (for translating back to editor positions).
    source_line_index: LineIndex,
    /// LineIndex over the preprocessed text (spans from parser/HIR are offsets into this).
    pp_line_index: LineIndex,
    /// Preprocessed text — the text that was actually parsed.
    pp_text: String,
    /// Maps preprocessed byte ranges back to original source locations.
    expansion_map: cobol_pp::ExpansionMap,
    /// Detected source format (fixed or free).
    source_format: cobol_lexer::SourceFormat,
    hir: Option<HirModule>,
    interner: Interner,
}

/// Detect the source format (fixed vs free) from the file content.
///
/// Looks for `>>SOURCE FORMAT IS FREE` or `>>SOURCE FORMAT IS FIXED`
/// directives. Falls back to fixed-format unless the file starts with
/// a free-format indicator.
fn detect_source_format(text: &str) -> cobol_lexer::SourceFormat {
    for line in text.lines().take(20) {
        let trimmed = line.trim().to_uppercase();
        if trimmed.contains(">>SOURCE") && trimmed.contains("FREE") {
            return cobol_lexer::SourceFormat::Free;
        }
        if trimmed.contains(">>SOURCE") && trimmed.contains("FIXED") {
            return cobol_lexer::SourceFormat::Fixed;
        }
    }
    // Heuristic: if no lines are longer than 80 chars and column 7 has
    // indicator characters, it's likely fixed format.  Otherwise check
    // if the first IDENTIFICATION keyword starts in column 1-6 (free) or 7+ (fixed).
    for line in text.lines().take(10) {
        let upper = line.to_uppercase();
        if upper.trim_start().starts_with("IDENTIFICATION") {
            // If IDENTIFICATION starts before column 7, treat as free
            if let Some(pos) = upper.find("IDENTIFICATION") {
                if pos < 7 {
                    return cobol_lexer::SourceFormat::Free;
                }
            }
        }
    }
    cobol_lexer::SourceFormat::Fixed
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

        // Detect source format from file content
        let source_format = detect_source_format(&text);

        // Run pipeline: preprocess → lex → parse → HIR lower
        let pp_result = cobol_pp::preprocess(&text, file_id, &state.vfs);
        let pp_text = pp_result.text.clone();
        let expansion_map = pp_result.expansion_map;
        let source_line_index = LineIndex::new(&text);
        let pp_line_index = LineIndex::new(&pp_text);
        let tokens = cobol_lexer::lex(&pp_text, file_id, source_format);
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
                source_line_index,
                pp_line_index,
                pp_text,
                expansion_map,
                source_format,
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

/// Translate a preprocessed byte offset to a source document position using
/// the expansion map.  For spans that originate in copybooks (different file),
/// returns `None` — the reference is not in the current editor buffer.
fn pp_offset_to_source_position(pp_offset: usize, file: &FileState) -> Option<Position> {
    // Walk expansion entries to find the one covering this offset.
    for entry in &file.expansion_map.entries {
        if entry.output_range.contains(&pp_offset) {
            // Only map back if the span came from the same source file
            if entry.source_file == file.file_id {
                let delta = pp_offset - entry.output_range.start;
                let source_byte = entry.source_offset + delta;
                return Some(file.source_line_index.position(source_byte as u32));
            } else {
                return None; // copybook — skip
            }
        }
    }
    // Fallback: use the pp_line_index directly (no COPY expansion)
    Some(file.pp_line_index.position(pp_offset as u32))
}

/// Find all token-level occurrences of a symbol name in the preprocessed text.
/// Uses the lexer to avoid matching inside strings, comments, or operators.
/// Returns (start_byte, end_byte) pairs in the preprocessed text.
fn find_symbol_token_spans(
    pp_text: &str,
    file_id: FileId,
    format: cobol_lexer::SourceFormat,
    symbol: &str,
) -> Vec<(usize, usize)> {
    let tokens = cobol_lexer::lex(pp_text, file_id, format);
    let upper = symbol.to_uppercase();
    let mut spans = Vec::new();

    for token in &tokens {
        // Only match Word tokens (identifiers), not keywords or literals
        if token.kind == cobol_lexer::TokenKind::Word && token.text.eq_ignore_ascii_case(&upper) {
            let start: u32 = token.span.range.start().into();
            let end: u32 = token.span.range.end().into();
            spans.push((start as usize, end as usize));
        }
    }

    spans
}

fn uri_to_path(uri: &Url) -> PathBuf {
    uri.to_file_path()
        .unwrap_or_else(|_| PathBuf::from(uri.path()))
}

// ---------------------------------------------------------------------------
// Semantic token types for syntax highlighting
// ---------------------------------------------------------------------------

const SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::NAMESPACE, // 0 — divisions, section headers
    SemanticTokenType::TYPE,      // 1 — PIC, USAGE, data description
    SemanticTokenType::FUNCTION,  // 2 — paragraph/section names (unused in tokens, reserved)
    SemanticTokenType::VARIABLE,  // 3 — data item references (Word tokens)
    SemanticTokenType::STRING,    // 4 — string/hex literals
    SemanticTokenType::NUMBER,    // 5 — numeric literals, level numbers
    SemanticTokenType::KEYWORD,   // 6 — verbs, reserved words, scope terminators
    SemanticTokenType::COMMENT,   // 7 — comment lines
    SemanticTokenType::OPERATOR,  // 8 — arithmetic/comparison operators
];

/// Maps a lexer TokenKind to a semantic token type index, or None if the
/// token should not receive semantic highlighting.
fn token_kind_to_semantic_type(kind: cobol_lexer::TokenKind) -> Option<u32> {
    use cobol_lexer::TokenKind::*;
    match kind {
        // Division/section headers → namespace
        IdentificationDivision
        | EnvironmentDivision
        | DataDivision
        | ProcedureDivision
        | ConfigurationSection
        | InputOutputSection
        | FileSection
        | WorkingStorageSection
        | LinkageSection
        | LocalStorageSection
        | ScreenSection
        | ReportSection
        | CommunicationSection => Some(0),

        // Data description → type
        Pic | Picture | Usage | Comp | Comp1 | Comp2 | Comp3 | Comp4 | Comp5 | Binary
        | PackedDecimal => Some(1),

        // String literals → string
        StringLiteral | HexLiteral => Some(4),

        // Numeric literals and level numbers → number
        IntegerLiteral | DecimalLiteral | Level | Level66 | Level77 | Level88 => Some(5),

        // Verbs → keyword
        Accept | Add | Alter | Call | Cancel | Close | Compute | Continue | Delete | Display
        | Divide | Enter | Evaluate | ExecSql | Exit | GoTo | GoBack | If | Initialize
        | Inspect | Merge | Move | Multiply | Open | Perform | Read | Release | Return_
        | Rewrite | Search | Set | Sort | Start | Stop | String_ | Subtract | Unstring | Write_
        | Copy => Some(6),

        // Scope terminators and reserved words → keyword
        EndAdd | EndCall | EndCompute | EndDelete | EndDivide | EndEvaluate | EndIf
        | EndMultiply | EndPerform | EndRead | EndReturn | EndRewrite | EndSearch | EndStart
        | EndString | EndSubtract | EndUnstring | EndWrite | Else | Not | And | Or | Also | Any
        | At | Before | After | By | Corresponding | Corr | Delimited | Delimiter | End | From
        | Giving | In | Of | Into | On | Other | Replacing | Rounded | Tallying | Test | Than
        | Then | Through | Thru | Times | To | Until | Up | Down | Using | Varying | With
        | True_ | False_ | Null_ | Nulls | Returning | Run | Fd | Sd | Select | Assign
        | SizeError | OnSizeError | NotOnSizeError | InvalidKey | NotInvalidKey | AtEnd
        | NotAtEnd | Value | Values | Redefines | Occurs | Depending | Ascending | Descending
        | Key | Indexed | Filler | Renames | Blank | When | Zero | Zeroes | Zeros | Justified
        | Just | Sign_ | Leading | Trailing | Separate | Sync | Synchronized | Global
        | External | Reference | Content | All | ProgramId | SectionKw | ParagraphKw => Some(6),

        // Comments → comment
        Comment => Some(7),

        // Operators → operator
        Plus | Minus | Star | Slash | DoubleStar | Ampersand | EqualSign | GreaterThan
        | LessThan | GreaterEqual | LessEqual | NotEqual => Some(8),

        // Word, whitespace, punctuation, etc. → no semantic type
        _ => None,
    }
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
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
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
        let word = match Self::word_at_position(&file.pp_text, offset) {
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
        let word = match Self::word_at_position(&file.pp_text, offset) {
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
        let word = match Self::word_at_position(&file.pp_text, offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Use lexer-based token search for accurate symbol matching
        let spans = find_symbol_token_spans(&file.pp_text, file.file_id, file.source_format, word);

        let mut locations = Vec::new();
        for (start, end) in spans {
            // Map preprocessed offsets back to source document positions
            if let (Some(start_pos), Some(end_pos)) = (
                pp_offset_to_source_position(start, file),
                pp_offset_to_source_position(end, file),
            ) {
                locations.push(Location::new(uri.clone(), Range::new(start_pos, end_pos)));
            }
        }

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let pos = params.position;
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
        let word = match Self::word_at_position(&file.pp_text, offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Only allow rename if the word is a known symbol
        let is_symbol = Self::find_data_item(hir, &file.interner, word).is_some()
            || Self::find_paragraph(hir, &file.interner, word).is_some()
            || Self::find_section(hir, &file.interner, word).is_some();

        if !is_symbol {
            return Ok(None);
        }

        // Return the range of the word under cursor, mapped to source positions
        let bytes = file.pp_text.as_bytes();
        let off = offset as usize;
        let mut start = off;
        while start > 0 && is_cobol_name_char(bytes[start - 1]) {
            start -= 1;
        }
        let mut end = off;
        while end < bytes.len() && is_cobol_name_char(bytes[end]) {
            end += 1;
        }

        let start_pos = match pp_offset_to_source_position(start, file) {
            Some(p) => p,
            None => return Ok(None),
        };
        let end_pos = match pp_offset_to_source_position(end, file) {
            Some(p) => p,
            None => return Ok(None),
        };

        Ok(Some(PrepareRenameResponse::Range(Range::new(
            start_pos, end_pos,
        ))))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let new_name = &params.new_name;
        let state = self.state.lock().await;

        let file = match state.files.get(uri) {
            Some(f) => f,
            None => return Ok(None),
        };

        let offset = file.pp_line_index.offset(pos);
        let word = match Self::word_at_position(&file.pp_text, offset) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Use lexer-based token search for accurate symbol matching
        let spans = find_symbol_token_spans(&file.pp_text, file.file_id, file.source_format, word);

        let mut edits = Vec::new();
        for (start, end) in spans {
            // Map preprocessed offsets back to source document positions
            if let (Some(start_pos), Some(end_pos)) = (
                pp_offset_to_source_position(start, file),
                pp_offset_to_source_position(end, file),
            ) {
                edits.push(TextEdit::new(
                    Range::new(start_pos, end_pos),
                    new_name.clone(),
                ));
            }
        }

        if edits.is_empty() {
            return Ok(None);
        }

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), edits);

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let state = self.state.lock().await;

        let file = match state.files.get(uri) {
            Some(f) => f,
            None => return Ok(None),
        };

        // Re-lex the preprocessed text for precise token positions
        let tokens = cobol_lexer::lex(&file.pp_text, file.file_id, file.source_format);

        let mut data = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_col = 0u32;

        for token in &tokens {
            let sem_type = match token_kind_to_semantic_type(token.kind) {
                Some(t) => t,
                None => continue,
            };

            let start_offset: u32 = token.span.range.start().into();
            let end_offset: u32 = token.span.range.end().into();
            let pos = file.pp_line_index.position(start_offset);
            let length = end_offset - start_offset;

            if length == 0 {
                continue;
            }

            let delta_line = pos.line - prev_line;
            let delta_start = if delta_line == 0 {
                pos.character - prev_col
            } else {
                pos.character
            };

            data.push(SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type: sem_type,
                token_modifiers_bitset: 0,
            });

            prev_line = pos.line;
            prev_col = pos.character;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
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

        let mut ranges = Vec::new();

        // Sections
        for sec in &hir.sections {
            let start = file.pp_line_index.position(sec.span.range.start().into());
            let end = file.pp_line_index.position(sec.span.range.end().into());
            if end.line > start.line {
                ranges.push(FoldingRange {
                    start_line: start.line,
                    start_character: Some(start.character),
                    end_line: end.line,
                    end_character: Some(end.character),
                    kind: Some(FoldingRangeKind::Region),
                    collapsed_text: None,
                });
            }
        }

        // Paragraphs
        for para in &hir.paragraphs {
            let start = file.pp_line_index.position(para.span.range.start().into());
            let end = file.pp_line_index.position(para.span.range.end().into());
            if end.line > start.line {
                ranges.push(FoldingRange {
                    start_line: start.line,
                    start_character: Some(start.character),
                    end_line: end.line,
                    end_character: Some(end.character),
                    kind: Some(FoldingRangeKind::Region),
                    collapsed_text: None,
                });
            }
        }

        // Level-01 group items in DATA DIVISION
        for (_, item) in hir.data_items.iter() {
            if item.level == 1 && item.is_group {
                let start = file.pp_line_index.position(item.span.range.start().into());
                let end = file.pp_line_index.position(item.span.range.end().into());
                if end.line > start.line {
                    ranges.push(FoldingRange {
                        start_line: start.line,
                        start_character: Some(start.character),
                        end_line: end.line,
                        end_character: Some(end.character),
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    });
                }
            }
        }

        Ok(Some(ranges))
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

    #[test]
    fn semantic_token_type_mapping() {
        use cobol_lexer::TokenKind;
        // Verbs → keyword (6)
        assert_eq!(token_kind_to_semantic_type(TokenKind::Move), Some(6));
        assert_eq!(token_kind_to_semantic_type(TokenKind::Add), Some(6));
        assert_eq!(token_kind_to_semantic_type(TokenKind::If), Some(6));

        // Division headers → namespace (0)
        assert_eq!(
            token_kind_to_semantic_type(TokenKind::ProcedureDivision),
            Some(0)
        );
        assert_eq!(
            token_kind_to_semantic_type(TokenKind::DataDivision),
            Some(0)
        );

        // Literals
        assert_eq!(
            token_kind_to_semantic_type(TokenKind::StringLiteral),
            Some(4)
        );
        assert_eq!(
            token_kind_to_semantic_type(TokenKind::IntegerLiteral),
            Some(5)
        );

        // Operators
        assert_eq!(token_kind_to_semantic_type(TokenKind::Plus), Some(8));
        assert_eq!(token_kind_to_semantic_type(TokenKind::EqualSign), Some(8));

        // Comments
        assert_eq!(token_kind_to_semantic_type(TokenKind::Comment), Some(7));

        // Word → no semantic type
        assert_eq!(token_kind_to_semantic_type(TokenKind::Word), None);

        // Whitespace → no semantic type
        assert_eq!(token_kind_to_semantic_type(TokenKind::Whitespace), None);
    }

    #[test]
    fn semantic_token_types_length() {
        // Ensure our constant matches the expected count
        assert_eq!(SEMANTIC_TOKEN_TYPES.len(), 9);
    }

    #[test]
    fn detect_source_format_free_directive() {
        let src = ">>SOURCE FORMAT IS FREE\nIDENTIFICATION DIVISION.\n";
        assert!(matches!(
            detect_source_format(src),
            cobol_lexer::SourceFormat::Free
        ));
    }

    #[test]
    fn detect_source_format_fixed_directive() {
        let src = ">>SOURCE FORMAT IS FIXED\n       IDENTIFICATION DIVISION.\n";
        assert!(matches!(
            detect_source_format(src),
            cobol_lexer::SourceFormat::Fixed
        ));
    }

    #[test]
    fn detect_source_format_heuristic_free() {
        // IDENTIFICATION starting at column 0 → free
        let src = "IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\n";
        assert!(matches!(
            detect_source_format(src),
            cobol_lexer::SourceFormat::Free
        ));
    }

    #[test]
    fn detect_source_format_heuristic_fixed() {
        // IDENTIFICATION starting at column 7 → fixed
        let src = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n";
        assert!(matches!(
            detect_source_format(src),
            cobol_lexer::SourceFormat::Fixed
        ));
    }

    #[test]
    fn find_symbol_token_spans_basic() {
        let src = "       MOVE WS-A TO WS-B\n       ADD WS-A TO WS-B\n";
        let fid = FileId::new(0);
        let spans = find_symbol_token_spans(src, fid, cobol_lexer::SourceFormat::Fixed, "WS-A");
        // WS-A should appear exactly twice (once in MOVE, once in ADD)
        assert_eq!(spans.len(), 2);
    }

    #[test]
    fn find_symbol_token_spans_excludes_strings() {
        // WS-A inside a string literal should NOT be found
        let src = "       DISPLAY \"WS-A\" WS-A\n";
        let fid = FileId::new(0);
        let spans = find_symbol_token_spans(src, fid, cobol_lexer::SourceFormat::Fixed, "WS-A");
        // Only the standalone WS-A after the string, not the one inside quotes
        assert_eq!(spans.len(), 1);
    }

    #[test]
    fn find_symbol_token_spans_case_insensitive() {
        let src = "       MOVE ws-a TO WS-A\n";
        let fid = FileId::new(0);
        let spans = find_symbol_token_spans(src, fid, cobol_lexer::SourceFormat::Fixed, "WS-A");
        assert_eq!(spans.len(), 2);
    }

    #[test]
    fn free_format_source_lexes_correctly() {
        // Verify the free-format sample file can be lexed and parsed
        let src = include_str!("../../../tests/smoke/SMOKE-FREE.cob");
        let fid = FileId::new(0);
        let format = detect_source_format(src);
        assert!(matches!(format, cobol_lexer::SourceFormat::Free));

        let vfs = cobol_vfs::Vfs::new();
        let pp = cobol_pp::preprocess(src, fid, &vfs);
        let tokens = cobol_lexer::lex(&pp.text, fid, format);
        let parse_result = cobol_parser::parse(&tokens);
        // Should parse without fatal errors
        assert!(
            parse_result.errors.len() <= 2,
            "too many parse errors in free-format: {:?}",
            parse_result.errors
        );
    }
}
