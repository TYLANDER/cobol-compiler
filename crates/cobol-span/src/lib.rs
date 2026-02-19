// Re-export text-size types so downstream crates need not depend on text-size directly.
pub use text_size::{TextRange, TextSize};

use std::fmt;

// ---------------------------------------------------------------------------
// FileId
// ---------------------------------------------------------------------------

/// Opaque handle that identifies a source file inside the virtual filesystem.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(u32);

impl FileId {
    /// Creates a new `FileId` from a raw index.
    #[inline]
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// Returns the underlying raw index.
    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

impl fmt::Debug for FileId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FileId({})", self.0)
    }
}

// ---------------------------------------------------------------------------
// ExpansionId
// ---------------------------------------------------------------------------

/// Identifies a COPY expansion.
///
/// [`ExpansionId::ROOT`] represents the original, non-expanded source.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ExpansionId(u32);

impl ExpansionId {
    /// The root expansion, i.e. code that has not been COPY-expanded.
    pub const ROOT: ExpansionId = ExpansionId(0);

    /// Creates a new `ExpansionId` from a raw index.
    #[inline]
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// Returns the underlying raw index.
    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

impl fmt::Debug for ExpansionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ExpansionId({})", self.0)
    }
}

// ---------------------------------------------------------------------------
// Span
// ---------------------------------------------------------------------------

/// A 12-byte source span that tracks file, byte range, and expansion context.
///
/// This is the primary location type threaded through every node in the
/// compiler pipeline. It is kept small (12 bytes) so it can be cheaply
/// copied everywhere.
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    /// The file this span belongs to.
    pub file: FileId,
    /// The byte range within the file.
    pub range: TextRange,
    /// Which COPY expansion produced this span (ROOT for original source).
    pub expansion: ExpansionId,
}

impl Span {
    /// Creates a new span.
    #[inline]
    pub const fn new(file: FileId, range: TextRange, expansion: ExpansionId) -> Self {
        Self {
            file,
            range,
            expansion,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Span")
            .field("file", &self.file)
            .field("range", &self.range)
            .field("expansion", &self.expansion)
            .finish()
    }
}

// ---------------------------------------------------------------------------
// ExpansionInfo
// ---------------------------------------------------------------------------

/// Records metadata about a single COPY expansion so that diagnostics can
/// walk back through the expansion chain to show the user where the COPY
/// statement appeared.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpansionInfo {
    /// The span in the *caller* where the `COPY` statement appeared.
    pub call_site: Span,
    /// The file that was COPY-ed in.
    pub file: FileId,
    /// If this expansion is nested inside another COPY, the parent expansion.
    pub parent: Option<ExpansionId>,
}

// ---------------------------------------------------------------------------
// SourceMap
// ---------------------------------------------------------------------------

/// Stores expansion information so that diagnostics can trace the origin
/// of COPY-expanded code back through the full expansion chain.
///
/// Index 0 is reserved for [`ExpansionId::ROOT`] (no expansion), so the
/// first real expansion is at index 1.
#[derive(Debug, Default)]
pub struct SourceMap {
    expansions: Vec<ExpansionInfo>,
}

impl SourceMap {
    /// Creates a new, empty source map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Registers a new expansion and returns its [`ExpansionId`].
    ///
    /// The returned id starts at 1 because 0 is reserved for
    /// [`ExpansionId::ROOT`].
    pub fn add_expansion(&mut self, info: ExpansionInfo) -> ExpansionId {
        self.expansions.push(info);
        // +1 because ROOT is 0 and is never stored in the vec.
        ExpansionId::new(self.expansions.len() as u32)
    }

    /// Retrieves the expansion info for the given id.
    ///
    /// Returns `None` for [`ExpansionId::ROOT`] or an out-of-range id.
    pub fn get(&self, id: ExpansionId) -> Option<&ExpansionInfo> {
        if id.raw() == 0 {
            return None; // ROOT has no expansion info
        }
        self.expansions.get((id.raw() - 1) as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_creation_and_field_access() {
        let file = FileId::new(3);
        let range = TextRange::new(TextSize::from(10), TextSize::from(25));
        let expansion = ExpansionId::ROOT;
        let span = Span::new(file, range, expansion);

        assert_eq!(span.file, FileId::new(3));
        assert_eq!(span.range.start(), TextSize::from(10));
        assert_eq!(span.range.end(), TextSize::from(25));
        assert_eq!(span.expansion, ExpansionId::ROOT);
    }

    #[test]
    fn source_map_add_and_get() {
        let mut map = SourceMap::new();

        let call_site = Span::new(
            FileId::new(0),
            TextRange::new(TextSize::from(100), TextSize::from(120)),
            ExpansionId::ROOT,
        );

        let info = ExpansionInfo {
            call_site,
            file: FileId::new(1),
            parent: None,
        };

        let id = map.add_expansion(info.clone());
        assert_eq!(id.raw(), 1);

        let retrieved = map.get(id).expect("expansion should exist");
        assert_eq!(retrieved, &info);
    }

    #[test]
    fn source_map_root_returns_none() {
        let map = SourceMap::new();
        assert!(map.get(ExpansionId::ROOT).is_none());
    }

    #[test]
    fn expansion_id_root_is_zero() {
        assert_eq!(ExpansionId::ROOT.raw(), 0);
    }

    #[test]
    fn nested_expansions() {
        let mut map = SourceMap::new();

        let first_call = Span::new(
            FileId::new(0),
            TextRange::new(TextSize::from(50), TextSize::from(70)),
            ExpansionId::ROOT,
        );
        let first_id = map.add_expansion(ExpansionInfo {
            call_site: first_call,
            file: FileId::new(1),
            parent: None,
        });

        let second_call = Span::new(
            FileId::new(1),
            TextRange::new(TextSize::from(10), TextSize::from(30)),
            first_id,
        );
        let second_id = map.add_expansion(ExpansionInfo {
            call_site: second_call,
            file: FileId::new(2),
            parent: Some(first_id),
        });

        let info = map.get(second_id).unwrap();
        assert_eq!(info.parent, Some(first_id));
        assert_eq!(info.file, FileId::new(2));
    }

    #[test]
    fn span_size_is_compact() {
        // FileId(u32) + TextRange(2×u32) + ExpansionId(u32) = 16 bytes
        assert_eq!(std::mem::size_of::<Span>(), 16);
    }
}
