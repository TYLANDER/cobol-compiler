use cobol_span::FileId;
use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// VfsError
// ---------------------------------------------------------------------------

/// Errors that can occur when loading files through the virtual filesystem.
#[derive(Debug, Clone)]
pub enum VfsError {
    /// The requested file was not found on disk.
    NotFound(PathBuf),
    /// An I/O error occurred while reading the file.
    IoError(String),
    /// The file could not be decoded as valid UTF-8.
    Encoding(String),
}

impl fmt::Display for VfsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VfsError::NotFound(path) => write!(f, "file not found: {}", path.display()),
            VfsError::IoError(msg) => write!(f, "I/O error: {}", msg),
            VfsError::Encoding(msg) => write!(f, "encoding error: {}", msg),
        }
    }
}

impl std::error::Error for VfsError {}

// ---------------------------------------------------------------------------
// FileLoader trait
// ---------------------------------------------------------------------------

/// Trait for loading source files and resolving COPY book names.
pub trait FileLoader {
    /// Loads the contents of the file at `path`.
    fn load(&self, path: &Path) -> Result<String, VfsError>;

    /// Resolves a COPY book name (e.g. `"DATEUTIL"`) to a filesystem path.
    fn resolve_copybook(&self, name: &str) -> Result<PathBuf, VfsError>;
}

// ---------------------------------------------------------------------------
// FileEntry (internal)
// ---------------------------------------------------------------------------

/// Internal storage for a single file.
#[derive(Debug, Clone)]
struct FileEntry {
    path: PathBuf,
    content: String,
}

// ---------------------------------------------------------------------------
// Vfs
// ---------------------------------------------------------------------------

/// An in-memory virtual filesystem that serves both the CLI compiler and
/// the LSP server.
///
/// In CLI mode, files are loaded from disk on demand. In LSP mode, the
/// editor can overlay file contents with [`Vfs::set_file_content`] so that
/// unsaved edits are visible to the compiler.
#[derive(Debug, Default)]
pub struct Vfs {
    /// Maps `FileId` to the stored file entry.
    files: HashMap<u32, FileEntry>,
    /// Reverse lookup: canonical path to `FileId`.
    path_to_id: HashMap<PathBuf, FileId>,
    /// Counter for allocating the next `FileId`.
    next_id: u32,
    /// Directories to search when resolving COPY book names (the `-I` flag).
    copybook_dirs: Vec<PathBuf>,
}

impl Vfs {
    /// Creates a new, empty virtual filesystem.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a directory to the COPY book search path.
    pub fn add_copybook_dir(&mut self, dir: PathBuf) {
        self.copybook_dirs.push(dir);
    }

    /// Loads a file from disk into the VFS, returning its [`FileId`].
    ///
    /// If the file has already been loaded (based on its canonical path),
    /// the existing [`FileId`] is returned without re-reading.
    pub fn load_file(&mut self, path: &Path) -> Result<FileId, VfsError> {
        // Canonicalize if the file exists; otherwise use the path as-is for
        // a cleaner error message.
        let canonical = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());

        if let Some(&id) = self.path_to_id.get(&canonical) {
            return Ok(id);
        }

        let content = std::fs::read_to_string(path).map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                VfsError::NotFound(path.to_path_buf())
            } else if e.kind() == std::io::ErrorKind::InvalidData {
                VfsError::Encoding(format!("{}: {}", path.display(), e))
            } else {
                VfsError::IoError(format!("{}: {}", path.display(), e))
            }
        })?;

        let id = self.alloc_id();
        self.files.insert(
            id.raw(),
            FileEntry {
                path: canonical.clone(),
                content,
            },
        );
        self.path_to_id.insert(canonical, id);
        Ok(id)
    }

    /// Sets (or overwrites) the content for a file at the given path.
    ///
    /// This is the primary entry point for LSP overlay: the editor sends
    /// `textDocument/didOpen` or `textDocument/didChange` and we store the
    /// latest content here without touching disk.
    pub fn set_file_content(&mut self, path: PathBuf, content: String) -> FileId {
        if let Some(&id) = self.path_to_id.get(&path) {
            // Update existing entry in place.
            if let Some(entry) = self.files.get_mut(&id.raw()) {
                entry.content = content;
            }
            return id;
        }

        let id = self.alloc_id();
        self.path_to_id.insert(path.clone(), id);
        self.files.insert(id.raw(), FileEntry { path, content });
        id
    }

    /// Returns the content of a previously loaded file, or `None` if the
    /// `FileId` is unknown.
    pub fn file_content(&self, id: FileId) -> Option<&str> {
        self.files.get(&id.raw()).map(|e| e.content.as_str())
    }

    /// Returns the path of a previously loaded file, or `None` if the
    /// `FileId` is unknown.
    pub fn file_path(&self, id: FileId) -> Option<&Path> {
        self.files.get(&id.raw()).map(|e| e.path.as_path())
    }

    /// Allocates the next [`FileId`] and bumps the counter.
    fn alloc_id(&mut self) -> FileId {
        let id = FileId::new(self.next_id);
        self.next_id += 1;
        id
    }
}

// ---------------------------------------------------------------------------
// FileLoader impl for Vfs
// ---------------------------------------------------------------------------

impl FileLoader for Vfs {
    fn load(&self, path: &Path) -> Result<String, VfsError> {
        // First check if we already have it in memory.
        let canonical = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        if let Some(&id) = self.path_to_id.get(&canonical) {
            if let Some(entry) = self.files.get(&id.raw()) {
                return Ok(entry.content.clone());
            }
        }

        // Fall back to reading from disk.
        std::fs::read_to_string(path).map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                VfsError::NotFound(path.to_path_buf())
            } else if e.kind() == std::io::ErrorKind::InvalidData {
                VfsError::Encoding(format!("{}: {}", path.display(), e))
            } else {
                VfsError::IoError(format!("{}: {}", path.display(), e))
            }
        })
    }

    fn resolve_copybook(&self, name: &str) -> Result<PathBuf, VfsError> {
        // Common COBOL copybook extensions to try.
        let extensions = ["", ".cpy", ".CPY", ".cbl", ".CBL", ".cob", ".COB"];

        for dir in &self.copybook_dirs {
            for ext in &extensions {
                let candidate = dir.join(format!("{}{}", name, ext));
                if candidate.exists() {
                    return Ok(candidate);
                }
            }
        }

        VfsError::NotFound(PathBuf::from(name)).into_result()
    }
}

// Small helper so we can write `VfsError::NotFound(...).into_result()`.
impl VfsError {
    fn into_result<T>(self) -> Result<T, VfsError> {
        Err(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_and_retrieve_content() {
        let mut vfs = Vfs::new();
        let path = PathBuf::from("/tmp/test_cobol_vfs/HELLO.cbl");
        let content = "       IDENTIFICATION DIVISION.\n".to_string();

        let id = vfs.set_file_content(path.clone(), content.clone());
        assert_eq!(vfs.file_content(id), Some(content.as_str()));
    }

    #[test]
    fn file_path_returns_correct_path() {
        let mut vfs = Vfs::new();
        let path = PathBuf::from("/tmp/test_cobol_vfs/WORLD.cbl");
        let id = vfs.set_file_content(path.clone(), "content".to_string());
        assert_eq!(vfs.file_path(id), Some(path.as_path()));
    }

    #[test]
    fn load_file_not_found() {
        let mut vfs = Vfs::new();
        let result = vfs.load_file(Path::new("/nonexistent/path/to/file.cbl"));
        assert!(result.is_err());
        match result.unwrap_err() {
            VfsError::NotFound(p) => {
                assert_eq!(p, PathBuf::from("/nonexistent/path/to/file.cbl"));
            }
            other => panic!("expected NotFound, got: {:?}", other),
        }
    }

    #[test]
    fn set_file_content_overwrites() {
        let mut vfs = Vfs::new();
        let path = PathBuf::from("/tmp/test_cobol_vfs/UPDATE.cbl");
        let id1 = vfs.set_file_content(path.clone(), "v1".to_string());
        let id2 = vfs.set_file_content(path.clone(), "v2".to_string());
        // Same file, same id.
        assert_eq!(id1, id2);
        assert_eq!(vfs.file_content(id1), Some("v2"));
    }

    #[test]
    fn unknown_file_id_returns_none() {
        let vfs = Vfs::new();
        let bogus = FileId::new(999);
        assert!(vfs.file_content(bogus).is_none());
        assert!(vfs.file_path(bogus).is_none());
    }

    #[test]
    fn vfs_error_display() {
        let err = VfsError::NotFound(PathBuf::from("missing.cbl"));
        assert_eq!(err.to_string(), "file not found: missing.cbl");

        let err = VfsError::IoError("permission denied".into());
        assert_eq!(err.to_string(), "I/O error: permission denied");

        let err = VfsError::Encoding("invalid utf-8".into());
        assert_eq!(err.to_string(), "encoding error: invalid utf-8");
    }
}
