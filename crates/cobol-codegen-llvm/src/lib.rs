use cobol_mir::MirModule;

/// Trait that both LLVM and Cranelift backends implement.
pub trait CodegenBackend {
    /// Compile a MIR module to an object file.
    fn compile(&self, module: &MirModule, output: &std::path::Path) -> Result<(), CodegenError>;

    /// Get the backend name for display purposes.
    fn name(&self) -> &str;

    /// Get the default file extension for output.
    fn output_extension(&self) -> &str;
}

#[derive(Debug)]
pub enum CodegenError {
    /// LLVM/Cranelift internal error.
    BackendError(String),
    /// I/O error writing output.
    IoError(std::io::Error),
    /// Unsupported MIR instruction for this backend.
    UnsupportedInstruction(String),
    /// Type mismatch during codegen.
    TypeMismatch(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::BackendError(msg) => write!(f, "backend error: {}", msg),
            CodegenError::IoError(err) => write!(f, "I/O error: {}", err),
            CodegenError::UnsupportedInstruction(msg) => {
                write!(f, "unsupported instruction: {}", msg)
            }
            CodegenError::TypeMismatch(msg) => write!(f, "type mismatch: {}", msg),
        }
    }
}

impl std::error::Error for CodegenError {}

impl From<std::io::Error> for CodegenError {
    fn from(err: std::io::Error) -> Self {
        CodegenError::IoError(err)
    }
}

/// Optimization level for codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    O0,
    O1,
    O2,
    O3,
    Os,
}

/// LLVM codegen backend.
///
/// Decimal strategy:
/// - Small decimals (<=18 digits): i64 scaled integers (single add instruction)
/// - Medium decimals (19-38 digits): i128
/// - Large decimals (39+ digits): runtime BCD calls
///
/// This fast-path covers 90%+ of real-world COBOL.
pub struct LlvmBackend {
    opt_level: OptLevel,
}

impl LlvmBackend {
    pub fn new(opt_level: OptLevel) -> Self {
        Self { opt_level }
    }

    pub fn opt_level(&self) -> OptLevel {
        self.opt_level
    }
}

impl CodegenBackend for LlvmBackend {
    fn compile(&self, _module: &MirModule, _output: &std::path::Path) -> Result<(), CodegenError> {
        // TODO: implement LLVM codegen via inkwell
        Err(CodegenError::BackendError(
            "LLVM backend not yet implemented".to_string(),
        ))
    }

    fn name(&self) -> &str {
        "llvm"
    }

    fn output_extension(&self) -> &str {
        "o"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn llvm_backend_new_and_name() {
        let backend = LlvmBackend::new(OptLevel::O2);
        assert_eq!(backend.name(), "llvm");
        assert_eq!(backend.opt_level(), OptLevel::O2);
        assert_eq!(backend.output_extension(), "o");
    }

    #[test]
    fn llvm_backend_compile_returns_not_implemented() {
        let backend = LlvmBackend::new(OptLevel::O0);
        let module = MirModule::default();
        let result = backend.compile(&module, std::path::Path::new("out.o"));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            matches!(err, CodegenError::BackendError(_)),
            "expected BackendError, got: {:?}",
            err,
        );
    }

    #[test]
    fn codegen_error_display() {
        let err = CodegenError::BackendError("test".to_string());
        assert_eq!(format!("{}", err), "backend error: test");

        let err = CodegenError::UnsupportedInstruction("foo".to_string());
        assert_eq!(format!("{}", err), "unsupported instruction: foo");

        let err = CodegenError::TypeMismatch("bar".to_string());
        assert_eq!(format!("{}", err), "type mismatch: bar");
    }

    #[test]
    fn codegen_error_from_io_error() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "not found");
        let err: CodegenError = io_err.into();
        assert!(matches!(err, CodegenError::IoError(_)));
        assert!(format!("{}", err).contains("not found"));
    }

    #[test]
    fn opt_level_equality() {
        assert_eq!(OptLevel::O0, OptLevel::O0);
        assert_ne!(OptLevel::O0, OptLevel::O3);
        assert_eq!(OptLevel::Os, OptLevel::Os);
    }
}
