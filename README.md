# cobolc — The World's Best Open-Source COBOL Compiler

A modern, Rust-based COBOL compiler targeting LLVM and Cranelift, with first-class IDE support via LSP.

## Why?

There are ~250 billion lines of COBOL in production processing $3 trillion in daily commerce. The open-source tooling hasn't kept up. `cobolc` aims to change that.

## Goals

- **5-10x faster** runtime performance vs GnuCOBOL (direct LLVM codegen, no C transpilation)
- **Industry-leading error messages** (inspired by Rust's compiler diagnostics)
- **First-class IDE support** via Language Server Protocol
- **COBOL-85 compliance** with extensions for IBM, MicroFocus, and GnuCOBOL dialects
- **Dual backend**: LLVM for optimized release builds, Cranelift for fast debug builds

## Building

```bash
# Prerequisites: Rust 1.75+
cargo build --workspace

# Run the compiler
cargo run --bin cobolc -- hello.cob

# Run tests
cargo test --workspace
```

## Usage

```bash
# Compile a COBOL program
cobolc hello.cob -o hello

# Specify backend and optimization
cobolc program.cob --backend=llvm -O2

# Dump intermediate representations
cobolc program.cob --emit=tokens    # Show tokenized output
cobolc program.cob --emit=ast       # Show parse tree
cobolc program.cob --emit=hir       # Show high-level IR
cobolc program.cob --emit=mir       # Show mid-level IR
cobolc program.cob --emit=llvm-ir   # Show LLVM IR

# Source format and copybooks
cobolc program.cob --format=free -I copybooks/
```

## Architecture

```
Source → Preprocessor → Lexer → Parser → AST → HIR → MIR → LLVM/Cranelift → Binary
```

16 crates in a Cargo workspace:

| Layer | Crates |
|-------|--------|
| **Foundation** | `cobol-intern`, `cobol-span`, `cobol-vfs` |
| **Frontend** | `cobol-pp`, `cobol-lexer`, `cobol-parser`, `cobol-ast` |
| **Semantic** | `cobol-hir`, `cobol-mir` |
| **Backend** | `cobol-codegen-llvm`, `cobol-codegen-cranelift` |
| **Runtime** | `cobol-runtime` |
| **Infrastructure** | `cobol-diag`, `cobol-db`, `cobol-lsp`, `cobol-driver` |

## Status

**Phase 1: Minimal Viable Compiler** (in progress)

- [x] Project scaffold and interface contracts
- [ ] Lexer (fixed-format)
- [ ] Parser (core divisions)
- [ ] Basic HIR and MIR
- [ ] LLVM codegen (Hello World)
- [ ] Runtime (DISPLAY, basic decimal)
- [ ] Smoke tests passing

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT License ([LICENSE-MIT](LICENSE-MIT))

at your option.
