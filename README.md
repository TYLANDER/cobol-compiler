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

**Phase 1: Minimal Viable Compiler** — complete

- [x] 16-crate Rust workspace with Cranelift backend
- [x] Pipeline: Lexer → Parser (rowan CST) → HIR → MIR (SSA) → Cranelift codegen → native binary
- [x] 11 initial smoke tests passing

**Phase 2: NIST CCVS Compliance** — complete

- [x] 379/382 NIST CCVS tests passing (1107 sub-tests), 3 skipped
- [x] 30 smoke tests passing
- [x] Nucleus (NC): 300 tests — arithmetic, control flow, tables, INSPECT, OCCURS, INDEXED BY, SEARCH, STRING/UNSTRING
- [x] Sequential I/O (SQ): 25 tests — OPEN, READ, WRITE, CLOSE, multi-file handling
- [x] Source Manipulation (SM): 30 tests — COPY, REPLACING, nested copybooks
- [x] Inter-program Communication (IC): 27 tests — CALL, USING, BY REFERENCE/CONTENT/VALUE, subprograms
- [x] COMP/COMP-3 encoding (binary and packed BCD) with encoding-aware arithmetic
- [x] Level-88 conditions, EVALUATE/ALSO, DIVIDE/REMAINDER, SORT, INITIALIZE

**Phase 3: Extended Language Coverage** — in progress

- [ ] Intrinsic Functions (IF module)
- [ ] SORT/MERGE statements (ST module)
- [ ] Indexed I/O (IX module)
- [ ] Relative I/O (RL module)
- [ ] Regression expected-output baseline for all passing tests

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT License ([LICENSE-MIT](LICENSE-MIT))

at your option.
