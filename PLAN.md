# COBOL Compiler — Project Plan

## Context

There are ~250 billion lines of COBOL in production processing $3 trillion in daily commerce, yet the open-source tooling is stuck in the 1990s. GnuCOBOL — the only viable open-source compiler — transpiles COBOL to C, then compiles with GCC/Clang. This two-stage approach produces poor error messages, hampers debugging, limits optimization, and makes IDE integration an afterthought. Commercial alternatives (IBM Enterprise COBOL, Rocket Visual COBOL) are prohibitively expensive and platform-locked.

We will build **the world's best open-source COBOL compiler** — a modern, Rust-based compiler targeting LLVM and Cranelift, with first-class IDE support via LSP, COBOL-specific optimizations, and industry-leading error messages. The goal is to beat GnuCOBOL on every axis: runtime performance (5-10x via direct LLVM codegen), compile speed, error quality, and developer experience.

---

## Competitive Landscape

| Compiler | Approach | Strengths | Weaknesses |
|----------|----------|-----------|------------|
| **GnuCOBOL** | COBOL → C → GCC/Clang | 99.79% NIST compliance, broad platform support, 37K+ tests | C-level errors, two-stage overhead, no real IDE support |
| **GCC COBOL (gcobol)** | GCC frontend | GCC ecosystem | Early development, GCC complexity |
| **Otterkit** | COBOL → C# .NET | Targets COBOL 2023 | Alpha stage, .NET dependency |
| **IBM Enterprise COBOL** | Native z/OS | 17x faster than GnuCOBOL, mature | z/OS only, expensive |
| **Rocket Visual COBOL** | Proprietary | Good IDE, cross-platform | Expensive licensing |

**Our edge**: Direct LLVM codegen (no transpilation), Rust implementation (memory safety, modern contributor base), dual-backend strategy (LLVM for release, Cranelift for fast debug), shared frontend for compiler + LSP, and COBOL-specific IR optimizations for decimal arithmetic and PERFORM loops.

---

## Architecture Overview

```
                        COBOL Source (.cob, .cbl)
                                  │
                                  ▼
                 ┌────────────────────────────────┐
                 │        cobol-driver (CLI)       │
                 │            cobolc               │
                 └────────────────────────────────┘
                                  │
          ┌───────────┬───────────┼───────────┬───────────┐
          ▼           ▼           ▼           ▼           ▼
     cobol-vfs   cobol-pp    cobol-lexer  cobol-lsp  cobol-diag
     (virtual    (COPY/      (fixed+free  (LSP       (rich error
      files)     REPLACE)     format)      server)    messages)
                     │           │
                     ▼           ▼
                     └─────┬─────┘
                           ▼
                ┌─────────────────────┐
                │    cobol-parser     │
                │  (CST via rowan)    │
                └─────────────────────┘
                           │
                           ▼
                ┌─────────────────────┐
                │     cobol-ast       │
                │  (typed CST view)   │
                └─────────────────────┘
                           │
                           ▼
                ┌─────────────────────┐
                │     cobol-hir       │
                │  (name resolution,  │
                │   PIC types, data   │
                │   layout, scopes)   │
                └─────────────────────┘
                           │
                           ▼
                ┌─────────────────────┐
                │     cobol-mir       │
                │  (SSA IR, basic     │
                │   blocks, decimal   │
                │   ops, optimizations│
                └─────────────────────┘
                           │
              ┌────────────┴────────────┐
              ▼                         ▼
   ┌──────────────────┐     ┌──────────────────┐
   │ cobol-codegen-   │     │ cobol-codegen-   │
   │ llvm             │     │ cranelift        │
   │ (optimized       │     │ (fast debug      │
   │  release builds) │     │  builds)         │
   └──────────────────┘     └──────────────────┘
              │                         │
              └────────────┬────────────┘
                           ▼
                ┌─────────────────────┐
                │   cobol-runtime     │
                │  (libcobolrt.a)     │
                │  decimal, file I/O, │
                │  SORT, intrinsics   │
                └─────────────────────┘
```

All connected via **cobol-db** (Salsa-based incremental computation database) — the architectural linchpin enabling both batch compilation (CLI) and incremental IDE analysis (LSP) from the same codebase.

---

## Crate Map (16 crates in a Cargo workspace)

| Crate | Responsibility | Key Design Decisions |
|-------|---------------|---------------------|
| `cobol-intern` | Case-insensitive string interning | All COBOL names normalized to uppercase at intern time; O(1) comparisons everywhere downstream |
| `cobol-span` | Source locations across COPY expansions | 12-byte `Span` struct with `ExpansionId` indirection for nested COPY chains |
| `cobol-vfs` | Virtual filesystem for CLI + LSP | Copybook resolution happens here (knows `-I` paths); preprocessor is a pure function |
| `cobol-pp` | COPY/REPLACE preprocessing | **Separate pass before lexing** — two-stage (Stage 1: COPY+REPLACING, Stage 2: REPLACE). Required because REPLACING operates on partial tokens (LEADING/TRAILING) |
| `cobol-lexer` | Tokenization (fixed + free format) | **Two-layer design**: Layer 1 (LineNormalizer) handles format-specific columns/continuation/comments → produces format-agnostic LogicalLines. Layer 2 (Tokenizer) operates identically on both formats. **Contextual keywords**: lexer emits `Word`; parser promotes to keyword by context (handles legacy code using reserved words as data names) |
| `cobol-parser` | Recursive-descent parser → lossless CST | Uses **rowan** (rust-analyzer's tree library). Parsing never fails — always produces a tree with error nodes. **Period-based error recovery**: skip to next `.` on failure. Trivia tokens (whitespace, comments) preserved for LSP formatting |
| `cobol-ast` | Typed view over CST | Zero-cost newtype wrappers around `SyntaxNode` with accessor methods (rust-analyzer pattern). No memory overhead |
| `cobol-hir` | Semantic analysis core | **PIC types**: raw PIC string → `PictureType` (category, size, scale, sign) + `Usage` → `StorageDescriptor` (exact bytes, encoding). **Hierarchical data**: tree of `DataItem` (Group/Elementary) in arena with computed offsets. **PERFORM THRU**: recorded as label range, resolved via `label_order` |
| `cobol-mir` | SSA-based IR with COBOL-specific ops | Dedicated instructions for `DecimalAdd/Sub/Mul/Div`, `PackedDecimal` conversions, `FileRead/Write`, `PerformPush/Pop`. **PERFORM THRU** → explicit perform stack (push return addr, jump to range, fall through paragraphs, pop and return). **ALTER** → indirect jump through mutable target. **GO TO DEPENDING** → switch/jump-table |
| `cobol-codegen-llvm` | LLVM backend via `inkwell` | **Decimal strategy**: small decimals (≤18 digits) → i64 scaled integers (single `add` instruction); medium (19-38) → i128; large (39+) → runtime BCD calls. This fast-path covers 90%+ of real-world COBOL |
| `cobol-codegen-cranelift` | Fast backend for debug/IDE | Same `MirModule` input as LLVM. Trades optimization for 20% faster codegen. Both implement `CodegenBackend` trait |
| `cobol-runtime` | Runtime library (libcobolrt) | `extern "C"` ABI, `#[no_mangle]` — callable from both backends. Modules: `decimal` (BCD arithmetic), `file_io` (sequential/relative/indexed via VBISAM), `sort`, `intrinsics` (87 functions), `perform` (stack management) |
| `cobol-diag` | Diagnostics engine | Rich errors with COPY expansion chains ("in expansion of COPY FOO"). Renders to terminal (via `ariadne`/`miette`) or LSP `Diagnostic` format |
| `cobol-db` | Salsa incremental database | Input queries (file text, config) → derived queries (preprocessed → tokens → CST → AST → HIR → MIR → diagnostics). CLI: set once, run all. LSP: update incrementally, re-derive only invalidated queries |
| `cobol-lsp` | Language Server Protocol | Uses `tower-lsp`. Full LSP feature set: completion, hover, go-to-def, references, rename, formatting, semantic tokens, code actions, code lens. Uses Cranelift for "run" actions |
| `cobol-driver` | `cobolc` CLI binary | `--backend=llvm|cranelift`, `--dialect=cobol85|ibm|microfocus|gnucobol`, `--emit=exe|obj|llvm-ir|mir|hir|ast|tokens|preprocessed`, `-O0/-O1/-O2/-O3/-Os`, `-I` copybook paths, `--free`/`--fixed` |

---

## Lowering Pipeline

```
Source → [cobol-pp] → Expanded Text → [cobol-lexer] → Tokens → [cobol-parser] → CST
  → [cobol-ast] → Typed AST → [cobol-hir] → HIR → [cobol-mir] → MIR → [codegen] → Binary
```

| Transition | What Happens |
|-----------|-------------|
| Source → CST | Format normalization, error-tolerant parsing, trivia preservation |
| CST → AST | Zero-cost typed wrappers (no data copying) |
| AST → HIR | Name resolution, PIC interpretation, data layout computation, PERFORM range resolution, type checking |
| HIR → MIR | Decompose MOVE into load/convert/store, expand PERFORM THRU into basic blocks, generate decimal instructions, lay out file descriptors |
| MIR → Backend | Map MIR types to machine types, emit runtime calls, apply backend optimizations, generate debug info |

---

## Testing & Benchmarking Strategy

### Smoke Tests (< 30 seconds, every PR)

| Test | Validates | Pass Criteria |
|------|-----------|---------------|
| `HELLO-WORLD.cob` | Full pipeline, DISPLAY, STOP RUN | stdout = `HELLO, WORLD!` |
| `SMOKE-ARITH.cob` | WORKING-STORAGE, PIC, ADD/GIVING | stdout = `00006912` |
| `SMOKE-STRING.cob` | STRING verb, POINTER phrase | stdout = `HELLO COBOL` |
| `SMOKE-IF.cob` | PERFORM VARYING, IF/ELSE | stdout = `PASS` (sum 1..10 = 55) |
| `SMOKE-DECIMAL.cob` | Decimal precision, V, ROUNDED, MULTIPLY | stdout = `00833.33` |
| `SMOKE-FILE.cob` | Sequential file I/O cycle | stdout = `PASS` |
| `SMOKE-CALL.cob` | Inter-program CALL, parameter passing | stdout = `PASS` |
| Error cases | Diagnostic quality | Non-zero exit, stderr has line:col |

### NIST CCVS Compliance (8,846 tests)

Phased approach with known-failure tracking (`known_failures.toml`):

| Phase | Modules | Tests | Target Pass Rate | Gate |
|-------|---------|-------|------------------|------|
| 1 (Core) | NC + SM + IC | ~4,911 | 95% | Alpha release |
| 2 (File I/O) | SQ + RL + IX + ST | ~3,150 | 95% | Beta release |
| 3 (Intrinsics) | IF | ~735 | 98% | Beta release |
| 4 (Advanced) | RW + SG | ~250 | Match GnuCOBOL | 1.0 release |
| **Overall** | **All** | **~8,846** | **≥ 99.5%** | **1.0 release** |

PR gate: no regressions (any previously-passing test that now fails blocks merge).

### Performance Benchmarks

| Benchmark | What It Measures | Target vs GnuCOBOL |
|-----------|-----------------|---------------------|
| **N-Queens (N=13)** | CPU-bound computation, loops, arrays | **5-10x faster** |
| **Telco** | Decimal arithmetic precision + speed | Faster + exact checksums |
| **File I/O (1M records)** | Sequential read/write throughput | **3-5x faster** |
| **SORT (1M records)** | Sort performance | < 10 seconds |
| **String Ops (100K records)** | INSPECT/STRING/UNSTRING throughput | **2-3x faster** |
| **COMP-3 vs BINARY** | Packed decimal vs binary arithmetic | Narrow the gap |
| **CALL overhead** | Inter-program call latency | < 100ns per call |
| **Compile time (10K lines)** | Compiler speed | < 1 second |

### Additional Testing Layers

- **Property-based testing** (proptest): Decimal arithmetic commutativity, PIC size consistency, MOVE truncation/padding rules
- **Differential testing**: Compile + run same programs with both our compiler and GnuCOBOL, compare outputs (nightly)
- **Fuzz testing** (cargo-fuzz): 4 targets — lexer (raw bytes), parser (structured), full pipeline, decimal engine. No panics, no segfaults
- **Real-world validation**: Test against open-source COBOL repos (Martinfx/Cobol, writ3it/cobol-examples, neopragma/cobol-samples, IBM/cobol-is-fun)
- **Snapshot testing** (insta): AST snapshots, error message golden tests

### CI/CD

| Tier | Trigger | Time | Contents |
|------|---------|------|----------|
| **PR Validation** | Every push | ~3 min | fmt + clippy + unit tests + smoke tests + NIST regression check |
| **Nightly** | 2:00 AM UTC | ~60 min | Full NIST suite + differential testing + all benchmarks + 30-min fuzz per target + real-world repos |
| **Release** | Tag push | ~120 min | Full NIST (no tolerance) + extended benchmarks (30 runs) + 4-hour fuzz + cross-platform + release gates |

---

## Implementation Phases

### Phase 1: Minimal Viable Compiler — COMPLETE
**Goal**: Compile and run "Hello World" and simple arithmetic

- [x] Foundation crates: `cobol-intern`, `cobol-span`, `cobol-vfs`
- [x] `cobol-lexer` (fixed-format only)
- [x] `cobol-parser` (IDENTIFICATION + basic DATA + simple PROCEDURE division)
- [x] `cobol-ast`, minimal `cobol-hir` (WORKING-STORAGE, DISPLAY, MOVE, ADD, STOP RUN)
- [x] Minimal `cobol-mir`, `cobol-codegen-cranelift` (basic codegen)
- [x] Minimal `cobol-runtime` (DISPLAY, basic decimal)
- [x] 11 smoke tests passing
- **Deliverable**: `cobolc hello.cob` produces a running binary

### Phase 2: Core Language — COMPLETE
**Goal**: Pass 95%+ of NIST Nucleus tests

- [x] `cobol-pp` (COPY/REPLACE)
- [x] Full DATA DIVISION (all levels, REDEFINES, OCCURS, all USAGE types)
- [x] All arithmetic (ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE with ON SIZE ERROR)
- [x] All control flow (PERFORM all variants, IF, EVALUATE, GO TO, ALTER)
- [x] String operations (STRING, UNSTRING, INSPECT)
- [x] COMP/COMP-3 encoding (binary and packed BCD)
- [x] Level-88 conditions, EVALUATE/ALSO, DIVIDE/REMAINDER
- [x] 30 smoke tests passing
- [x] 379/382 NIST tests passing (1107 sub-tests), NC + SM + IC + SQ modules
- **Deliverable**: Alpha release

### Phase 3: File I/O & Inter-Program Communication — COMPLETE
**Goal**: Pass 95%+ of all File I/O NIST tests

- [x] Sequential file I/O (SQ module — 25 tests)
- [x] SORT/MERGE statements (ST module — 8 tests)
- [x] CALL/CANCEL (IC module — 27 tests, CALL RETURNING, ON EXCEPTION)
- [x] Intrinsic functions — 14 implemented (LENGTH, UPPER-CASE, LOWER-CASE, REVERSE, TRIM, MAX, MIN, ORD, MOD, ABS, INTEGER, NUMVAL, CURRENT-DATE, nested functions)
- [x] IF module — 15 tests
- [x] 418/421 NIST tests passing (3 SM skips), 31 smoke tests
- [x] Clippy clean (0 warnings), rustfmt clean
- [x] Relative I/O (RL module — 8 tests)
- [x] Indexed I/O (IX module — 8 tests)
- [x] Free-format source support (already implemented in lexer; `--format free` CLI flag; verified with SMOKE-FREE test)
- [ ] Report Writer (basic) — deferred to Phase 5; rarely used in modern COBOL
- **Deliverable**: Beta release

### Phase 4: IDE & Fast Backend
**Goal**: Usable IDE experience

- [ ] `cobol-db` (Salsa integration for incrementality)
- [ ] `cobol-lsp` (diagnostics, completion, go-to-def, hover, rename, formatting)
- [ ] `cobol-codegen-llvm` (optimized release builds — currently Cranelift only)
- [ ] Performance optimization passes (PERFORM inlining, decimal strength reduction)
- [ ] Benchmark suite fully operational

### Phase 5: Production Readiness
**Goal**: Ship 1.0

- [ ] Dialect support (IBM, MicroFocus, GnuCOBOL extensions)
- [ ] Nested programs, OOP (COBOL 2002+)
- [ ] Whole-program optimization (LTO, dead paragraph elimination)
- [ ] Differential testing fully green
- [ ] NIST overall ≥ 99.5%
- [ ] Real-world repos ≥ 95% compilation rate
- [ ] Faster than GnuCOBOL on 6/8 benchmarks
- **Deliverable**: 1.0 release

---

## Verification Plan

1. **After Phase 1**: Run `cobolc` on HELLO-WORLD.cob — produces binary, prints "HELLO, WORLD!", exits 0
2. **After Phase 2**: Run NIST NC module — ≥95% pass rate; all 8 smoke tests green
3. **After Phase 3**: Run full NIST suite — ≥90% overall; file I/O benchmarks operational
4. **After Phase 4**: Open a .cob file in VS Code with our LSP — diagnostics, completion, go-to-def all work; Cranelift backend produces correct output for all smoke tests
5. **After Phase 5**: Full benchmark suite shows faster-than-GnuCOBOL on majority of benchmarks; NIST ≥99.5%; all fuzz targets clean; release gates pass
