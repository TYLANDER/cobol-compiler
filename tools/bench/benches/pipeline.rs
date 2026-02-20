//! Criterion benchmarks for the COBOL compiler pipeline.
//!
//! Run with:  cargo bench -p cobol-bench
//!
//! Benchmarks each stage independently (lex, parse, HIR, MIR) and the
//! full pipeline end-to-end.  Uses a real NIST test file as input for
//! realistic measurements.

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use cobol_ast::AstNode;
use cobol_intern::Interner;
use cobol_span::FileId;

/// Embedded NIST test program — a realistic COBOL file with data items,
/// arithmetic, control flow, and DISPLAY output.
const NIST_SOURCE: &str = include_str!("../../../tests/nist/NC101A.cob");

/// Larger test: use the arithmetic-heavy smoke test.
const ARITH_SOURCE: &str = include_str!("../../../tests/smoke/SMOKE-ARITH.cob");

fn file_id() -> FileId {
    FileId::new(0)
}

fn bench_lex(c: &mut Criterion) {
    let fid = file_id();
    c.bench_function("lex/NC101A", |b| {
        b.iter(|| {
            let tokens = cobol_lexer::lex(
                black_box(NIST_SOURCE),
                fid,
                cobol_lexer::SourceFormat::Fixed,
            );
            black_box(&tokens);
        });
    });

    c.bench_function("lex/SMOKE-ARITH", |b| {
        b.iter(|| {
            let tokens = cobol_lexer::lex(
                black_box(ARITH_SOURCE),
                fid,
                cobol_lexer::SourceFormat::Fixed,
            );
            black_box(&tokens);
        });
    });
}

fn bench_parse(c: &mut Criterion) {
    let fid = file_id();
    let tokens = cobol_lexer::lex(NIST_SOURCE, fid, cobol_lexer::SourceFormat::Fixed);

    c.bench_function("parse/NC101A", |b| {
        b.iter(|| {
            let result = cobol_parser::parse(black_box(&tokens));
            black_box(&result);
        });
    });

    let arith_tokens = cobol_lexer::lex(ARITH_SOURCE, fid, cobol_lexer::SourceFormat::Fixed);
    c.bench_function("parse/SMOKE-ARITH", |b| {
        b.iter(|| {
            let result = cobol_parser::parse(black_box(&arith_tokens));
            black_box(&result);
        });
    });
}

fn bench_hir(c: &mut Criterion) {
    let fid = file_id();
    let tokens = cobol_lexer::lex(NIST_SOURCE, fid, cobol_lexer::SourceFormat::Fixed);
    let parse_result = cobol_parser::parse(&tokens);
    let root = parse_result.syntax();
    let sf = cobol_ast::SourceFile::cast(root).unwrap();

    c.bench_function("hir/NC101A", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            let hir = cobol_hir::lower(black_box(&sf), &mut interner, fid);
            black_box(&hir);
        });
    });
}

fn bench_mir(c: &mut Criterion) {
    let fid = file_id();
    let tokens = cobol_lexer::lex(NIST_SOURCE, fid, cobol_lexer::SourceFormat::Fixed);
    let parse_result = cobol_parser::parse(&tokens);
    let root = parse_result.syntax();
    let sf = cobol_ast::SourceFile::cast(root).unwrap();
    let mut interner = Interner::new();
    let hir = cobol_hir::lower(&sf, &mut interner, fid);

    c.bench_function("mir/NC101A", |b| {
        b.iter(|| {
            let mir = cobol_mir::lower(black_box(&hir), &interner);
            black_box(&mir);
        });
    });
}

fn bench_full_pipeline(c: &mut Criterion) {
    let fid = file_id();

    c.bench_function("full-pipeline/NC101A", |b| {
        b.iter(|| {
            let tokens = cobol_lexer::lex(
                black_box(NIST_SOURCE),
                fid,
                cobol_lexer::SourceFormat::Fixed,
            );
            let parse_result = cobol_parser::parse(&tokens);
            let root = parse_result.syntax();
            let sf = cobol_ast::SourceFile::cast(root).unwrap();
            let mut interner = Interner::new();
            let hir = cobol_hir::lower(&sf, &mut interner, fid);
            let mir = cobol_mir::lower(&hir, &interner);
            black_box(&mir);
        });
    });

    c.bench_function("full-pipeline/SMOKE-ARITH", |b| {
        b.iter(|| {
            let tokens = cobol_lexer::lex(
                black_box(ARITH_SOURCE),
                fid,
                cobol_lexer::SourceFormat::Fixed,
            );
            let parse_result = cobol_parser::parse(&tokens);
            let root = parse_result.syntax();
            let sf = cobol_ast::SourceFile::cast(root).unwrap();
            let mut interner = Interner::new();
            let hir = cobol_hir::lower(&sf, &mut interner, fid);
            let mir = cobol_mir::lower(&hir, &interner);
            black_box(&mir);
        });
    });
}

fn bench_preprocess(c: &mut Criterion) {
    let fid = file_id();
    let vfs = cobol_vfs::Vfs::new();

    c.bench_function("preprocess/NC101A", |b| {
        b.iter(|| {
            let result = cobol_pp::preprocess(black_box(NIST_SOURCE), fid, &vfs);
            black_box(&result);
        });
    });
}

criterion_group!(
    benches,
    bench_lex,
    bench_parse,
    bench_hir,
    bench_mir,
    bench_full_pipeline,
    bench_preprocess,
);
criterion_main!(benches);
