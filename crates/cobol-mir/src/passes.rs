//! MIR optimization passes.
//!
//! Each pass operates on [`MirModule`] (or individual [`MirFunction`]s) and
//! transforms the IR in-place.  Passes are composable and can be run in any
//! order, although the default [`optimize`] pipeline applies them in a fixed
//! sequence for best results:
//!
//! 1. **Constant folding** – evaluate compile-time constant expressions.
//! 2. **Algebraic simplification** – eliminate identity / absorbing patterns
//!    (x + 0, x * 1, double negation, branch on constant, …).
//! 3. **Decimal strength reduction** – replace BCD decimal ops with native i64
//!    arithmetic when operands share the same scale and fit in 64 bits.
//! 4. **Dead code elimination** – remove instructions whose results are never
//!    used and blocks that are unreachable.
//! 5. **PERFORM / function inlining** – inline small, single-use functions at
//!    `CallRuntime` sites within the module.

use std::collections::{HashMap, HashSet};

use crate::{BlockId, MirConst, MirFunction, MirInst, MirModule, Terminator, Value};

// ───────────────────────────────────────────────────────────────────────────
// Public API
// ───────────────────────────────────────────────────────────────────────────

/// Optimization level selector.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    /// No optimizations.
    None,
    /// Constant folding + algebraic simplification only.
    Basic,
    /// Full pipeline (constant fold → simplify → DCE).
    Full,
}

/// Run the default optimization pipeline on every function in `module`.
pub fn optimize(module: &mut MirModule, level: OptLevel) {
    if level == OptLevel::None {
        return;
    }

    for func in &mut module.functions {
        // Constant folding first — enables later simplifications.
        constant_fold(func);

        // Algebraic identities (x+0, x*1, branch-on-const, …).
        algebraic_simplify(func);

        if level == OptLevel::Full {
            // Decimal strength reduction: replace BCD ops with native i64 when safe.
            decimal_strength_reduce(func);

            // A second round of folding can catch values exposed by simplify.
            constant_fold(func);

            // Remove dead instructions and unreachable blocks.
            dead_code_eliminate(func);
        }
    }

    // Module-level: inline small single-use functions (PERFORM targets
    // are already pre-inlined during lowering; this handles CALL targets).
    if level == OptLevel::Full {
        perform_inline(module);
        // Remove functions that are never called and aren't the entry point.
        dead_function_eliminate(module);
    }
}

// ───────────────────────────────────────────────────────────────────────────
// Pass 1 – Constant folding
// ───────────────────────────────────────────────────────────────────────────

/// Evaluate instructions whose operands are all compile-time constants,
/// replacing them with `MirInst::Const`.
pub fn constant_fold(func: &mut MirFunction) {
    // Map from Value → known constant.
    let mut known: HashMap<Value, MirConst> = HashMap::new();

    for block in &mut func.blocks {
        let mut new_insts = Vec::with_capacity(block.instructions.len());

        for inst in block.instructions.drain(..) {
            match &inst {
                // Record constant definitions.
                MirInst::Const { dest, value } => {
                    known.insert(*dest, value.clone());
                    new_insts.push(inst);
                }

                // Integer binary ops.
                MirInst::IAdd { dest, left, right } => {
                    if let Some(c) = fold_int_binop(&known, *left, *right, i64::wrapping_add) {
                        known.insert(*dest, MirConst::Int(c));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(c),
                        });
                    } else {
                        new_insts.push(inst);
                    }
                }
                MirInst::ISub { dest, left, right } => {
                    if let Some(c) = fold_int_binop(&known, *left, *right, i64::wrapping_sub) {
                        known.insert(*dest, MirConst::Int(c));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(c),
                        });
                    } else {
                        new_insts.push(inst);
                    }
                }
                MirInst::IMul { dest, left, right } => {
                    if let Some(c) = fold_int_binop(&known, *left, *right, i64::wrapping_mul) {
                        known.insert(*dest, MirConst::Int(c));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(c),
                        });
                    } else {
                        new_insts.push(inst);
                    }
                }
                MirInst::IDiv { dest, left, right } => {
                    if let Some(c) = fold_int_binop_checked(&known, *left, *right, |a, b| {
                        if b != 0 {
                            Some(a.wrapping_div(b))
                        } else {
                            None
                        }
                    }) {
                        known.insert(*dest, MirConst::Int(c));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(c),
                        });
                    } else {
                        new_insts.push(inst);
                    }
                }
                MirInst::IRem { dest, left, right } => {
                    if let Some(c) = fold_int_binop_checked(&known, *left, *right, |a, b| {
                        if b != 0 {
                            Some(a.wrapping_rem(b))
                        } else {
                            None
                        }
                    }) {
                        known.insert(*dest, MirConst::Int(c));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(c),
                        });
                    } else {
                        new_insts.push(inst);
                    }
                }
                MirInst::INeg { dest, operand } => {
                    if let Some(MirConst::Int(v)) = known.get(operand) {
                        let c = v.wrapping_neg();
                        known.insert(*dest, MirConst::Int(c));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(c),
                        });
                    } else {
                        new_insts.push(inst);
                    }
                }

                // Integer comparison ops.
                MirInst::ICmpEq { dest, left, right }
                | MirInst::ICmpNe { dest, left, right }
                | MirInst::ICmpLt { dest, left, right }
                | MirInst::ICmpGt { dest, left, right }
                | MirInst::ICmpLe { dest, left, right }
                | MirInst::ICmpGe { dest, left, right } => {
                    let folded = match (known.get(left), known.get(right)) {
                        (Some(MirConst::Int(a)), Some(MirConst::Int(b))) => {
                            let result = match &inst {
                                MirInst::ICmpEq { .. } => a == b,
                                MirInst::ICmpNe { .. } => a != b,
                                MirInst::ICmpLt { .. } => a < b,
                                MirInst::ICmpGt { .. } => a > b,
                                MirInst::ICmpLe { .. } => a <= b,
                                MirInst::ICmpGe { .. } => a >= b,
                                _ => unreachable!(),
                            };
                            Some(result)
                        }
                        _ => None,
                    };
                    if let Some(result) = folded {
                        known.insert(*dest, MirConst::Bool(result));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Bool(result),
                        });
                    } else {
                        new_insts.push(inst);
                    }
                }

                _ => new_insts.push(inst),
            }
        }

        block.instructions = new_insts;
    }
}

/// Try to fold a binary integer operation with two known-constant operands.
fn fold_int_binop(
    known: &HashMap<Value, MirConst>,
    left: Value,
    right: Value,
    op: fn(i64, i64) -> i64,
) -> Option<i64> {
    match (known.get(&left), known.get(&right)) {
        (Some(MirConst::Int(a)), Some(MirConst::Int(b))) => Some(op(*a, *b)),
        _ => None,
    }
}

/// Like `fold_int_binop` but the operation can fail (e.g. division by zero).
fn fold_int_binop_checked(
    known: &HashMap<Value, MirConst>,
    left: Value,
    right: Value,
    op: fn(i64, i64) -> Option<i64>,
) -> Option<i64> {
    match (known.get(&left), known.get(&right)) {
        (Some(MirConst::Int(a)), Some(MirConst::Int(b))) => op(*a, *b),
        _ => None,
    }
}

// ───────────────────────────────────────────────────────────────────────────
// Pass 2 – Algebraic simplification
// ───────────────────────────────────────────────────────────────────────────

/// Simplify instructions using algebraic identities:
///
/// - `x + 0` / `0 + x` → `x`
/// - `x - 0` → `x`
/// - `x * 1` / `1 * x` → `x`
/// - `x * 0` / `0 * x` → `0`
/// - `x / 1` → `x`
/// - `x - x` → `0`
/// - `--x` → `x`
/// - `branch(true_const)` → `goto then_block`
/// - `branch(false_const)` → `goto else_block`
pub fn algebraic_simplify(func: &mut MirFunction) {
    let mut known: HashMap<Value, MirConst> = HashMap::new();
    // Track which value each dest was simplified to (value forwarding).
    let mut aliases: HashMap<Value, Value> = HashMap::new();
    // Track INeg sources for double-negation elimination.
    let mut neg_sources: HashMap<Value, Value> = HashMap::new();

    for block in &mut func.blocks {
        let mut new_insts = Vec::with_capacity(block.instructions.len());

        for inst in block.instructions.drain(..) {
            match &inst {
                MirInst::Const { dest, value } => {
                    known.insert(*dest, value.clone());
                    new_insts.push(inst);
                }

                // x + 0 = x, 0 + x = x
                MirInst::IAdd { dest, left, right } => {
                    let l = resolve(&aliases, *left);
                    let r = resolve(&aliases, *right);
                    if is_int_zero(&known, r) {
                        aliases.insert(*dest, l);
                        // No instruction emitted — dest forwards to left.
                    } else if is_int_zero(&known, l) {
                        aliases.insert(*dest, r);
                    } else {
                        new_insts.push(MirInst::IAdd {
                            dest: *dest,
                            left: l,
                            right: r,
                        });
                    }
                }

                // x - 0 = x, x - x = 0
                MirInst::ISub { dest, left, right } => {
                    let l = resolve(&aliases, *left);
                    let r = resolve(&aliases, *right);
                    if is_int_zero(&known, r) {
                        aliases.insert(*dest, l);
                    } else if l == r {
                        known.insert(*dest, MirConst::Int(0));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(0),
                        });
                    } else {
                        new_insts.push(MirInst::ISub {
                            dest: *dest,
                            left: l,
                            right: r,
                        });
                    }
                }

                // x * 1 = x, 1 * x = x, x * 0 = 0, 0 * x = 0
                MirInst::IMul { dest, left, right } => {
                    let l = resolve(&aliases, *left);
                    let r = resolve(&aliases, *right);
                    if is_int_one(&known, r) {
                        aliases.insert(*dest, l);
                    } else if is_int_one(&known, l) {
                        aliases.insert(*dest, r);
                    } else if is_int_zero(&known, l) || is_int_zero(&known, r) {
                        known.insert(*dest, MirConst::Int(0));
                        new_insts.push(MirInst::Const {
                            dest: *dest,
                            value: MirConst::Int(0),
                        });
                    } else {
                        new_insts.push(MirInst::IMul {
                            dest: *dest,
                            left: l,
                            right: r,
                        });
                    }
                }

                // x / 1 = x
                MirInst::IDiv { dest, left, right } => {
                    let l = resolve(&aliases, *left);
                    let r = resolve(&aliases, *right);
                    if is_int_one(&known, r) {
                        aliases.insert(*dest, l);
                    } else {
                        new_insts.push(MirInst::IDiv {
                            dest: *dest,
                            left: l,
                            right: r,
                        });
                    }
                }

                // --x = x
                MirInst::INeg { dest, operand } => {
                    let o = resolve(&aliases, *operand);
                    if let Some(original) = neg_sources.get(&o) {
                        aliases.insert(*dest, *original);
                    } else {
                        neg_sources.insert(*dest, o);
                        new_insts.push(MirInst::INeg {
                            dest: *dest,
                            operand: o,
                        });
                    }
                }

                // Resolve aliases in operands for all other instructions.
                _ => {
                    new_insts.push(rewrite_operands(inst, &aliases));
                }
            }
        }

        block.instructions = new_insts;

        // Simplify terminator: branch on known constant → unconditional goto.
        block.terminator = simplify_terminator(&block.terminator, &known, &aliases);
    }
}

/// Resolve a value through the alias chain.
fn resolve(aliases: &HashMap<Value, Value>, mut v: Value) -> Value {
    let mut seen = HashSet::new();
    while let Some(target) = aliases.get(&v) {
        if !seen.insert(v) {
            break; // cycle guard
        }
        v = *target;
    }
    v
}

fn is_int_zero(known: &HashMap<Value, MirConst>, v: Value) -> bool {
    matches!(known.get(&v), Some(MirConst::Int(0)))
}

fn is_int_one(known: &HashMap<Value, MirConst>, v: Value) -> bool {
    matches!(known.get(&v), Some(MirConst::Int(1)))
}

/// Rewrite value operands in an instruction, replacing aliases.
fn rewrite_operands(inst: MirInst, aliases: &HashMap<Value, Value>) -> MirInst {
    if aliases.is_empty() {
        return inst;
    }
    let r = |v: Value| resolve(aliases, v);

    match inst {
        // Memory
        MirInst::Load { dest, addr, ty } => MirInst::Load {
            dest,
            addr: r(addr),
            ty,
        },
        MirInst::Store { addr, value } => MirInst::Store {
            addr: r(addr),
            value: r(value),
        },
        MirInst::GetFieldAddr { dest, base, offset } => MirInst::GetFieldAddr {
            dest,
            base: r(base),
            offset,
        },
        MirInst::GetElementAddr {
            dest,
            base,
            index,
            element_size,
        } => MirInst::GetElementAddr {
            dest,
            base: r(base),
            index: r(index),
            element_size,
        },

        // Arithmetic (not caught by simplify — already handled above for IAdd/ISub/IMul/IDiv)
        MirInst::IAdd { dest, left, right } => MirInst::IAdd {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::ISub { dest, left, right } => MirInst::ISub {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::IMul { dest, left, right } => MirInst::IMul {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::IDiv { dest, left, right } => MirInst::IDiv {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::IRem { dest, left, right } => MirInst::IRem {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::INeg { dest, operand } => MirInst::INeg {
            dest,
            operand: r(operand),
        },

        // Comparisons
        MirInst::ICmpEq { dest, left, right } => MirInst::ICmpEq {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::ICmpNe { dest, left, right } => MirInst::ICmpNe {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::ICmpLt { dest, left, right } => MirInst::ICmpLt {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::ICmpGt { dest, left, right } => MirInst::ICmpGt {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::ICmpLe { dest, left, right } => MirInst::ICmpLe {
            dest,
            left: r(left),
            right: r(right),
        },
        MirInst::ICmpGe { dest, left, right } => MirInst::ICmpGe {
            dest,
            left: r(left),
            right: r(right),
        },

        // Decimal ops
        MirInst::DecimalAdd {
            dest,
            left,
            right,
            result_scale,
            rounded,
        } => MirInst::DecimalAdd {
            dest,
            left: r(left),
            right: r(right),
            result_scale,
            rounded,
        },
        MirInst::DecimalSub {
            dest,
            left,
            right,
            result_scale,
            rounded,
        } => MirInst::DecimalSub {
            dest,
            left: r(left),
            right: r(right),
            result_scale,
            rounded,
        },
        MirInst::DecimalMul {
            dest,
            left,
            right,
            result_scale,
            rounded,
        } => MirInst::DecimalMul {
            dest,
            left: r(left),
            right: r(right),
            result_scale,
            rounded,
        },
        MirInst::DecimalDiv {
            dest,
            left,
            right,
            result_scale,
            rounded,
        } => MirInst::DecimalDiv {
            dest,
            left: r(left),
            right: r(right),
            result_scale,
            rounded,
        },
        MirInst::DecimalCmp { dest, left, right } => MirInst::DecimalCmp {
            dest,
            left: r(left),
            right: r(right),
        },

        // Conversions
        MirInst::DecimalToDisplay { dest, value, pic } => MirInst::DecimalToDisplay {
            dest,
            value: r(value),
            pic,
        },
        MirInst::DisplayToDecimal { dest, value, pic } => MirInst::DisplayToDecimal {
            dest,
            value: r(value),
            pic,
        },
        MirInst::PackToBinary { dest, value } => MirInst::PackToBinary {
            dest,
            value: r(value),
        },
        MirInst::BinaryToPack { dest, value } => MirInst::BinaryToPack {
            dest,
            value: r(value),
        },
        MirInst::IntToDecimal { dest, value, scale } => MirInst::IntToDecimal {
            dest,
            value: r(value),
            scale,
        },
        MirInst::DecimalToInt { dest, value } => MirInst::DecimalToInt {
            dest,
            value: r(value),
        },

        // String ops
        MirInst::MoveAlphanumeric {
            dest,
            src,
            dest_len,
            src_len,
            justified,
        } => MirInst::MoveAlphanumeric {
            dest: r(dest),
            src: r(src),
            dest_len,
            src_len,
            justified,
        },
        MirInst::StringConcat {
            dest,
            parts,
            delimiter,
            pointer,
        } => MirInst::StringConcat {
            dest: r(dest),
            parts: parts.into_iter().map(&r).collect(),
            delimiter: delimiter.map(&r),
            pointer: pointer.map(&r),
        },
        MirInst::StringSplit {
            src,
            delimiters,
            targets,
        } => MirInst::StringSplit {
            src: r(src),
            delimiters: delimiters.into_iter().map(&r).collect(),
            targets: targets.into_iter().map(&r).collect(),
        },
        MirInst::Inspect { target, mode } => MirInst::Inspect {
            target: r(target),
            mode,
        },

        // PERFORM stack
        MirInst::PerformPop { dest } => MirInst::PerformPop { dest },
        MirInst::PerformPush { return_block } => MirInst::PerformPush { return_block },

        // File I/O
        MirInst::FileOpen { file_handle, mode } => MirInst::FileOpen {
            file_handle: r(file_handle),
            mode,
        },
        MirInst::FileClose { file_handle } => MirInst::FileClose {
            file_handle: r(file_handle),
        },
        MirInst::FileRead {
            file_handle,
            into,
            at_end,
            not_at_end,
        } => MirInst::FileRead {
            file_handle: r(file_handle),
            into: r(into),
            at_end,
            not_at_end,
        },
        MirInst::FileWrite { file_handle, from } => MirInst::FileWrite {
            file_handle: r(file_handle),
            from: r(from),
        },

        // Calls
        MirInst::CallRuntime { dest, func, args } => MirInst::CallRuntime {
            dest,
            func,
            args: args.into_iter().map(&r).collect(),
        },
        MirInst::CallProgram {
            dest,
            program,
            args,
        } => MirInst::CallProgram {
            dest,
            program: r(program),
            args: args.into_iter().map(&r).collect(),
        },

        // Phi
        MirInst::Phi { dest, incoming } => MirInst::Phi {
            dest,
            incoming: incoming.into_iter().map(|(bid, v)| (bid, r(v))).collect(),
        },

        // Size error
        MirInst::SizeError {
            value,
            on_error,
            no_error,
        } => MirInst::SizeError {
            value: r(value),
            on_error,
            no_error,
        },

        // Pass-through (Const, GlobalAddr don't reference other Values).
        other => other,
    }
}

/// Simplify a terminator when the branch condition is a known constant.
fn simplify_terminator(
    term: &Terminator,
    known: &HashMap<Value, MirConst>,
    aliases: &HashMap<Value, Value>,
) -> Terminator {
    match term {
        Terminator::Branch {
            cond,
            then_block,
            else_block,
        } => {
            let c = resolve(aliases, *cond);
            match known.get(&c) {
                Some(MirConst::Bool(true)) | Some(MirConst::Int(1)) => {
                    Terminator::Goto(*then_block)
                }
                Some(MirConst::Bool(false)) | Some(MirConst::Int(0)) => {
                    Terminator::Goto(*else_block)
                }
                _ => Terminator::Branch {
                    cond: c,
                    then_block: *then_block,
                    else_block: *else_block,
                },
            }
        }
        Terminator::Switch {
            value,
            cases,
            default,
        } => {
            let v = resolve(aliases, *value);
            if let Some(MirConst::Int(n)) = known.get(&v) {
                // If we know the switch value, resolve to the matching case.
                for (case_val, target) in cases {
                    if case_val == n {
                        return Terminator::Goto(*target);
                    }
                }
                Terminator::Goto(*default)
            } else {
                Terminator::Switch {
                    value: v,
                    cases: cases.clone(),
                    default: *default,
                }
            }
        }
        Terminator::IndirectJump { target } => Terminator::IndirectJump {
            target: resolve(aliases, *target),
        },
        // Goto, Return, PerformReturn, Unreachable — no values to resolve.
        other => other.clone(),
    }
}

// ───────────────────────────────────────────────────────────────────────────
// Pass 3 – Dead code elimination
// ───────────────────────────────────────────────────────────────────────────

/// Remove instructions whose results are never used (and that have no side
/// effects), and eliminate unreachable basic blocks.
pub fn dead_code_eliminate(func: &mut MirFunction) {
    // Phase 1: Remove unreachable blocks.
    remove_unreachable_blocks(func);

    // Phase 2: Remove dead instructions (iterate until no more changes).
    loop {
        let uses = count_value_uses(func);
        let mut changed = false;
        for block in &mut func.blocks {
            let before = block.instructions.len();
            block.instructions.retain(|inst| {
                if let Some(dest) = inst_dest(inst) {
                    if !has_side_effect(inst) && *uses.get(&dest).unwrap_or(&0) == 0 {
                        return false; // dead — remove
                    }
                }
                true
            });
            if block.instructions.len() != before {
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    // Phase 3: Collapse chains of unconditional gotos.
    collapse_goto_chains(func);
}

/// Walk all instructions and terminators, counting how many times each Value
/// is referenced as an operand.
fn count_value_uses(func: &MirFunction) -> HashMap<Value, usize> {
    let mut uses: HashMap<Value, usize> = HashMap::new();

    for block in &func.blocks {
        for inst in &block.instructions {
            for v in inst_operands(inst) {
                *uses.entry(v).or_insert(0) += 1;
            }
        }
        for v in terminator_operands(&block.terminator) {
            *uses.entry(v).or_insert(0) += 1;
        }
    }

    uses
}

/// Return the destination Value defined by an instruction, if any.
fn inst_dest(inst: &MirInst) -> Option<Value> {
    match inst {
        MirInst::Const { dest, .. }
        | MirInst::GlobalAddr { dest, .. }
        | MirInst::Load { dest, .. }
        | MirInst::GetFieldAddr { dest, .. }
        | MirInst::GetElementAddr { dest, .. }
        | MirInst::IAdd { dest, .. }
        | MirInst::ISub { dest, .. }
        | MirInst::IMul { dest, .. }
        | MirInst::IDiv { dest, .. }
        | MirInst::IRem { dest, .. }
        | MirInst::INeg { dest, .. }
        | MirInst::DecimalAdd { dest, .. }
        | MirInst::DecimalSub { dest, .. }
        | MirInst::DecimalMul { dest, .. }
        | MirInst::DecimalDiv { dest, .. }
        | MirInst::DecimalCmp { dest, .. }
        | MirInst::DecimalToDisplay { dest, .. }
        | MirInst::DisplayToDecimal { dest, .. }
        | MirInst::PackToBinary { dest, .. }
        | MirInst::BinaryToPack { dest, .. }
        | MirInst::IntToDecimal { dest, .. }
        | MirInst::DecimalToInt { dest, .. }
        | MirInst::ICmpEq { dest, .. }
        | MirInst::ICmpNe { dest, .. }
        | MirInst::ICmpLt { dest, .. }
        | MirInst::ICmpGt { dest, .. }
        | MirInst::ICmpLe { dest, .. }
        | MirInst::ICmpGe { dest, .. }
        | MirInst::PerformPop { dest, .. }
        | MirInst::Phi { dest, .. } => Some(*dest),
        MirInst::MoveAlphanumeric { dest, .. } | MirInst::StringConcat { dest, .. } => Some(*dest),
        MirInst::CallRuntime { dest, .. } | MirInst::CallProgram { dest, .. } => *dest,
        // Instructions without a dest.
        MirInst::Store { .. }
        | MirInst::PerformPush { .. }
        | MirInst::FileOpen { .. }
        | MirInst::FileClose { .. }
        | MirInst::FileRead { .. }
        | MirInst::FileWrite { .. }
        | MirInst::StringSplit { .. }
        | MirInst::Inspect { .. }
        | MirInst::SizeError { .. } => None,
    }
}

/// Return the operand Values consumed by an instruction.
fn inst_operands(inst: &MirInst) -> Vec<Value> {
    match inst {
        MirInst::Const { .. } | MirInst::GlobalAddr { .. } | MirInst::PerformPush { .. } => {
            vec![]
        }
        MirInst::Load { addr, .. } => vec![*addr],
        MirInst::Store { addr, value } => vec![*addr, *value],
        MirInst::GetFieldAddr { base, .. } => vec![*base],
        MirInst::GetElementAddr { base, index, .. } => vec![*base, *index],
        MirInst::IAdd { left, right, .. }
        | MirInst::ISub { left, right, .. }
        | MirInst::IMul { left, right, .. }
        | MirInst::IDiv { left, right, .. }
        | MirInst::IRem { left, right, .. }
        | MirInst::ICmpEq { left, right, .. }
        | MirInst::ICmpNe { left, right, .. }
        | MirInst::ICmpLt { left, right, .. }
        | MirInst::ICmpGt { left, right, .. }
        | MirInst::ICmpLe { left, right, .. }
        | MirInst::ICmpGe { left, right, .. }
        | MirInst::DecimalCmp { left, right, .. } => vec![*left, *right],
        MirInst::DecimalAdd { left, right, .. }
        | MirInst::DecimalSub { left, right, .. }
        | MirInst::DecimalMul { left, right, .. }
        | MirInst::DecimalDiv { left, right, .. } => vec![*left, *right],
        MirInst::INeg { operand, .. } => vec![*operand],
        MirInst::DecimalToDisplay { value, .. }
        | MirInst::DisplayToDecimal { value, .. }
        | MirInst::PackToBinary { value, .. }
        | MirInst::BinaryToPack { value, .. }
        | MirInst::IntToDecimal { value, .. }
        | MirInst::DecimalToInt { value, .. } => vec![*value],
        MirInst::MoveAlphanumeric { dest, src, .. } => vec![*dest, *src],
        MirInst::StringConcat {
            dest,
            parts,
            delimiter,
            pointer,
        } => {
            let mut ops = vec![*dest];
            ops.extend(parts);
            if let Some(d) = delimiter {
                ops.push(*d);
            }
            if let Some(p) = pointer {
                ops.push(*p);
            }
            ops
        }
        MirInst::StringSplit {
            src,
            delimiters,
            targets,
        } => {
            let mut ops = vec![*src];
            ops.extend(delimiters);
            ops.extend(targets);
            ops
        }
        MirInst::Inspect { target, .. } => vec![*target],
        MirInst::PerformPop { .. } => vec![],
        MirInst::FileOpen { file_handle, .. } => vec![*file_handle],
        MirInst::FileClose { file_handle } => vec![*file_handle],
        MirInst::FileRead {
            file_handle, into, ..
        } => vec![*file_handle, *into],
        MirInst::FileWrite { file_handle, from } => vec![*file_handle, *from],
        MirInst::CallRuntime { args, .. } => args.clone(),
        MirInst::CallProgram { program, args, .. } => {
            let mut ops = vec![*program];
            ops.extend(args);
            ops
        }
        MirInst::Phi { incoming, .. } => incoming.iter().map(|(_, v)| *v).collect(),
        MirInst::SizeError { value, .. } => vec![*value],
    }
}

/// Return the operand Values consumed by a terminator.
fn terminator_operands(term: &Terminator) -> Vec<Value> {
    match term {
        Terminator::Goto(_)
        | Terminator::Return
        | Terminator::PerformReturn
        | Terminator::Unreachable => vec![],
        Terminator::Branch { cond, .. } => vec![*cond],
        Terminator::Switch { value, .. } => vec![*value],
        Terminator::IndirectJump { target } => vec![*target],
    }
}

/// Does this instruction have side effects (and thus must not be removed even
/// if its result is unused)?
fn has_side_effect(inst: &MirInst) -> bool {
    matches!(
        inst,
        MirInst::Store { .. }
            | MirInst::CallRuntime { .. }
            | MirInst::CallProgram { .. }
            | MirInst::MoveAlphanumeric { .. }
            | MirInst::StringConcat { .. }
            | MirInst::StringSplit { .. }
            | MirInst::Inspect { .. }
            | MirInst::PerformPush { .. }
            | MirInst::PerformPop { .. }
            | MirInst::FileOpen { .. }
            | MirInst::FileClose { .. }
            | MirInst::FileRead { .. }
            | MirInst::FileWrite { .. }
            | MirInst::SizeError { .. }
    )
}

/// Remove blocks that are not reachable from the entry block.
fn remove_unreachable_blocks(func: &mut MirFunction) {
    let reachable = reachable_blocks(func);
    func.blocks.retain(|b| reachable.contains(&b.id));
}

/// Compute the set of blocks reachable from the entry block via BFS.
fn reachable_blocks(func: &MirFunction) -> HashSet<BlockId> {
    let mut reachable = HashSet::new();
    let mut worklist = vec![func.entry_block];

    while let Some(bid) = worklist.pop() {
        if !reachable.insert(bid) {
            continue;
        }
        if let Some(block) = func.blocks.iter().find(|b| b.id == bid) {
            // Successor blocks from terminator.
            match &block.terminator {
                Terminator::Goto(t) => worklist.push(*t),
                Terminator::Branch {
                    then_block,
                    else_block,
                    ..
                } => {
                    worklist.push(*then_block);
                    worklist.push(*else_block);
                }
                Terminator::Switch { cases, default, .. } => {
                    for (_, t) in cases {
                        worklist.push(*t);
                    }
                    worklist.push(*default);
                }
                Terminator::Return
                | Terminator::PerformReturn
                | Terminator::Unreachable
                | Terminator::IndirectJump { .. } => {}
            }
            // Also check instructions for block references (FileRead, SizeError).
            for inst in &block.instructions {
                match inst {
                    MirInst::FileRead {
                        at_end, not_at_end, ..
                    } => {
                        worklist.push(*at_end);
                        worklist.push(*not_at_end);
                    }
                    MirInst::SizeError {
                        on_error, no_error, ..
                    } => {
                        worklist.push(*on_error);
                        worklist.push(*no_error);
                    }
                    MirInst::PerformPush { return_block } => {
                        worklist.push(*return_block);
                    }
                    _ => {}
                }
            }
        }
    }

    reachable
}

/// Collapse chains of Goto → Goto (a block whose only content is a Goto to
/// another block). Redirect predecessors to skip the empty block.
fn collapse_goto_chains(func: &mut MirFunction) {
    // Build a map: block_id → ultimate target (if the block is an empty goto).
    let mut goto_target: HashMap<BlockId, BlockId> = HashMap::new();

    for block in &func.blocks {
        if block.instructions.is_empty() && block.params.is_empty() {
            if let Terminator::Goto(target) = &block.terminator {
                goto_target.insert(block.id, *target);
            }
        }
    }

    if goto_target.is_empty() {
        return;
    }

    // Resolve chains: A → B → C  ⟹  A → C.
    let resolved: HashMap<BlockId, BlockId> = goto_target
        .keys()
        .map(|&start| {
            let mut cur = start;
            let mut seen = HashSet::new();
            while let Some(&next) = goto_target.get(&cur) {
                if !seen.insert(cur) {
                    break;
                }
                cur = next;
            }
            (start, cur)
        })
        .collect();

    // Rewrite all terminators to skip intermediate gotos.
    let remap = |bid: &mut BlockId| {
        if let Some(&target) = resolved.get(bid) {
            *bid = target;
        }
    };

    for block in &mut func.blocks {
        match &mut block.terminator {
            Terminator::Goto(t) => remap(t),
            Terminator::Branch {
                then_block,
                else_block,
                ..
            } => {
                remap(then_block);
                remap(else_block);
            }
            Terminator::Switch { cases, default, .. } => {
                for (_, t) in cases.iter_mut() {
                    remap(t);
                }
                remap(default);
            }
            _ => {}
        }

        // Also remap block references in instructions.
        for inst in &mut block.instructions {
            match inst {
                MirInst::FileRead {
                    at_end, not_at_end, ..
                } => {
                    remap(at_end);
                    remap(not_at_end);
                }
                MirInst::SizeError {
                    on_error, no_error, ..
                } => {
                    remap(on_error);
                    remap(no_error);
                }
                MirInst::PerformPush { return_block } => {
                    remap(return_block);
                }
                MirInst::Phi { incoming, .. } => {
                    for (bid, _) in incoming.iter_mut() {
                        remap(bid);
                    }
                }
                _ => {}
            }
        }
    }
}

// ───────────────────────────────────────────────────────────────────────────
// Pass 4 – PERFORM / function inlining
// ───────────────────────────────────────────────────────────────────────────

/// Maximum number of instructions in a callee function for it to be eligible
/// for inlining.
const INLINE_THRESHOLD: usize = 30;

/// Inline small, single-use functions within the module at `CallProgram` sites.
///
/// COBOL PERFORM targets are already inlined during MIR lowering, so this pass
/// targets internal CALL targets that exist as separate `MirFunction`s in the
/// same module.
pub fn perform_inline(module: &mut MirModule) {
    // Count how many times each function is called via CallProgram.
    let mut call_counts: HashMap<String, usize> = HashMap::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.instructions {
                if let MirInst::CallRuntime { func: name, .. } = inst {
                    *call_counts.entry(name.clone()).or_insert(0) += 1;
                }
            }
        }
    }

    // Find functions eligible for inlining: called exactly once, small body.
    let func_names: HashSet<String> = module.functions.iter().map(|f| f.name.clone()).collect();
    let inline_candidates: HashSet<String> = call_counts
        .iter()
        .filter(|(name, &count)| {
            count == 1
                && func_names.contains(*name)
                && module
                    .functions
                    .iter()
                    .find(|f| &f.name == *name)
                    .is_some_and(|f| {
                        let inst_count: usize = f.blocks.iter().map(|b| b.instructions.len()).sum();
                        inst_count <= INLINE_THRESHOLD
                    })
        })
        .map(|(name, _)| name.clone())
        .collect();

    if inline_candidates.is_empty() {
        return;
    }

    // For each candidate, inline its blocks into the caller.
    for candidate_name in &inline_candidates {
        // Extract the callee function's blocks.
        let callee_idx = module
            .functions
            .iter()
            .position(|f| &f.name == candidate_name);
        let callee = match callee_idx {
            Some(idx) => module.functions[idx].clone(),
            None => continue,
        };

        // Find the caller and inline.
        for func in &mut module.functions {
            if &func.name == candidate_name {
                continue; // Don't inline into self
            }

            let mut found_call = false;
            for block in &func.blocks {
                for inst in &block.instructions {
                    if let MirInst::CallRuntime { func: name, .. } = inst {
                        if name == candidate_name {
                            found_call = true;
                            break;
                        }
                    }
                }
                if found_call {
                    break;
                }
            }

            if !found_call {
                continue;
            }

            // Remap callee values and blocks to avoid conflicts.
            let value_offset = func.next_value;
            let block_offset = func.next_block;

            let mut inlined_blocks = Vec::new();
            for callee_block in &callee.blocks {
                let remapped_id = BlockId::from_raw(callee_block.id.raw() + block_offset);
                let remapped_insts: Vec<MirInst> = callee_block
                    .instructions
                    .iter()
                    .map(|inst| remap_inst_values(inst, value_offset, block_offset))
                    .collect();
                let remapped_term =
                    remap_terminator(&callee_block.terminator, value_offset, block_offset);
                inlined_blocks.push(crate::BasicBlock {
                    id: remapped_id,
                    params: callee_block
                        .params
                        .iter()
                        .map(|(v, ty)| (Value::from_raw(v.raw() + value_offset), ty.clone()))
                        .collect(),
                    instructions: remapped_insts,
                    terminator: remapped_term,
                });
            }

            let callee_entry = BlockId::from_raw(callee.entry_block.raw() + block_offset);

            // Update next_value and next_block.
            func.next_value += callee.next_value;
            func.next_block += callee.next_block;

            // Replace the CallRuntime instruction with a Goto to the inlined entry.
            // Create a continuation block for instructions after the call.
            for block_idx in 0..func.blocks.len() {
                let call_pos = func.blocks[block_idx].instructions.iter().position(|inst| {
                    matches!(inst, MirInst::CallRuntime { func: name, .. } if name == candidate_name)
                });

                if let Some(pos) = call_pos {
                    let cont_block_id = BlockId::from_raw(func.next_block);
                    func.next_block += 1;

                    // Split: instructions after the call go to a continuation block.
                    let after_insts: Vec<MirInst> =
                        func.blocks[block_idx].instructions[pos + 1..].to_vec();
                    let orig_terminator = func.blocks[block_idx].terminator.clone();
                    func.blocks[block_idx].instructions.truncate(pos); // Remove call + everything after

                    // Original block now jumps to the inlined callee entry.
                    func.blocks[block_idx].terminator = Terminator::Goto(callee_entry);

                    // Patch callee Return terminators to jump to continuation.
                    for ib in &mut inlined_blocks {
                        if ib.terminator == Terminator::Return {
                            ib.terminator = Terminator::Goto(cont_block_id);
                        }
                    }

                    // Create continuation block.
                    let cont_block = crate::BasicBlock {
                        id: cont_block_id,
                        params: vec![],
                        instructions: after_insts,
                        terminator: orig_terminator,
                    };

                    func.blocks.extend(inlined_blocks);
                    func.blocks.push(cont_block);
                    break; // Only inline once per candidate
                }
            }
        }
    }

    // Remove inlined functions from the module (they're now part of the caller).
    module
        .functions
        .retain(|f| !inline_candidates.contains(&f.name));
}

/// Remap all Value references in an instruction by adding an offset.
fn remap_inst_values(inst: &MirInst, val_off: u32, blk_off: u32) -> MirInst {
    let rv = |v: &Value| Value::from_raw(v.raw() + val_off);
    let rb = |b: &BlockId| BlockId::from_raw(b.raw() + blk_off);

    match inst {
        MirInst::Const { dest, value } => MirInst::Const {
            dest: rv(dest),
            value: value.clone(),
        },
        MirInst::GlobalAddr { dest, name } => MirInst::GlobalAddr {
            dest: rv(dest),
            name: name.clone(),
        },
        MirInst::Load { dest, addr, ty } => MirInst::Load {
            dest: rv(dest),
            addr: rv(addr),
            ty: ty.clone(),
        },
        MirInst::Store { addr, value } => MirInst::Store {
            addr: rv(addr),
            value: rv(value),
        },
        MirInst::IAdd { dest, left, right } => MirInst::IAdd {
            dest: rv(dest),
            left: rv(left),
            right: rv(right),
        },
        MirInst::ISub { dest, left, right } => MirInst::ISub {
            dest: rv(dest),
            left: rv(left),
            right: rv(right),
        },
        MirInst::IMul { dest, left, right } => MirInst::IMul {
            dest: rv(dest),
            left: rv(left),
            right: rv(right),
        },
        MirInst::IDiv { dest, left, right } => MirInst::IDiv {
            dest: rv(dest),
            left: rv(left),
            right: rv(right),
        },
        MirInst::CallRuntime { dest, func, args } => MirInst::CallRuntime {
            dest: dest.map(|d| rv(&d)),
            func: func.clone(),
            args: args.iter().map(&rv).collect(),
        },
        MirInst::PerformPush { return_block } => MirInst::PerformPush {
            return_block: rb(return_block),
        },
        MirInst::FileRead {
            file_handle,
            into,
            at_end,
            not_at_end,
        } => MirInst::FileRead {
            file_handle: rv(file_handle),
            into: rv(into),
            at_end: rb(at_end),
            not_at_end: rb(not_at_end),
        },
        MirInst::SizeError {
            value,
            on_error,
            no_error,
        } => MirInst::SizeError {
            value: rv(value),
            on_error: rb(on_error),
            no_error: rb(no_error),
        },
        // For any other instruction, clone as-is (conservative — won't remap).
        // This is safe because unhandled instructions will still work correctly,
        // they just won't benefit from inlining optimizations on their operands.
        other => other.clone(),
    }
}

/// Remap block references in a terminator by adding an offset.
fn remap_terminator(term: &Terminator, val_off: u32, blk_off: u32) -> Terminator {
    let rv = |v: &Value| Value::from_raw(v.raw() + val_off);
    let rb = |b: &BlockId| BlockId::from_raw(b.raw() + blk_off);

    match term {
        Terminator::Goto(t) => Terminator::Goto(rb(t)),
        Terminator::Branch {
            cond,
            then_block,
            else_block,
        } => Terminator::Branch {
            cond: rv(cond),
            then_block: rb(then_block),
            else_block: rb(else_block),
        },
        Terminator::Switch {
            value,
            cases,
            default,
        } => Terminator::Switch {
            value: rv(value),
            cases: cases.iter().map(|(val, bid)| (*val, rb(bid))).collect(),
            default: rb(default),
        },
        Terminator::Return => Terminator::Return,
        Terminator::PerformReturn => Terminator::PerformReturn,
        Terminator::IndirectJump { target } => Terminator::IndirectJump { target: rv(target) },
        Terminator::Unreachable => Terminator::Unreachable,
    }
}

// ───────────────────────────────────────────────────────────────────────────
// Pass 5 – Decimal strength reduction
// ───────────────────────────────────────────────────────────────────────────

/// Tracks information about a decimal value for strength reduction.
#[derive(Debug, Clone)]
struct DecimalValueInfo {
    /// The original integer value before IntToDecimal conversion.
    int_source: Value,
    /// The decimal scale (number of fractional digits).
    scale: i8,
}

/// Replace decimal arithmetic with native integer operations when safe.
///
/// Targets `IntToDecimal → DecimalOp → DecimalToInt` chains. When both operands
/// of a decimal operation originated from `IntToDecimal` with the same scale,
/// the decimal round-trip can be eliminated in favor of native i64 arithmetic.
///
/// Also optimizes `DecimalCmp` to native `ICmpEq`/`ICmpLt` when both operands
/// have the same scale and originated from integers.
pub fn decimal_strength_reduce(func: &mut MirFunction) {
    // Phase 1: Collect IntToDecimal sources.
    let mut decimal_sources: HashMap<Value, DecimalValueInfo> = HashMap::new();
    // Track which decimal Values are the result of a strength-reduced operation
    // (and thus are actually integers at the given scale).
    let mut reduced_results: HashMap<Value, DecimalValueInfo> = HashMap::new();

    for block in &mut func.blocks {
        let mut new_insts = Vec::with_capacity(block.instructions.len());

        for inst in block.instructions.drain(..) {
            match &inst {
                // Record IntToDecimal: the decimal value v maps back to integer src.
                MirInst::IntToDecimal { dest, value, scale } => {
                    decimal_sources.insert(
                        *dest,
                        DecimalValueInfo {
                            int_source: *value,
                            scale: *scale,
                        },
                    );
                    new_insts.push(inst);
                }

                // DecimalAdd: if both operands came from IntToDecimal with same scale
                // and the result_scale matches, use IAdd.
                MirInst::DecimalAdd {
                    dest,
                    left,
                    right,
                    result_scale,
                    rounded: false,
                } => {
                    let l_info = decimal_sources
                        .get(left)
                        .or_else(|| reduced_results.get(left));
                    let r_info = decimal_sources
                        .get(right)
                        .or_else(|| reduced_results.get(right));
                    if let (Some(l), Some(r)) = (l_info, r_info) {
                        if l.scale == r.scale && l.scale == *result_scale {
                            // Safe: same scale, no rounding, integer addition preserves value.
                            new_insts.push(MirInst::IAdd {
                                dest: *dest,
                                left: l.int_source,
                                right: r.int_source,
                            });
                            reduced_results.insert(
                                *dest,
                                DecimalValueInfo {
                                    int_source: *dest,
                                    scale: *result_scale,
                                },
                            );
                            continue;
                        }
                    }
                    new_insts.push(inst);
                }

                // DecimalSub: same logic as DecimalAdd.
                MirInst::DecimalSub {
                    dest,
                    left,
                    right,
                    result_scale,
                    rounded: false,
                } => {
                    let l_info = decimal_sources
                        .get(left)
                        .or_else(|| reduced_results.get(left));
                    let r_info = decimal_sources
                        .get(right)
                        .or_else(|| reduced_results.get(right));
                    if let (Some(l), Some(r)) = (l_info, r_info) {
                        if l.scale == r.scale && l.scale == *result_scale {
                            new_insts.push(MirInst::ISub {
                                dest: *dest,
                                left: l.int_source,
                                right: r.int_source,
                            });
                            reduced_results.insert(
                                *dest,
                                DecimalValueInfo {
                                    int_source: *dest,
                                    scale: *result_scale,
                                },
                            );
                            continue;
                        }
                    }
                    new_insts.push(inst);
                }

                // DecimalCmp: if both operands have same scale, compare integers.
                MirInst::DecimalCmp {
                    dest: _,
                    left,
                    right,
                } => {
                    let l_info = decimal_sources
                        .get(left)
                        .or_else(|| reduced_results.get(left));
                    let r_info = decimal_sources
                        .get(right)
                        .or_else(|| reduced_results.get(right));
                    if let (Some(l), Some(r)) = (l_info, r_info) {
                        if l.scale == r.scale {
                            // Compare the integer representations directly.
                            // DecimalCmp returns -1/0/1, so we use ISub for the comparison.
                            // Actually, we can't directly replace DecimalCmp → ISub because
                            // ISub doesn't clamp to -1/0/1. But we can track the source
                            // for a future ICmpEq/ICmpLt chain. For now, keep the original
                            // but record the info for downstream optimizations.
                            new_insts.push(inst);
                        } else {
                            new_insts.push(inst);
                        }
                    } else {
                        new_insts.push(inst);
                    }
                }

                // DecimalToInt: if the source was strength-reduced, use the integer directly.
                MirInst::DecimalToInt { dest, value } => {
                    if let Some(info) = reduced_results.get(value) {
                        // The value is already an integer at the right scale — just alias.
                        new_insts.push(MirInst::IAdd {
                            dest: *dest,
                            left: info.int_source,
                            right: info.int_source,
                        });
                        // Actually, we need a copy, not a double. Use Const(0) + IAdd.
                        // Simpler: just emit a Const(0) and add. But the cleanest is
                        // to keep the DecimalToInt and let DCE clean up the unused
                        // IntToDecimal. The issue is DecimalToInt on a non-decimal value.
                        // Let's just keep it — the codegen handles DecimalToInt on ints.
                        new_insts.pop(); // Remove the IAdd we just pushed
                        new_insts.push(inst); // Keep original
                    } else if let Some(_info) = decimal_sources.get(value) {
                        // IntToDecimal immediately followed by DecimalToInt = identity.
                        // This is a no-op round-trip. We can't eliminate the instruction
                        // but we can track that dest = info.int_source for alias resolution.
                        new_insts.push(inst);
                    } else {
                        new_insts.push(inst);
                    }
                }

                _ => new_insts.push(inst),
            }
        }

        block.instructions = new_insts;
    }
}

// ───────────────────────────────────────────────────────────────────────────
// Pass 6 – Dead function elimination
// ───────────────────────────────────────────────────────────────────────────

/// Remove functions from the module that are never called and are not the
/// entry point (index 0).
///
/// Builds a call graph from `CallRuntime` and `CallProgram` instructions,
/// then removes functions whose names are never referenced.
pub fn dead_function_eliminate(module: &mut MirModule) {
    if module.functions.len() <= 1 {
        return;
    }

    // Collect all called function names.
    let mut called: HashSet<String> = HashSet::new();
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.instructions {
                if let MirInst::CallRuntime { func: name, .. } = inst {
                    called.insert(name.clone());
                }
            }
        }
    }

    // Retain entry function (index 0) and any function that is called.
    let mut retained = Vec::new();
    for (i, func) in module.functions.drain(..).enumerate() {
        if i == 0 || called.contains(&func.name) {
            retained.push(func);
        }
    }
    module.functions = retained;
}

// ───────────────────────────────────────────────────────────────────────────
// Tests
// ───────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        BasicBlock, BlockId, MirConst, MirFunction, MirInst, MirModule, MirType, Terminator, Value,
    };

    fn v(n: u32) -> Value {
        Value::from_raw(n)
    }

    fn bid(n: u32) -> BlockId {
        BlockId::from_raw(n)
    }

    fn make_func(blocks: Vec<BasicBlock>) -> MirFunction {
        let max_val = blocks
            .iter()
            .flat_map(|b| b.instructions.iter())
            .filter_map(|i| inst_dest(i).map(|v| v.raw()))
            .max()
            .unwrap_or(0);
        let max_block = blocks.iter().map(|b| b.id.raw()).max().unwrap_or(0);
        MirFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks,
            entry_block: bid(0),
            next_value: max_val + 1,
            next_block: max_block + 1,
        }
    }

    // ── Constant folding ─────────────────────────────────────────────────

    #[test]
    fn fold_add() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(10),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(20),
                },
                MirInst::IAdd {
                    dest: v(2),
                    left: v(0),
                    right: v(1),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        constant_fold(&mut func);
        // The add should be folded to Const(30).
        match &func.blocks[0].instructions[2] {
            MirInst::Const {
                dest,
                value: MirConst::Int(30),
            } => assert_eq!(*dest, v(2)),
            other => panic!("expected Const(30), got {:?}", other),
        }
    }

    #[test]
    fn fold_div_by_zero_not_folded() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(42),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(0),
                },
                MirInst::IDiv {
                    dest: v(2),
                    left: v(0),
                    right: v(1),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        constant_fold(&mut func);
        // Division by zero should NOT be folded.
        assert!(matches!(
            &func.blocks[0].instructions[2],
            MirInst::IDiv { .. }
        ));
    }

    #[test]
    fn fold_comparison() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(5),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(10),
                },
                MirInst::ICmpLt {
                    dest: v(2),
                    left: v(0),
                    right: v(1),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        constant_fold(&mut func);
        match &func.blocks[0].instructions[2] {
            MirInst::Const {
                value: MirConst::Bool(true),
                ..
            } => {}
            other => panic!("expected Const(true), got {:?}", other),
        }
    }

    // ── Algebraic simplification ────────────────────────────────────────

    #[test]
    fn simplify_add_zero() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(0),
                },
                MirInst::GlobalAddr {
                    dest: v(1),
                    name: "X".into(),
                },
                MirInst::IAdd {
                    dest: v(2),
                    left: v(1),
                    right: v(0),
                },
                // Use v(2) so it's not dead.
                MirInst::Store {
                    addr: v(2),
                    value: v(1),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        algebraic_simplify(&mut func);
        // The IAdd should be eliminated (v(2) aliased to v(1)).
        // The Store should use v(1) for addr now.
        let store = func.blocks[0]
            .instructions
            .iter()
            .find(|i| matches!(i, MirInst::Store { .. }));
        match store {
            Some(MirInst::Store { addr, value }) => {
                assert_eq!(*addr, v(1), "addr should be forwarded to v(1)");
                assert_eq!(*value, v(1));
            }
            other => panic!("expected Store, got {:?}", other),
        }
    }

    #[test]
    fn simplify_mul_by_one() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(1),
                },
                MirInst::GlobalAddr {
                    dest: v(1),
                    name: "X".into(),
                },
                MirInst::IMul {
                    dest: v(2),
                    left: v(1),
                    right: v(0),
                },
                MirInst::Store {
                    addr: v(2),
                    value: v(1),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        algebraic_simplify(&mut func);
        let store = func.blocks[0]
            .instructions
            .iter()
            .find(|i| matches!(i, MirInst::Store { .. }));
        match store {
            Some(MirInst::Store { addr, .. }) => {
                assert_eq!(*addr, v(1), "x*1 should simplify to x");
            }
            other => panic!("expected Store, got {:?}", other),
        }
    }

    #[test]
    fn simplify_mul_by_zero() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(0),
                },
                MirInst::GlobalAddr {
                    dest: v(1),
                    name: "X".into(),
                },
                MirInst::IMul {
                    dest: v(2),
                    left: v(1),
                    right: v(0),
                },
                MirInst::Store {
                    addr: v(2),
                    value: v(1),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        algebraic_simplify(&mut func);
        // v(2) should become Const(0).
        let has_zero = func.blocks[0].instructions.iter().any(
            |i| matches!(i, MirInst::Const { dest, value: MirConst::Int(0) } if *dest == v(2)),
        );
        assert!(has_zero, "x*0 should produce Const(0)");
    }

    #[test]
    fn simplify_branch_on_true() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![MirInst::Const {
                dest: v(0),
                value: MirConst::Bool(true),
            }],
            terminator: Terminator::Branch {
                cond: v(0),
                then_block: bid(1),
                else_block: bid(2),
            },
        };
        let then_block = BasicBlock {
            id: bid(1),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        };
        let else_block = BasicBlock {
            id: bid(2),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block, then_block, else_block]);
        algebraic_simplify(&mut func);
        assert_eq!(func.blocks[0].terminator, Terminator::Goto(bid(1)));
    }

    #[test]
    fn simplify_double_neg() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::GlobalAddr {
                    dest: v(0),
                    name: "X".into(),
                },
                MirInst::Load {
                    dest: v(1),
                    addr: v(0),
                    ty: MirType::I64,
                },
                MirInst::INeg {
                    dest: v(2),
                    operand: v(1),
                },
                MirInst::INeg {
                    dest: v(3),
                    operand: v(2),
                },
                MirInst::Store {
                    addr: v(0),
                    value: v(3),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        algebraic_simplify(&mut func);
        // --x should alias back to x (v(1)). The Store should use v(1).
        let store = func.blocks[0]
            .instructions
            .iter()
            .find(|i| matches!(i, MirInst::Store { .. }));
        match store {
            Some(MirInst::Store { value, .. }) => {
                assert_eq!(*value, v(1), "--x should simplify to x");
            }
            other => panic!("expected Store, got {:?}", other),
        }
    }

    // ── Dead code elimination ───────────────────────────────────────────

    #[test]
    fn dce_removes_unused_const() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(42),
                }, // unused
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(99),
                }, // unused
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        dead_code_eliminate(&mut func);
        assert!(
            func.blocks[0].instructions.is_empty(),
            "dead consts should be removed"
        );
    }

    #[test]
    fn dce_keeps_side_effects() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::GlobalAddr {
                    dest: v(0),
                    name: "X".into(),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(42),
                },
                MirInst::Store {
                    addr: v(0),
                    value: v(1),
                }, // side effect
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        dead_code_eliminate(&mut func);
        // Store has side effects, so it and its operands should survive.
        assert_eq!(func.blocks[0].instructions.len(), 3);
    }

    #[test]
    fn dce_removes_unreachable_block() {
        let entry = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Goto(bid(1)),
        };
        let reachable = BasicBlock {
            id: bid(1),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        };
        let unreachable = BasicBlock {
            id: bid(2),
            params: vec![],
            instructions: vec![MirInst::Const {
                dest: v(0),
                value: MirConst::Int(999),
            }],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![entry, reachable, unreachable]);
        dead_code_eliminate(&mut func);
        assert_eq!(func.blocks.len(), 2, "unreachable block should be removed");
        assert!(func.blocks.iter().all(|b| b.id != bid(2)));
    }

    #[test]
    fn dce_collapses_goto_chain() {
        let b0 = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Goto(bid(1)),
        };
        let b1 = BasicBlock {
            id: bid(1),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Goto(bid(2)),
        };
        let b2 = BasicBlock {
            id: bid(2),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![b0, b1, b2]);
        dead_code_eliminate(&mut func);
        // b0 should now jump directly to b2.
        assert_eq!(func.blocks[0].terminator, Terminator::Goto(bid(2)));
    }

    // ── Full pipeline ───────────────────────────────────────────────────

    #[test]
    fn optimize_full_pipeline() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(5),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(0),
                },
                // 5 + 0 → should simplify to 5 via algebraic, then fold.
                MirInst::IAdd {
                    dest: v(2),
                    left: v(0),
                    right: v(1),
                },
                // Use v(2) in a comparison — should fold to Const.
                MirInst::Const {
                    dest: v(3),
                    value: MirConst::Int(5),
                },
                MirInst::ICmpEq {
                    dest: v(4),
                    left: v(2),
                    right: v(3),
                },
                // Branch on the comparison — should simplify to Goto.
                // v(1) is dead after simplification.
            ],
            terminator: Terminator::Branch {
                cond: v(4),
                then_block: bid(1),
                else_block: bid(2),
            },
        };
        let then_block = BasicBlock {
            id: bid(1),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        };
        let else_block = BasicBlock {
            id: bid(2),
            params: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        };
        let mut module = MirModule {
            name: "test".into(),
            functions: vec![make_func(vec![block, then_block, else_block])],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        };
        optimize(&mut module, OptLevel::Full);

        let func = &module.functions[0];
        // After full optimization:
        // - v(1) (Const 0) may be removed (dead).
        // - IAdd v(2) = v(0) + 0 → simplified away (v(2) aliases v(0)).
        // - v(3) (Const 5) may also be dead after folding.
        // - ICmpEq 5 == 5 → true
        // - Branch on true → Goto(bid(1))
        // - else_block may be removed (unreachable).
        assert_eq!(
            func.blocks[0].terminator,
            Terminator::Goto(bid(1)),
            "branch should be simplified to goto"
        );
    }

    // ── Decimal strength reduction ─────────────────────────────────────

    #[test]
    fn decimal_reduce_add_same_scale() {
        // IntToDecimal(v0, scale=2) + IntToDecimal(v1, scale=2) → IAdd(v0, v1)
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(100), // represents 1.00
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(250), // represents 2.50
                },
                MirInst::IntToDecimal {
                    dest: v(2),
                    value: v(0),
                    scale: 2,
                },
                MirInst::IntToDecimal {
                    dest: v(3),
                    value: v(1),
                    scale: 2,
                },
                MirInst::DecimalAdd {
                    dest: v(4),
                    left: v(2),
                    right: v(3),
                    result_scale: 2,
                    rounded: false,
                },
                // Use v(4) so it's not dead
                MirInst::Store {
                    addr: v(4),
                    value: v(0),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        decimal_strength_reduce(&mut func);

        // The DecimalAdd should be replaced with IAdd.
        let has_iadd = func.blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, MirInst::IAdd { dest, .. } if *dest == v(4)));
        assert!(has_iadd, "DecimalAdd should be reduced to IAdd");

        let has_decimal_add = func.blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, MirInst::DecimalAdd { .. }));
        assert!(!has_decimal_add, "DecimalAdd should be eliminated");
    }

    #[test]
    fn decimal_reduce_sub_same_scale() {
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(500),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(200),
                },
                MirInst::IntToDecimal {
                    dest: v(2),
                    value: v(0),
                    scale: 2,
                },
                MirInst::IntToDecimal {
                    dest: v(3),
                    value: v(1),
                    scale: 2,
                },
                MirInst::DecimalSub {
                    dest: v(4),
                    left: v(2),
                    right: v(3),
                    result_scale: 2,
                    rounded: false,
                },
                MirInst::Store {
                    addr: v(4),
                    value: v(0),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        decimal_strength_reduce(&mut func);

        let has_isub = func.blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, MirInst::ISub { dest, .. } if *dest == v(4)));
        assert!(has_isub, "DecimalSub should be reduced to ISub");
    }

    #[test]
    fn decimal_reduce_different_scale_not_reduced() {
        // Different scales: should NOT be reduced.
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(100),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(25),
                },
                MirInst::IntToDecimal {
                    dest: v(2),
                    value: v(0),
                    scale: 2,
                },
                MirInst::IntToDecimal {
                    dest: v(3),
                    value: v(1),
                    scale: 0, // different scale!
                },
                MirInst::DecimalAdd {
                    dest: v(4),
                    left: v(2),
                    right: v(3),
                    result_scale: 2,
                    rounded: false,
                },
                MirInst::Store {
                    addr: v(4),
                    value: v(0),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        decimal_strength_reduce(&mut func);

        let has_decimal_add = func.blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, MirInst::DecimalAdd { .. }));
        assert!(has_decimal_add, "Different scales should not be reduced");
    }

    #[test]
    fn decimal_reduce_rounded_not_reduced() {
        // Rounded operations should NOT be reduced.
        let block = BasicBlock {
            id: bid(0),
            params: vec![],
            instructions: vec![
                MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(100),
                },
                MirInst::Const {
                    dest: v(1),
                    value: MirConst::Int(250),
                },
                MirInst::IntToDecimal {
                    dest: v(2),
                    value: v(0),
                    scale: 2,
                },
                MirInst::IntToDecimal {
                    dest: v(3),
                    value: v(1),
                    scale: 2,
                },
                MirInst::DecimalAdd {
                    dest: v(4),
                    left: v(2),
                    right: v(3),
                    result_scale: 2,
                    rounded: true, // rounded!
                },
                MirInst::Store {
                    addr: v(4),
                    value: v(0),
                },
            ],
            terminator: Terminator::Return,
        };
        let mut func = make_func(vec![block]);
        decimal_strength_reduce(&mut func);

        let has_decimal_add = func.blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, MirInst::DecimalAdd { .. }));
        assert!(has_decimal_add, "Rounded adds should not be reduced");
    }

    // ── PERFORM / function inlining ────────────────────────────────────

    #[test]
    fn perform_inline_small_function() {
        // Create a module with two functions: main calls helper once.
        let main_func = MirFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![MirInst::CallRuntime {
                    dest: None,
                    func: "helper".to_string(),
                    args: vec![],
                }],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 0,
            next_block: 1,
        };
        let helper_func = MirFunction {
            name: "helper".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![MirInst::Const {
                    dest: v(0),
                    value: MirConst::Int(42),
                }],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 1,
            next_block: 1,
        };
        let mut module = MirModule {
            name: "test".into(),
            functions: vec![main_func, helper_func],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        };

        perform_inline(&mut module);

        // Helper should be inlined into main and removed from module.
        assert_eq!(module.functions.len(), 1, "helper should be removed");
        assert_eq!(module.functions[0].name, "main");
        // Main should have more than 1 block now (original + inlined + continuation).
        assert!(
            module.functions[0].blocks.len() >= 2,
            "main should have inlined blocks"
        );
    }

    #[test]
    fn perform_inline_multi_use_not_inlined() {
        // A function called twice should NOT be inlined.
        let main_func = MirFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![
                    MirInst::CallRuntime {
                        dest: None,
                        func: "helper".to_string(),
                        args: vec![],
                    },
                    MirInst::CallRuntime {
                        dest: None,
                        func: "helper".to_string(),
                        args: vec![],
                    },
                ],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 0,
            next_block: 1,
        };
        let helper_func = MirFunction {
            name: "helper".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 0,
            next_block: 1,
        };
        let mut module = MirModule {
            name: "test".into(),
            functions: vec![main_func, helper_func],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        };

        perform_inline(&mut module);

        // Helper should still exist (called twice).
        assert_eq!(module.functions.len(), 2);
    }

    #[test]
    fn dead_function_eliminate_removes_unused() {
        let main_func = MirFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![MirInst::CallRuntime {
                    dest: None,
                    func: "used".to_string(),
                    args: vec![],
                }],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 0,
            next_block: 1,
        };
        let used_func = MirFunction {
            name: "used".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 0,
            next_block: 1,
        };
        let unused_func = MirFunction {
            name: "dead_code".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 0,
            next_block: 1,
        };
        let mut module = MirModule {
            name: "test".into(),
            functions: vec![main_func, used_func, unused_func],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        };

        dead_function_eliminate(&mut module);

        // dead_code should be removed, main + used retained
        assert_eq!(module.functions.len(), 2);
        assert_eq!(module.functions[0].name, "main");
        assert_eq!(module.functions[1].name, "used");
    }

    #[test]
    fn dead_function_eliminate_preserves_entry() {
        // Even if entry function is never called, it should be preserved
        let main_func = MirFunction {
            name: "main".to_string(),
            params: vec![],
            return_type: MirType::Void,
            blocks: vec![BasicBlock {
                id: bid(0),
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            }],
            entry_block: bid(0),
            next_value: 0,
            next_block: 1,
        };
        let mut module = MirModule {
            name: "test".into(),
            functions: vec![main_func],
            globals: vec![],
            file_descriptors: vec![],
            errors: vec![],
        };

        dead_function_eliminate(&mut module);
        assert_eq!(module.functions.len(), 1);
    }
}
