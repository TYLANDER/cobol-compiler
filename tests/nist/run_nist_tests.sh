#!/usr/bin/env bash
# NIST CCVS test runner for cobolc
# Runs all NC*, SM*, and IC* test programs and checks for PASS/FAIL.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COBOLC="${COBOLC:-cargo run --quiet --bin cobolc --}"
COPYBOOK_DIR="$SCRIPT_DIR/copybooks"
KNOWN_FAILURES="$SCRIPT_DIR/known_failures.toml"

PASS=0
FAIL=0
SKIP=0
TOTAL_TESTS=0
TOTAL_PASS=0
TOTAL_FAIL=0
FAILED_TESTS=""

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC_COLOR='\033[0m'

# Parse known_failures.toml for skip list
SKIP_LIST=""
if [ -f "$KNOWN_FAILURES" ]; then
    # Simple parser: extract test names from compile_fail, runtime_fail, skip arrays
    SKIP_LIST=$(grep -E '^\s*"[A-Z0-9]' "$KNOWN_FAILURES" 2>/dev/null | \
        sed 's/.*"\([^"]*\)".*/\1/' || true)
fi

is_skipped() {
    local name="$1"
    echo "$SKIP_LIST" | grep -qw "$name" 2>/dev/null
}

run_nist_test() {
    local test_file="$1"
    local test_name
    test_name=$(basename "$test_file" .cob)

    # Check if this test should be skipped
    if is_skipped "$test_name"; then
        printf "${YELLOW}SKIP${NC_COLOR} %s (known failure)\n" "$test_name"
        SKIP=$((SKIP + 1))
        return
    fi

    local output_dir
    output_dir=$(mktemp -d)
    local binary="$output_dir/$test_name"

    # Build compile command args
    local compile_args=("$test_file")

    # For IC tests, include the subprogram file
    local sub_file="$SCRIPT_DIR/${test_name}-SUB.cob"
    if [ -f "$sub_file" ]; then
        compile_args+=("$sub_file")
    fi

    # For SM tests and any test needing copybooks, add -I
    if [ -d "$COPYBOOK_DIR" ]; then
        compile_args+=("-I" "$COPYBOOK_DIR")
    fi

    compile_args+=("-o" "$binary")

    # Try to compile
    if ! $COBOLC "${compile_args[@]}" 2>"$output_dir/compile.err"; then
        printf "${RED}FAIL${NC_COLOR} %s (compilation failed)\n" "$test_name"
        head -3 "$output_dir/compile.err" 2>/dev/null || true
        FAIL=$((FAIL + 1))
        FAILED_TESTS="$FAILED_TESTS $test_name"
        rm -rf "$output_dir"
        return
    fi

    # Run the program
    local actual
    actual=$("$binary" 2>&1) || true

    # Count individual sub-tests (PASS/FAIL lines)
    local test_passes test_fails
    test_passes=$(echo "$actual" | grep -c "PASS" || true)
    test_fails=$(echo "$actual" | grep -c "FAIL" || true)
    TOTAL_TESTS=$((TOTAL_TESTS + test_passes + test_fails))
    TOTAL_PASS=$((TOTAL_PASS + test_passes))
    TOTAL_FAIL=$((TOTAL_FAIL + test_fails))

    # A test program passes if it has at least one PASS and no FAILs
    if [ "$test_fails" -eq 0 ] && [ "$test_passes" -gt 0 ]; then
        printf "${GREEN}PASS${NC_COLOR} %s (%d sub-tests)\n" "$test_name" "$test_passes"
        PASS=$((PASS + 1))
    elif [ "$test_passes" -eq 0 ] && [ "$test_fails" -eq 0 ]; then
        printf "${RED}FAIL${NC_COLOR} %s (no output)\n" "$test_name"
        FAIL=$((FAIL + 1))
        FAILED_TESTS="$FAILED_TESTS $test_name"
    else
        printf "${RED}FAIL${NC_COLOR} %s (%d pass, %d fail)\n" "$test_name" "$test_passes" "$test_fails"
        # Show failing sub-tests
        echo "$actual" | grep "FAIL" | head -5
        FAIL=$((FAIL + 1))
        FAILED_TESTS="$FAILED_TESTS $test_name"
    fi

    rm -rf "$output_dir"
}

echo "=== cobolc NIST CCVS Tests ==="
echo ""

# Run NC tests (Nucleus)
echo "--- Nucleus (NC) ---"
for f in "$SCRIPT_DIR"/NC*.cob; do
    [ -f "$f" ] && run_nist_test "$f"
done
echo ""

# Run SM tests (Source Manipulation)
echo "--- Source Manipulation (SM) ---"
for f in "$SCRIPT_DIR"/SM*.cob; do
    [ -f "$f" ] && run_nist_test "$f"
done
echo ""

# Run IC tests (Inter-program Communication)
# Skip -SUB files (they are subprograms, not main tests)
echo "--- Inter-program Communication (IC) ---"
for f in "$SCRIPT_DIR"/IC*.cob; do
    [ -f "$f" ] || continue
    # Skip subprogram files
    case "$(basename "$f")" in
        *-SUB.cob) continue ;;
    esac
    run_nist_test "$f"
done
echo ""

# Summary
echo "=== Program Results ==="
echo "Pass: $PASS  Fail: $FAIL  Skip: $SKIP  Total: $((PASS + FAIL + SKIP))"
echo ""
echo "=== Sub-test Results ==="
echo "Pass: $TOTAL_PASS  Fail: $TOTAL_FAIL  Total: $TOTAL_TESTS"

if [ -n "$FAILED_TESTS" ]; then
    echo ""
    echo "Failed programs:$FAILED_TESTS"
fi

if [ $FAIL -gt 0 ]; then
    exit 1
fi
