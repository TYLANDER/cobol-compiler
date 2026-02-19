#!/usr/bin/env bash
# Smoke test runner for cobolc
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COBOLC="${COBOLC:-cargo run --quiet --bin cobolc --}"
PASS=0
FAIL=0
SKIP=0

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

run_test() {
    local name="$1"
    shift
    local source="$SCRIPT_DIR/${name}.cob"
    local expected="$SCRIPT_DIR/expected/${name}.txt"
    local output_dir=$(mktemp -d)
    local binary="$output_dir/$name"

    if [ ! -f "$expected" ]; then
        printf "${YELLOW}SKIP${NC} %s (no expected output)\n" "$name"
        SKIP=$((SKIP + 1))
        return
    fi

    # Build list of args: source files and compiler flags.
    # Arguments starting with '-' are treated as compiler flags.
    # Flags that take a value (like -I, -o, --include) consume the next arg too.
    local flags=()
    local sources=("$source")
    local skip_next=false
    local args=("$@")
    local i=0
    while [ $i -lt ${#args[@]} ]; do
        local arg="${args[$i]}"
        case "$arg" in
            -I|--include)
                flags+=("$arg")
                i=$((i + 1))
                if [ $i -lt ${#args[@]} ]; then
                    flags+=("${args[$i]}")
                fi
                ;;
            -*)
                flags+=("$arg")
                ;;
            *)
                sources+=("$arg")
                ;;
        esac
        i=$((i + 1))
    done

    # Try to compile
    if ! $COBOLC "${sources[@]}" ${flags[@]+"${flags[@]}"} -o "$binary" 2>"$output_dir/compile.err"; then
        printf "${RED}FAIL${NC} %s (compilation failed)\n" "$name"
        cat "$output_dir/compile.err" | head -5
        FAIL=$((FAIL + 1))
        rm -rf "$output_dir"
        return
    fi

    # Try to run
    local actual
    actual=$("$binary" 2>&1) || true

    # Compare output
    local expected_text
    expected_text=$(cat "$expected")

    # Trim trailing whitespace for comparison
    actual=$(echo "$actual" | sed 's/[[:space:]]*$//')
    expected_text=$(echo "$expected_text" | sed 's/[[:space:]]*$//')

    if [ "$actual" = "$expected_text" ]; then
        printf "${GREEN}PASS${NC} %s\n" "$name"
        PASS=$((PASS + 1))
    else
        printf "${RED}FAIL${NC} %s\n" "$name"
        echo "  Expected: $expected_text"
        echo "  Actual:   $actual"
        FAIL=$((FAIL + 1))
    fi

    rm -rf "$output_dir"
}

echo "=== cobolc Smoke Tests ==="
echo ""

run_test "HELLO-WORLD"
run_test "SMOKE-ARITH"
run_test "SMOKE-STRING"
run_test "SMOKE-IF"
run_test "SMOKE-DECIMAL"
run_test "SMOKE-FILE"
run_test "SMOKE-CALL" "$SCRIPT_DIR/CALL-SUB.cob"
run_test "SMOKE-EVALUATE"
run_test "SMOKE-COPY" "-I" "$SCRIPT_DIR/copybooks"
run_test "SMOKE-GOTO"
run_test "SMOKE-ACCEPT"
run_test "SMOKE-COMPUTE"
run_test "SMOKE-SUB"
run_test "SMOKE-MULTIPLY"
run_test "SMOKE-PERFORM"
run_test "SMOKE-SIGNED"
run_test "SMOKE-INIT"
run_test "SMOKE-NUMEDIT"
run_test "SMOKE-SEARCH"
run_test "SMOKE-FIGCONST"
run_test "SMOKE-DIVIDE"
run_test "SMOKE-SET"

echo ""
echo "=== Results ==="
echo "Pass: $PASS  Fail: $FAIL  Skip: $SKIP  Total: $((PASS + FAIL + SKIP))"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
