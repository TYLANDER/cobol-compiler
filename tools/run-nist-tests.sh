#!/usr/bin/env bash
# Run NIST CCVS tests against cobolc
#
# Usage:
#   ./tools/run-nist-tests.sh [--module NC] [--filter NC101] [--verbose] [--json]
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CARGO="${CARGO:-cargo}"
SUITE_DIR="${SUITE_DIR:-$PROJECT_ROOT/tests/nist}"
KNOWN_FAILURES="${KNOWN_FAILURES:-$SUITE_DIR/known_failures.toml}"

# Parse arguments to pass through to nist-runner
EXTRA_ARGS=()
FORMAT="text"
for arg in "$@"; do
    if [ "$arg" = "--json" ]; then
        FORMAT="json"
    else
        EXTRA_ARGS+=("$arg")
    fi
done

echo "=== Building cobolc ==="
$CARGO build --bin cobolc --quiet 2>&1

echo "=== Building nist-runner ==="
$CARGO build --bin nist-runner --quiet 2>&1

# Determine the target directory
TARGET_DIR="$PROJECT_ROOT/target/debug"
if [ ! -f "$TARGET_DIR/cobolc" ]; then
    # Try release
    TARGET_DIR="$PROJECT_ROOT/target/release"
fi

COBOLC="$TARGET_DIR/cobolc"
NIST_RUNNER="$TARGET_DIR/nist-runner"

if [ ! -f "$COBOLC" ]; then
    echo "ERROR: Could not find cobolc binary at $COBOLC"
    echo "Falling back to wrapper script..."
    COBOLC_WRAPPER=$(mktemp)
    cat > "$COBOLC_WRAPPER" <<WRAPPER
#!/usr/bin/env bash
exec $CARGO run --quiet --bin cobolc -- "\$@"
WRAPPER
    chmod +x "$COBOLC_WRAPPER"
    COBOLC="$COBOLC_WRAPPER"
    trap "rm -f '$COBOLC_WRAPPER'" EXIT
fi

echo "=== Running NIST CCVS Tests ==="
echo "Compiler: $COBOLC"
echo ""

"$NIST_RUNNER" \
    --suite-dir "$SUITE_DIR" \
    --compiler "$COBOLC" \
    --known-failures "$KNOWN_FAILURES" \
    --format "$FORMAT" \
    "${EXTRA_ARGS[@]}"
