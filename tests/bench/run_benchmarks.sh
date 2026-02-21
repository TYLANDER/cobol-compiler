#!/usr/bin/env bash
# Runtime benchmark comparison: cobolc vs GnuCOBOL
# Usage: ./run_benchmarks.sh [cobolc-path] [gnucobol-path]
#
# Compiles and runs each benchmark with both compilers,
# measuring wall-clock execution time via /usr/bin/time.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COBOLC="${1:-$(dirname "$SCRIPT_DIR")/../../target/release/cobolc}"
GNUCOBC="${2:-cobc}"
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

BENCHMARKS=(
    BENCH-ARITH
    BENCH-MOVE
    BENCH-DECIMAL
    BENCH-PERFORM
    BENCH-COMPARE
    BENCH-TABLE
    BENCH-STRING
    BENCH-INTRINSIC
)

# Check for GnuCOBOL
HAS_GNUCOBOL=false
if command -v "$GNUCOBC" &>/dev/null; then
    HAS_GNUCOBOL=true
fi

printf "${BOLD}%-18s %12s %12s %10s${NC}\n" "Benchmark" "cobolc (ms)" "GnuCOBOL (ms)" "Speedup"
printf "%-18s %12s %12s %10s\n" "------------------" "------------" "-------------" "----------"

COBOLC_WINS=0
TOTAL_BENCHMARKS=0

for BENCH in "${BENCHMARKS[@]}"; do
    SRC="$SCRIPT_DIR/${BENCH}.cob"
    if [ ! -f "$SRC" ]; then
        printf "%-18s %12s %12s %10s\n" "$BENCH" "SKIP" "SKIP" "-"
        continue
    fi

    # Compile with cobolc
    COBOLC_BIN="$TMPDIR/${BENCH}-cobolc"
    if "$COBOLC" "$SRC" -o "$COBOLC_BIN" 2>/dev/null; then
        # Run 3 times, take the median
        COBOLC_TIMES=()
        for _ in 1 2 3; do
            START=$(python3 -c "import time; print(int(time.time_ns()))")
            "$COBOLC_BIN" >/dev/null 2>&1 || true
            END=$(python3 -c "import time; print(int(time.time_ns()))")
            MS=$(( (END - START) / 1000000 ))
            COBOLC_TIMES+=("$MS")
        done
        # Sort and take median
        IFS=$'\n' SORTED_C=($(sort -n <<<"${COBOLC_TIMES[*]}")); unset IFS
        COBOLC_MS="${SORTED_C[1]}"
    else
        COBOLC_MS="FAIL"
    fi

    # Compile with GnuCOBOL
    GNU_MS="N/A"
    if $HAS_GNUCOBOL; then
        GNU_BIN="$TMPDIR/${BENCH}-gnu"
        if "$GNUCOBC" -x -o "$GNU_BIN" "$SRC" 2>/dev/null; then
            GNU_TIMES=()
            for _ in 1 2 3; do
                START=$(python3 -c "import time; print(int(time.time_ns()))")
                "$GNU_BIN" >/dev/null 2>&1 || true
                END=$(python3 -c "import time; print(int(time.time_ns()))")
                MS=$(( (END - START) / 1000000 ))
                GNU_TIMES+=("$MS")
            done
            IFS=$'\n' SORTED_G=($(sort -n <<<"${GNU_TIMES[*]}")); unset IFS
            GNU_MS="${SORTED_G[1]}"
        else
            GNU_MS="FAIL"
        fi
    fi

    # Calculate speedup
    SPEEDUP="-"
    if [[ "$COBOLC_MS" != "FAIL" ]] && [[ "$GNU_MS" != "N/A" ]] && [[ "$GNU_MS" != "FAIL" ]]; then
        TOTAL_BENCHMARKS=$((TOTAL_BENCHMARKS + 1))
        if [ "$COBOLC_MS" -gt 0 ]; then
            SPEEDUP_RAW=$(python3 -c "print(f'{$GNU_MS / $COBOLC_MS:.2f}x')")
            if [ "$COBOLC_MS" -le "$GNU_MS" ]; then
                SPEEDUP="${GREEN}${SPEEDUP_RAW}${NC}"
                COBOLC_WINS=$((COBOLC_WINS + 1))
            else
                SPEEDUP="${RED}${SPEEDUP_RAW}${NC}"
            fi
        fi
    fi

    printf "%-18s %12s %12s %10b\n" "$BENCH" "$COBOLC_MS" "$GNU_MS" "$SPEEDUP"
done

echo ""
printf "${BOLD}=== Summary ===${NC}\n"
if $HAS_GNUCOBOL; then
    printf "cobolc wins: %d / %d benchmarks\n" "$COBOLC_WINS" "$TOTAL_BENCHMARKS"
else
    printf "${YELLOW}GnuCOBOL not installed — showing cobolc times only${NC}\n"
    echo "Install GnuCOBOL (brew install gnu-cobol) to compare."
fi
