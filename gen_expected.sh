#!/usr/bin/env bash
set -uo pipefail

PROJECT="/Users/tylerschmidt/Projects/cobol-compiler"
COBOLC="$PROJECT/target/debug/cobolc"
NIST_DIR="$PROJECT/tests/nist"
EXPECTED_DIR="$NIST_DIR/expected"
COPYBOOK_DIR="$NIST_DIR/copybooks"

mkdir -p "$EXPECTED_DIR"

GENERATED=0
SKIPPED_COMPILE=0
SKIPPED_TIMEOUT=0
SKIPPED_EXISTING=0
SKIPPED_OTHER=0
TOTAL=0

SKIP_TESTS="SQ103A SQ104A SQ105A"

for f in "$NIST_DIR"/*.cob; do
    name=$(basename "$f" .cob)

    [[ "$name" == *-SUB* ]] && continue

    TOTAL=$((TOTAL + 1))

    for skip in $SKIP_TESTS; do
        if [ "$name" = "$skip" ]; then
            SKIPPED_OTHER=$((SKIPPED_OTHER + 1))
            continue 2
        fi
    done

    if [ -f "$EXPECTED_DIR/${name}.txt" ]; then
        SKIPPED_EXISTING=$((SKIPPED_EXISTING + 1))
        continue
    fi

    output_dir=$(mktemp -d)
    binary="$output_dir/$name"

    compile_args=("$f")

    if [[ "$name" == IC* ]]; then
        for sub_file in "$NIST_DIR/${name}"-SUB*.cob; do
            [ -f "$sub_file" ] && compile_args+=("$sub_file")
        done
    fi

    if [[ "$name" == SM* ]] && [ -d "$COPYBOOK_DIR" ]; then
        compile_args+=("-I" "$COPYBOOK_DIR")
    fi

    compile_args+=("-o" "$binary")

    if ! "$COBOLC" "${compile_args[@]}" 2>"$output_dir/compile.err"; then
        echo "COMPILE_FAIL: $name"
        SKIPPED_COMPILE=$((SKIPPED_COMPILE + 1))
        rm -rf "$output_dir"
        continue
    fi

    exit_code=0
    actual=$(timeout 10 "$binary" 2>&1) || exit_code=$?

    if [ "$exit_code" -eq 124 ]; then
        echo "TIMEOUT: $name"
        SKIPPED_TIMEOUT=$((SKIPPED_TIMEOUT + 1))
        rm -rf "$output_dir"
        continue
    fi

    pass_count=$(echo "$actual" | grep -c "PASS" || true)
    if [ "$pass_count" -gt 0 ]; then
        echo "$actual" > "$EXPECTED_DIR/${name}.txt"
        GENERATED=$((GENERATED + 1))
        echo "GENERATED: $name ($pass_count PASS lines)"
    else
        echo "NO_OUTPUT: $name (no PASS lines, skipping)"
        SKIPPED_OTHER=$((SKIPPED_OTHER + 1))
    fi

    rm -rf "$output_dir"
done

echo ""
echo "========================================="
echo "  Expected Output Generation Summary"
echo "========================================="
echo "Total .cob files (non-SUB):  $TOTAL"
echo "Already had expected output:  $SKIPPED_EXISTING"
echo "Newly generated:              $GENERATED"
echo "Skipped (compile failure):    $SKIPPED_COMPILE"
echo "Skipped (timeout):            $SKIPPED_TIMEOUT"
echo "Skipped (other):              $SKIPPED_OTHER"
echo "========================================="
total_now=$(ls "$EXPECTED_DIR"/*.txt 2>/dev/null | wc -l | tr -d ' ')
echo "Total expected files now:     $total_now"
