#!/bin/bash
#
# Benchmark script for comparing opengrep vs semgrep taint analysis
# Usage: ./benchmark_taint.sh [--no-semgrep] <yaml_file_or_dir> <target_file_or_dir> [num_runs] [sha1] [sha2] ...
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OPENGREP_BIN="$SCRIPT_DIR/bin/opengrep"

# Check for --no-semgrep flag anywhere in args
NO_SEMGREP=false
ARGS=()
for arg in "$@"; do
    if [[ "$arg" == "--no-semgrep" ]]; then
        NO_SEMGREP=true
    else
        ARGS+=("$arg")
    fi
done
set -- "${ARGS[@]}"

YAML_FILE="$1"
TARGET_FILE="$2"
NUM_RUNS="${3:-3}"

if [[ -z "$YAML_FILE" || -z "$TARGET_FILE" ]]; then
    echo "Usage: $0 [--no-semgrep] <yaml_file_or_dir> <target_file_or_dir> [num_runs] [sha1] [sha2] ..."
    exit 1
fi

# Collect SHAs (arguments after num_runs)
shift 3 2>/dev/null || true
SHAS=("$@")

# Arrays to store all results for final table
declare -a result_labels result_findings result_times

# Function to run and extract findings + time
run_benchmark() {
    local label="$1"
    local cmd="$2"
    local findings_list=""
    local times_list=""

    echo "=== $label ==="
    for i in $(seq 1 $NUM_RUNS); do
        # Run with time, capture output
        output=$( { time $cmd 2>&1; } 2>&1 )

        # Extract findings count (grep for "findings" line)
        findings=$(echo "$output" | grep -oE '[0-9]+ findings' | grep -oE '[0-9]+' | head -1 || echo "0")

        # Extract real time
        real_time=$(echo "$output" | grep "^real" | awk '{print $2}' | sed 's/m/*60+/;s/s//' | bc)

        echo "  Run $i: $findings findings, ${real_time}s"
        findings_list="$findings_list $findings"
        times_list="$times_list $real_time"
    done

    # Compute averages and std
    avg_findings=$(echo $findings_list | tr ' ' '\n' | grep -v '^$' | awk '{s+=$1; n++} END {printf "%.1f", s/n}')
    std_findings=$(echo $findings_list | tr ' ' '\n' | grep -v '^$' | awk '{for(i=1;i<=NF;i++){s+=$i; ss+=$i*$i; n++}} END {if(n>1) printf "%.1f", sqrt((ss-s*s/n)/(n-1)); else print "0"}')
    avg_time=$(echo $times_list | tr ' ' '\n' | grep -v '^$' | awk '{s+=$1; n++} END {printf "%.2f", s/n}')
    std_time=$(echo $times_list | tr ' ' '\n' | grep -v '^$' | awk '{for(i=1;i<=NF;i++){s+=$i; ss+=$i*$i; n++}} END {if(n>1) printf "%.2f", sqrt((ss-s*s/n)/(n-1)); else print "0"}')

    # Store for final table
    result_labels+=("$label")
    result_findings+=("$avg_findings ± $std_findings")
    result_times+=("$avg_time ± $std_time")
    echo ""
}

echo ""
echo "Benchmarking: $YAML_FILE on $TARGET_FILE"
echo "Runs: $NUM_RUNS"
echo ""

# Run Semgrep (unless --no-semgrep flag is set)
if [[ "$NO_SEMGREP" == "false" ]]; then
    run_benchmark "Semgrep (no intrafile)" "semgrep -f $YAML_FILE $TARGET_FILE --timeout 9"
    run_benchmark "Semgrep (pro-intrafile)" "semgrep -f $YAML_FILE $TARGET_FILE --timeout 9 --pro-intrafile"
else
    echo "Skipping Semgrep benchmarks (--no-semgrep flag set)"
    echo ""
fi

# Save current branch
ORIGINAL_REF=$(git -C "$SCRIPT_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || git -C "$SCRIPT_DIR" rev-parse HEAD)

# If no SHAs provided, just run current branch
# If SHAs provided, always include current as well
if [[ ${#SHAS[@]} -eq 0 ]]; then
    SHAS=("current")
else
    # Prepend current to compare against
    SHAS=("current" "${SHAS[@]}")
fi

for sha in "${SHAS[@]}"; do
    if [[ "$sha" != "current" ]]; then
        echo "========================================"
        echo "Checking out: $sha"
        echo "========================================"
        git -C "$SCRIPT_DIR" checkout "$sha" --quiet
    fi

    # Compile opengrep
    echo "Compiling opengrep..."
    (cd "$SCRIPT_DIR" && pipenv run make core && pipenv run make install) >/dev/null 2>&1
    echo "Compilation done."
    echo ""

    # Get label
    if [[ "$sha" == "current" ]]; then
        label="$ORIGINAL_REF"
    else
        label=$(git -C "$SCRIPT_DIR" rev-parse --short "$sha" 2>/dev/null || echo "$sha")
    fi

    run_benchmark "OpenGrep [$label] (no intra)" "$OPENGREP_BIN -f $YAML_FILE $TARGET_FILE --timeout 0"
    run_benchmark "OpenGrep [$label] (intrafile)" "$OPENGREP_BIN -f $YAML_FILE $TARGET_FILE --timeout 0 --taint-intrafile"
done

# Return to original branch
if [[ "$ORIGINAL_REF" != "HEAD" ]]; then
    git -C "$SCRIPT_DIR" checkout "$ORIGINAL_REF" --quiet
fi

# Print final summary table
echo "================================================================================"
echo "SUMMARY (averages over $NUM_RUNS runs)"
echo "================================================================================"
printf "%-35s %15s %15s\n" "Configuration" "Findings" "Time (s)"
echo "--------------------------------------------------------------------------------"
for i in "${!result_labels[@]}"; do
    printf "%-35s %15s %15s\n" "${result_labels[$i]}" "${result_findings[$i]}" "${result_times[$i]}"
done
echo "================================================================================"
