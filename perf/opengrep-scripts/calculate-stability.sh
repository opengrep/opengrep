#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only
#
# Calculate stability statistics for benchmark output files.
#
# Usage: ./calculate-stability.sh [OPTIONS] <directory>
#
# Analyzes multiple runs of the same tool on the same repo to measure
# determinism. A perfectly stable tool has std=0 and CV=0%.

set -euo pipefail

# Default configuration
intrafile_suffix=""
experimental_suffix=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --intrafile)
      intrafile_suffix="_intrafile"
      shift
      ;;
    --experimental)
      experimental_suffix="_experimental"
      shift
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS] <directory>"
      echo ""
      echo "Calculate stability statistics for benchmark output files."
      echo ""
      echo "Options:"
      echo "  --intrafile       Match files with _intrafile suffix"
      echo "  --experimental    Match files with _experimental suffix"
      echo "  -h, --help        Show this help message"
      exit 0
      ;;
    -*)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
    *)
      break
      ;;
  esac
done

if [[ $# -lt 1 ]]; then
    echo "Error: Directory argument required" >&2
    echo "Usage: $0 [OPTIONS] <directory>" >&2
    exit 1
fi

OUTPUT_DIR="$1"

if [[ ! -d "$OUTPUT_DIR" ]]; then
    echo "Error: Directory not found: $OUTPUT_DIR" >&2
    exit 1
fi

suffix="${intrafile_suffix}${experimental_suffix}"

# Print header
echo "Stability Analysis"
echo "=================="
echo "Directory: $OUTPUT_DIR"
echo "Suffix filter: ${suffix:-"(none)"}"
echo ""

# Function to calculate statistics using awk
calculate_stats() {
    local counts="$1"
    echo "$counts" | awk '
    BEGIN { n=0; sum=0; sumsq=0; min=""; max="" }
    {
        n++
        sum += $1
        sumsq += $1 * $1
        if (min == "" || $1 < min) min = $1
        if (max == "" || $1 > max) max = $1
    }
    END {
        if (n == 0) {
            print "0 0 0 0 0 0"
            exit
        }
        mean = sum / n
        if (n > 1) {
            variance = (sumsq - sum*sum/n) / (n-1)
            if (variance < 0) variance = 0  # floating point protection
            std = sqrt(variance)
        } else {
            std = 0
        }
        cv = (mean > 0) ? (std / mean * 100) : 0
        printf "%d %.1f %.1f %d %d %.2f\n", n, mean, std, min, max, cv
    }'
}

# Function to count findings in a JSON file
count_findings() {
    jq '.results | length' "$1" 2>/dev/null || echo "0"
}

# Collect all tool+repo combinations
declare -A tool_repo_files

# Enable nullglob so globs that match nothing expand to empty
shopt -s nullglob

# Find reference files (opengrep_ref_*)
for file in "$OUTPUT_DIR"/opengrep_ref_*"${suffix}"_[0-9]*.json; do
    [[ -f "$file" ]] || continue
    bname=$(basename "$file")
    # Extract repo name
    repo=$(echo "$bname" | sed 's/opengrep_ref_//' | sed "s/${suffix}_[0-9]\{6\}\.json\$//")
    key="opengrep (ref)|$repo"
    tool_repo_files["$key"]+="$file "
done

# Find opengrep comparison files (opengrep_* but not opengrep_ref_*)
for file in "$OUTPUT_DIR"/opengrep_*"${suffix}"_[0-9]*.json; do
    [[ -f "$file" ]] || continue
    bname=$(basename "$file")
    # Skip reference files
    [[ "$bname" == opengrep_ref_* ]] && continue
    # Extract repo name
    repo=$(echo "$bname" | sed 's/opengrep_//' | sed "s/${suffix}_[0-9]\{6\}\.json\$//")
    key="opengrep|$repo"
    tool_repo_files["$key"]+="$file "
done

# Find semgrep files
for file in "$OUTPUT_DIR"/semgrep_*"${suffix}"_[0-9]*.json; do
    [[ -f "$file" ]] || continue
    bname=$(basename "$file")
    # Extract repo name
    repo=$(echo "$bname" | sed 's/semgrep_//' | sed "s/${suffix}_[0-9]\{6\}\.json\$//")
    key="semgrep|$repo"
    tool_repo_files["$key"]+="$file "
done

shopt -u nullglob

if [[ ${#tool_repo_files[@]} -eq 0 ]]; then
    echo "No output files found in $OUTPUT_DIR for suffix '$suffix'" >&2
    exit 1
fi

# Process each tool+repo combination (sort keys for consistent output)
# Filter to only those with >1 run (can't measure stability with 1 sample)
mapfile -t sorted_keys < <(printf '%s\n' "${!tool_repo_files[@]}" | sort)
filtered_keys=()
for key in "${sorted_keys[@]}"; do
    files="${tool_repo_files[$key]}"
    file_count=$(echo $files | wc -w | tr -d ' ')
    if [[ "$file_count" -gt 1 ]]; then
        filtered_keys+=("$key")
    fi
done

if [[ ${#filtered_keys[@]} -eq 0 ]]; then
    echo "No tool/repo combinations with multiple runs found." >&2
    exit 1
fi

# Collect rows and format with column
{
    echo "Tool|Repo|Runs|Mean|Std|Min|Max|CV%"

    for key in "${filtered_keys[@]}"; do
        IFS='|' read -r tool repo <<< "$key"
        files="${tool_repo_files[$key]}"

        # Count findings in each file
        counts=""
        for file in $files; do
            count=$(count_findings "$file")
            counts+="$count"$'\n'
        done
        counts=$(echo "$counts" | grep -v '^$')

        # Calculate statistics
        read -r runs mean std min max cv <<< $(calculate_stats "$counts")

        # Format CV
        if [[ "$std" == "0.0" ]]; then
            cv_display="0.00%"
        else
            cv_display="${cv}%"
        fi

        printf "%s|%s|%d|%.1f|%.1f|%d|%d|%s\n" \
            "$tool" "$repo" "$runs" "$mean" "$std" "$min" "$max" "$cv_display"
    done
} | column -t -s'|' | awk 'NR==1 {print; gsub(/[^ ]/, "-"); print; next} {print}'

echo ""

# Summary
unstable=0
for key in "${filtered_keys[@]}"; do
    files="${tool_repo_files[$key]}"
    counts=""
    for file in $files; do
        counts+="$(count_findings "$file")"$'\n'
    done
    counts=$(echo "$counts" | grep -v '^$')
    read -r runs mean std min max cv <<< $(calculate_stats "$counts")
    [[ "$std" != "0.0" ]] && ((unstable++)) || true
done

total=${#filtered_keys[@]}
stable=$((total - unstable))

echo "Summary: $stable/$total stable (std=0), $unstable/$total unstable"
