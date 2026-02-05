#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only
#
# Summarize benchmark timing and memory results from hyperfine output.
#
# Usage: ./summarize-benchmark.sh [OPTIONS] <directory>

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
      echo "Summarize benchmark timing and memory results from hyperfine JSON output."
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
suffix="${intrafile_suffix}${experimental_suffix}"

if [[ ! -d "$OUTPUT_DIR" ]]; then
    echo "Error: Directory not found: $OUTPUT_DIR" >&2
    exit 1
fi

# Auto-detect mode from metadata.json if not specified via flags
metadata_file="$OUTPUT_DIR/metadata.json"
if [[ -z "$suffix" ]] && [[ -f "$metadata_file" ]]; then
    if [[ "$(jq -r '.experimental // ""' "$metadata_file")" == "yes" ]]; then
        experimental_suffix="_experimental"
    fi
    if [[ "$(jq -r '.intrafile // ""' "$metadata_file")" == "yes" ]]; then
        intrafile_suffix="_intrafile"
    fi
    suffix="${intrafile_suffix}${experimental_suffix}"
fi

# Find results JSON files matching suffix
shopt -s nullglob
if [[ -n "$suffix" ]]; then
    result_files=("$OUTPUT_DIR"/results_*"${suffix}".json)
else
    result_files=("$OUTPUT_DIR"/results_*.json)
fi
shopt -u nullglob

if [[ ${#result_files[@]} -eq 0 ]]; then
    echo "Error: No results_*.json files found in $OUTPUT_DIR for suffix '$suffix'" >&2
    exit 1
fi

# Build mode description
mode_desc=""
[[ -n "$experimental_suffix" ]] && mode_desc="${mode_desc}experimental "
[[ -n "$intrafile_suffix" ]] && mode_desc="${mode_desc}intrafile "
mode_desc="${mode_desc:-standard }"

echo "Benchmark Summary"
echo "================="
echo "Directory: $OUTPUT_DIR"
echo "Mode: ${mode_desc}"
echo ""

# Collect all rows
{
    echo "Repo|Command|Mean(s)|±|Min|Max|Relative|Mem(MB)|Mem±"

    for result_file in "${result_files[@]}"; do
        # Extract repo name from filename: results_<repo>.json or results_<repo>_<suffix>.json
        filename=$(basename "$result_file" .json)
        repo=${filename#results_}
        # Strip suffix from repo name
        [[ -n "$suffix" ]] && repo=${repo%"$suffix"}

        # Get reference (first command) mean for relative calculation
        ref_mean=$(jq -r '.results[0].mean' "$result_file")

        # Process each command in the results
        jq -r --arg repo "$repo" --argjson ref_mean "$ref_mean" '
            .results[] |
            # Handle null stddev (single run)
            (.stddev // 0) as $stddev |
            # Calculate memory stats
            (.memory_usage_byte | add / length / 1048576) as $mem_mean |
            (.memory_usage_byte | length) as $mem_len |
            (if $mem_len > 1 then
                (.memory_usage_byte |
                    (add / length) as $mean |
                    (map(. - $mean | . * .) | add / length | sqrt) / 1048576)
            else 0 end) as $mem_std |
            # Calculate relative
            (.mean / $ref_mean) as $rel |
            # Format output
            "\($repo)|\(.command)|\(.mean | . * 1000 | round / 1000)|\($stddev | . * 1000 | round / 1000)|\(.min | . * 1000 | round / 1000)|\(.max | . * 1000 | round / 1000)|\($rel | . * 100 | round / 100)|\($mem_mean | round)|\($mem_std | . * 10 | round / 10)"
        ' "$result_file"
    done
} | column -t -s'|' | awk 'NR==1 {print; gsub(/[^ ] /, "--"); gsub(/[^ ]/, "-"); print; next} {print}'

echo ""

# Print totals/averages per command
echo "Aggregate by Command"
echo "--------------------"
{
    echo "Command|TotalTime(s)|AvgMem(MB)|Repos"

    # Collect all data and aggregate
    for result_file in "${result_files[@]}"; do
        jq -r '
            .results[] |
            "\(.command)|\(.mean)|\(.memory_usage_byte | add / length / 1048576)"
        ' "$result_file"
    done | awk -F'|' '
    {
        cmd = $1
        time[cmd] += $2
        mem[cmd] += $3
        count[cmd]++
    }
    END {
        for (cmd in time) {
            printf "%s|%.2f|%.0f|%d\n", cmd, time[cmd], mem[cmd]/count[cmd], count[cmd]
        }
    }' | sort
} | column -t -s'|' | awk 'NR==1 {print; gsub(/[^ ] /, "--"); gsub(/[^ ]/, "-"); print; next} {print}'
