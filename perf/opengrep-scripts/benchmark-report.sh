#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only
#
# Generate a comprehensive benchmark report combining timing, findings, and stability analysis.
#
# Usage: ./benchmark-report.sh <directory>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ $# -lt 1 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    echo "Usage: $0 <directory>"
    echo ""
    echo "Generate a comprehensive benchmark report including:"
    echo "  - Timing and memory summary (from hyperfine results)"
    echo "  - Findings comparison (reference vs comparison binary)"
    echo "  - Stability analysis (determinism across runs)"
    exit 0
fi

OUTPUT_DIR="$1"

if [[ ! -d "$OUTPUT_DIR" ]]; then
    echo "Error: Directory not found: $OUTPUT_DIR" >&2
    exit 1
fi

# Read metadata
metadata_file="$OUTPUT_DIR/metadata.json"
if [[ ! -f "$metadata_file" ]]; then
    echo "Error: metadata.json not found in $OUTPUT_DIR" >&2
    exit 1
fi

# Extract metadata
timestamp=$(jq -r '.timestamp // "unknown"' "$metadata_file")
ref_binary=$(jq -r '.reference_binary // "unknown"' "$metadata_file")
ref_version=$(jq -r '.reference_version // "unknown"' "$metadata_file")
cmp_binary=$(jq -r '.binary // "unknown"' "$metadata_file")
cmp_version=$(jq -r '.binary_version // "unknown"' "$metadata_file")
cmp_variant=$(jq -r '.binary_variant // "unknown"' "$metadata_file")
rules_dir=$(jq -r '.rules_dir // "unknown"' "$metadata_file")
num_cpus=$(jq -r '.num_cpus // "unknown"' "$metadata_file")
max_time=$(jq -r '.max_time_minutes // "unknown"' "$metadata_file")
experimental=$(jq -r '.experimental // ""' "$metadata_file")
intrafile=$(jq -r '.intrafile // ""' "$metadata_file")

# Build mode string
mode=""
[[ "$experimental" == "yes" ]] && mode="${mode}experimental "
[[ "$intrafile" == "yes" ]] && mode="${mode}intrafile "
mode="${mode:-standard}"

echo "BENCHMARK REPORT"
echo "================"
echo ""
echo "Directory:  $OUTPUT_DIR"
echo "Timestamp:  $timestamp"
echo "Mode:       $mode"
echo ""
echo "Reference:  $ref_binary ($ref_version)"
echo "Comparison: $cmp_binary ($cmp_version) [variant: $cmp_variant]"
echo ""
echo "Rules:      $rules_dir"
echo "CPUs:       $num_cpus"
echo "Max time:   ${max_time} minutes"
echo ""

echo "TIMING & MEMORY"
echo "---------------"
echo ""

# Run summarize-benchmark (it will auto-detect mode from metadata)
"$SCRIPT_DIR/summarise-benchmark.sh" "$OUTPUT_DIR" 2>/dev/null | tail -n +6

echo ""
echo "FINDINGS COMPARISON"
echo "-------------------"
echo ""

# Build args for compare-outputs
compare_args=(--output-dir "$OUTPUT_DIR")
[[ "$intrafile" == "yes" ]] && compare_args+=(--intrafile)
[[ "$experimental" == "yes" ]] && compare_args+=(--experimental)

"$SCRIPT_DIR/compare-outputs.sh" "${compare_args[@]}" 2>/dev/null || echo "(No comparison data available)"

echo ""
echo "STABILITY ANALYSIS"
echo "------------------"
echo ""

# Build args for calculate-stability
stability_args=()
[[ "$intrafile" == "yes" ]] && stability_args+=(--intrafile)
[[ "$experimental" == "yes" ]] && stability_args+=(--experimental)
stability_args+=("$OUTPUT_DIR")

"$SCRIPT_DIR/calculate-stability.sh" "${stability_args[@]}" 2>/dev/null | tail -n +6 || echo "(No stability data available - need multiple runs)"
