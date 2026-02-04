#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only
#
# Compare benchmark output files and produce a summary table.
# Usage: ./compare-outputs.sh [OPTIONS]
#
# Expects files named: opengrep_ref_<repo>[_suffix]_<timestamp>.json (reference)
#                      <variant>_<repo>[_suffix]_<timestamp>.json (comparison)
# where <variant> is 'opengrep' or 'semgrep'

set -euo pipefail

# Default configuration
OUTPUT_DIR="."
TOLERANCE="${TOLERANCE:-8}"
intrafile_suffix=""
experimental_suffix=""
comparison_variant=""

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
    --output-dir)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --variant)
      comparison_variant="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --output-dir DIR  Directory containing output files (default: .)"
      echo "  --variant TYPE    Comparison variant: opengrep or semgrep (auto-detected if not specified)"
      echo "  --intrafile       Match files with _intrafile suffix"
      echo "  --experimental    Match files with _experimental suffix"
      echo "  -h, --help        Show this help message"
      echo ""
      echo "Environment variables:"
      echo "  TOLERANCE         Line tolerance for proximity matching (default: 8)"
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
  esac
done

suffix="${intrafile_suffix}${experimental_suffix}"

# Auto-detect comparison variant if not specified
if [[ -z "$comparison_variant" ]]; then
    if ls "$OUTPUT_DIR"/semgrep_*"${suffix}"_[0-9]*.json &>/dev/null; then
        comparison_variant="semgrep"
    elif ls "$OUTPUT_DIR"/opengrep_[!r]*"${suffix}"_[0-9]*.json &>/dev/null; then
        # Match opengrep_* but not opengrep_ref_*
        comparison_variant="opengrep"
    else
        echo "Error: Could not auto-detect comparison variant. Use --variant." >&2
        exit 1
    fi
fi

# Capitalize first letter for headers
variant_label="${comparison_variant^}"

# Find all unique repo names from reference files (filtering by suffix if specified)
repos=$(ls "$OUTPUT_DIR"/opengrep_ref_*"${suffix}"_[0-9]*.json 2>/dev/null \
    | xargs -n1 basename 2>/dev/null \
    | sed 's/opengrep_ref_//' \
    | sed "s/${suffix}_[0-9]\{6\}\.json\$//" \
    | sort -u) || true

if [[ -z "$repos" ]]; then
    echo "No reference files found in $OUTPUT_DIR for suffix '$suffix'" >&2
    exit 1
fi

# Collect rows and path coverage warnings
path_warnings=""

{
    echo "Repo|Reference|${variant_label}|Exact|Proximity|Only Ref|Only ${variant_label}"

    for repo in $repos; do
        # Get first file for each tool
        ref_file=$(ls "$OUTPUT_DIR"/opengrep_ref_"${repo}${suffix}"_*.json 2>/dev/null | head -1)
        cmp_file=$(ls "$OUTPUT_DIR"/"${comparison_variant}_${repo}${suffix}"_*.json 2>/dev/null | head -1)

        if [[ -z "$ref_file" || -z "$cmp_file" ]]; then
            echo "# Skipping $repo: missing output file" >&2
            continue
        fi

        # Get totals from source files
        ref_total=$(jq '.results | length' "$ref_file")
        cmp_total=$(jq '.results | length' "$cmp_file")

        # Run comparison and extract stats via JSON
        cmp_json=$(opengrep-diff "$ref_file" "$cmp_file" -t "$TOLERANCE" --json)
        exact=$(jq '.exact_match_count' <<< "$cmp_json")
        proximity=$(jq '.proximity_match_count' <<< "$cmp_json")
        only_ref=$(jq '.only_in_first_count' <<< "$cmp_json")
        only_cmp=$(jq '.only_in_second_count' <<< "$cmp_json")

        # Check path coverage
        only_ref_paths=$(jq '.path_coverage.only_scanned_by_first' <<< "$cmp_json")
        only_cmp_paths=$(jq '.path_coverage.only_scanned_by_second' <<< "$cmp_json")
        if [[ "$only_ref_paths" -gt 0 || "$only_cmp_paths" -gt 0 ]]; then
            path_warnings="${path_warnings}${repo}: ref scanned ${only_ref_paths} unique paths, ${comparison_variant} scanned ${only_cmp_paths} unique paths\n"
        fi

        echo "${repo}|${ref_total}|${cmp_total}|${exact}|${proximity}|${only_ref}|${only_cmp}"
    done
} | column -t -s'|' | awk 'NR==1 {print; gsub(/[^ ] /, "--"); gsub(/[^ ]/, "-"); print; next} {print}'

# Print path coverage warnings if any
if [[ -n "$path_warnings" ]]; then
    echo ""
    echo "Path coverage differences detected:"
    echo -e "$path_warnings"
fi
