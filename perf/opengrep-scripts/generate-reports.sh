#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only
#
# Generate benchmark reports for all configurations in a results directory.
#
# Usage: ./generate-reports.sh [directory]
#
# If no directory is specified, uses the most recent bench_results directory.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

if [[ $# -ge 1 ]]; then
    if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
        echo "Usage: $0 [directory]"
        echo ""
        echo "Generate benchmark reports for all configurations in a results directory."
        echo "If no directory is specified, uses the most recent bench_results directory."
        exit 0
    fi
    run_dir="$1"
else
    # Find most recent benchmark results directory
    run_dir=$(ls -dt bench_results/*/ 2>/dev/null | head -1)
    if [[ -z "$run_dir" ]]; then
        echo "Error: No benchmark results found in bench_results/" >&2
        exit 1
    fi
    # Remove trailing slash
    run_dir="${run_dir%/}"
fi

if [[ ! -d "$run_dir" ]]; then
    echo "Error: Directory not found: $run_dir" >&2
    exit 1
fi

echo "Generating reports for: $run_dir"
echo

# Check if this is a multi-config directory (has subdirectories) or single config
subdirs=("$run_dir"/*/)
if [[ -d "${subdirs[0]}" ]]; then
    # Multi-config: iterate over subdirectories
    for config_dir in "$run_dir"/*/; do
        config_name=$(basename "$config_dir")
        echo "--- Report: $config_name ---"
        ./benchmark-report.sh "$config_dir"
        echo
    done
else
    # Single config: run directly on the directory
    echo "--- Report ---"
    ./benchmark-report.sh "$run_dir"
    echo
fi

echo "=== Reports complete ==="
