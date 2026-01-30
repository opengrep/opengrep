#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only
#
# Main benchmark script: prepares repos and rules, then runs bench.sh.
# All arguments are forwarded to bench.sh.
#
# Usage: ./run_benchmarks.sh [bench.sh options]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check if --dry-run is in arguments
dry_run=""
for arg in "$@"; do
  if [[ "$arg" == "--dry-run" ]]; then
    dry_run="yes"
    break
  fi
done

if [[ -z "$dry_run" ]]; then
  # Download benchmark repositories
  echo "=== Preparing benchmark repositories ==="
  ./download-bench-repos.sh
  echo

  # Download and prepare rules
  echo "=== Preparing rules ==="
  if [[ -d "opengrep-rules/.git" ]]; then
    echo "opengrep-rules: already present"
  else
    echo "Cloning opengrep-rules..."
    git clone git@github.com:opengrep/opengrep-rules.git

    echo "Removing non-rule files..."
    ./remove-non-rule-files.sh
  fi
  echo

  # Create taint rules directory
  echo "=== Preparing taint rules ==="
  if [[ -d "opengrep-rules-taint" ]]; then
    echo "opengrep-rules-taint: already present"
  else
    echo "Extracting taint rules..."
    ./copy-taint-rules.sh
  fi
  echo

  # Create non-taint rules directory
  echo "=== Preparing non-taint rules ==="
  if [[ -d "opengrep-rules-non-taint" ]]; then
    echo "opengrep-rules-non-taint: already present"
  else
    echo "Extracting non-taint rules..."
    ./copy-non-taint-rules.sh
  fi
  echo
fi

# Create timestamped run directory
run_timestamp=$(date +%Y%m%d_%H%M%S)
run_dir="bench_results/$run_timestamp"
if [[ -z "$dry_run" ]]; then
  mkdir -p "$run_dir"
fi
echo "=== Results will be saved to: $run_dir ==="
echo

# Run benchmarks for each configuration
echo "=== Running benchmarks: taint rules (no intrafile) ==="
./bench.sh --output-dir "$run_dir/no_intrafile_taint" --taint-rules-only "$@"
echo

echo "=== Running benchmarks: search rules (no intrafile) ==="
./bench.sh --output-dir "$run_dir/no_intrafile_search" --no-taint-rules "$@"
echo

echo "=== Running benchmarks: taint rules (intrafile) ==="
./bench.sh --output-dir "$run_dir/intrafile_taint" --intrafile --taint-rules-only "$@"
echo

echo "=== All benchmarks complete ==="
echo "Results saved to: $run_dir"
