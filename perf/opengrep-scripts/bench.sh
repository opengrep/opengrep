#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only

set -eu

# Default configuration
reference_binary=""
binary=""
binary_variant="opengrep"
explicit_binary_variant=""
output_base="bench_results"
output_dir=""
run_name=""
dry_run=""
max_time="15"  # minutes
intrafile=""
intrafile_suffix=""
experimental=""
experimental_suffix=""
user_repos=""
taint_rules_only=""
no_taint_rules=""
extra_tool_args=""

# Parse arguments
while [ $# -gt 0 ]; do
  case "$1" in
    --intrafile)
      intrafile="yes"
      intrafile_suffix="_intrafile"
      shift
      ;;
    --experimental)
      experimental="yes"
      experimental_suffix="_experimental"
      shift
      ;;
    --reference-binary)
      reference_binary="$2"
      shift 2
      ;;
    --binary)
      binary="$2"
      shift 2
      ;;
    --binary-variant)
      binary_variant="$2"
      explicit_binary_variant="yes"
      shift 2
      ;;
    --output-base)
      output_base="$2"
      shift 2
      ;;
    --output-dir)
      output_dir="$2"
      shift 2
      ;;
    --name)
      run_name="$2"
      shift 2
      ;;
    --dry-run)
      dry_run="yes"
      shift
      ;;
    --max-time)
      max_time="$2"
      shift 2
      ;;
    --repo)
      if [ -z "$user_repos" ]; then
        user_repos="$2"
      else
        user_repos="$user_repos $2"
      fi
      shift 2
      ;;
    --taint-rules-only)
      taint_rules_only="yes"
      shift
      ;;
    --no-taint-rules)
      no_taint_rules="yes"
      shift
      ;;
    --)
      shift
      extra_tool_args="$*"
      break
      ;;
    -h|--help)
      echo "Usage: $0 --reference-binary PATH --binary PATH [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --reference-binary PATH  Path to reference opengrep binary (required)"
      echo "  --binary PATH            Path to binary to compare (required)"
      echo "  --binary-variant TYPE    Type of --binary: opengrep or semgrep (auto-detected from path)"
      echo "  --output-base DIR        Base directory for results (default: bench_results)"
      echo "  --output-dir DIR         Exact output directory (overrides --output-base and --name)"
      echo "  --name NAME              Optional name suffix for the run directory"
      echo "  --max-time MINS          Hard timeout per command in minutes (default: 15)"
      echo "  --dry-run                Print commands without executing them"
      echo "  --repo NAME[:RUNS]       Benchmark specific repo (can be repeated, default runs: 3)"
      echo "  --taint-rules-only       Use only taint rules (opengrep-rules-taint)"
      echo "  --no-taint-rules         Use only non-taint rules (opengrep-rules-non-taint)"
      echo "  --intrafile              Enable intrafile taint analysis"
      echo "  --experimental           Enable experimental mode"
      echo "  -h, --help               Show this help message"
      echo "  -- ARGS                  Pass additional arguments to both tools (e.g., -- --time)"
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
  esac
done

# Validate required arguments
if [ -z "$reference_binary" ]; then
  echo "Error: --reference-binary is required" >&2
  echo "Hint: ~/.local/bin/opengrep is a common location" >&2
  exit 1
fi

if [ -z "$binary" ]; then
  echo "Error: --binary is required" >&2
  exit 1
fi

# Auto-detect binary variant if not explicitly set
if [ -z "$explicit_binary_variant" ]; then
  case "$binary" in
    semgrep|*/semgrep)
      binary_variant="semgrep"
      ;;
  esac
fi

if [ "$binary_variant" != "opengrep" ] && [ "$binary_variant" != "semgrep" ]; then
  echo "Error: --binary-variant must be 'opengrep' or 'semgrep'" >&2
  exit 1
fi

if [ "$reference_binary" = "$binary" ]; then
  if [ -n "$dry_run" ]; then
    echo "Warning: --reference-binary and --binary are the same path" >&2
  else
    echo "Error: --reference-binary and --binary cannot be the same path" >&2
    exit 1
  fi
fi

# Validate output options
if [ -n "$output_dir" ] && { [ "$output_base" != "bench_results" ] || [ -n "$run_name" ]; }; then
  echo "Error: --output-dir cannot be used with --output-base or --name" >&2
  exit 1
fi

if [ -n "$output_dir" ] && [ -d "$output_dir" ]; then
  echo "Error: --output-dir '$output_dir' already exists" >&2
  exit 1
fi

# Validate dependencies
MIN_HYPERFINE_VERSION="1.20.0"
if ! command -v hyperfine &>/dev/null; then
  echo "Error: hyperfine not found. Install it from https://github.com/sharkdp/hyperfine" >&2
  exit 1
fi
hf_version=$(hyperfine --version | awk '{print $2}')
if printf '%s\n%s\n' "$MIN_HYPERFINE_VERSION" "$hf_version" | sort -V | head -1 | grep -qv "^${MIN_HYPERFINE_VERSION}$"; then
  echo "Error: hyperfine >= $MIN_HYPERFINE_VERSION required (found $hf_version)" >&2
  exit 1
fi

# Print configuration summary
rules_desc="all"
[ -n "$taint_rules_only" ] && rules_desc="taint only"
[ -n "$no_taint_rules" ] && rules_desc="non-taint only"

echo "Configuration:"
printf "  %-16s %s\n" "reference:" "$reference_binary"
printf "  %-16s %s (%s)\n" "binary:" "$binary" "$binary_variant"
printf "  %-16s %s minutes\n" "max-time:" "$max_time"
printf "  %-16s %s\n" "intrafile:" "$([ -n "$intrafile" ] && echo enabled || echo disabled)"
printf "  %-16s %s\n" "experimental:" "$([ -n "$experimental" ] && echo enabled || echo disabled)"
printf "  %-16s %s\n" "rules:" "$rules_desc"
[ -n "$run_name" ] && printf "  %-16s %s\n" "name:" "$run_name"
[ -n "$extra_tool_args" ] && printf "  %-16s %s\n" "extra args:" "$extra_tool_args"
echo ""

# Create output directory
run_timestamp=$(date +%Y%m%d_%H%M%S)
if [ -z "$output_dir" ]; then
  if [ -n "$run_name" ]; then
    output_dir="$output_base/${run_timestamp}_${run_name}"
  else
    output_dir="$output_base/$run_timestamp"
  fi
fi
if [ -z "$dry_run" ]; then
  mkdir -p "$output_dir"
fi
echo "Results will be saved to: $output_dir"

# Find timeout command (gtimeout on macOS with coreutils)
if command -v timeout >/dev/null 2>&1; then
  timeout_cmd="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
  timeout_cmd="gtimeout"
else
  echo "Error: timeout command not found (install coreutils)" >&2
  exit 1
fi

# Verify binaries exist
if ! command -v "$reference_binary" >/dev/null 2>&1; then
  echo "Error: reference binary not found: $reference_binary" >&2
  exit 1
fi

if ! command -v "$binary" >/dev/null 2>&1; then
  echo "Error: binary not found: $binary" >&2
  exit 1
fi

# Check semgrep login status for pro features (only relevant if binary is semgrep)
semgrep_logged_in=""
if [ "$binary_variant" = "semgrep" ]; then
  identity_output=$("$binary" show identity 2>&1 || true)
  if echo "$identity_output" | grep -q "SUCCESS  You are logged in"; then
    semgrep_logged_in="yes"
  fi

  if [ -n "$intrafile" ] && [ -z "$semgrep_logged_in" ]; then
    echo "Error: semgrep pro feature --pro-intrafile requires login" >&2
    echo "Run: $binary login" >&2
    exit 1
  fi

  if [ -z "$intrafile" ] && [ -z "$semgrep_logged_in" ]; then
    echo "Warning: semgrep not logged in, skipping --pro-languages flag" >&2
  fi
fi

# Determine rules directory based on taint flags
if [ -n "$taint_rules_only" ] && [ -n "$no_taint_rules" ]; then
  echo "Error: --taint-rules-only and --no-taint-rules are mutually exclusive" >&2
  exit 1
fi

rules_dir="opengrep-rules"
if [ -n "$taint_rules_only" ]; then
  rules_dir="${rules_dir}-taint"
elif [ -n "$no_taint_rules" ]; then
  rules_dir="${rules_dir}-non-taint"
fi
rules_dir="${rules_dir}/"

num_cpus=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo "4")

# Get versions
reference_version=$("$reference_binary" --version | head -n1 | awk '{print $NF}')
if [ "$binary_variant" = "semgrep" ]; then
  binary_version=$("$binary" --disable-version-check --version | head -n1 | awk '{print $NF}')
else
  binary_version=$("$binary" --version | head -n1 | awk '{print $NF}')
fi

# Write metadata
if [ -z "$dry_run" ]; then
  git_branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
  git_commit=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
  cat > "$output_dir/metadata.json" <<EOF
{
  "timestamp": "$run_timestamp",
  "name": "$run_name",
  "git_branch": "$git_branch",
  "git_commit": "$git_commit",
  "reference_binary": "$reference_binary",
  "reference_version": "$reference_version",
  "binary": "$binary",
  "binary_version": "$binary_version",
  "binary_variant": "$binary_variant",
  "rules_dir": "$rules_dir",
  "num_cpus": "$num_cpus",
  "max_time_minutes": $max_time,
  "intrafile": "$intrafile",
  "experimental": "$experimental",
  "extra_args": "$extra_tool_args"
}
EOF
fi

# Repositories: name:runs
if [ -n "$user_repos" ]; then
  repos=()
  for r in $user_repos; do
    # Add default runs count if not specified
    case "$r" in
      *:*) repos+=("$r") ;;
      *)   repos+=("$r:3") ;;
    esac
  done
else
  repos=(
    "jellyfin:3"
    "lemur:3"
    "ComfyUI-Custom-Scripts:3"
    "pmd:3"
    "leakcanary:3"
    "grafana:1"
    "gitlab:1"
  )
fi

# Benchmark each repository
for repo_config in "${repos[@]}"; do
  IFS=':' read -r repo runs <<< "$repo_config"

  echo ""
  echo ""
  echo "Benchmarking $repo..."
  echo ""

  common_params="-c $rules_dir -j $num_cpus --timeout 0 --json --quiet $extra_tool_args"

  # Reference binary is always opengrep
  reference_extra=""
  if [ -n "$intrafile" ]; then
    reference_extra="$reference_extra --taint-intrafile"
  fi
  if [ -n "$experimental" ]; then
    reference_extra="$reference_extra --experimental"
  fi

  # Binary flags depend on variant
  binary_extra=""
  binary_prefix=""
  if [ "$binary_variant" = "semgrep" ]; then
    binary_prefix="--metrics off --disable-version-check"
    if [ -n "$intrafile" ]; then
      binary_extra="$binary_extra --pro-intrafile"
    elif [ -n "$semgrep_logged_in" ]; then
      binary_extra="$binary_extra --pro-languages"
    fi
    if [ -n "$experimental" ]; then
      binary_extra="$binary_extra --experimental"
    fi
  else
    # opengrep variant
    if [ -n "$intrafile" ]; then
      binary_extra="$binary_extra --taint-intrafile"
    fi
    if [ -n "$experimental" ]; then
      binary_extra="$binary_extra --experimental"
    fi
  fi

  suffix="${intrafile_suffix}${experimental_suffix}"

  reference_cmd="$timeout_cmd ${max_time}m $reference_binary scan $common_params $reference_extra repos/$repo \
    1> $output_dir/opengrep_ref_${repo}${suffix}_\$(date +%H%M%S).json; \
    ret=\$?; [ \$ret -eq 124 ] && echo 'TIMEOUT: opengrep (ref) exceeded ${max_time} minutes on ${repo}' >&2; exit \$ret"

  binary_cmd="$timeout_cmd ${max_time}m $binary scan $binary_prefix $common_params $binary_extra repos/$repo \
    1> $output_dir/${binary_variant}_${repo}${suffix}_\$(date +%H%M%S).json; \
    ret=\$?; [ \$ret -eq 124 ] && echo 'TIMEOUT: ${binary_variant} exceeded ${max_time} minutes on ${repo}' >&2; exit \$ret"

  if [ -n "$dry_run" ]; then
    echo "hyperfine \\"
    echo "  --runs $runs \\"
    echo "  --conclude \\"
    echo "  'echo \"Sleeping for 30 seconds between runs to let system cool down...\"; sleep 30' \\"
    echo "  --export-json $output_dir/results_${repo}${suffix}.json \\"
    echo "  --export-markdown $output_dir/results_${repo}${suffix}.md \\"
    echo "  --reference \\"
    echo "  '$reference_cmd' \\"
    echo "  --reference-name opengrep-$reference_version \\"
    echo "  --command-name $binary_variant-$binary_version \\"
    echo "  '$binary_cmd'"
  else
    hyperfine \
      --runs "$runs" \
      --conclude \
      'echo "Sleeping for 30 seconds between runs to let system cool down..."; sleep 30' \
      --export-json "$output_dir/results_${repo}${suffix}.json" \
      --export-markdown "$output_dir/results_${repo}${suffix}.md" \
      --reference \
      "$reference_cmd" \
      --reference-name "opengrep-$reference_version" \
      --command-name "$binary_variant-$binary_version" \
      "$binary_cmd"

    # Format output JSON files
    echo "Formatting output JSON files..."
    for json_file in "$output_dir"/opengrep_ref_${repo}${suffix}_*.json "$output_dir"/${binary_variant}_${repo}${suffix}_*.json; do
      [ -f "$json_file" ] && jq . "$json_file" > "${json_file}.tmp" && mv "${json_file}.tmp" "$json_file"
    done
  fi

done
