#!/usr/bin/env bash
# Copyright (c) 2026 Opengrep Authors
#
# SPDX-License-Identifier: LGPL-2.1-only
#
# Benchmark current HEAD against a reference commit.
#
# Usage: ./bench-against-sha.sh <reference-sha> [bench.sh options]
#
# The reference SHA (typically a tag) is built once and cached.
# HEAD is always rebuilt to capture current changes.
#
# Example:
#   ./bench-against-sha.sh v1.15.0 --repo jellyfin:1
#   ./bench-against-sha.sh v1.14.0 --repo jellyfin:3 --taint-rules-only
#   ./bench-against-sha.sh v1.15.0 --dry-run --repo jellyfin:1

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OPENGREP_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BINARIES_DIR="$SCRIPT_DIR/binaries"
RESTORE_FILE="$SCRIPT_DIR/.bench-against-sha-restore"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}==>${NC} $1"; }
log_warn() { echo -e "${YELLOW}Warning:${NC} $1"; }
log_error() { echo -e "${RED}Error:${NC} $1" >&2; }

usage() {
  echo "Usage: $0 <reference-sha> [bench.sh options]"
  echo ""
  echo "Benchmarks current HEAD against a reference commit."
  echo ""
  echo "Arguments:"
  echo "  reference-sha    Reference commit to compare against (e.g., v1.15.0)"
  echo ""
  echo "All additional arguments are forwarded to bench.sh"
  echo ""
  echo "Example:"
  echo "  $0 v1.15.0 --repo jellyfin:1"
  echo "  $0 v1.14.0 --repo jellyfin:3 --taint-rules-only"
  echo "  $0 v1.15.0 --dry-run --repo jellyfin:1"
  exit 1
}

cleanup() {
  if [[ -f "$RESTORE_FILE" ]]; then
    local original_ref
    original_ref=$(cat "$RESTORE_FILE")
    log_warn "Restoring original state: $original_ref"
    cd "$OPENGREP_ROOT"
    git checkout "$original_ref" 2>/dev/null || true
    git submodule update --init --recursive 2>/dev/null || true
    rm -f "$RESTORE_FILE"
  fi
}

# Get a human-friendly name for a commit (tag if available, otherwise short SHA)
get_commit_label() {
  local sha_full="$1"
  local tag
  tag=$(git tag --points-at "$sha_full" 2>/dev/null | head -n1)
  if [[ -n "$tag" ]]; then
    echo "$tag"
  else
    git rev-parse --short "$sha_full"
  fi
}

# Parse arguments
if [[ $# -lt 1 ]]; then
  usage
fi

REF_SHA="$1"
shift

# Check if --dry-run is in arguments
dry_run=""
BENCH_ARGS=()
for arg in "$@"; do
  BENCH_ARGS+=("$arg")
  if [[ "$arg" == "--dry-run" ]]; then
    dry_run="yes"
  fi
done

cd "$OPENGREP_ROOT"

# Resolve commits
log_info "Resolving commits..."
REF_SHA_FULL=$(git rev-parse "$REF_SHA")
REF_LABEL=$(get_commit_label "$REF_SHA_FULL")
HEAD_SHA_FULL=$(git rev-parse HEAD)
HEAD_LABEL=$(get_commit_label "$HEAD_SHA_FULL")

echo "  Reference: $REF_SHA -> $REF_LABEL ($REF_SHA_FULL)"
echo "  HEAD:      $HEAD_LABEL ($HEAD_SHA_FULL)"

if [[ "$REF_SHA_FULL" == "$HEAD_SHA_FULL" ]]; then
  log_error "HEAD is the same as reference: $REF_SHA_FULL"
  exit 1
fi

# Determine binary paths
REF_BINARY="$BINARIES_DIR/$REF_LABEL/venv/bin/opengrep"
HEAD_BINARY="$BINARIES_DIR/$HEAD_LABEL/venv/bin/opengrep"

# Check what needs building
need_build_ref=""
[[ ! -x "$REF_BINARY" ]] && need_build_ref="yes"
# HEAD is always rebuilt (developer is iterating on it)

# In dry-run mode, just show what would happen
if [[ -n "$dry_run" ]]; then
  log_info "Dry-run mode"
  echo ""
  echo "Binaries:"
  if [[ -n "$need_build_ref" ]]; then
    echo "  $REF_LABEL: needs checkout + build"
  else
    echo "  $REF_LABEL: cached"
  fi
  echo "  $HEAD_LABEL: build in place (always rebuilt)"
  echo ""
  echo "Would run:"
  echo "  ./bench.sh --reference-binary $REF_BINARY --binary $HEAD_BINARY ${BENCH_ARGS[*]}"
  echo ""

  cd "$SCRIPT_DIR"
  exec ./bench.sh \
    --reference-binary "$REF_BINARY" \
    --binary "$HEAD_BINARY" \
    "${BENCH_ARGS[@]}"
fi

# Function to build at current HEAD
do_build() {
  local label="$1"
  local target_dir="$BINARIES_DIR/$label"
  local venv_dir="$target_dir/venv"
  local binary_path="$venv_dir/bin/opengrep"

  log_info "Building $label..."

  log_info "  Running make core..."
  make core

  log_info "  Running make copy-core-for-cli..."
  make copy-core-for-cli

  log_info "  Creating venv at $venv_dir..."
  mkdir -p "$target_dir"
  python3 -m venv "$venv_dir"

  log_info "  Installing CLI into venv..."
  "$venv_dir/bin/pip" install --quiet "$OPENGREP_ROOT/cli"

  if [[ ! -x "$binary_path" ]]; then
    log_error "Build failed: $binary_path not found"
    exit 1
  fi

  log_info "  Build complete: $binary_path"
}

# Build reference if needed (requires checkout)
if [[ -n "$need_build_ref" ]]; then
  # Need checkout - verify git state is clean
  log_info "Checking working tree status..."
  if ! git diff --quiet; then
    log_error "Working tree has unstaged changes. Please commit or stash them."
    exit 1
  fi
  if ! git diff --cached --quiet; then
    log_error "Working tree has staged changes. Please commit or stash them."
    exit 1
  fi
  log_info "Checking submodule status..."
  submodule_status=$(git submodule status --recursive)
  if echo "$submodule_status" | grep -q '^+'; then
    log_error "Submodules have uncommitted changes:"
    echo "$submodule_status" | grep '^+'
    exit 1
  fi

  # Save for restore-on-error
  ORIGINAL_REF=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "$HEAD_SHA_FULL")
  echo "$ORIGINAL_REF" > "$RESTORE_FILE"
  trap cleanup EXIT

  log_info "Checking out $REF_LABEL..."
  git checkout "$REF_SHA_FULL"
  git submodule update --init --recursive
  do_build "$REF_LABEL"
  echo ""

  # Return to HEAD
  log_info "Returning to $ORIGINAL_REF..."
  git checkout "$ORIGINAL_REF"
  git submodule update --init --recursive
  rm -f "$RESTORE_FILE"
  trap - EXIT
else
  log_info "Reference binary cached: $REF_BINARY"
fi

# Always build HEAD (no checkout, we're already here)
do_build "$HEAD_LABEL"
echo ""

# Run benchmarks
log_info "Running benchmarks..."
echo "  Reference: $REF_BINARY"
echo "  HEAD:      $HEAD_BINARY"
echo ""

cd "$SCRIPT_DIR"
exec ./bench.sh \
  --reference-binary "$REF_BINARY" \
  --binary "$HEAD_BINARY" \
  "${BENCH_ARGS[@]}"
