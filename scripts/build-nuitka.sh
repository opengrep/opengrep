#!/usr/bin/env bash

set -euo pipefail

VERSION="${1:-}"
FORCE_ONEFILE="${2:-false}"
SRC_SEMGREP_DIR="${3:-src/semgrep}"

PYTHON_BIN="${PYTHON_BIN:-python}"

if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 <VERSION> [FORCE_ONEFILE=false] [SRC_SEMGREP_DIR=src/semgrep]"
  exit 1
fi

EXTRA_ARGS=()

# On linux we only compile to --onefile if forced using FORCE_ONEFILE.
# In that case, we fix the tempdir to a reasonable cache directory, for
# performance reasons. Nuitka expands {CACHE_DIR} suitably for each platform.
if [[ "$FORCE_ONEFILE" == "true" || "$(uname)" == "Darwin" || "$(uname -s)" =~ ^(CYGWIN|MINGW|MSYS) ]]; then
  EXTRA_ARGS+=(
    --onefile
    --onefile-tempdir-spec="{CACHE_DIR}/opengrep/$VERSION"
  )
fi

# We need to do this when building on Windows, otherwise Nuitka
# does not include opengrep-core.exe and the necessary DLLs.
if [[ "$(uname -s)" =~ ^(CYGWIN|MINGW|MSYS) ]]; then
  EXTRA_ARGS+=(
    --include-data-files="$SRC_SEMGREP_DIR/bin/*.exe=semgrep/bin/"
    --include-data-files="$SRC_SEMGREP_DIR/bin/*.dll=semgrep/bin/"
    --assume-yes-for-downloads
    --python-flag=-O
    --force-runtime-environment-variable=PYTHONIOENCODING=utf-8
  )
else
  EXTRA_ARGS+=(
    --include-data-dir="$SRC_SEMGREP_DIR/bin=semgrep/bin"
  )
fi

# NOTE: This is not working so well at the moment...
# if [ -f /etc/alpine-release ]; then
#   EXTRA_ARGS+=(
#     --static-libpython=yes
#   )
# fi

pushd cli

"$PYTHON_BIN" -m nuitka \
  --standalone \
  "${EXTRA_ARGS[@]}" \
  --product-name=opengrep \
  --product-version="${VERSION:1}" \
  --file-description="Opengrep CLI" \
  --output-filename=opengrep \
  --include-data-dir="$SRC_SEMGREP_DIR/templates=semgrep/templates" \
  --include-data-file="$SRC_SEMGREP_DIR/semgrep_interfaces/lang.json=semgrep/semgrep_interfaces/lang.json" \
  --include-data-file="$SRC_SEMGREP_DIR/semgrep_interfaces/rule_schema_v1.yaml=semgrep/semgrep_interfaces/rule_schema_v1.yaml" \
  --include-package=google.protobuf \
  --include-package=jaraco \
  --include-module=chardet \
  --no-deployment-flag=self-execution \
  --windows-icon-from-ico=spec/opengrep.ico \
  --linux-icon=spec/opengrep.ico \
  --noinclude-setuptools-mode=nofollow \
  "$SRC_SEMGREP_DIR/console_scripts/entrypoint.py"

popd 
