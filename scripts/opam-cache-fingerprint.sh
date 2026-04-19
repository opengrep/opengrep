#!/usr/bin/env sh
set -eu

script_dir=$(cd "$(dirname "$0")" && pwd)
cd "$script_dir/.."

mkdir -p .ci-cache
grep -v '^version:' opam/semgrep.opam > .ci-cache/opam-fingerprint.txt
