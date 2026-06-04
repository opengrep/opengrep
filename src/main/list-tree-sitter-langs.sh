#! /usr/bin/env bash
#
# List of tree-sitter parsers into a config file
# This runs from the project root and looks for tree-sitter parsers
# in the standard locations.
#
set -eu

# Paths are relative to the project root which can be
# semgrep, semgrep-proprietary, or semgrep-proprietary/OSS depending
# on context.
semgrep_paths=$(echo languages/*/tree-sitter/semgrep-*)
opengrep_paths=$(echo languages/*/tree-sitter/opengrep-*)

# Extract 'some_lang' from folders named 'semgrep-some-lang' or 'opengrep-some-lang'
for path in $semgrep_paths; do
  dir=$(basename "$path")
  echo "${dir#semgrep-}" | tr '-' '_'
done
for path in $opengrep_paths; do
  dir=$(basename "$path")
  echo "${dir#opengrep-}" | tr '-' '_'
done
