#!/usr/bin/env sh

set -eu

rg --files-without-match 'mode: taint' opengrep-rules -g '**/*.yaml' \
  | sed 's|^opengrep-rules/||' \
  | rsync -av --files-from=- opengrep-rules/ opengrep-rules-non-taint/
