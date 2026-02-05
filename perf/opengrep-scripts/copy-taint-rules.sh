#!/usr/bin/env sh

set -eu

rg -l 'mode: taint' opengrep-rules -g '**/*.yaml' \
  | sed 's|^opengrep-rules/||' \
  | rsync -av --files-from=- opengrep-rules/ opengrep-rules-taint/
