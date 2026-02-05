#!/usr/bin/env sh

set -eu

rg -lv 'mode: taint' opengrep-rules -g '**/*.yaml' \
  | sed 's|^opengrep-rules/||' \
  | rsync -av --files-from=- opengrep-rules/ opengrep-rules-non-taint/
