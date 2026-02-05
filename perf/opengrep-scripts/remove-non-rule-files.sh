#!/usr/bin/env sh

set -eu

rm -f \
  opengrep-rules/stats/web_frameworks.yml \
  opengrep-rules/stats/cwe_to_metacategory.yml \
  opengrep-rules/stats/metacategory_to_support_tier.yml \
  opengrep-rules/.github/stale.yml \
  opengrep-rules/.github/workflows/semgrep-rule-lints.yaml \
  opengrep-rules/.github/workflows/validate-registry-metadata.yaml \
  opengrep-rules/.github/workflows/semgrep-rules-test.yml \
  opengrep-rules/.github/workflows/pre-commit.yml \
  opengrep-rules/template.yaml \
  opengrep-rules/.pre-commit-config.yaml
