# Interfile taint tests

Regression tests for interfile taint analysis.  Each test case is a
self-contained directory with source files and a taint rule.  The call
graph is built in-process by `opengrep-project-index` (projidx) — no
external indexer is needed.

## Running tests

All interfile tests:

    ./test -s interfile

A single test:

    ./test -s "interfile taint > go > cycle_pruning"

## Manual CLI verification

Each test case can also be run manually with the CLI.  Example for
`cycle_pruning`:

    bin/opengrep scan \
      -f tests/interfile/go/cycle_pruning/rule.yaml \
      tests/interfile/go/cycle_pruning/ \
      --taint-interfile \
      --json

## Adding a new test case

1. Create a directory under `tests/interfile/<lang>/<case_name>/`.
2. Add source files with `// ruleid: <rule-id>` and `// ok: <rule-id>`
   annotations on the lines where findings are (or are not) expected.
3. Add `rule.yaml` with the taint rule(s).
4. The test is automatically discovered.
