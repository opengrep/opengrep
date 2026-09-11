<!-- reference
id: flag-guarded-taint-signatures
kind: flag
name: --guarded-taint-signatures
summary: Drop a cross-function taint finding when the branch leading to the sink cannot be taken.
commands: [scan]
related: [flag-taint-intrafile, opt-taint_intrafile, key-mode-taint]
-->
# `--guarded-taint-signatures`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`--taint-intrafile`](taint-intrafile.md), [`taint_intrafile`](../rule-options/taint_intrafile.md), [`mode: taint`](../rule-syntax/taint-mode.md), [`guarded_taint_signatures`](../rule-options/guarded_taint_signatures.md)
<!-- END GENERATED: facts -->

When [`--taint-intrafile`](taint-intrafile.md) summarises a function, the
summary says that tainted arguments reach a sink, but not under what
condition. A sink guarded by `if mode == 1:` is reported even for a caller
that passes `2`.

This flag records the branch conditions along the way and checks them at each
call site, dropping the effects whose guard cannot hold. Fewer false
positives, at the cost of more work per function.

It is experimental. Without it, only Clojure keeps the arity guards that its
multi-arity dispatch needs. The rule option is `guarded_taint_signatures`.

## Examples

### A sink behind a condition the caller does not meet

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: input-to-system
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: input reaches os.system
    languages: [python]
    severity: ERROR
```

**`guard.py`**
```python title="guard.py"
import os

def run(cmd, mode):
    if mode == 1:
        os.system(cmd)

run(input(), 2)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --taint-intrafile --files-with-matches guard.py
guard.py
$ opengrep scan --config rule.yaml --taint-intrafile --guarded-taint-signatures --files-with-matches guard.py
```

The call passes `2`, so the sink under `mode == 1` is unreachable. The second
command prints nothing, having dropped the finding the first one reports.
