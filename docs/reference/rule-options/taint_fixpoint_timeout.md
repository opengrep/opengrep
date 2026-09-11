<!-- reference
id: opt-taint_fixpoint_timeout
kind: option
name: taint_fixpoint_timeout
summary: Meant to limit the time spent on the taint analysis of one function; has no effect.
value: seconds, a number
related: []
-->
# `taint_fixpoint_timeout`

<!-- BEGIN GENERATED: facts -->
- **Value:** seconds, a number
<!-- END GENERATED: facts -->

This option is meant to limit, in seconds, the time a
[taint rule](../rule-syntax/taint-mode.md) spends analysing one function. It
is accepted, but it has no effect in this version: even a limit of `0` lets
the analysis run to completion, and no timeout is reported.

To limit the time a rule takes, see [`--timeout`](../flags/timeout.md).

## Examples

### A limit of zero

**`zero.yaml`**
```yaml title="zero.yaml"
rules:
  - id: input-to-system
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: user input reaches os.system
    languages: [python]
    severity: ERROR
    options:
      taint_fixpoint_timeout: 0
```

**`zero.py`**
```python title="zero.py"
import os

def build():
    cmd = input()
    while more():
        cmd = cmd + next_part()
    # ruleid: input-to-system
    os.system(cmd)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
$ opengrep scan --config zero.yaml zero.py 2>&1 | grep -c 'Fixpoint timeout'
0
```
