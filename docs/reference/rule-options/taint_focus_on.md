<!-- reference
id: opt-taint_focus_on
kind: option
name: taint_focus_on
summary: Report a taint finding at the source instead of at the sink.
value: `sink` or `source`
default: sink
related: [key-mode-taint]
covers: [opt-taint_match_on]
-->
# `taint_focus_on`

<!-- BEGIN GENERATED: facts -->
- **Value:** `sink` or `source`
- **Default:** `sink`
- **See also:** [`mode: taint`](../rule-syntax/taint-mode.md)
<!-- END GENERATED: facts -->

A [taint rule](../rule-syntax/taint-mode.md) reports its finding at the sink
that tainted data reaches. With `taint_focus_on: source`, the finding is
reported at the source instead, once for each source whose data reaches the
sink.

`taint_match_on` is an older name for this option. It is still accepted,
without a warning.

## Examples

### The same flow, reported at either end

The rule `input-to-system` turns the option on.

**`focus.yaml`**
```yaml title="focus.yaml"
rules:
  - id: input-to-system-default
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: user input reaches os.system
    languages: [python]
    severity: ERROR
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
      taint_focus_on: source
```

**`focus.py`**
```python title="focus.py"
import os

def one_source():
    # ruleid: input-to-system
    name = input()
    # ruleid: input-to-system-default
    os.system("ls " + name)

def two_sources():
    # ruleid: input-to-system
    first = input()
    # ruleid: input-to-system
    second = input()
    # ruleid: input-to-system-default
    os.system(first + second)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
