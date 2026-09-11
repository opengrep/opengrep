<!-- reference
id: opt-guarded_taint_signatures
kind: option
name: guarded_taint_signatures
summary: Drop a cross-function taint finding when the branch leading to the sink cannot be taken.
value: `true` or `false`
default: false
related: [flag-guarded-taint-signatures, opt-taint_intrafile]
-->
# `guarded_taint_signatures`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`--guarded-taint-signatures`](../flags/guarded-taint-signatures.md), [`taint_intrafile`](taint_intrafile.md)
<!-- END GENERATED: facts -->

This is the rule option behind
[`--guarded-taint-signatures`](../flags/guarded-taint-signatures.md). When a
function is analysed for its callers, the conditions under which its tainted
arguments reach a sink are recorded as well, and a call that cannot meet them
does not produce a finding. It is experimental.

It works on calls between functions, so it matters only when the rule follows
them, with [`taint_intrafile`](taint_intrafile.md) or the flag
`--taint-intrafile`.

The flag turns it on for every rule of the scan. A rule cannot turn it off
while the flag is given: `guarded_taint_signatures: false` then has no effect.

## Examples

### A sink behind a condition the caller does not meet

The rule `input-to-system` turns the option on. Both rules set
`taint_intrafile`, so they follow the call to `run`.

**`guard.yaml`**
```yaml title="guard.yaml"
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
    options:
      taint_intrafile: true
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
      taint_intrafile: true
      guarded_taint_signatures: true
```

**`guard.py`**
```python title="guard.py"
import os

def run(cmd, mode):
    if mode == 1:
        # ruleid: input-to-system-default
        os.system(cmd)

def run_always(cmd, mode):
    if mode == 1:
        # ruleid: input-to-system-default, input-to-system
        os.system(cmd)

run(input(), 2)
run_always(input(), 1)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

`run` is called with `2`, so with the option its sink is not reported;
`run_always` is called with `1`, so both rules report its sink.

### The flag wins over `false`

**`off.yaml`**
```yaml title="off.yaml"
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
      taint_intrafile: true
      guarded_taint_signatures: false
```

**`guarded.py`**
```python title="guarded.py"
import os

def run(cmd, mode):
    if mode == 1:
        os.system(cmd)

run(input(), 2)
```

**Command and result:**
```console
$ opengrep scan --config off.yaml --files-with-matches guarded.py
guarded.py
$ opengrep scan --config off.yaml --guarded-taint-signatures --files-with-matches guarded.py
```

The second command prints nothing: the flag drops the finding although the
rule sets the option to `false`.
