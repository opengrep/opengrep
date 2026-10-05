<!-- reference
id: flag-disable-intrafile
kind: flag
name: --disable-intrafile
summary: Keep every taint rule within single functions, even rules that ask to follow calls.
commands: [scan, ci]
related: [flag-taint-intrafile, opt-taint_intrafile, flag-disable-interfile]
-->
# `--disable-intrafile`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--taint-intrafile`](taint-intrafile.md), [`taint_intrafile`](../rule-options/taint_intrafile.md), [`--disable-interfile`](disable-interfile.md)
<!-- END GENERATED: facts -->

Turns off the analysis that follows taint through calls to functions defined
in the same file, for every [taint rule](../rule-syntax/taint-mode.md) of the
scan. It wins over [`--taint-intrafile`](taint-intrafile.md) and over the rule
option [`taint_intrafile`](../rule-options/taint_intrafile.md).

The cross-file analysis is built on the same per-function summaries, so it is
turned off too: [`--taint-interfile`](taint-interfile.md) and the rule option
[`taint_interfile`](../rule-options/taint_interfile.md) have no effect either.
To keep the analysis within each file instead, use
[`--disable-interfile`](disable-interfile.md).

Each taint rule then analyses one function at a time. Taint that flows from a
source to a sink inside a single function is still reported.

## Examples

### A rule that asks to follow calls

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
    options:
      taint_intrafile: true
```

**`app.py`**
```python title="app.py"
import os

def run(cmd):
    os.system(cmd)

run(input())
os.system(input())
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  error  input-to-system
  input reaches os.system

    4 │ os.system(cmd)

    7 │ os.system(input())
$ opengrep scan --config rule.yaml --disable-intrafile app.py
app.py

  error  input-to-system
  input reaches os.system

    7 │ os.system(input())
```

With the flag, the call to `run` is no longer followed, and only the sink on
line 7, which the source reaches directly, is reported.
