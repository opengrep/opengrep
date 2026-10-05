<!-- reference
id: flag-disable-interfile
kind: flag
name: --disable-interfile
summary: Keep every taint rule within single files, even rules that ask to follow taint across files.
commands: [scan, ci]
related: [flag-taint-interfile, opt-taint_interfile]
-->
# `--disable-interfile`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--taint-interfile`](taint-interfile.md), [`taint_interfile`](../rule-options/taint_interfile.md), [`--disable-intrafile`](disable-intrafile.md)
<!-- END GENERATED: facts -->

Turns off the analysis that follows taint into functions defined in other
files, for every [taint rule](../rule-syntax/taint-mode.md) of the scan. It
wins over [`--taint-interfile`](taint-interfile.md) and over the rule option
[`taint_interfile`](../rule-options/taint_interfile.md).

A rule that asks for the cross-file analysis keeps the analysis within each
file, the one [`--taint-intrafile`](taint-intrafile.md) gives, since that is
part of what it asked for.

To keep taint rules within single functions,
[`--disable-intrafile`](disable-intrafile.md) turns off both analyses.

## Examples

### A rule that asks to follow taint across files

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
      taint_interfile: true
```

**`util.py`**
```python title="util.py"
import os

def run(cmd):
    os.system(cmd)
```

**`main.py`**
```python title="main.py"
import os
from util import run

def shell(cmd):
    os.system(cmd)

run(input())
shell(input())
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml .
main.py

  error  input-to-system
  input reaches os.system

    5 │ os.system(cmd)

    from: main.py:8  input()

util.py

  error  input-to-system
  input reaches os.system

    4 │ os.system(cmd)

    from: main.py:7  input()
$ opengrep scan --config rule.yaml --disable-interfile .
main.py

  error  input-to-system
  input reaches os.system

    5 │ os.system(cmd)
```

With the flag, the call into `util.py` is no longer followed. The call to
`shell`, defined in the same file, still is.
