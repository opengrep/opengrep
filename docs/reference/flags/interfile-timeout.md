<!-- reference
id: flag-interfile-timeout
kind: flag
name: --interfile-timeout
summary: Time a rule may spend on the cross-file analysis.
commands: [scan, ci]
value: `INT`, in seconds
default: 0, no limit
related: [flag-taint-interfile, flag-taint-interfile-depth, flag-timeout]
-->
# `--interfile-timeout`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`, in seconds
- **Default:** `0, no limit`
- **See also:** [`--taint-interfile`](taint-interfile.md), [`--taint-interfile-depth`](taint-interfile-depth.md), [`--timeout`](timeout.md), [`taint_interfile`](../rule-options/taint_interfile.md)
<!-- END GENERATED: facts -->

[`--taint-interfile`](taint-interfile.md) analyses a rule across several files
at once, which is a different job from running it on one file and has its own
budget. This is how many seconds a rule may spend on it. `0`, the default,
means no limit.

[`--timeout`](timeout.md) is the per-file limit and does not bound this work,
so a project whose call graph pulls in many files can spend a long time here
with no cap. The other lever is
[`--taint-interfile-depth`](taint-interfile-depth.md), which bounds how many
files are pulled in to begin with.

## Examples

### A budget for the cross-file analysis

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

**`util.py`**
```python title="util.py"
import os

def run(cmd):
    os.system(cmd)
```

**`main.py`**
```python title="main.py"
from util import run

run(input())
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --taint-interfile --interfile-timeout 60 --files-with-matches .
util.py
```
