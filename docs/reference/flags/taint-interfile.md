<!-- reference
id: flag-taint-interfile
kind: flag
name: --taint-interfile
summary: Follow taint across files, for every taint rule.
commands: [scan, ci]
related: [opt-taint_interfile, flag-taint-interfile-depth, flag-interfile-timeout, flag-taint-intrafile]
-->
# `--taint-interfile`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`taint_interfile`](../rule-options/taint_interfile.md), [`--taint-interfile-depth`](taint-interfile-depth.md), [`--interfile-timeout`](interfile-timeout.md), [`--taint-intrafile`](taint-intrafile.md), [`mode: taint`](../rule-syntax/taint-mode.md)
<!-- END GENERATED: facts -->

Follows taint through calls into functions defined in other files, for every
[taint rule](../rule-syntax/taint-mode.md) of the scan. It implies
[`--taint-intrafile`](taint-intrafile.md), since the cross-file analysis is
built on the per-function summaries that flag produces.

Opengrep builds a call graph of the project. A file holding sources but no
sinks, or the reverse, is analysed together with the files it is connected to,
following call chains up to [`--taint-interfile-depth`](taint-interfile-depth.md)
deep. The finding is reported in the file that holds the sink.

This costs more than a file-by-file scan: the whole project is parsed to build
the graph. [`--interfile-timeout`](interfile-timeout.md) caps the time a rule
may spend on it. The rule option
[`taint_interfile`](../rule-options/taint_interfile.md) asks for the same
analysis for one rule, which is the cheaper way when only a few rules need it.

## Examples

### Taint that crosses a file boundary

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
$ opengrep scan --config rule.yaml --taint-interfile .
util.py

  error  input-to-system
  input reaches os.system

    4 │ os.system(cmd)
```
