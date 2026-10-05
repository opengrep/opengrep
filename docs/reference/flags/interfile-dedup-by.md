<!-- reference
id: flag-interfile-dedup-by
kind: flag
name: --interfile-dedup-by
summary: Whether a sink that several sources reach across files is one finding or one per source.
commands: [scan, ci]
value: `sink` or `source-sink`
default: sink
related: [flag-taint-interfile, opt-taint_interfile]
-->
# `--interfile-dedup-by`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `sink` or `source-sink`
- **Default:** `sink`
- **See also:** [`--taint-interfile`](taint-interfile.md), [`taint_interfile`](../rule-options/taint_interfile.md)
<!-- END GENERATED: facts -->

A taint finding is reported at its sink. When the cross-file analysis of
[`--taint-interfile`](taint-interfile.md) or the rule option
[`taint_interfile`](../rule-options/taint_interfile.md) finds several sources
that reach the same sink, this flag decides how many findings they make:

| Value | Findings |
|---|---|
| `sink` | One for the sink, whichever source it shows. |
| `source-sink` | One for each source that reaches the sink. |

In the JSON and SARIF output, `source-sink` gives each source a result of its
own. The text report shows the sink once, with a `from:` line for each source.

Rules that do not use the cross-file analysis always report a sink once.

## Examples

### Two callers in other files

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

**`other.py`**
```python title="other.py"
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

    from: main.py:3  input()
$ opengrep scan --config rule.yaml --taint-interfile --interfile-dedup-by source-sink .
util.py

  error  input-to-system
  input reaches os.system

    4 │ os.system(cmd)

    from: main.py:3  input()
    from: other.py:3  input()
```
