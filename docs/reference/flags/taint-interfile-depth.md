<!-- reference
id: flag-taint-interfile-depth
kind: flag
name: --taint-interfile-depth
summary: How many calls deep the cross-file taint analysis follows a chain.
commands: [scan, ci]
value: `INT`
default: 3
related: [flag-taint-interfile, opt-taint_interfile_depth, flag-interfile-timeout]
-->
# `--taint-interfile-depth`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`
- **Default:** `3`
- **See also:** [`--taint-interfile`](taint-interfile.md), [`taint_interfile_depth`](../rule-options/taint_interfile_depth.md), [`--interfile-timeout`](interfile-timeout.md), [`taint_interfile`](../rule-options/taint_interfile.md)
<!-- END GENERATED: facts -->

[`--taint-interfile`](taint-interfile.md) gathers the files a file is
connected to through the call graph. This is how far it follows those
connections: with the default of 3, a source reaches a sink three calls away
but not four. A negative value removes the limit.

The cost grows with the depth, since each step pulls more files into the
analysis. Raising it is what to try when a flow you expect is not reported and
you can count more than three calls between the source and the sink.

A rule can set its own depth with the `taint_interfile_depth` option, which
wins over this flag; a rule that leaves it unset uses the flag's value.

## Examples

### A chain four calls long

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

**`d.py`**
```python title="d.py"
import os

def sink_it(cmd):
    os.system(cmd)
```

**`c.py`**
```python title="c.py"
from d import sink_it

def step_c(x):
    sink_it(x)
```

**`b.py`**
```python title="b.py"
from c import step_c

def step_b(x):
    step_c(x)
```

**`a.py`**
```python title="a.py"
from b import step_b

step_b(input())
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --taint-interfile --taint-interfile-depth 2 --files-with-matches .
$ opengrep scan --config rule.yaml --taint-interfile --taint-interfile-depth 3 --files-with-matches .
d.py
```

At depth 2 the chain is not followed far enough and nothing is reported.
