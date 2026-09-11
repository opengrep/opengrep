<!-- reference
id: opt-taint_interfile_depth
kind: option
name: taint_interfile_depth
summary: How many calls deep this rule's cross-file taint analysis follows a chain.
value: an integer
related: []
-->
# `taint_interfile_depth`

<!-- BEGIN GENERATED: facts -->
- **Value:** an integer
- **See also:** [`--taint-interfile-depth`](../flags/taint-interfile-depth.md), [`taint_interfile`](taint_interfile.md)
<!-- END GENERATED: facts -->

When a rule follows taint across files, with
[`taint_interfile`](taint_interfile.md) or the flag `--taint-interfile`, this
sets how many calls deep the analysis follows a chain for that rule. A
negative value means no limit.

A depth set by the rule wins over
[`--taint-interfile-depth`](../flags/taint-interfile-depth.md), whether it is
larger or smaller. A rule that does not set it uses the flag, whose default is
3. Without cross-file analysis the option has no effect.

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
    options:
      taint_interfile: true
```

**`deep.yaml`**
```yaml title="deep.yaml"
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
      taint_interfile_depth: 3
```

**`unlimited.yaml`**
```yaml title="unlimited.yaml"
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
      taint_interfile_depth: -1
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
$ opengrep scan --config rule.yaml --taint-interfile-depth 2 --files-with-matches .
$ opengrep scan --config deep.yaml --taint-interfile-depth 2 --files-with-matches .
d.py
$ opengrep scan --config unlimited.yaml --taint-interfile-depth 1 --files-with-matches .
d.py
```

With the rule's depth unset, the flag's 2 is too short for the chain and the
first command prints nothing. The other two rules set their own depth, which
the flag does not change.
