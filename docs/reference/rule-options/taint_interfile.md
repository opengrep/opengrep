<!-- reference
id: opt-taint_interfile
kind: option
name: taint_interfile
aliases: [interfile]
covers: [opt-interfile]
summary: Follow taint across files, through calls to functions defined in other files.
value: `true` or `false`
default: false
related: [flag-taint-interfile, flag-taint-interfile-depth, opt-taint_interfile_depth, flag-interfile-timeout, opt-taint_intrafile, key-mode-taint]
-->
# `taint_interfile`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `interfile`
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`--taint-interfile`](../flags/taint-interfile.md), [`--taint-interfile-depth`](../flags/taint-interfile-depth.md), [`taint_interfile_depth`](taint_interfile_depth.md), [`--interfile-timeout`](../flags/interfile-timeout.md), [`taint_intrafile`](taint_intrafile.md), [`mode: taint`](../rule-syntax/taint-mode.md)
<!-- END GENERATED: facts -->

With `taint_interfile: true`, a [taint rule](../rule-syntax/taint-mode.md)
follows taint through calls to functions defined in other files of the
project. It implies [`taint_intrafile`](taint_intrafile.md), which does the
same within a file.

Opengrep builds a call graph of the whole project. When a file has sources but
no sinks, or sinks but no sources, the files it is connected to in the graph
are analysed together with it. The search follows call chains up to
`taint_interfile_depth` calls deep. The rule's option wins over
`--taint-interfile-depth`, which defaults to 3. A negative depth means no
limit. `--interfile-timeout` limits the time spent on this analysis.

A rule with this option runs only in this cross-file analysis, not file by
file. The finding is reported at the sink.

`interfile: true` is another name for this option, used by published rules.
Setting both `interfile` and `taint_interfile` makes the rule invalid.

The flag `--taint-interfile` turns this on for every taint rule of the scan.

## Examples

### Taint across two files

**`rule.yaml`**
```yaml title="rule.yaml"
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
from util import run

run(input())
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml .
util.py

  error  input-to-system
  user input reaches os.system

    4 │ os.system(cmd)
```

### Both names set

**`rule.yaml`**
```yaml title="rule.yaml"
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
      interfile: true
      taint_interfile: true
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml . 2>&1 | grep 'only one'
only one of interfile and taint_interfile can be set
```
