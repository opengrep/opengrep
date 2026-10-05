<!-- reference
id: key-focus-metavariable
kind: rule-key
name: focus-metavariable
summary: Report only the code a metavariable matched, not the whole match.
related: [key-patterns]
-->
# `focus-metavariable`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md)
<!-- END GENERATED: facts -->

`focus-metavariable` is an item of [`patterns`](patterns.md). It names a
metavariable bound by the other items, and the finding is reported at the code
that metavariable matched instead of at the whole match. The rule finds the
same places; only the reported location changes.

The value can be a list of metavariables: each one then gives a finding of
its own. Several `focus-metavariable` items instead narrow one finding to the
code all of them matched, and give nothing when they matched different code.

## Examples

### The command, not the call

**`shell-command.yaml`**
```yaml title="shell-command.yaml"
rules:
  - id: shell-command
    patterns:
      - pattern: subprocess.run($CMD, shell=True, ...)
      - focus-metavariable: $CMD
    message: the command run through a shell
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
import subprocess

subprocess.run(
    "tar czf " + name + ".tgz data/",
    shell=True,
    check=True,
)
```

**Command and result:**
```console
$ opengrep scan --config shell-command.yaml app.py
app.py

  warn  shell-command
  the command run through a shell

    4 │ "tar czf " + name + ".tgz data/",

```

### A list, and two items

**`copy.yaml`**
```yaml title="copy.yaml"
rules:
  - id: each-path
    patterns:
      - pattern: shutil.copy($SRC, $DST)
      - focus-metavariable: [$SRC, $DST]
    message: a path given to shutil.copy
    languages: [python]
    severity: INFO
  - id: both-paths
    patterns:
      - pattern: shutil.copy($SRC, $DST)
      - focus-metavariable: $SRC
      - focus-metavariable: $DST
    message: a path given to shutil.copy
    languages: [python]
    severity: INFO
```

**`app.py`**
```python title="app.py"
import shutil

shutil.copy(
    upload_path,
    target_path,
)
```

**Command and result:**
```console
$ opengrep scan --config copy.yaml app.py
app.py

  info  each-path
  a path given to shutil.copy

    4 │ upload_path,

    5 │ target_path,

```

`each-path` reports both arguments, as two findings under one heading.
`both-paths` reports nothing, since `$SRC` and `$DST` matched different code.
