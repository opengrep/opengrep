<!-- reference
id: flag-dryrun
kind: flag
name: --dryrun
aliases: [-n, --dry-run, --no-dryrun]
summary: Show what would be changed without changing it; the two commands mean different things by it.
commands: [scan, install-ci]
default: false
related: [flag-autofix, cmd-install-ci]
-->
# `--dryrun`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep install-ci`](../commands/install-ci.md)
- **Also spelled:** `-n`, `--dry-run`, `--no-dryrun`
- **Default:** `false`
- **See also:** [`--autofix`](autofix.md), [`opengrep install-ci`](../commands/install-ci.md), [`--replacement`](replacement.md)
<!-- END GENERATED: facts -->

The two commands that take this flag use it for different things.

## With `scan`

Only `--autofix` writes to your files, and `--dryrun` stops it from doing so:
the report shows each fix on a `fix:` line and the files are left as they
were. Without `--autofix` the flag does nothing. `--no-dryrun` turns it back
off.

## With `install-ci`

[`opengrep install-ci`](../commands/install-ci.md) skips the workflow
operations altogether and only logs what it would have done. It still prints
its `SUCCESS` line, so that line does not mean a workflow was written. The
spellings `-n` and `--dry-run` are accepted by `install-ci` only.

## Examples

### Seeing a fix without applying it

**`fix.yaml`**
```yaml title="fix.yaml"
rules:
  - id: use-is-none
    pattern: $X == None
    fix: $X is None
    message: use "is None"
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
if x == None:
    pass
```

**Command and result:**
```console
$ opengrep scan --config fix.yaml --autofix --dryrun app.py
app.py

  warn  use-is-none
  use "is None"

    1 │ if (x is None):

    fix: (x is None)

$ cat app.py
if x == None:
    pass
$ opengrep scan --config fix.yaml --autofix app.py > /dev/null 2>&1
$ cat app.py
if (x is None):
    pass
```

The report shows the line as it would look after the fix, while the file on
disk only changes once `--autofix` runs without `--dryrun`.
