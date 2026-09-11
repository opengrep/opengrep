<!-- reference
id: flag-autofix
kind: flag
name: --autofix
aliases: [-a, --no-autofix]
summary: Apply the fixes rules suggest, rewriting your files.
commands: [scan]
default: false
related: [flag-dryrun, flag-replacement, key-fix, cmd-test]
-->
# `--autofix`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Also spelled:** `-a`, `--no-autofix`
- **Default:** `false`
- **See also:** [`--dryrun`](dryrun.md), [`--replacement`](replacement.md), `fix`, [`opengrep test`](../commands/test.md)
<!-- END GENERATED: facts -->

A rule with a `fix:` key says what its finding should become.
`--autofix` writes those fixes to the files. The report shows each change on a
`fix:` line, as it does without the flag, but the file on disk now holds the
new text.

This rewrites your working tree, so keep it under version control and read the
diff afterwards. The opengrep sources call the mode experimental.
[`--dryrun`](dryrun.md) shows what would change and writes nothing.

A rule's fix can be checked before you trust it: put the expected result in a
`<name>.fixed.<ext>` file and run [`opengrep test`](../commands/test.md).

## Examples

### Rewriting a file

**`rule.yaml`**
```yaml title="rule.yaml"
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
$ opengrep scan --config rule.yaml --autofix app.py
app.py

  warn  use-is-none
  use "is None"

    1 │ if x == None:

    fix: (x is None)

$ cat app.py
if (x is None):
    pass
```
