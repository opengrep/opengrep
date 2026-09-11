<!-- reference
id: flag-semgrepignore-filename
kind: flag
name: --semgrepignore-filename
summary: Read the list of skipped paths from a differently named file.
commands: [scan]
value: `FILENAME`
default: .semgrepignore
related: [flag-exclude, flag-use-git-ignore, flag-project-root]
-->
# `--semgrepignore-filename`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Value:** `FILENAME`
- **Default:** `.semgrepignore`
- **See also:** [`--exclude`](exclude.md), [`--use-git-ignore`](use-git-ignore.md), [`--project-root`](project-root.md)
<!-- END GENERATED: facts -->

Opengrep skips the paths listed in `.semgrepignore` at the project root. This
flag reads that list from FILENAME instead. It helps when one repository holds
several scans that should ignore different things, or when `.semgrepignore` is
already spoken for.

The file uses the `.gitignore` pattern syntax. When the project has no such
file at all, a built-in list applies, which skips directories like
`node_modules/`, `vendor/`, `test/` and `tests/`. Which directory counts as the
project root is settled by [`--project-root`](project-root.md).

## Examples

### An ignore list under another name

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`src/a.py`**
```python title="src/a.py"
eval(1)
```

**`mystuff/m.py`**
```python title="mystuff/m.py"
eval(2)
```

**`.myignore`**
```title=".myignore"
mystuff/
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --files-with-matches .
mystuff/m.py
src/a.py
$ opengrep scan --config rule.yaml --files-with-matches --semgrepignore-filename .myignore .
src/a.py
```
