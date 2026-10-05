<!-- reference
id: flag-force-exclude
kind: flag
name: --force-exclude
summary: Apply --include and --exclude to files named on the command line as well.
commands: [scan, ci]
related: [flag-exclude, flag-include]
-->
# `--force-exclude`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--exclude`](exclude.md), [`--include`](include.md), [`--scan-unknown-extensions`](scan-unknown-extensions.md)
<!-- END GENERATED: facts -->

[`--exclude`](exclude.md) and [`--include`](include.md) filter the files
opengrep finds by walking a directory. A file named on the command line is
scanned whatever those patterns say, on the grounds that you asked for it.
`--force-exclude` applies the patterns that match a file's name, such as
`*.py` or `gen.py`, to those files too, which is what a pre-commit hook or a
script passing a list of changed files wants.

## Examples

### Excluding a file that was named explicitly

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --exclude '*.py' app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)

$ opengrep scan --config rule.yaml --exclude '*.py' --force-exclude app.py
```

The second command prints nothing: the pattern now applies to the named file,
so there is nothing left to scan.
