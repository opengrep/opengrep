<!-- reference
id: flag-include
kind: flag
name: --include
summary: Scan only the files whose path matches one of these patterns.
commands: [scan, ci]
value: `PATTERN`, repeatable
related: [flag-exclude, flag-force-exclude, key-paths]
-->
# `--include`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `PATTERN`, repeatable
- **See also:** [`--exclude`](exclude.md), [`--force-exclude`](force-exclude.md), [`paths`](../rule-syntax/paths.md), [`--scan-unknown-extensions`](scan-unknown-extensions.md)
<!-- END GENERATED: facts -->

Keeps only the files whose path matches at least one PATTERN, and drops the
rest. Repeat the flag to keep the files matching any of several patterns.

`--include` runs last, after [`--exclude`](exclude.md), after the filtering
git does, and after `.semgrepignore`. It therefore narrows what those left
behind; it cannot bring back a file they dropped.

PATTERN is a glob, in the same syntax as `--exclude`: `--include='*.py'` keeps
Python files anywhere, and `--include=lib` keeps what is under a directory
named `lib`. Quote a pattern containing `*` so the shell leaves it alone.

## Examples

### Scanning one directory of a tree

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

**`lib/other.py`**
```python title="lib/other.py"
eval(2)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --include lib .
lib/other.py

  warn  find-eval
  found eval

    1 │ eval(2)
```
