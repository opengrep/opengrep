<!-- reference
id: flag-rewrite-rule-ids
kind: flag
name: --rewrite-rule-ids
aliases: [--no-rewrite-rule-ids]
summary: Whether a rule loaded from a directory gets its path as a prefix; on by default.
commands: [scan, ci]
default: true
related: [flag-config, flag-exclude-rule]
-->
# `--rewrite-rule-ids`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `--no-rewrite-rule-ids`
- **Default:** `true`
- **See also:** [`--config`](config.md), [`--exclude-rule`](exclude-rule.md)
<!-- END GENERATED: facts -->

When rules come from a directory, opengrep prefixes each rule id with the path
of the file it came from, dots for slashes: `find-eval` in `rules/eval.yaml`
is reported as `rules.find-eval`. Two rules in different files can then share
a name, and a finding says where its rule lives. That is the default.

`--no-rewrite-rule-ids` keeps the ids as the files write them. Use it when
something downstream matches on rule ids — a triage database, a suppression
list — and should not see them change with the directory layout.

The prefixed form is the id that [`--exclude-rule`](exclude-rule.md) expects.

## Examples

### An id with and without its path

**`rules/eval.yaml`**
```yaml title="rules/eval.yaml"
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
$ opengrep scan --config rules/ app.py
app.py

  warn  rules.find-eval
  found eval

    1 │ eval(1)

$ opengrep scan --config rules/ --no-rewrite-rule-ids app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
