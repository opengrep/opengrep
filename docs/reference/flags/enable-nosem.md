<!-- reference
id: flag-enable-nosem
kind: flag
name: --enable-nosem
aliases: [--disable-nosem]
summary: Whether a nosem comment silences findings on its line; on by default.
commands: [scan, ci]
default: true
related: [flag-opengrep-ignore-pattern, flag-exclude-rule]
-->
# `--enable-nosem`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `--disable-nosem`
- **Default:** `true`
- **See also:** [`--opengrep-ignore-pattern`](opengrep-ignore-pattern.md), [`--exclude-rule`](exclude-rule.md)
<!-- END GENERATED: facts -->

A comment at the end of a line beginning with `nosem`, `nosemgrep` or
`noopengrep` tells opengrep to report nothing on that line. That is the
default, and `--enable-nosem` asks for it explicitly.

`--disable-nosem` reports those findings anyway, which is how to see what the
comments are hiding, or to count them before a clean-up.

[`--opengrep-ignore-pattern`](opengrep-ignore-pattern.md) adds another prefix
beside the three built-in ones.

## Examples

### Seeing what a nosem comment hides

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
eval(1)  # nosem
eval(2)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  warn  find-eval
  found eval

    2 │ eval(2)

$ opengrep scan --config rule.yaml --disable-nosem app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)  # nosem

  warn  find-eval
  found eval

    2 │ eval(2)
```
