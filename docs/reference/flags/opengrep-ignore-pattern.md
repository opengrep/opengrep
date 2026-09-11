<!-- reference
id: flag-opengrep-ignore-pattern
kind: flag
name: --opengrep-ignore-pattern
summary: Recognise one more comment prefix that silences findings on a line.
commands: [scan, ci, test]
value: `PREFIX`
related: [flag-enable-nosem, flag-exclude-rule]
-->
# `--opengrep-ignore-pattern`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md)
- **Value:** `PREFIX`
- **See also:** [`--enable-nosem`](enable-nosem.md), [`--exclude-rule`](exclude-rule.md), [`--sarif-output`](sarif-output.md)
<!-- END GENERATED: facts -->

A comment at the end of a line whose text begins with `nosem`, `nosemgrep` or
`noopengrep` tells opengrep to report nothing on that line. This flag adds one
more prefix of your choosing, which is useful for a codebase that already
marks such lines for another tool.

The three built-in prefixes keep working alongside it. To stop honouring these
comments altogether, use `--disable-nosem`. To silence a rule everywhere
rather than on one line, see [`--exclude-rule`](exclude-rule.md).

## Examples

### Honouring another tool's marker

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
eval(2)  # noscan
eval(3)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  warn  find-eval
  found eval

    2 │ eval(2)  # noscan

  warn  find-eval
  found eval

    3 │ eval(3)

$ opengrep scan --config rule.yaml --opengrep-ignore-pattern=noscan app.py
app.py

  warn  find-eval
  found eval

    3 │ eval(3)
```

Line 1 is silent in both runs: `nosem` is recognised without asking.
