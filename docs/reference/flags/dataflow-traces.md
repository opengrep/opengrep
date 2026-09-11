<!-- reference
id: flag-dataflow-traces
kind: flag
name: --dataflow-traces
summary: Show how a value reaches the finding, for taint rules.
commands: [scan, ci]
related: [key-mode-taint, opt-taint_intrafile, flag-sarif]
-->
# `--dataflow-traces`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`mode: taint`](../rule-syntax/taint-mode.md), [`taint_intrafile`](../rule-options/taint_intrafile.md), [`--sarif`](sarif.md), [`--matching-explanations`](matching-explanations.md), [`--output-enclosing-context`](output-enclosing-context.md)
<!-- END GENERATED: facts -->

Adds the path the tainted value took to each finding of a
[taint rule](../rule-syntax/taint-mode.md): where the taint came from, the
variables it passed through, and how it reached the sink. Without the flag
only the sink is shown.

It affects the text and SARIF output only. The JSON output carries the same
information whether or not the flag is given.

## Examples

### The path from source to sink

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: input-to-eval
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: eval(...)
    message: input reaches eval
    languages: [python]
    severity: ERROR
```

**`app.py`**
```python title="app.py"
x = input()
y = x
eval(y)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --dataflow-traces app.py
app.py

  error  input-to-eval
  input reaches eval

    3 │ eval(y)
    │
    ├─ Taint comes from:
    │  1 │ x = input()
    │
    ├─ Taint flows through these intermediate variables:
    │  1 │ x = input()
    │  2 │ y = x
    │
    └─ This is how taint reaches the sink:
       3 │ eval(y)
```
