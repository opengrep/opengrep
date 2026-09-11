<!-- reference
id: flag-severity
kind: flag
name: --severity
summary: Report only the findings of rules with these severities.
commands: [scan]
value: `INFO`, `WARNING` or `ERROR`, repeatable
related: [flag-exclude-rule, flag-error, flag-config]
-->
# `--severity`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Value:** `INFO`, `WARNING` or `ERROR`, repeatable
- **See also:** [`--exclude-rule`](exclude-rule.md), [`--error`](error.md), [`--config`](config.md)
<!-- END GENERATED: facts -->

Keeps only the findings whose rule carries one of the named severities, and
drops the rest. Repeat the flag to keep several. Without it every rule's
findings are reported.

The value is one of `INFO`, `WARNING` or `ERROR`, spelled as a rule's
`severity:` key spells it. Anything else is rejected as a bad option value.

This filters the report, not the run: the rules still execute. To keep a rule
from running at all, name it with [`--exclude-rule`](exclude-rule.md) or give
it a narrower [`paths`](../rule-syntax/paths.md).

## Examples

### Only the serious findings

**`rules.yaml`**
```yaml title="rules.yaml"
rules:
  - id: high
    pattern: eval(...)
    message: eval found
    languages: [python]
    severity: ERROR
  - id: low
    pattern: print(...)
    message: print found
    languages: [python]
    severity: INFO
```

**`app.py`**
```python title="app.py"
eval(1)
print(2)
```

**Command and result:**
```console
$ opengrep scan --config rules.yaml app.py
app.py

  error  high
  eval found

    1 │ eval(1)

  info  low
  print found

    2 │ print(2)

$ opengrep scan --config rules.yaml --severity ERROR app.py
app.py

  error  high
  eval found

    1 │ eval(1)
```
