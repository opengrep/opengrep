<!-- reference
id: flag-max-chars-per-line
kind: flag
name: --max-chars-per-line
summary: Width at which a finding's code lines are wrapped in the text report.
commands: [scan, ci]
value: `INT`
default: 160
related: [flag-max-lines-per-finding, flag-skin, flag-text]
-->
# `--max-chars-per-line`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`
- **Default:** `160`
- **See also:** [`--max-lines-per-finding`](max-lines-per-finding.md), [`--skin`](skin.md), [`--text`](text.md), [`COLUMNS`](../env/COLUMNS.md)
<!-- END GENERATED: facts -->

A matched line longer than this is wrapped in the text report, and what
follows is indented under it. The terminal is still the upper bound: a value
wider than the terminal does not make lines longer than it.

It changes the report only. The match itself is unaffected, and the
machine-readable formats carry the line in full.

## Examples

### Wrapping a long line

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`long.py`**
```python title="long.py"
eval(some_extremely_long_variable_name_here + "aaaa bbbb cccc dddd")
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --max-chars-per-line 40 long.py
long.py

  warn  find-eval
  found eval

    1 │ eval(some_extremely_long_variable_
        name_here + "aaaa bbbb cccc dddd")
```
