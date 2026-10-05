<!-- reference
id: env-NO_COLOR
kind: env
name: NO_COLOR
aliases: [OPENGREP_FORCE_NO_COLOR, SEMGREP_FORCE_NO_COLOR]
covers: [env-OPENGREP_FORCE_NO_COLOR]
summary: Turn off colour and other text styling in all output, even on a terminal.
value: any non-empty value
related: [flag-force-color]
-->
# `NO_COLOR`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `OPENGREP_FORCE_NO_COLOR`, `SEMGREP_FORCE_NO_COLOR`
- **Value:** any non-empty value
- **See also:** [`--force-color`](../flags/force-color.md), [`OPENGREP_FORCE_COLOR`](OPENGREP_FORCE_COLOR.md)
<!-- END GENERATED: facts -->

Any non-empty value turns off colour, bold and other styling in the report and
in the logs, following [no-color.org](https://no-color.org).
`OPENGREP_FORCE_NO_COLOR` and `SEMGREP_FORCE_NO_COLOR` do the same.

Opengrep decides on styling the same way for each place it writes to:

1. `--force-color`, or `OPENGREP_FORCE_COLOR` set to true, turns styling on.
2. Otherwise, `NO_COLOR` or `OPENGREP_FORCE_NO_COLOR` turns it off.
3. Otherwise, the report is styled only when standard output is a terminal,
   and the logs only when standard error is one. A text report written to a
   file is not styled.

## Examples

### `--force-color` wins

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
eval(code)
```

**Command and result:**
```console
$ NO_COLOR=1 opengrep scan --config rule.yaml --force-color app.py | grep -q $'\e\[' && echo styled || echo plain
styled
```
