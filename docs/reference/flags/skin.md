<!-- reference
id: flag-skin
kind: flag
name: --skin
summary: Which layout the text report uses.
commands: [scan]
value: `legacy`, `simple` or `vivid`
related: [flag-force-color, flag-max-chars-per-line, flag-max-lines-per-finding, flag-text]
-->
# `--skin`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Value:** `legacy`, `simple` or `vivid`
- **See also:** [`--force-color`](force-color.md), [`--max-chars-per-line`](max-chars-per-line.md), [`--max-lines-per-finding`](max-lines-per-finding.md), [`--text`](text.md), [`COLUMNS`](../env/COLUMNS.md)
<!-- END GENERATED: facts -->

Chooses how the text report is laid out. The findings are the same in each;
only their presentation differs.

| Value | Layout |
|---|---|
| `legacy` | The report opengrep has always printed: findings inside drawn boxes, with a heading for the count. |
| `simple` | Terse and unadorned: the file, then each finding as a severity word, the rule id, the message and the line. |
| `vivid` | Uses colour and rules across the width of the terminal, which is easiest to read on a terminal that has both. |

The width comes from the terminal, or from `COLUMNS` when the output is not a
terminal. Colour follows the usual rules: on a terminal unless `NO_COLOR` is
set, and always with [`--force-color`](force-color.md).

## Examples

### The same finding in each layout

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
$ opengrep scan --config rule.yaml --skin simple app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)

$ COLUMNS=40 opengrep scan --config rule.yaml --skin vivid app.py
app.py ─────────────────────────────────

  ▌  WARN   find-eval
  ▌ found eval
  ▌
  ▌   1   eval(1)

$ opengrep scan --config rule.yaml --skin legacy app.py


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    app.py
    ❯❱ find-eval
          found eval

            1┆ eval(1)
```
