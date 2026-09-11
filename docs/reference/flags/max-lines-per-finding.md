<!-- reference
id: flag-max-lines-per-finding
kind: flag
name: --max-lines-per-finding
summary: How many lines of code a finding may show before the rest is trimmed.
commands: [scan, ci]
value: `INT`
default: 10
related: [flag-max-chars-per-line, flag-skin]
-->
# `--max-lines-per-finding`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`
- **Default:** `10`
- **See also:** [`--max-chars-per-line`](max-chars-per-line.md), [`--skin`](skin.md), [`--max-log-list-entries`](max-log-list-entries.md), [`--text`](text.md)
<!-- END GENERATED: facts -->

A finding that spans many lines is trimmed in the text report after this many
lines, and a line saying how many were left out takes their place. `0` shows
the match in full however long it is. This only changes the report; the finding
itself is unaffected, and the machine-readable formats always carry the whole
match.

## Examples

### Trimming a match that spans several lines

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: eval-call
    pattern: eval($X)
    message: found eval of $X
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(
  1
)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  warn  eval-call
  found eval of 1

    1 │ eval(
    2 │   1
    3 │ )

$ opengrep scan --config rule.yaml --max-lines-per-finding 1 app.py
app.py

  warn  eval-call
  found eval of 1

    1 │ eval(
    … 2 lines more
```
