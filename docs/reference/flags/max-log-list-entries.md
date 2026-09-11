<!-- reference
id: flag-max-log-list-entries
kind: flag
name: --max-log-list-entries
summary: How many items a list in the logs may show before it is replaced by a note.
commands: [scan, ci]
value: `INT`
default: 100
related: [flag-verbose, flag-quiet, flag-max-lines-per-finding]
-->
# `--max-log-list-entries`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`
- **Default:** `100`
- **See also:** [`--verbose`](verbose.md), [`--quiet`](quiet.md), [`--max-lines-per-finding`](max-lines-per-finding.md)
<!-- END GENERATED: facts -->

The logs list things: the rules that will run, the files that were skipped and
why, and so on. A list longer than this is left out and replaced by a note
saying so, which keeps a scan of a large repository from burying its logs. Zero
or a negative value removes the limit.

It changes the logs on standard error only, never the findings.

## Examples

### A list that is too long to show

**`rules.yaml`**
```yaml title="rules.yaml"
rules:
  - id: r1
    pattern: eval(...)
    message: m
    languages: [python]
    severity: INFO
  - id: r2
    pattern: exec(...)
    message: m
    languages: [python]
    severity: INFO
  - id: r3
    pattern: print(...)
    message: m
    languages: [python]
    severity: INFO
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config rules.yaml --verbose app.py 2>&1 >/dev/null | grep -A3 '^Rules:'
Rules:
- r1
- r2
- r3
$ opengrep scan --config rules.yaml --verbose --max-log-list-entries 2 app.py 2>&1 >/dev/null | grep -A1 '^Rules:'
Rules:
<SKIPPED DATA (too many entries; adjust with --max-log-list-entries)>
```
