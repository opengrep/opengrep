<!-- reference
id: flag-text
kind: flag
name: --text
summary: Print the human-readable report, which is what a scan does anyway.
commands: [scan, ci]
related: [flag-skin, flag-json, flag-output, flag-max-chars-per-line, flag-max-lines-per-finding]
-->
# `--text`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--skin`](skin.md), [`--json`](json.md), [`--output`](output.md), [`--max-chars-per-line`](max-chars-per-line.md), [`--max-lines-per-finding`](max-lines-per-finding.md), [`--text-output`](text-output.md)
<!-- END GENERATED: facts -->

Asks for the human-readable report. A scan prints it anyway, so the flag
matters only when something else would have chosen a different format.

How the report looks is set by [`--skin`](skin.md), and how much of a match it
shows by [`--max-chars-per-line`](max-chars-per-line.md) and
[`--max-lines-per-finding`](max-lines-per-finding.md).

[`--text-output`](text-output.md) writes the report to a file. Unlike
[`-o`](output.md) it does not take over standard output, so a run can print
JSON for a machine and leave the readable report in a file for a person.

## Examples

### A readable report beside the JSON

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
$ opengrep scan --config rule.yaml --json --text-output report.txt app.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "find-eval",
      "path": "app.py",
      "start": {
        "line": 1,
        "col": 1,
        "offset": 0
      },
      "end": {
        "line": 1,
        "col": 8,
        "offset": 7
      },
      "extra": {
        "metavars": {},
        "message": "found eval",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "cc2ee9ac3b87e874db7ede72562f586db0607e44a6ebc8879fc1772bd2699a2f302d90e0cf6bde9dce450cb0891646fb0b3d70263351681e1d2e48ca0d26d960_0",
        "lines": "eval(1)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    }
  ],
  "errors": [],
  "paths": {
    "scanned": [
      "app.py"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
$ cat report.txt
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
