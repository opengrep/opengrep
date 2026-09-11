<!-- reference
id: flag-json-output
kind: flag
name: --json-output
summary: Also write the findings as JSON to a file.
commands: [scan, ci]
value: `FILE`
related: [flag-json, flag-output]
-->
# `--json-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--json`](json.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Writes the JSON document that [`--json`](json.md) prints to FILE, in addition
to what the run prints on standard output. A scan can show its usual report on
screen and leave a JSON copy for another program.

Give the flag several times to write several copies, and combine it with the
other `--<format>-output` flags to write several formats in one scan. The file
is written even when the scan finds nothing. [`--output`](output.md) describes
the rules that all output files follow.

## Examples

### A report on screen and a JSON copy

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
$ opengrep scan --config rule.yaml --json-output findings.json app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
$ jq . findings.json
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
```
