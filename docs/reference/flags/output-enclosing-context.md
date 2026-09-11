<!-- reference
id: flag-output-enclosing-context
kind: flag
name: --output-enclosing-context
aliases: [--no-output-enclosing-context]
summary: Record which function or class each finding sits in.
commands: [scan]
default: false
related: [flag-json, flag-dataflow-traces]
-->
# `--output-enclosing-context`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Also spelled:** `--no-output-enclosing-context`
- **Default:** `false`
- **See also:** [`--json`](json.md), [`--dataflow-traces`](dataflow-traces.md)
<!-- END GENERATED: facts -->

Adds an `enclosing_context` array to each finding's `extra` in the
[JSON output](json.md), naming the syntactic construct the match sits in: its
`kind`, such as `function` or `class`, its `name`, and where it starts and
ends. Off by default, so the field is absent unless the flag is given.

This gives a report enough to group findings by function, or to show a reader
where a match lives without opening the file.

## Examples

### The function a finding sits in

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
def handler(user):
    eval(user)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --json --output-enclosing-context app.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "find-eval",
      "path": "app.py",
      "start": {
        "line": 2,
        "col": 5,
        "offset": 23
      },
      "end": {
        "line": 2,
        "col": 15,
        "offset": 33
      },
      "extra": {
        "metavars": {},
        "message": "found eval",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "cc2ee9ac3b87e874db7ede72562f586db0607e44a6ebc8879fc1772bd2699a2f302d90e0cf6bde9dce450cb0891646fb0b3d70263351681e1d2e48ca0d26d960_0",
        "lines": "    eval(user)",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS",
        "enclosing_context": [
          {
            "kind": "function",
            "name": "handler",
            "start": {
              "line": 1,
              "col": 1,
              "offset": 0
            },
            "end": {
              "line": 2,
              "col": 15,
              "offset": 33
            }
          }
        ]
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
