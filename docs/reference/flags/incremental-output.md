<!-- reference
id: flag-incremental-output
kind: flag
name: --incremental-output
covers: [flag-incremental-output-postprocess]
summary: Print each finding as it is produced instead of all of them at the end.
commands: [scan]
related: [flag-json, flag-output, flag-jobs]
-->
# `--incremental-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`--json`](json.md), [`--output`](output.md), [`--jobs`](jobs.md)
<!-- END GENERATED: facts -->

Opengrep normally collects every finding and prints the report when the scan
ends. With this flag it prints each finding as soon as it has it, which lets
an editor or a dashboard show results while a long scan is still running.

This changes the shape of the [JSON output](json.md). Instead of one document
with a `results` array, opengrep prints one JSON object per finding, each on
its own line: newline-delimited JSON, which a reader must parse line by line.
The wrapper fields of a normal JSON run — `version`, `errors`, `paths` — are
not part of the stream.

`--incremental-output-postprocess` applies the post-processing steps to that
stream. It is documented as requiring `--incremental-output`; given on its
own it is ignored rather than refused.

## Examples

### One object per finding

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
eval(2)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --json app.py 2>/dev/null | jq .
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
    },
    {
      "check_id": "find-eval",
      "path": "app.py",
      "start": {
        "line": 2,
        "col": 1,
        "offset": 8
      },
      "end": {
        "line": 2,
        "col": 8,
        "offset": 15
      },
      "extra": {
        "metavars": {},
        "message": "found eval",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "cc2ee9ac3b87e874db7ede72562f586db0607e44a6ebc8879fc1772bd2699a2f302d90e0cf6bde9dce450cb0891646fb0b3d70263351681e1d2e48ca0d26d960_1",
        "lines": "eval(2)",
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
$ opengrep scan --config rule.yaml --json --incremental-output app.py 2>/dev/null
{"check_id":"find-eval","path":"app.py","start":{"line":1,"col":1,"offset":0},"end":{"line":1,"col":8,"offset":7},"extra":{"metavars":{},"message":"found eval","metadata":{},"severity":"WARNING","fingerprint":"cc2ee9ac3b87e874db7ede72562f586db0607e44a6ebc8879fc1772bd2699a2f302d90e0cf6bde9dce450cb0891646fb0b3d70263351681e1d2e48ca0d26d960","lines":"eval(1)","is_ignored":false,"validation_state":"NO_VALIDATOR","engine_kind":"OSS"}}
{"check_id":"find-eval","path":"app.py","start":{"line":2,"col":1,"offset":8},"end":{"line":2,"col":8,"offset":15},"extra":{"metavars":{},"message":"found eval","metadata":{},"severity":"WARNING","fingerprint":"cc2ee9ac3b87e874db7ede72562f586db0607e44a6ebc8879fc1772bd2699a2f302d90e0cf6bde9dce450cb0891646fb0b3d70263351681e1d2e48ca0d26d960","lines":"eval(2)","is_ignored":false,"validation_state":"NO_VALIDATOR","engine_kind":"OSS"}}
```

The first command prints one document, formatted here by `jq`. The second
prints one JSON object per finding, each on a line of its own; together the
lines are not a single JSON document.
