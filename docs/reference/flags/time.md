<!-- reference
id: flag-time
kind: flag
name: --time
aliases: [--no-time]
summary: Report how long the scan took, per rule and per file.
commands: [scan, ci]
default: false
related: [flag-json, flag-jobs, flag-timeout]
-->
# `--time`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `--no-time`
- **Default:** `false`
- **See also:** [`--json`](json.md), [`--jobs`](jobs.md), [`--timeout`](timeout.md)
<!-- END GENERATED: facts -->

Adds timings to the run. The text report gains a summary on standard error:
the total, the time spent loading rules and running the engine, and the
slowest files and rules. With [`--json`](json.md) the document gains a `time`
object holding the time for each pair of rule and target, the parse times, the
peak memory, and the number of bytes scanned.

The numbers depend on the machine and the run, so they are for finding the
expensive rule or file, not for comparing across machines. The opengrep source
describes this output as meant for internal use, and it may change between
releases.

## Examples

### The timing object in the JSON output

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
$ opengrep scan --config rule.yaml --json --time app.py 2>/dev/null | jq .
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
  "time": {
    "rules": [
      "find-eval"
    ],
    "rules_parse_time": 0.00022482872009277344,
    "profiling_times": {
      "config_time": 0.00023102760314941406,
      "core_time": 0.005033969879150391,
      "ignores_time": 0.00004291534423828125,
      "total_time": 0.005557060241699219
    },
    "targets": [
      {
        "path": "app.py",
        "num_bytes": 8,
        "match_times": [
          [
            "find-eval",
            0.000030994415283203125
          ]
        ],
        "parse_time": 0.00011205673217773438,
        "run_time": 0.00021195411682128906
      }
    ],
    "total_bytes": 8,
    "max_memory_bytes": 91444224
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
```
