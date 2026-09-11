<!-- reference
id: flag-text-output
kind: flag
name: --text-output
summary: Also write the text report to a file.
commands: [scan, ci]
value: `FILE`
related: [flag-text, flag-output]
-->
# `--text-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--text`](text.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Writes the text report to FILE, in addition to what the run prints on standard
output. A scan can print JSON for a program and leave the readable report in a
file for a person.

The file never contains colour codes, even with
[`--force-color`](force-color.md). Give the flag several times to write
several copies. When the scan finds nothing, the file is written and empty.
[`--output`](output.md) describes the rules that all output files follow.

## Examples

### JSON on screen, text in a file

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
$ opengrep scan --config rule.yaml --json --force-color --text-output report.txt app.py 2>/dev/null | jq .
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
$ grep -c $'\x1b' report.txt
0
```

The last command counts the lines with a colour code in the file: none, despite
`--force-color`.
