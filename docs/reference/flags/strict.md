<!-- reference
id: flag-strict
kind: flag
name: --strict
aliases: [--no-strict]
summary: Fail the run when a file could not be parsed or another warning-level error occurred.
commands: [scan, test]
default: false
related: [flag-error, flag-skip-invalid-configs, cmd-validate]
-->
# `--strict`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep test`](../commands/test.md)
- **Also spelled:** `--no-strict`
- **Default:** `false`
- **See also:** [`--error`](error.md), [`--skip-invalid-configs`](skip-invalid-configs.md), [`opengrep validate`](../commands/validate.md), [`--timeout-threshold`](timeout-threshold.md)
<!-- END GENERATED: facts -->

A file opengrep cannot parse does not fail a scan: the problem is reported,
the other files are scanned, and the run exits 0. The summary mentions it —
`1 files only partially analyzed due to a parsing or internal Opengrep error`
— and the [JSON output](json.md) carries it in `errors`.

`--strict` turns those warning-level errors into a non-zero exit: status 3
when a target could not be parsed. That is what a CI job wants if a file
silently going unscanned is as bad as a finding.

Two things it does not do. It says nothing about findings, which is
[`--error`](error.md); when both are given and there are findings, the exit
status is 1. And a config file skipped with
[`--skip-invalid-configs`](skip-invalid-configs.md) stays a warning: the run
still exits 0.

## Examples

### A file that will not parse

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-key
    pattern: |
      {"a": ...}
    message: found key a
    languages: [json]
    severity: WARNING
```

**`good.json`**
```json title="good.json"
{ "a": 1 }
```

**`bad.json`**
```title="bad.json"
{ this is not json
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --json . 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "find-key",
      "path": "good.json",
      "start": {
        "line": 1,
        "col": 1,
        "offset": 0
      },
      "end": {
        "line": 1,
        "col": 11,
        "offset": 10
      },
      "extra": {
        "metavars": {},
        "message": "found key a",
        "metadata": {},
        "severity": "WARNING",
        "fingerprint": "dd28ba81312da62a48a705f3d6eb29b69c8884549abed7e279d380f6c700aeb124f7ff00aa6cd1dfb0bce2e23f75a9757e09498d12d42a17bfeffaf3e3ae280a_0",
        "lines": "{ \"a\": 1 }",
        "is_ignored": false,
        "validation_state": "NO_VALIDATOR",
        "engine_kind": "OSS"
      }
    }
  ],
  "errors": [
    {
      "code": 3,
      "level": "warn",
      "type": "Syntax error",
      "message": "Syntax error at line bad.json:1:\n `is` was unexpected",
      "path": "bad.json"
    }
  ],
  "paths": {
    "scanned": [
      "bad.json",
      "good.json"
    ]
  },
  "interfile_languages_used": [],
  "skipped_rules": []
}
$ opengrep scan --config rule.yaml . > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ opengrep scan --config rule.yaml --strict . > /dev/null 2>&1; echo "exit status: $?"
exit status: 3
```
