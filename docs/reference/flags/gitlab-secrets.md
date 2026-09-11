<!-- reference
id: flag-gitlab-secrets
kind: flag
name: --gitlab-secrets
summary: Print the findings as a GitLab secret detection report.
commands: [scan, ci]
related: [flag-gitlab-sast, flag-json, flag-sarif, flag-output]
-->
# `--gitlab-secrets`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--gitlab-sast`](gitlab-sast.md), [`--json`](json.md), [`--sarif`](sarif.md), [`--output`](output.md), [`--gitlab-secrets-output`](gitlab-secrets-output.md)
<!-- END GENERATED: facts -->

Prints a GitLab report in the shape of the `secret_detection` artifact. It has
the same top level as the [SAST report](gitlab-sast.md) — `$schema`,
`version`, `scan` and `vulnerabilities` — and the vulnerabilities carry
category `secret_detection`.

The flag changes the shape of the report, not which rules run: every finding
of the scan appears in it, whether or not the rule looks for a secret. Point
it at rules that find secrets, or the report will describe them as such
wrongly.

[`--gitlab-secrets-output`](gitlab-secrets-output.md) writes the same document
to a file. Only one format goes to standard output at a time.

## Examples

### The shape of the report

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
$ opengrep scan --config rule.yaml --gitlab-secrets app.py 2>/dev/null | jq .
{
  "$schema": "https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/sast-report-format.json",
  "version": "15.0.4",
  "scan": {
    "start_time": "2026-09-15T09:12:23",
    "end_time": "2026-09-15T09:12:23",
    "analyzer": {
      "id": "opengrep",
      "name": "Opengrep",
      "url": "https://opengrep.dev",
      "version": "X.Y.Z",
      "vendor": {
        "name": "Opengrep"
      }
    },
    "scanner": {
      "id": "opengrep",
      "name": "Opengrep",
      "url": "https://opengrep.dev",
      "version": "X.Y.Z",
      "vendor": {
        "name": "Opengrep"
      }
    },
    "version": "X.Y.Z",
    "status": "success",
    "type": "sast"
  },
  "vulnerabilities": [
    {
      "id": "752d5925-3fdf-44e8-5075-75f6f0d3364c",
      "category": "secret_detection",
      "cve": "app.py:568470d013cd12e4f388206520da39ab9a4e4c3c6b95846cbc281abc1ba3c959:find-eval",
      "message": "found eval",
      "description": "found eval",
      "severity": "Medium",
      "scanner": {
        "id": "opengrep",
        "name": "Opengrep",
        "vendor": {
          "name": "Opengrep"
        }
      },
      "location": {
        "file": "app.py",
        "start_line": 1,
        "end_line": 1
      },
      "identifiers": [
        {
          "type": "opengrep_type",
          "name": "Opengrep - find-eval",
          "value": "find-eval",
          "url": "not available"
        }
      ],
      "flags": [],
      "details": {},
      "raw_source_code_extract": [
        "eval(1)\n"
      ],
      "commit": {
        "date": "1970-01-01T00:00:00Z",
        "sha": "0000000"
      }
    }
  ]
}
```
