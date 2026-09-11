<!-- reference
id: flag-gitlab-sast-output
kind: flag
name: --gitlab-sast-output
summary: Also write the findings as a GitLab SAST report to a file.
commands: [scan, ci]
value: `FILE`
related: [flag-gitlab-sast, flag-output]
-->
# `--gitlab-sast-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--gitlab-sast`](gitlab-sast.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Writes the GitLab SAST report that [`--gitlab-sast`](gitlab-sast.md) prints to
FILE, in addition to what the run prints on standard output. In a GitLab CI
job, point the job's `artifacts:reports:sast` at this file and keep the
readable report in the job log.

Give the flag several times to write several copies. The file is written even
when the scan finds nothing. [`--output`](output.md) describes the rules that
all output files follow.

## Examples

### A report for GitLab

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
$ opengrep scan --config rule.yaml --gitlab-sast-output gl-sast-report.json app.py > /dev/null 2>&1
$ jq . gl-sast-report.json
{
  "$schema": "https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/sast-report-format.json",
  "version": "15.0.4",
  "scan": {
    "start_time": "2026-09-15T09:12:22",
    "end_time": "2026-09-15T09:12:22",
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
      "category": "sast",
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
      "details": {}
    }
  ]
}
```
