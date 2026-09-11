<!-- reference
id: flag-gitlab-secrets-output
kind: flag
name: --gitlab-secrets-output
summary: Also write the findings as a GitLab secret detection report to a file.
commands: [scan, ci]
value: `FILE`
related: [flag-gitlab-secrets, flag-output]
-->
# `--gitlab-secrets-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--gitlab-secrets`](gitlab-secrets.md), [`--output`](output.md)
<!-- END GENERATED: facts -->

Writes the GitLab secret detection report that
[`--gitlab-secrets`](gitlab-secrets.md) prints to FILE, in addition to what the
run prints on standard output. In a GitLab CI job, point the job's
`artifacts:reports:secret_detection` at this file.

Every finding of the scan goes into the report, whatever its rule looks for,
so run only secret-detection rules with this flag.

Give the flag several times to write several copies. The file is written even
when the scan finds nothing. [`--output`](output.md) describes the rules that
all output files follow.

## Examples

### A report for GitLab

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: hardcoded-token
    pattern: token = "..."
    message: hard-coded token
    languages: [python]
    severity: ERROR
```

**`settings.py`**
```python title="settings.py"
token = "ghp_example"
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --gitlab-secrets-output gl-secret-detection-report.json settings.py > /dev/null 2>&1
$ jq . gl-secret-detection-report.json
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
      "id": "9922a53e-15e4-ee1d-711c-c9e1868f4b3b",
      "category": "secret_detection",
      "cve": "settings.py:dc2c79b556e950acc7dea90175fd5febba22bf9fa4faaba1b61c3b3d5acdc9e9:hardcoded-token",
      "message": "hard-coded token",
      "description": "hard-coded token",
      "severity": "High",
      "scanner": {
        "id": "opengrep",
        "name": "Opengrep",
        "vendor": {
          "name": "Opengrep"
        }
      },
      "location": {
        "file": "settings.py",
        "start_line": 1,
        "end_line": 1
      },
      "identifiers": [
        {
          "type": "opengrep_type",
          "name": "Opengrep - hardcoded-token",
          "value": "hardcoded-token",
          "url": "not available"
        }
      ],
      "flags": [],
      "details": {},
      "raw_source_code_extract": [
        "token = \"ghp_example\"\n"
      ],
      "commit": {
        "date": "1970-01-01T00:00:00Z",
        "sha": "0000000"
      }
    }
  ]
}
```
