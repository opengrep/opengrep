<!-- reference
id: flag-gitlab-sast
kind: flag
name: --gitlab-sast
summary: Print the findings as a GitLab SAST report.
commands: [scan, ci]
related: [flag-gitlab-secrets, flag-json, flag-sarif, flag-output]
-->
# `--gitlab-sast`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--gitlab-secrets`](gitlab-secrets.md), [`--json`](json.md), [`--sarif`](sarif.md), [`--output`](output.md), [`--gitlab-sast-output`](gitlab-sast-output.md), [`--junit-xml`](junit-xml.md)
<!-- END GENERATED: facts -->

Prints a [GitLab SAST report](https://docs.gitlab.com/ee/development/integrations/secure.html),
the document a GitLab job hands to the platform as its `sast` artifact. It
holds `$schema`, `version`, `scan` (what ran) and `vulnerabilities` (the
findings). Each vulnerability has category `sast`, the rule's message and
description, a `severity` translated from the rule's own, the location, and an
identifier built from the file and the rule id.

[`--gitlab-sast-output`](gitlab-sast-output.md) writes the same document to a
file, which is the usual way to use it: the job prints its normal report and
leaves the artifact behind. Only one format goes to standard output at a time.

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
$ opengrep scan --config rule.yaml --gitlab-sast app.py 2>/dev/null | jq .
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
