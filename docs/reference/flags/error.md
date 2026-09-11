<!-- reference
id: flag-error
kind: flag
name: --error
aliases: [--no-error]
summary: Exit 1 when there are findings, so a script or CI job fails on them.
commands: [scan]
default: false
related: [flag-severity, cmd-ci, flag-strict]
-->
# `--error`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Also spelled:** `--no-error`
- **Default:** `false`
- **See also:** [`--severity`](severity.md), [`opengrep ci`](../commands/ci.md), [`--strict`](strict.md)
<!-- END GENERATED: facts -->

A scan that runs to the end exits 0 whether or not it found anything: findings
are a result, not a failure. `--error` makes any finding exit 1 instead, which
is what a script or a CI step needs in order to fail.

[`opengrep ci`](../commands/ci.md) already does this for blocking findings and
needs no flag. To fail on some findings only, narrow the run first with
[`--severity`](severity.md) or [`--exclude-rule`](exclude-rule.md).

`--error` says nothing about errors during the scan, such as a file that would
not parse; that is [`--strict`](strict.md).

Findings suppressed by a `nosemgrep` comment do not count, including those
that [`--sarif-output`](sarif-output.md) keeps in its file.

## Examples

### Failing on a finding

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
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ opengrep scan --config rule.yaml --error app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 1
```

### A suppressed finding

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`quiet.py`**
```python title="quiet.py"
eval(1)  # nosemgrep
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --error --sarif-output findings.sarif quiet.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ jq . findings.sarif
{
  "version": "2.1.0",
  "runs": [
    {
      "invocations": [
        {
          "executionSuccessful": true,
          "toolExecutionNotifications": []
        }
      ],
      "results": [
        {
          "fingerprints": {
            "matchBasedId/v1": "3ac126e0ba6a8506917fc31d10d52dea19c7f0f2f2db7491b641a88226e282df010772ba42b9349cd5fd080302c9a51ea357ac8786faf9b66044b83e75f6aaf3_0"
          },
          "locations": [
            {
              "physicalLocation": {
                "artifactLocation": {
                  "uri": "quiet.py",
                  "uriBaseId": "%SRCROOT%"
                },
                "region": {
                  "endColumn": 8,
                  "endLine": 1,
                  "snippet": {
                    "text": "eval(1)  # nosemgrep"
                  },
                  "startColumn": 1,
                  "startLine": 1
                }
              }
            }
          ],
          "message": {
            "text": "found eval"
          },
          "properties": {},
          "ruleId": "find-eval",
          "suppressions": [
            {
              "kind": "inSource"
            }
          ]
        }
      ],
      "tool": {
        "driver": {
          "name": "Opengrep OSS",
          "rules": [
            {
              "defaultConfiguration": {
                "level": "warning"
              },
              "fullDescription": {
                "text": "found eval"
              },
              "help": {
                "markdown": "found eval",
                "text": "found eval"
              },
              "id": "find-eval",
              "name": "find-eval",
              "properties": {
                "precision": "very-high",
                "tags": []
              },
              "shortDescription": {
                "text": "Opengrep Finding: find-eval"
              }
            }
          ],
          "semanticVersion": "X.Y.Z"
        }
      }
    }
  ],
  "$schema": "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
}
```

The SARIF file reports the suppressed finding, but the scan still exits 0.
