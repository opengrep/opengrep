<!-- reference
id: flag-sarif-output
kind: flag
name: --sarif-output
summary: Also write the findings as SARIF to a file, suppressed findings included.
commands: [scan, ci]
value: `FILE`
related: [flag-sarif, flag-output, flag-opengrep-ignore-pattern]
-->
# `--sarif-output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `FILE`
- **See also:** [`--sarif`](sarif.md), [`--output`](output.md), [`--opengrep-ignore-pattern`](opengrep-ignore-pattern.md)
<!-- END GENERATED: facts -->

Writes the SARIF document that [`--sarif`](sarif.md) prints to FILE, in
addition to what the run prints on standard output. This is the usual way to
produce a file for a code-scanning upload while keeping the report readable in
the log.

Unlike the other formats, SARIF keeps the findings that a `nosemgrep` comment
suppresses, and marks each with a suppression of kind `inSource`. Only the
SARIF file includes them: standard output and the other output files still
leave them out.

Give the flag several times to write several copies. The file is written even
when the scan finds nothing. [`--output`](output.md) describes the rules that
all output files follow.

## Examples

### A suppressed finding in the SARIF file

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
eval(1)  # nosemgrep
eval(2)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --sarif-output findings.sarif app.py
app.py

  warn  find-eval
  found eval

    2 │ eval(2)
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
            "matchBasedId/v1": "cc2ee9ac3b87e874db7ede72562f586db0607e44a6ebc8879fc1772bd2699a2f302d90e0cf6bde9dce450cb0891646fb0b3d70263351681e1d2e48ca0d26d960_0"
          },
          "locations": [
            {
              "physicalLocation": {
                "artifactLocation": {
                  "uri": "app.py",
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
        },
        {
          "fingerprints": {
            "matchBasedId/v1": "cc2ee9ac3b87e874db7ede72562f586db0607e44a6ebc8879fc1772bd2699a2f302d90e0cf6bde9dce450cb0891646fb0b3d70263351681e1d2e48ca0d26d960_1"
          },
          "locations": [
            {
              "physicalLocation": {
                "artifactLocation": {
                  "uri": "app.py",
                  "uriBaseId": "%SRCROOT%"
                },
                "region": {
                  "endColumn": 8,
                  "endLine": 2,
                  "snippet": {
                    "text": "eval(2)"
                  },
                  "startColumn": 1,
                  "startLine": 2
                }
              }
            }
          ],
          "message": {
            "text": "found eval"
          },
          "properties": {},
          "ruleId": "find-eval"
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

The report on screen shows only line 2. The SARIF file has both findings, and
the one on line 1 carries a suppression of kind `inSource`.
