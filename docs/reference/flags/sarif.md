<!-- reference
id: flag-sarif
kind: flag
name: --sarif
summary: Print the findings as a SARIF 2.1.0 document.
commands: [scan, ci]
related: [flag-json, flag-output, flag-dataflow-traces, flag-gitlab-sast]
-->
# `--sarif`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--json`](json.md), [`--output`](output.md), [`--dataflow-traces`](dataflow-traces.md), [`--gitlab-sast`](gitlab-sast.md), [`--gitlab-secrets`](gitlab-secrets.md), [`--junit-xml`](junit-xml.md), [`--sarif-output`](sarif-output.md)
<!-- END GENERATED: facts -->

Prints the findings as [SARIF](https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/sarif-v2.1.0-os.html)
2.1.0, the format GitHub code scanning and many other tools read. The document
has `version`, `$schema` and one entry in `runs`, which holds `tool` (the
rules, with their metadata), `results` (the findings, each with its locations
and fingerprints) and `invocations`.

[`--dataflow-traces`](dataflow-traces.md) adds the path of a tainted value to
the SARIF results, as it does for the text report.

[`--sarif-output`](sarif-output.md) writes the same document to a file,
alongside whatever the run prints, which is the usual way to leave SARIF
behind for an uploader. Only one format goes to standard output at a time.

## Examples

### The shape of the document

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
$ opengrep scan --config rule.yaml --sarif app.py 2>/dev/null | jq .
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
                    "text": "eval(1)"
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
