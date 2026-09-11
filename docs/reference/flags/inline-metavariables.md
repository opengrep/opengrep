<!-- reference
id: flag-inline-metavariables
kind: flag
name: --inline-metavariables
summary: Replace metavariables in a rule's metadata with what they matched.
commands: [scan, ci]
related: [flag-json]
-->
# `--inline-metavariables`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **See also:** [`--json`](json.md)
<!-- END GENERATED: facts -->

A finding's message always has its metavariables filled in: a rule whose
message is `found eval of $X` reports `found eval of user`. The strings in a
rule's `metadata` do not get that treatment, and reach the output as written.
`--inline-metavariables` applies the same substitution to them.

Opengrep walks the whole metadata of every finding to do it, which costs time
when the metadata is large or deeply nested.

## Examples

### A metavariable inside metadata

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: eval-call
    pattern: eval($X)
    message: found eval of $X
    metadata:
      note: "the argument was $X"
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
def handler(user):
    eval(user)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --json app.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "eval-call",
      "path": "app.py",
      "start": {
        "line": 2,
        "col": 5,
        "offset": 23
      },
      "end": {
        "line": 2,
        "col": 15,
        "offset": 33
      },
      "extra": {
        "metavars": {
          "$X": {
            "start": {
              "line": 2,
              "col": 10,
              "offset": 28
            },
            "end": {
              "line": 2,
              "col": 14,
              "offset": 32
            },
            "abstract_content": "user"
          }
        },
        "message": "found eval of user",
        "metadata": {
          "note": "the argument was $X"
        },
        "severity": "WARNING",
        "fingerprint": "5100909fef918cfe3c3b611def4015a653bb8e4446f51f5032cbaecd6b98b01f96d5cfed25a9c0d2ed7b6759ec5057fc25dfc0c99a337e60a96237f7b03baaa9_0",
        "lines": "    eval(user)",
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
$ opengrep scan --config rule.yaml --json --inline-metavariables app.py 2>/dev/null | jq .
{
  "version": "X.Y.Z",
  "results": [
    {
      "check_id": "eval-call",
      "path": "app.py",
      "start": {
        "line": 2,
        "col": 5,
        "offset": 23
      },
      "end": {
        "line": 2,
        "col": 15,
        "offset": 33
      },
      "extra": {
        "metavars": {
          "$X": {
            "start": {
              "line": 2,
              "col": 10,
              "offset": 28
            },
            "end": {
              "line": 2,
              "col": 14,
              "offset": 32
            },
            "abstract_content": "user"
          }
        },
        "message": "found eval of user",
        "metadata": {
          "note": "the argument was user"
        },
        "severity": "WARNING",
        "fingerprint": "5100909fef918cfe3c3b611def4015a653bb8e4446f51f5032cbaecd6b98b01f96d5cfed25a9c0d2ed7b6759ec5057fc25dfc0c99a337e60a96237f7b03baaa9_0",
        "lines": "    eval(user)",
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
```
