<!-- reference
id: flag-json
kind: flag
name: --json
summary: Print the findings as a JSON document instead of the text report.
commands: [scan, ci, test, show]
related: [flag-sarif, flag-emacs, flag-vim, flag-output, flag-time, flag-matching-explanations]
-->
# `--json`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md), [`opengrep show`](../commands/show.md)
- **See also:** [`--sarif`](sarif.md), [`--emacs`](emacs.md), [`--vim`](vim.md), [`--output`](output.md), [`--time`](time.md), [`--matching-explanations`](matching-explanations.md), [`--files-with-matches`](files-with-matches.md), [`--gitlab-sast`](gitlab-sast.md), [`--gitlab-secrets`](gitlab-secrets.md), [`--html`](html.md), [`--incremental-output`](incremental-output.md), [`--inline-metavariables`](inline-metavariables.md), [`--json-output`](json-output.md), [`--junit-xml`](junit-xml.md), [`--output-enclosing-context`](output-enclosing-context.md), [`--text`](text.md)
<!-- END GENERATED: facts -->

Prints one JSON document on standard output. Its top level holds `version`,
`results`, `errors`, `paths`, `skipped_rules` and `interfile_languages_used`.
Each entry of `results` names the rule as `check_id`, the file as `path`, the
`start` and `end` of the match as line, column and byte offset, and carries
the rest under `extra`: the `message` with its metavariables filled in, the
`metavars` themselves, the rule's `metadata` and `severity`, the matched
`lines`, and a `fingerprint` that stays the same when code moves.

[`--time`](time.md) adds a `time` object, and
[`--matching-explanations`](matching-explanations.md) an `explanations` one.

[`--json-output`](json-output.md) writes the same document to a file,
alongside whatever the run prints. Only one format goes to standard output at
a time, so `--json` cannot be combined with `--sarif`, `--emacs`, `--vim` and
the others.

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
$ opengrep scan --config rule.yaml --json app.py 2>/dev/null | jq .
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
```
