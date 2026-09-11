<!-- reference
id: flag-output
kind: flag
name: --output
aliases: [-o]
summary: Write the findings to a file instead of standard output.
commands: [scan, ci]
value: `FILE`
related: [flag-json, flag-text, flag-emacs, flag-vim, flag-sarif]
-->
# `--output`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `-o`
- **Value:** `FILE`
- **See also:** [`--json`](json.md), [`--text`](text.md), [`--emacs`](emacs.md), [`--vim`](vim.md), [`--sarif`](sarif.md), [`--emacs-output`](emacs-output.md), [`--files-with-matches`](files-with-matches.md), [`--gitlab-sast-output`](gitlab-sast-output.md), [`--gitlab-sast`](gitlab-sast.md), [`--gitlab-secrets-output`](gitlab-secrets-output.md), [`--gitlab-secrets`](gitlab-secrets.md), [`--incremental-output`](incremental-output.md), [`--json-output`](json-output.md), [`--junit-xml-output`](junit-xml-output.md), [`--junit-xml`](junit-xml.md), [`--sarif-output`](sarif-output.md), [`--text-output`](text-output.md), [`--vim-output`](vim-output.md)
<!-- END GENERATED: facts -->

Sends the report to FILE. Standard output then stays empty, while the
progress and the summary still go to standard error. The format is whatever
the run would otherwise print, so `-o` writes the text report unless
[`--json`](json.md), [`--sarif`](sarif.md) or another format flag is given.

To keep the report on screen *and* save a copy, use the per-format flags
instead: [`--text-output`](text-output.md), [`--json-output`](json-output.md),
[`--emacs-output`](emacs-output.md), [`--vim-output`](vim-output.md),
[`--sarif-output`](sarif-output.md),
[`--gitlab-sast-output`](gitlab-sast-output.md),
[`--gitlab-secrets-output`](gitlab-secrets-output.md) and
[`--junit-xml-output`](junit-xml-output.md). Each can be given several times,
and they combine with each other, with `-o`, and with any format on standard
output.

All these output files follow the same rules:

- Missing parent directories are created.
- The file is written even when the scan finds nothing. A text, Emacs or Vim
  file is then empty.
- Two formats cannot go to the same file. The run stops before the scan, with
  exit status 2 and the message `Cannot write both --json-output and
  --sarif-output to FILE`, or, when one of them is `-o`, `Invalid output
  configuration: same output destination (FILE) with multiple formats.`
- A destination that is a symbolic link is refused before the scan, with
  `Output is symlink: FILE`, and the file it points to is left alone.
- A URL is refused before the scan: `Sending output to a URL (URL) is not
  supported yet by opengrep`.
- A destination that cannot be written, such as a file in a read-only
  directory, stops the run with exit status 2 once the scan has finished:
  `Cannot write output: FILE: Permission denied`.
- With [`--incremental-output`](incremental-output.md), `-o` is ignored with
  the warning `Writing incremental output to a file is not supported`. The
  per-format flags still write their files.

## Examples

### Writing the report to a file

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
$ opengrep scan --config rule.yaml -o findings.txt app.py
$ cat findings.txt
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```

The first command prints nothing: the report went to the file.

### JSON in one file, SARIF in another

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
$ opengrep scan --config rule.yaml --json -o findings.json --sarif-output findings.sarif app.py 2>/dev/null
$ jq . findings.json
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

### Two formats, one file

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
$ opengrep scan --config rule.yaml --json-output findings.out --sarif-output findings.out app.py 2>&1; echo "exit status: $?"
[00.00][ERROR]: Cannot write both --json-output and --sarif-output to findings.out
exit status: 2
$ test -e findings.out || echo "no file written"
no file written
```

### A symbolic link as the destination

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
$ printf 'keep me\n' > notes.txt
$ ln -s notes.txt findings.txt
$ opengrep scan --config rule.yaml -o findings.txt app.py 2>&1 | grep symlink
[00.00][ERROR]: Output is symlink: findings.txt
$ cat notes.txt
keep me
```
