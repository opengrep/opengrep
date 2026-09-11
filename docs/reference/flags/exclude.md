<!-- reference
id: flag-exclude
kind: flag
name: --exclude
summary: Skip files and directories whose path matches a gitignore-style pattern.
commands: [scan, ci]
value: `PATTERN`, repeatable
related: [flag-include, flag-force-exclude, flag-semgrepignore-filename, key-paths]
-->
# `--exclude`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), `ci`
- **Value:** `PATTERN`, repeatable
- **See also:** `--include`, `--force-exclude`, `--semgrepignore-filename`, [`paths`](../rule-syntax/paths.md)
<!-- END GENERATED: facts -->

Skips the files and directories whose path matches PATTERN. PATTERN uses the
[gitignore syntax](https://git-scm.com/docs/gitignore#_pattern_format):

- A pattern without a slash, or with only a trailing slash, matches at any
  depth. `--exclude=tests` skips `tests/foo.py` and `a/b/tests/c/foo.py`, and
  `--exclude='*.py'` skips every Python file.
- A slash anywhere else anchors the pattern at the project root.
  `--exclude=/tests` and `--exclude=tests/foo.py` skip `tests/foo.py` but not
  `a/b/tests/c/foo.py`.
- `**/` at the start matches at any depth.

Quote patterns that contain `*`, or the shell expands them. Give `--exclude`
several times to skip paths that match any of the patterns.

`--exclude` adds to the other filters on the files opengrep finds under
directory targets: `.gitignore`, `.semgrepignore`, and the default ignore list
(see [`opengrep scan`](../commands/scan.md)). `--include` then keeps only the
files matching one of its patterns. A rule's own [`paths`](../rule-syntax/paths.md)
narrow the files further for that rule.

Files named on the command line are scanned even when they match. With
`--force-exclude`, the patterns apply to them too, but currently only the
patterns that match the file name, such as `*.py` or `gen.py`, do so.

## Examples

### Skip a directory

```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```python title="app.py"
eval(code)
```

```python title="generated/gen.py"
eval(code)
```

```console
$ opengrep scan --config rule.yaml --exclude generated .


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    app.py
    ❯❱ find-eval
          found eval

            1┆ eval(code)
```

### Files named on the command line

```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```python title="generated/gen.py"
eval(code)
```

```console
$ opengrep scan --config rule.yaml --exclude generated generated/gen.py


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    generated/gen.py
    ❯❱ find-eval
          found eval

            1┆ eval(code)

$ opengrep scan --config rule.yaml --exclude 'gen.py' --force-exclude generated/gen.py
```
