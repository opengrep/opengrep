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
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `PATTERN`, repeatable
- **See also:** [`--include`](include.md), [`--force-exclude`](force-exclude.md), [`--semgrepignore-filename`](semgrepignore-filename.md), [`paths`](../rule-syntax/paths.md), [`--exclude-minified-files`](exclude-minified-files.md), [`--exclude-rule`](exclude-rule.md), [`--max-target-bytes`](max-target-bytes.md), [`--subdir`](subdir.md), [`--use-git-ignore`](use-git-ignore.md)
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
eval(code)
```

**`generated/gen.py`**
```python title="generated/gen.py"
eval(code)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --exclude generated .
app.py

  warn  find-eval
  found eval

    1 │ eval(code)
```

### Files named on the command line

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`generated/gen.py`**
```python title="generated/gen.py"
eval(code)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --exclude generated generated/gen.py
generated/gen.py

  warn  find-eval
  found eval

    1 │ eval(code)

$ opengrep scan --config rule.yaml --exclude 'gen.py' --force-exclude generated/gen.py
```

The second command prints nothing: `--force-exclude` applies the pattern to
the file named on the command line, so nothing is scanned and there are no
findings.
