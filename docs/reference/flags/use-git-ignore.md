<!-- reference
id: flag-use-git-ignore
kind: flag
name: --use-git-ignore
aliases: [--no-git-ignore]
summary: Whether .gitignore decides which files are scanned; on by default.
commands: [scan, ci]
default: true
related: [flag-exclude, flag-semgrepignore-filename, flag-project-root]
-->
# `--use-git-ignore`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Also spelled:** `--no-git-ignore`
- **Default:** `true`
- **See also:** [`--exclude`](exclude.md), [`--semgrepignore-filename`](semgrepignore-filename.md), [`--project-root`](project-root.md)
<!-- END GENERATED: facts -->

Inside a git repository, opengrep skips what git ignores: the files that
`.gitignore` excludes, and git submodules. That is the default, and
`--use-git-ignore` asks for it explicitly.

`--no-git-ignore` scans those files anyway. It is what you want when the
interesting code is generated into an ignored directory, or when a vendored
dependency is worth scanning.

Outside a git repository the flag does nothing, because there is no
`.gitignore` handling to turn off. `.semgrepignore` is separate and applies
either way; see [`--semgrepignore-filename`](semgrepignore-filename.md).

## Examples

### Scanning a directory that git ignores

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

**`generated/gen.py`**
```python title="generated/gen.py"
eval(2)
```

**`.gitignore`**
```title=".gitignore"
generated/
```

**Command and result:**
```console
$ git init -q && git add -A && git commit -qm init
$ opengrep scan --config rule.yaml --files-with-matches .
app.py
$ opengrep scan --config rule.yaml --files-with-matches --no-git-ignore .
app.py
generated/gen.py
```
