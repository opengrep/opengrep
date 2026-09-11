<!-- reference
id: flag-project-root
kind: flag
name: --project-root
summary: Treat this folder as the project root, so its ignore files are read.
commands: [scan]
value: `PATH`
related: [flag-semgrepignore-filename, flag-use-git-ignore]
-->
# `--project-root`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Value:** `PATH`
- **See also:** [`--semgrepignore-filename`](semgrepignore-filename.md), [`--use-git-ignore`](use-git-ignore.md)
<!-- END GENERATED: facts -->

Opengrep works the project root out for itself: the git repository the
scanning root sits in, or, with no version control, the folder being scanned.
The root is where it looks for `.gitignore` and `.semgrepignore`.

That guess is wrong when you scan a subdirectory of a project whose ignore
file lives further up: the file is never read, and paths it lists are scanned.
`--project-root PATH` settles the question, and treats the project as one
without version control.

## Examples

### Scanning a subdirectory of a project

**`proj/rule.yaml`**
```yaml title="proj/rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`proj/src/a.py`**
```python title="proj/src/a.py"
eval(1)
```

**`proj/skipme/s.py`**
```python title="proj/skipme/s.py"
eval(2)
```

**`proj/.semgrepignore`**
```title="proj/.semgrepignore"
skipme/
```

**Command and result:**
```console
$ opengrep scan --config proj/rule.yaml --files-with-matches proj/skipme
proj/skipme/s.py
$ opengrep scan --config proj/rule.yaml --files-with-matches --project-root proj proj/skipme
```

The first command scans a directory that `proj/.semgrepignore` lists, because
the scanning root is `proj/skipme` and the ignore file above it is never read.
The second prints nothing: with the project root named, the file applies.
