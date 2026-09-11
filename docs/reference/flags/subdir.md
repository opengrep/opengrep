<!-- reference
id: flag-subdir
kind: flag
name: --subdir
summary: Scan only this directory, while the repository still provides the CI metadata.
commands: [ci]
value: `PATH`, relative to the current directory
related: [cmd-ci, flag-exclude]
-->
# `--subdir`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep ci`](../commands/ci.md)
- **Value:** `PATH`, relative to the current directory
- **See also:** [`opengrep ci`](../commands/ci.md), [`--exclude`](exclude.md)
<!-- END GENERATED: facts -->

[`opengrep ci`](../commands/ci.md) scans the directory it runs in, which is
meant to be the root of the repository. `--subdir PATH` narrows the scan to
PATH while the repository, branch, commit and baseline still come from the
whole repository. This is for monorepos, where one repository holds several
projects that are scanned separately.

PATH must name a directory inside the current one. Otherwise `ci` reports:

```
`opengrep ci --subdir` must be given a directory that is actually a subdirectory of the current directory
```

Because errors are suppressed by default, `ci` then still exits 0. Add
[`--no-suppress-errors`](suppress-errors.md) for that mistake to fail the job.

## Examples

### Scanning one directory of a repository

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

**`sub/other.py`**
```python title="sub/other.py"
eval(2)
```

**Command and result:**
```console
$ git init -q && git add . && git commit -qm init
$ opengrep ci --config rule.yaml --subdir sub


┌─────────────────────────┐
│ 1 Blocking Code Finding │
└─────────────────────────┘

    sub/other.py
    ❯❱ find-eval
          found eval

            1┆ eval(2)


  BLOCKING CODE RULES FIRED:
    find-eval
```
