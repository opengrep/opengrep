<!-- reference
id: cmd-scan
kind: command
name: opengrep scan
summary: Run rules on files and report what they match.
related: [cmd-test, flag-config]
-->
# `opengrep scan`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep test`](test.md), [`--config`](../flags/config.md)
<!-- END GENERATED: facts -->

```
opengrep scan [FLAGS] [TARGETS...]
```

Runs rules on the files under TARGETS and reports their findings. TARGETS are
files or directories. The default is the current directory. `scan` is the
default command, so `opengrep --config rule.yaml .` is the same as
`opengrep scan --config rule.yaml .`.

The rules come from [`--config`](../flags/config.md) or
[`OPENGREP_RULES`](../env/OPENGREP_RULES.md), or from a single pattern given
with `-e`/`--pattern` and `-l`/`--lang`. Without any of them, `scan` uses
`--config auto`, which fetches rules from the Semgrep registry.

In a directory, opengrep skips the files that `.gitignore` and `.semgrepignore`
exclude. When the project has no `.semgrepignore`, a default list applies: it
includes `node_modules/`, `vendor/`, `test/` and `tests/`. Files given
explicitly on the command line are always scanned. See
[`--exclude`](../flags/exclude.md).

Findings go to standard output, in the text format unless another format is
chosen (`--json`, `--sarif`, …). Progress and the summary go to standard
error.

## Exit status

| Status | Meaning |
|---|---|
| 0 | The scan ran. Findings do not change the status unless `--error` is given. |
| 1 | `--error` was given and there are findings. |
| 2 | Fatal error, including a bad command line or a bad environment variable value. |
| 3 | A target file could not be parsed. |
| 4 | A rule pattern could not be parsed. |
| 5 | A rule file is not valid YAML. |
| 7 | No valid configuration could be loaded, for example an invalid rule. |
| 8 | A rule names an unsupported language. |
| 141 | The reader of the output closed the pipe. |

## Flags

<!-- BEGIN GENERATED: flags -->
| Flag | Summary |
|---|---|
| `--allow-local-builds` | *not yet documented* |
| `--allow-rule-timeout-control` | *not yet documented* |
| `--autofix` | *not yet documented* |
| [`--baseline-commit`](../flags/baseline-commit.md) | Report only the findings that are not already present in the given commit. |
| [`--config`](../flags/config.md) | Load rules from a file, a directory, a URL, a git repository or the Semgrep registry. |
| `--dataflow-traces` | *not yet documented* |
| `--debug` | *not yet documented* |
| `--develop` | *not yet documented* |
| `--dryrun` | *not yet documented* |
| `--dump-ast` | *not yet documented* |
| `--dynamic-timeout` | *not yet documented* |
| `--dynamic-timeout-max-multiplier` | *not yet documented* |
| `--dynamic-timeout-unit-kb` | *not yet documented* |
| `--emacs` | *not yet documented* |
| `--emacs-output` | *not yet documented* |
| `--enable-nosem` | *not yet documented* |
| `--enable-version-check` | *not yet documented* |
| `--error` | *not yet documented* |
| [`--exclude`](../flags/exclude.md) | Skip files and directories whose path matches a gitignore-style pattern. |
| `--exclude-minified-files` | *not yet documented* |
| `--exclude-rule` | *not yet documented* |
| `--experimental` | *not yet documented* |
| `--files-with-matches` | *not yet documented* |
| `--force-color` | *not yet documented* |
| `--force-exclude` | *not yet documented* |
| `--gitlab-sast` | *not yet documented* |
| `--gitlab-sast-output` | *not yet documented* |
| `--gitlab-secrets` | *not yet documented* |
| `--gitlab-secrets-output` | *not yet documented* |
| `--guarded-taint-signatures` | *not yet documented* |
| `--include` | *not yet documented* |
| `--incremental-output` | *not yet documented* |
| `--incremental-output-postprocess` | *not yet documented* |
| `--inline-metavariables` | *not yet documented* |
| `--interfile-timeout` | *not yet documented* |
| `--jobs` | *not yet documented* |
| `--json` | *not yet documented* |
| `--json-output` | *not yet documented* |
| `--junit-xml` | *not yet documented* |
| `--junit-xml-output` | *not yet documented* |
| `--lang` | *not yet documented* |
| `--matching-explanations` | *not yet documented* |
| `--max-chars-per-line` | *not yet documented* |
| `--max-lines-per-finding` | *not yet documented* |
| `--max-log-list-entries` | *not yet documented* |
| `--max-match-per-file` | *not yet documented* |
| `--max-memory` | *not yet documented* |
| `--max-target-bytes` | *not yet documented* |
| `--opengrep-ignore-pattern` | *not yet documented* |
| `--optimizations` | *not yet documented* |
| `--output` | *not yet documented* |
| `--output-enclosing-context` | *not yet documented* |
| `--pattern` | *not yet documented* |
| `--profile` | *not yet documented* |
| `--project-root` | *not yet documented* |
| `--quiet` | *not yet documented* |
| `--replacement` | *not yet documented* |
| `--rewrite-rule-ids` | *not yet documented* |
| `--sarif` | *not yet documented* |
| `--sarif-output` | *not yet documented* |
| `--scan-unknown-extensions` | *not yet documented* |
| `--semgrepignore-filename` | *not yet documented* |
| `--severity` | *not yet documented* |
| `--show-supported-languages` | *not yet documented* |
| `--skin` | *not yet documented* |
| `--skip-invalid-configs` | *not yet documented* |
| `--strict` | *not yet documented* |
| `--taint-interfile` | *not yet documented* |
| `--taint-interfile-depth` | *not yet documented* |
| `--taint-intrafile` | *not yet documented* |
| `--test` | *not yet documented* |
| `--test-ignore-todo` | *not yet documented* |
| `--text` | *not yet documented* |
| `--text-output` | *not yet documented* |
| `--time` | *not yet documented* |
| [`--timeout`](../flags/timeout.md) | Maximum time in seconds for one rule on one file; 0 means no limit. |
| `--timeout-threshold` | *not yet documented* |
| `--use-git-ignore` | *not yet documented* |
| `--validate` | *not yet documented* |
| `--verbose` | *not yet documented* |
| `--version` | *not yet documented* |
| `--vim` | *not yet documented* |
| `--vim-output` | *not yet documented* |
| `--x-ignore-semgrepignore-files` | *not yet documented* |
| `--x-ls` | *not yet documented* |
| `--x-ls-long` | *not yet documented* |
<!-- END GENERATED: flags -->

## Examples

### Scan a file

```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```python title="app.py"
def run(expr):
    return eval(expr)
```

```console
$ opengrep scan --config rule.yaml app.py


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    app.py
    ❯❱ find-eval
          found eval

            2┆ return eval(expr)
```

### Fail when there are findings

```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

```python title="app.py"
eval(input())
```

```console
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ opengrep scan --config rule.yaml --error app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 1
```
