<!-- reference
id: cmd-scan
kind: command
name: opengrep scan
summary: Run rules on files and report what they match.
related: [cmd-test, flag-config]
-->
# `opengrep scan`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep test`](test.md), [`--config`](../flags/config.md), [`opengrep ci`](ci.md), [`opengrep lsp`](lsp.md), [`opengrep validate`](validate.md)
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
| [`--allow-local-builds`](../flags/allow-local-builds.md) | Let opengrep build the project to work out its dependencies; it has nothing to act on today. |
| [`--allow-rule-timeout-control`](../flags/allow-rule-timeout-control.md) | Let rules set their own time limits, which opengrep otherwise ignores. |
| [`--autofix`](../flags/autofix.md) | Apply the fixes rules suggest, rewriting your files. |
| [`--baseline-commit`](../flags/baseline-commit.md) | Report only the findings that are not already present in the given commit. |
| [`--config`](../flags/config.md) | Load rules from a file, a directory, a URL, a git repository or the Semgrep registry. |
| [`--dataflow-traces`](../flags/dataflow-traces.md) | Show how a value reaches the finding, for taint rules. |
| [`--debug`](../flags/debug.md) | Log everything --verbose does and the engine's own diagnostics as well. |
| `--develop` | *not yet documented* |
| [`--dryrun`](../flags/dryrun.md) | Show what would be changed without changing it; the two commands mean different things by it. |
| `--dump-ast` | *not yet documented* |
| [`--dynamic-timeout`](../flags/dynamic-timeout.md) | Scale each rule's time limit with the size of the file being scanned. |
| [`--dynamic-timeout-max-multiplier`](../flags/dynamic-timeout-max-multiplier.md) | The ceiling on how much a file's size may stretch the timeout. |
| [`--dynamic-timeout-unit-kb`](../flags/dynamic-timeout-unit-kb.md) | The file size, in KB, that counts as one step when scaling the timeout. |
| [`--emacs`](../flags/emacs.md) | Print one line per finding, in the form Emacs compilation buffers parse. |
| [`--emacs-output`](../flags/emacs-output.md) | Also write the findings in Emacs single-line format to a file. |
| [`--enable-nosem`](../flags/enable-nosem.md) | Whether a nosem comment silences findings on its line; on by default. |
| [`--enable-version-check`](../flags/enable-version-check.md) | Accepted for compatibility; opengrep never checks for a newer version. |
| [`--error`](../flags/error.md) | Exit 1 when there are findings, so a script or CI job fails on them. |
| [`--exclude`](../flags/exclude.md) | Skip files and directories whose path matches a gitignore-style pattern. |
| [`--exclude-minified-files`](../flags/exclude-minified-files.md) | Skip files that look minified. |
| [`--exclude-rule`](../flags/exclude-rule.md) | Skip a rule by its id, whichever config it came from. |
| [`--experimental`](../flags/experimental.md) | Accepted for compatibility; opengrep has only the one implementation. |
| [`--files-with-matches`](../flags/files-with-matches.md) | Print only the paths of the files that have findings. |
| [`--force-color`](../flags/force-color.md) | Style the output even when it is not going to a terminal. |
| [`--force-exclude`](../flags/force-exclude.md) | Apply --include and --exclude to files named on the command line as well. |
| [`--gitlab-sast`](../flags/gitlab-sast.md) | Print the findings as a GitLab SAST report. |
| [`--gitlab-sast-output`](../flags/gitlab-sast-output.md) | Also write the findings as a GitLab SAST report to a file. |
| [`--gitlab-secrets`](../flags/gitlab-secrets.md) | Print the findings as a GitLab secret detection report. |
| [`--gitlab-secrets-output`](../flags/gitlab-secrets-output.md) | Also write the findings as a GitLab secret detection report to a file. |
| [`--guarded-taint-signatures`](../flags/guarded-taint-signatures.md) | Drop a cross-function taint finding when the branch leading to the sink cannot be taken. |
| [`--include`](../flags/include.md) | Scan only the files whose path matches one of these patterns. |
| [`--incremental-output`](../flags/incremental-output.md) | Print each finding as it is produced instead of all of them at the end. |
| [`--incremental-output-postprocess`](../flags/incremental-output.md) | *not yet documented* |
| [`--inline-metavariables`](../flags/inline-metavariables.md) | Replace metavariables in a rule's metadata with what they matched. |
| [`--interfile-timeout`](../flags/interfile-timeout.md) | Time a rule may spend on the cross-file analysis. |
| [`--jobs`](../flags/jobs.md) | How many cores run rules in parallel. |
| [`--json`](../flags/json.md) | Print the findings as a JSON document instead of the text report. |
| [`--json-output`](../flags/json-output.md) | Also write the findings as JSON to a file. |
| [`--junit-xml`](../flags/junit-xml.md) | Print the findings as a JUnit XML report, so a CI system shows them as failed tests. |
| [`--junit-xml-output`](../flags/junit-xml-output.md) | Also write the findings as a JUnit XML report to a file. |
| [`--lang`](../flags/lang.md) | The language of a command-line pattern, and of the files it is run on. |
| [`--matching-explanations`](../flags/matching-explanations.md) | Add to the JSON output a trace of how each part of a rule matched. |
| [`--max-chars-per-line`](../flags/max-chars-per-line.md) | Width at which a finding's code lines are wrapped in the text report. |
| [`--max-lines-per-finding`](../flags/max-lines-per-finding.md) | How many lines of code a finding may show before the rest is trimmed. |
| [`--max-log-list-entries`](../flags/max-log-list-entries.md) | How many items a list in the logs may show before it is replaced by a note. |
| [`--max-match-per-file`](../flags/max-match-per-file.md) | How many findings one file may have, across all rules, before they are all dropped. |
| [`--max-memory`](../flags/max-memory.md) | Memory a single file's analysis may use before it is abandoned. |
| [`--max-target-bytes`](../flags/max-target-bytes.md) | Skip files larger than this when walking a directory. |
| [`--opengrep-ignore-pattern`](../flags/opengrep-ignore-pattern.md) | Recognise one more comment prefix that silences findings on a line. |
| [`--optimizations`](../flags/optimizations.md) | Turn the engine's optimizations, chiefly the prefilter, on or off. |
| [`--output`](../flags/output.md) | Write the findings to a file instead of standard output. |
| [`--output-enclosing-context`](../flags/output-enclosing-context.md) | Record which function or class each finding sits in. |
| [`--pattern`](../flags/pattern.md) | Search with a single pattern given on the command line, instead of a rule file. |
| `--profile` | *not yet documented* |
| [`--project-root`](../flags/project-root.md) | Treat this folder as the project root, so its ignore files are read. |
| [`--quiet`](../flags/quiet.md) | Print the findings and nothing else. |
| [`--replacement`](../flags/replacement.md) | The fix for a command-line pattern, as a rule's fix key would give it. |
| [`--rewrite-rule-ids`](../flags/rewrite-rule-ids.md) | Whether a rule loaded from a directory gets its path as a prefix; on by default. |
| [`--sarif`](../flags/sarif.md) | Print the findings as a SARIF 2.1.0 document. |
| [`--sarif-output`](../flags/sarif-output.md) | Also write the findings as SARIF to a file, suppressed findings included. |
| [`--scan-unknown-extensions`](../flags/scan-unknown-extensions.md) | Scan a file named on the command line even when its extension names no language. |
| [`--semgrepignore-filename`](../flags/semgrepignore-filename.md) | Read the list of skipped paths from a differently named file. |
| [`--severity`](../flags/severity.md) | Report only the findings of rules with these severities. |
| [`--show-supported-languages`](../flags/show-supported-languages.md) | Print the languages opengrep can parse, and exit. |
| [`--skin`](../flags/skin.md) | Which layout the text report uses. |
| [`--skip-invalid-configs`](../flags/skip-invalid-configs.md) | Skip files in a rules directory that are not rule configs, instead of stopping. |
| [`--strict`](../flags/strict.md) | Fail the run when a file could not be parsed or another warning-level error occurred. |
| [`--taint-interfile`](../flags/taint-interfile.md) | Follow taint across files, for every taint rule. |
| [`--taint-interfile-depth`](../flags/taint-interfile-depth.md) | How many calls deep the cross-file taint analysis follows a chain. |
| [`--taint-intrafile`](../flags/taint-intrafile.md) | Follow taint through calls to functions defined in the same file, for every taint rule. |
| [`--test`](../flags/test.md) | Run the rule tests and scan nothing; the older spelling of the test command. |
| [`--test-ignore-todo`](../flags/test-ignore-todo.md) | Documented as ignoring todoruleid annotations; in 1.30.0 it changes nothing. |
| [`--text`](../flags/text.md) | Print the human-readable report, which is what a scan does anyway. |
| [`--text-output`](../flags/text-output.md) | Also write the text report to a file. |
| [`--time`](../flags/time.md) | Report how long the scan took, per rule and per file. |
| [`--timeout`](../flags/timeout.md) | Maximum time in seconds for one rule on one file; 0 means no limit. |
| [`--timeout-threshold`](../flags/timeout-threshold.md) | How many rules may time out on a file before opengrep gives up on that file. |
| [`--use-git-ignore`](../flags/use-git-ignore.md) | Whether .gitignore decides which files are scanned; on by default. |
| [`--validate`](../flags/validate.md) | Check the rules and scan nothing; the older spelling of the validate command. |
| [`--verbose`](../flags/verbose.md) | Log what the scan is doing, at the info level. |
| [`--version`](../flags/version.md) | Print the opengrep version and exit. |
| [`--vim`](../flags/vim.md) | Print one line per finding, in the form Vim quickfix lists parse. |
| [`--vim-output`](../flags/vim-output.md) | Also write the findings in Vim quickfix format to a file. |
| `--x-ignore-semgrepignore-files` | *not yet documented* |
| `--x-ls` | *not yet documented* |
| `--x-ls-long` | *not yet documented* |
<!-- END GENERATED: flags -->

## Examples

### Scan a file

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
def run(expr):
    return eval(expr)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  warn  find-eval
  found eval

    2 │ return eval(expr)
```

### Fail when there are findings

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
eval(input())
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
$ opengrep scan --config rule.yaml --error app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 1
```
