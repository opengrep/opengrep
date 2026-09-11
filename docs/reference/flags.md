# Flags

<!-- BEGIN GENERATED: stamp -->
> Reference for **opengrep 1.30.0** (commit `d094c70bb`).
<!-- END GENERATED: stamp -->

A flag is documented once, even when several commands accept it. The
*Accepted by* column lists those commands. Flags may come before or after the
targets. A flag with a value takes it either as `--flag=VALUE` or as
`--flag VALUE`.

When a flag has an equivalent environment variable and both are given, the
flag wins and opengrep logs a warning naming the ignored variable. See
[Environment variables](environment.md).

<!-- BEGIN GENERATED: index -->
| Flag | Accepted by | Environment | Summary |
|---|---|---|---|
| [`--allow-local-builds`](flags/allow-local-builds.md) | scan, ci |  | Let opengrep build the project to work out its dependencies; it has nothing to act on today. |
| [`--allow-rule-timeout-control`](flags/allow-rule-timeout-control.md) | scan, ci |  | Let rules set their own time limits, which opengrep otherwise ignores. |
| [`--audit-on`](flags/audit-on.md) | ci | `OPENGREP_AUDIT_ON` | Report blocking findings but exit 0 when the CI event has one of these names. |
| [`--autofix`](flags/autofix.md) | scan |  | Apply the fixes rules suggest, rewriting your files. |
| [`--baseline-commit`](flags/baseline-commit.md) | scan, ci | `OPENGREP_BASELINE_COMMIT`, `OPENGREP_BASELINE_REF` | Report only the findings that are not already present in the given commit. |
| [`--config`](flags/config.md) | scan, ci, test | `OPENGREP_RULES` | Load rules from a file, a directory, a URL, a git repository or the Semgrep registry. |
| [`--dataflow-traces`](flags/dataflow-traces.md) | scan, ci |  | Show how a value reaches the finding, for taint rules. |
| [`--debug`](flags/debug.md) | scan, ci, test, validate, show, lsp, install-ci |  | Log everything --verbose does and the engine's own diagnostics as well. |
| [`--dryrun`](flags/dryrun.md) | scan, install-ci |  | Show what would be changed without changing it; the two commands mean different things by it. |
| [`--dynamic-timeout`](flags/dynamic-timeout.md) | scan, ci |  | Scale each rule's time limit with the size of the file being scanned. |
| [`--dynamic-timeout-max-multiplier`](flags/dynamic-timeout-max-multiplier.md) | scan, ci |  | The ceiling on how much a file's size may stretch the timeout. |
| [`--dynamic-timeout-unit-kb`](flags/dynamic-timeout-unit-kb.md) | scan, ci |  | The file size, in KB, that counts as one step when scaling the timeout. |
| [`--emacs`](flags/emacs.md) | scan, ci |  | Print one line per finding, in the form Emacs compilation buffers parse. |
| [`--emacs-output`](flags/emacs-output.md) | scan, ci |  | Also write the findings in Emacs single-line format to a file. |
| [`--enable-nosem`](flags/enable-nosem.md) | scan, ci |  | Whether a nosem comment silences findings on its line; on by default. |
| [`--enable-version-check`](flags/enable-version-check.md) | scan, ci |  | Accepted for compatibility; opengrep never checks for a newer version. |
| [`--env`](flags/env.md) | install-ci |  | The CI system to install a workflow for; only GitHub Actions is supported. |
| [`--error`](flags/error.md) | scan |  | Exit 1 when there are findings, so a script or CI job fails on them. |
| [`--exclude`](flags/exclude.md) | scan, ci |  | Skip files and directories whose path matches a gitignore-style pattern. |
| [`--exclude-minified-files`](flags/exclude-minified-files.md) | scan |  | Skip files that look minified. |
| [`--exclude-rule`](flags/exclude-rule.md) | scan, ci |  | Skip a rule by its id, whichever config it came from. |
| [`--experimental`](flags/experimental.md) | scan, ci, test, validate, show, lsp, install-ci |  | Accepted for compatibility; opengrep has only the one implementation. |
| [`--files-with-matches`](flags/files-with-matches.md) | scan, ci |  | Print only the paths of the files that have findings. |
| [`--force-color`](flags/force-color.md) | scan, ci, test, validate | `OPENGREP_FORCE_COLOR` | Style the output even when it is not going to a terminal. |
| [`--force-exclude`](flags/force-exclude.md) | scan, ci |  | Apply --include and --exclude to files named on the command line as well. |
| [`--gitlab-sast`](flags/gitlab-sast.md) | scan, ci |  | Print the findings as a GitLab SAST report. |
| [`--gitlab-sast-output`](flags/gitlab-sast-output.md) | scan, ci |  | Also write the findings as a GitLab SAST report to a file. |
| [`--gitlab-secrets`](flags/gitlab-secrets.md) | scan, ci |  | Print the findings as a GitLab secret detection report. |
| [`--gitlab-secrets-output`](flags/gitlab-secrets-output.md) | scan, ci |  | Also write the findings as a GitLab secret detection report to a file. |
| [`--guarded-taint-signatures`](flags/guarded-taint-signatures.md) | scan |  | Drop a cross-function taint finding when the branch leading to the sink cannot be taken. |
| [`--html`](flags/html.md) | show |  | Render a show dump as an HTML page instead of plain text. |
| [`--include`](flags/include.md) | scan, ci |  | Scan only the files whose path matches one of these patterns. |
| [`--incremental-output`](flags/incremental-output.md) | scan |  | Print each finding as it is produced instead of all of them at the end. |
| [`--inline-metavariables`](flags/inline-metavariables.md) | scan, ci |  | Replace metavariables in a rule's metadata with what they matched. |
| [`--interfile-timeout`](flags/interfile-timeout.md) | scan, ci |  | Time a rule may spend on the cross-file analysis. |
| [`--jobs`](flags/jobs.md) | scan, ci |  | How many cores run rules in parallel. |
| [`--json`](flags/json.md) | scan, ci, test, show |  | Print the findings as a JSON document instead of the text report. |
| [`--json-output`](flags/json-output.md) | scan, ci |  | Also write the findings as JSON to a file. |
| [`--junit-xml`](flags/junit-xml.md) | scan, ci |  | Print the findings as a JUnit XML report, so a CI system shows them as failed tests. |
| [`--junit-xml-output`](flags/junit-xml-output.md) | scan, ci |  | Also write the findings as a JUnit XML report to a file. |
| [`--lang`](flags/lang.md) | scan |  | The language of a command-line pattern, and of the files it is run on. |
| [`--matching-diagnosis`](flags/matching-diagnosis.md) | test |  | Explain why a failing rule test did not match. |
| [`--matching-explanations`](flags/matching-explanations.md) | scan, ci |  | Add to the JSON output a trace of how each part of a rule matched. |
| [`--max-chars-per-line`](flags/max-chars-per-line.md) | scan, ci |  | Width at which a finding's code lines are wrapped in the text report. |
| [`--max-lines-per-finding`](flags/max-lines-per-finding.md) | scan, ci |  | How many lines of code a finding may show before the rest is trimmed. |
| [`--max-log-list-entries`](flags/max-log-list-entries.md) | scan, ci |  | How many items a list in the logs may show before it is replaced by a note. |
| [`--max-match-per-file`](flags/max-match-per-file.md) | scan, ci |  | How many findings one file may have, across all rules, before they are all dropped. |
| [`--max-memory`](flags/max-memory.md) | scan, ci, test |  | Memory a single file's analysis may use before it is abandoned. |
| [`--max-target-bytes`](flags/max-target-bytes.md) | scan, ci |  | Skip files larger than this when walking a directory. |
| [`--opengrep-ignore-pattern`](flags/opengrep-ignore-pattern.md) | scan, ci, test |  | Recognise one more comment prefix that silences findings on a line. |
| [`--optimizations`](flags/optimizations.md) | scan, ci |  | Turn the engine's optimizations, chiefly the prefilter, on or off. |
| [`--output`](flags/output.md) | scan, ci |  | Write the findings to a file instead of standard output. |
| [`--output-enclosing-context`](flags/output-enclosing-context.md) | scan |  | Record which function or class each finding sits in. |
| [`--pattern`](flags/pattern.md) | scan |  | Search with a single pattern given on the command line, instead of a rule file. |
| [`--project-root`](flags/project-root.md) | scan |  | Treat this folder as the project root, so its ignore files are read. |
| [`--quiet`](flags/quiet.md) | scan, ci, test, validate, show, lsp, install-ci |  | Print the findings and nothing else. |
| [`--replacement`](flags/replacement.md) | scan |  | The fix for a command-line pattern, as a rule's fix key would give it. |
| [`--repo`](flags/repo.md) | install-ci |  | The repository to add the workflow to, as a path or as owner/repo. |
| [`--rewrite-rule-ids`](flags/rewrite-rule-ids.md) | scan, ci |  | Whether a rule loaded from a directory gets its path as a prefix; on by default. |
| [`--sarif`](flags/sarif.md) | scan, ci |  | Print the findings as a SARIF 2.1.0 document. |
| [`--sarif-output`](flags/sarif-output.md) | scan, ci |  | Also write the findings as SARIF to a file, suppressed findings included. |
| [`--scan-unknown-extensions`](flags/scan-unknown-extensions.md) | scan, ci |  | Scan a file named on the command line even when its extension names no language. |
| [`--semgrepignore-filename`](flags/semgrepignore-filename.md) | scan |  | Read the list of skipped paths from a differently named file. |
| [`--severity`](flags/severity.md) | scan |  | Report only the findings of rules with these severities. |
| [`--show-supported-languages`](flags/show-supported-languages.md) | scan |  | Print the languages opengrep can parse, and exit. |
| [`--skin`](flags/skin.md) | scan |  | Which layout the text report uses. |
| [`--skip-invalid-configs`](flags/skip-invalid-configs.md) | scan |  | Skip files in a rules directory that are not rule configs, instead of stopping. |
| [`--strict`](flags/strict.md) | scan, test |  | Fail the run when a file could not be parsed or another warning-level error occurred. |
| [`--subdir`](flags/subdir.md) | ci |  | Scan only this directory, while the repository still provides the CI metadata. |
| [`--suppress-errors`](flags/suppress-errors.md) | ci | `OPENGREP_SUPPRESS_ERRORS` | Whether an error fails opengrep ci; on by default, so errors exit 0. |
| [`--taint-interfile`](flags/taint-interfile.md) | scan, ci |  | Follow taint across files, for every taint rule. |
| [`--taint-interfile-depth`](flags/taint-interfile-depth.md) | scan, ci |  | How many calls deep the cross-file taint analysis follows a chain. |
| [`--taint-intrafile`](flags/taint-intrafile.md) | scan, ci, test |  | Follow taint through calls to functions defined in the same file, for every taint rule. |
| [`--test`](flags/test.md) | scan |  | Run the rule tests and scan nothing; the older spelling of the test command. |
| [`--test-ignore-todo`](flags/test-ignore-todo.md) | scan, test |  | Documented as ignoring todoruleid annotations; in 1.30.0 it changes nothing. |
| [`--text`](flags/text.md) | scan, ci |  | Print the human-readable report, which is what a scan does anyway. |
| [`--text-output`](flags/text-output.md) | scan, ci |  | Also write the text report to a file. |
| [`--time`](flags/time.md) | scan, ci |  | Report how long the scan took, per rule and per file. |
| [`--timeout`](flags/timeout.md) | scan, ci, test | `OPENGREP_TIMEOUT` | Maximum time in seconds for one rule on one file; 0 means no limit. |
| [`--timeout-threshold`](flags/timeout-threshold.md) | scan, ci, test |  | How many rules may time out on a file before opengrep gives up on that file. |
| [`--update`](flags/update.md) | install-ci |  | Replace a workflow that is already there instead of leaving it alone. |
| [`--use-git-ignore`](flags/use-git-ignore.md) | scan, ci |  | Whether .gitignore decides which files are scanned; on by default. |
| [`--validate`](flags/validate.md) | scan |  | Check the rules and scan nothing; the older spelling of the validate command. |
| [`--verbose`](flags/verbose.md) | scan, ci, test, validate, show, lsp, install-ci |  | Log what the scan is doing, at the info level. |
| [`--version`](flags/version.md) | scan |  | Print the opengrep version and exit. |
| [`--vim`](flags/vim.md) | scan, ci |  | Print one line per finding, in the form Vim quickfix lists parse. |
| [`--vim-output`](flags/vim-output.md) | scan, ci |  | Also write the findings in Vim quickfix format to a file. |
<!-- END GENERATED: index -->

## Examples

### Where a flag may stand, and how it takes its value

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
$ opengrep scan --config=rule.yaml app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)

$ opengrep scan app.py --config rule.yaml
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
