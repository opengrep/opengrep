<!-- reference
id: cmd-ci
kind: command
name: opengrep ci
summary: Scan a repository the way a CI job wants it: repository metadata, blocking findings, and a non-zero status when they appear.
related: [cmd-scan, flag-audit-on, flag-suppress-errors, flag-subdir, flag-baseline-commit]
-->
# `opengrep ci`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep scan`](scan.md), [`--audit-on`](../flags/audit-on.md), [`--suppress-errors`](../flags/suppress-errors.md), [`--subdir`](../flags/subdir.md), [`--baseline-commit`](../flags/baseline-commit.md), [`opengrep install-ci`](install-ci.md), [`--error`](../flags/error.md), [`OPENGREP_BRANCH`](../env/OPENGREP_BRANCH.md), [`OPENGREP_COMMIT`](../env/OPENGREP_COMMIT.md), [`OPENGREP_JOB_URL`](../env/OPENGREP_JOB_URL.md), [`OPENGREP_PR_ID`](../env/OPENGREP_PR_ID.md), [`OPENGREP_PR_TITLE`](../env/OPENGREP_PR_TITLE.md), [`OPENGREP_REPO_DISPLAY_NAME`](../env/OPENGREP_REPO_DISPLAY_NAME.md), [`OPENGREP_REPO_NAME`](../env/OPENGREP_REPO_NAME.md), [`OPENGREP_REPO_URL`](../env/OPENGREP_REPO_URL.md)
<!-- END GENERATED: facts -->

```
opengrep ci [FLAGS]
```

Runs the same engine as [`opengrep scan`](scan.md), with the defaults a CI job
wants. It takes no target arguments: it scans the current directory, or the
directory given by [`--subdir`](../flags/subdir.md), and it expects to be run
from the root of a git repository. Elsewhere it warns and carries on with
whatever it can work out.

Rules come from [`--config`](../flags/config.md) or
[`OPENGREP_RULES`](../env/OPENGREP_RULES.md), as for a scan. With neither,
`ci` falls back to `--config auto` and fetches rules from the registry.

What `ci` adds to a scan:

- **Repository metadata.** It works out the CI provider from the environment
  (GitHub Actions, GitLab CI, CircleCI, Jenkins, Bitbucket, Azure Pipelines,
  Buildkite, Travis; otherwise plain git) and reports what it found in a
  `SCAN ENVIRONMENT` block on standard error. Variables such as
  `OPENGREP_REPO_NAME` and `OPENGREP_BRANCH` are accepted for compatibility
  with semgrep, but only [`OPENGREP_PR_ID`](../env/OPENGREP_PR_ID.md) changes
  what `ci` does in general; see [Environment variables](../environment.md).
- **A baseline.** The baseline comes from that metadata, so in a pull or merge
  request only findings that the request introduces are reported.
  [`--baseline-commit`](../flags/baseline-commit.md) overrides it.
- **Blocking findings.** A finding is blocking or not according to its rule's
  metadata. `ci` prints the blocking ones under `BLOCKING CODE RULES FIRED:`
  and exits 1 when there is at least one, without needing `--error`.
  [`--audit-on`](../flags/audit-on.md) turns that status back into 0 for a
  given CI event.
- **Suppressed errors.** By default any other failure becomes exit status 0,
  so a broken rule file does not fail the build. See
  [`--suppress-errors`](../flags/suppress-errors.md).

## Exit status

| Status | Meaning |
|---|---|
| 0 | The scan ran and no blocking finding was reported. |
| 1 | At least one blocking finding was reported. |
| other | The codes of [`opengrep scan`](scan.md), but only with `--no-suppress-errors`; otherwise they become 0. |

## Flags

<!-- BEGIN GENERATED: flags -->
| Flag | Summary |
|---|---|
| [`--allow-local-builds`](../flags/allow-local-builds.md) | Let opengrep build the project to work out its dependencies; it has nothing to act on today. |
| [`--allow-rule-timeout-control`](../flags/allow-rule-timeout-control.md) | Let rules set their own time limits, which opengrep otherwise ignores. |
| [`--audit-on`](../flags/audit-on.md) | Report blocking findings but exit 0 when the CI event has one of these names. |
| [`--baseline-commit`](../flags/baseline-commit.md) | Report only the findings that are not already present in the given commit. |
| [`--config`](../flags/config.md) | Load rules from a file, a directory, a URL, a git repository or the Semgrep registry. |
| [`--dataflow-traces`](../flags/dataflow-traces.md) | Show how a value reaches the finding, for taint rules. |
| [`--debug`](../flags/debug.md) | Log everything --verbose does and the engine's own diagnostics as well. |
| `--develop` | *not yet documented* |
| [`--dynamic-timeout`](../flags/dynamic-timeout.md) | Scale each rule's time limit with the size of the file being scanned. |
| [`--dynamic-timeout-max-multiplier`](../flags/dynamic-timeout-max-multiplier.md) | The ceiling on how much a file's size may stretch the timeout. |
| [`--dynamic-timeout-unit-kb`](../flags/dynamic-timeout-unit-kb.md) | The file size, in KB, that counts as one step when scaling the timeout. |
| [`--emacs`](../flags/emacs.md) | Print one line per finding, in the form Emacs compilation buffers parse. |
| [`--emacs-output`](../flags/emacs-output.md) | Also write the findings in Emacs single-line format to a file. |
| [`--enable-nosem`](../flags/enable-nosem.md) | Whether a nosem comment silences findings on its line; on by default. |
| [`--enable-version-check`](../flags/enable-version-check.md) | Accepted for compatibility; opengrep never checks for a newer version. |
| [`--exclude`](../flags/exclude.md) | Skip files and directories whose path matches a gitignore-style pattern. |
| [`--exclude-rule`](../flags/exclude-rule.md) | Skip a rule by its id, whichever config it came from. |
| [`--experimental`](../flags/experimental.md) | Accepted for compatibility; opengrep has only the one implementation. |
| [`--files-with-matches`](../flags/files-with-matches.md) | Print only the paths of the files that have findings. |
| [`--force-color`](../flags/force-color.md) | Style the output even when it is not going to a terminal. |
| [`--force-exclude`](../flags/force-exclude.md) | Apply --include and --exclude to files named on the command line as well. |
| [`--gitlab-sast`](../flags/gitlab-sast.md) | Print the findings as a GitLab SAST report. |
| [`--gitlab-sast-output`](../flags/gitlab-sast-output.md) | Also write the findings as a GitLab SAST report to a file. |
| [`--gitlab-secrets`](../flags/gitlab-secrets.md) | Print the findings as a GitLab secret detection report. |
| [`--gitlab-secrets-output`](../flags/gitlab-secrets-output.md) | Also write the findings as a GitLab secret detection report to a file. |
| [`--include`](../flags/include.md) | Scan only the files whose path matches one of these patterns. |
| [`--inline-metavariables`](../flags/inline-metavariables.md) | Replace metavariables in a rule's metadata with what they matched. |
| [`--interfile-timeout`](../flags/interfile-timeout.md) | Time a rule may spend on the cross-file analysis. |
| [`--jobs`](../flags/jobs.md) | How many cores run rules in parallel. |
| [`--json`](../flags/json.md) | Print the findings as a JSON document instead of the text report. |
| [`--json-output`](../flags/json-output.md) | Also write the findings as JSON to a file. |
| [`--junit-xml`](../flags/junit-xml.md) | Print the findings as a JUnit XML report, so a CI system shows them as failed tests. |
| [`--junit-xml-output`](../flags/junit-xml-output.md) | Also write the findings as a JUnit XML report to a file. |
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
| `--profile` | *not yet documented* |
| [`--quiet`](../flags/quiet.md) | Print the findings and nothing else. |
| [`--rewrite-rule-ids`](../flags/rewrite-rule-ids.md) | Whether a rule loaded from a directory gets its path as a prefix; on by default. |
| [`--sarif`](../flags/sarif.md) | Print the findings as a SARIF 2.1.0 document. |
| [`--sarif-output`](../flags/sarif-output.md) | Also write the findings as SARIF to a file, suppressed findings included. |
| [`--scan-unknown-extensions`](../flags/scan-unknown-extensions.md) | Scan a file named on the command line even when its extension names no language. |
| [`--subdir`](../flags/subdir.md) | Scan only this directory, while the repository still provides the CI metadata. |
| [`--suppress-errors`](../flags/suppress-errors.md) | Whether an error fails opengrep ci; on by default, so errors exit 0. |
| [`--taint-interfile`](../flags/taint-interfile.md) | Follow taint across files, for every taint rule. |
| [`--taint-interfile-depth`](../flags/taint-interfile-depth.md) | How many calls deep the cross-file taint analysis follows a chain. |
| [`--taint-intrafile`](../flags/taint-intrafile.md) | Follow taint through calls to functions defined in the same file, for every taint rule. |
| [`--text`](../flags/text.md) | Print the human-readable report, which is what a scan does anyway. |
| [`--text-output`](../flags/text-output.md) | Also write the text report to a file. |
| [`--time`](../flags/time.md) | Report how long the scan took, per rule and per file. |
| [`--timeout`](../flags/timeout.md) | Maximum time in seconds for one rule on one file; 0 means no limit. |
| [`--timeout-threshold`](../flags/timeout-threshold.md) | How many rules may time out on a file before opengrep gives up on that file. |
| [`--use-git-ignore`](../flags/use-git-ignore.md) | Whether .gitignore decides which files are scanned; on by default. |
| [`--verbose`](../flags/verbose.md) | Log what the scan is doing, at the info level. |
| [`--vim`](../flags/vim.md) | Print one line per finding, in the form Vim quickfix lists parse. |
| [`--vim-output`](../flags/vim-output.md) | Also write the findings in Vim quickfix format to a file. |
<!-- END GENERATED: flags -->

## Examples

### A scan of a repository

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
$ git init -q && git add . && git commit -qm init
$ opengrep ci --config rule.yaml


┌─────────────────────────┐
│ 1 Blocking Code Finding │
└─────────────────────────┘

    app.py
    ❯❱ find-eval
          found eval

            1┆ eval(1)


  BLOCKING CODE RULES FIRED:
    find-eval

$ opengrep ci --config rule.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 1
```

The progress, the `SCAN ENVIRONMENT` block and the closing count go to
standard error:

**Command and result:**
```console
$ opengrep ci --config rule.yaml 2>&1 >/dev/null | tail -3
CI scan completed successfully.
  Found 1 finding (1 blocking) from 1 rule.
  Has findings for blocking rules so exiting with code 1
```
