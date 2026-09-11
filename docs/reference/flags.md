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
| [`--baseline-commit`](flags/baseline-commit.md) | scan, ci | `OPENGREP_BASELINE_COMMIT`, `OPENGREP_BASELINE_REF` | Report only the findings that are not already present in the given commit. |
| [`--config`](flags/config.md) | scan, ci, test | `OPENGREP_RULES` | Load rules from a file, a directory, a URL, a git repository or the Semgrep registry. |
| [`--exclude`](flags/exclude.md) | scan, ci |  | Skip files and directories whose path matches a gitignore-style pattern. |
| [`--timeout`](flags/timeout.md) | scan, ci, test | `OPENGREP_TIMEOUT` | Maximum time in seconds for one rule on one file; 0 means no limit. |

Not yet documented (84): `--allow-local-builds`, `--allow-rule-timeout-control`, `--audit-on`, `--autofix`, `--dataflow-traces`, `--debug`, `--dryrun`, `--dynamic-timeout`, `--dynamic-timeout-max-multiplier`, `--dynamic-timeout-unit-kb`, `--emacs`, `--emacs-output`, `--enable-nosem`, `--enable-version-check`, `--env`, `--error`, `--exclude-minified-files`, `--exclude-rule`, `--experimental`, `--files-with-matches`, `--force-color`, `--force-exclude`, `--gitlab-sast`, `--gitlab-sast-output`, `--gitlab-secrets`, `--gitlab-secrets-output`, `--guarded-taint-signatures`, `--html`, `--include`, `--incremental-output`, `--incremental-output-postprocess`, `--inline-metavariables`, `--interfile-timeout`, `--jobs`, `--json`, `--json-output`, `--junit-xml`, `--junit-xml-output`, `--lang`, `--matching-diagnosis`, `--matching-explanations`, `--max-chars-per-line`, `--max-lines-per-finding`, `--max-log-list-entries`, `--max-match-per-file`, `--max-memory`, `--max-target-bytes`, `--opengrep-ignore-pattern`, `--optimizations`, `--output`, `--output-enclosing-context`, `--pattern`, `--project-root`, `--quiet`, `--replacement`, `--repo`, `--rewrite-rule-ids`, `--sarif`, `--sarif-output`, `--scan-unknown-extensions`, `--semgrepignore-filename`, `--severity`, `--show-supported-languages`, `--skin`, `--skip-invalid-configs`, `--strict`, `--subdir`, `--suppress-errors`, `--taint-interfile`, `--taint-interfile-depth`, `--taint-intrafile`, `--test`, `--test-ignore-todo`, `--text`, `--text-output`, `--time`, `--timeout-threshold`, `--update`, `--use-git-ignore`, `--validate`, `--verbose`, `--version`, `--vim`, `--vim-output`
<!-- END GENERATED: index -->
