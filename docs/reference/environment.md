# Environment variables

<!-- BEGIN GENERATED: stamp -->
> Reference for **opengrep 1.30.0** (commit `d094c70bb`).
<!-- END GENERATED: stamp -->

## Names: `OPENGREP_*` and `SEMGREP_*`

Opengrep was forked from Semgrep, and it reads each of its `OPENGREP_*`
variables under the old `SEMGREP_*` name as well. For example,
`OPENGREP_TIMEOUT` can also be written `SEMGREP_TIMEOUT`. When both are set,
the `OPENGREP_*` one wins. This reference names variables by their `OPENGREP_*`
name and lists the `SEMGREP_*` name as an alias. Variables that never had a
`SEMGREP` name, such as `NO_COLOR`, have no alias.

A variable set to the empty string counts as unset.

## Variables and flags

Some variables are equivalent to a command-line flag (the *Equivalent flag*
column). When both are given, the flag wins and opengrep logs a warning such
as:

```
[00.04][WARNING]: --timeout is given; ignoring $OPENGREP_TIMEOUT
```

A variable whose value cannot be parsed is reported like a bad flag value,
and opengrep exits with status 2.

<!-- BEGIN GENERATED: index -->
| Variable | Equivalent flag | Summary |
|---|---|---|
| [`NO_COLOR`](env/NO_COLOR.md) |  | Turn off colour and other text styling in all output, even on a terminal. |
| [`OPENGREP_LOG_LEVEL`](env/OPENGREP_LOG_LEVEL.md) |  | Set the level of the logs on standard error, overriding --quiet, --verbose and --debug. |
| [`OPENGREP_RULES`](env/OPENGREP_RULES.md) | `--config` | Rule sources used when --config is not given, as a whitespace-separated list. |
| [`OPENGREP_TIMEOUT`](env/OPENGREP_TIMEOUT.md) | `--timeout` | The value of --timeout for scan and ci when the flag is not given. |

Not yet documented (23): `COLUMNS`, `GIT_SSH_COMMAND`, `HOME`, `HTTPS_PROXY`, `HTTP_PROXY`, `OPENGREP_APP_URL`, `OPENGREP_AUDIT_ON`, `OPENGREP_BASELINE_COMMIT`, `OPENGREP_BASELINE_REF`, `OPENGREP_BRANCH`, `OPENGREP_COMMIT`, `OPENGREP_FORCE_COLOR`, `OPENGREP_JOB_URL`, `OPENGREP_LOG_FILE`, `OPENGREP_LOG_SRCS`, `OPENGREP_LOG_TAGS`, `OPENGREP_PR_ID`, `OPENGREP_PR_TITLE`, `OPENGREP_REPO_DISPLAY_NAME`, `OPENGREP_REPO_NAME`, `OPENGREP_REPO_URL`, `OPENGREP_SUPPRESS_ERRORS`, `OPENGREP_URL`
<!-- END GENERATED: index -->
