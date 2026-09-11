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
| [`COLUMNS`](env/COLUMNS.md) |  | The width the text report is laid out for, between 40 and 120. |
| [`GIT_SSH_COMMAND`](env/GIT_SSH_COMMAND.md) |  | The ssh command git uses to clone a git+ssh rule repository; opengrep sets a non-prompting one when it is unset. |
| [`HOME`](env/HOME.md) |  | The home directory; opengrep keeps only the language server's cache there. |
| [`HTTP_PROXY`](env/HTTP_PROXY.md) |  | The proxy for opengrep's plain http downloads; https downloads use HTTPS_PROXY. |
| [`HTTPS_PROXY`](env/HTTPS_PROXY.md) |  | The proxy for opengrep's https downloads, such as rules from a URL or the registry. |
| [`NO_COLOR`](env/NO_COLOR.md) |  | Turn off colour and other text styling in all output, even on a terminal. |
| [`OPENGREP_APP_URL`](env/OPENGREP_APP_URL.md) |  | Another name for the registry base URL, used when OPENGREP_URL is not set. |
| [`OPENGREP_AUDIT_ON`](env/OPENGREP_AUDIT_ON.md) | `--audit-on` | Event names for --audit-on when the flag is not given, separated by whitespace. |
| [`OPENGREP_BASELINE_COMMIT`](env/OPENGREP_BASELINE_COMMIT.md) | `--baseline-commit` | The baseline for scan and ci when --baseline-commit is not given. |
| [`OPENGREP_BASELINE_REF`](env/OPENGREP_BASELINE_REF.md) | `--baseline-commit` | Another name for the baseline, used when OPENGREP_BASELINE_COMMIT is not set. |
| [`OPENGREP_BRANCH`](env/OPENGREP_BRANCH.md) |  | Accepted by opengrep ci for the branch name, but it changes neither the output nor the scan. |
| [`OPENGREP_COMMIT`](env/OPENGREP_COMMIT.md) |  | Accepted by opengrep ci for the commit being scanned, but it changes neither the output nor the scan. |
| [`OPENGREP_FORCE_COLOR`](env/OPENGREP_FORCE_COLOR.md) | `--force-color` | Style the output even through a pipe, when neither --force-color nor --no-force-color is given. |
| [`OPENGREP_JOB_URL`](env/OPENGREP_JOB_URL.md) |  | Accepted by opengrep ci for the CI job's URL, but used for nothing. |
| [`OPENGREP_LOG_FILE`](env/OPENGREP_LOG_FILE.md) |  | Also write the logs to this file, at the same level as standard error. |
| [`OPENGREP_LOG_LEVEL`](env/OPENGREP_LOG_LEVEL.md) |  | Set the level of the logs on standard error, overriding --quiet, --verbose and --debug. |
| [`OPENGREP_LOG_SRCS`](env/OPENGREP_LOG_SRCS.md) |  | Hear from parts of opengrep and its libraries whose logs are silent by default. |
| [`OPENGREP_LOG_TAGS`](env/OPENGREP_LOG_TAGS.md) |  | Choose which debug messages are shown, by the tags they carry. |
| [`OPENGREP_PR_ID`](env/OPENGREP_PR_ID.md) |  | Mark an opengrep ci run as a pull request, which makes its event pull_request. |
| [`OPENGREP_PR_TITLE`](env/OPENGREP_PR_TITLE.md) |  | Accepted by opengrep ci for the pull request's title, but used for nothing. |
| [`OPENGREP_REPO_DISPLAY_NAME`](env/OPENGREP_REPO_DISPLAY_NAME.md) |  | Accepted by opengrep ci for the repository's display name, but used for nothing. |
| [`OPENGREP_REPO_NAME`](env/OPENGREP_REPO_NAME.md) |  | The repository name for opengrep ci; it matters only to the GitHub merge-base lookup. |
| [`OPENGREP_REPO_URL`](env/OPENGREP_REPO_URL.md) |  | Accepted by opengrep ci for the repository URL, but used for nothing. |
| [`OPENGREP_RULES`](env/OPENGREP_RULES.md) | `--config` | Rule sources used when --config is not given, as a whitespace-separated list. |
| [`OPENGREP_SUPPRESS_ERRORS`](env/OPENGREP_SUPPRESS_ERRORS.md) | `--suppress-errors` | Whether errors fail opengrep ci, when the flag is not given. |
| [`OPENGREP_TIMEOUT`](env/OPENGREP_TIMEOUT.md) | `--timeout` | The value of --timeout for scan and ci when the flag is not given. |
| [`OPENGREP_URL`](env/OPENGREP_URL.md) |  | The base URL of the rule registry, for configs such as p/python and auto. |
<!-- END GENERATED: index -->

## Examples

### An empty value counts as unset

`OPENGREP_RULES` is empty here, so opengrep takes the rules from `--config`
and says nothing about the variable.

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
$ OPENGREP_RULES="" opengrep scan --config rule.yaml app.py 2>&1 | grep -c WARNING
0
```
