# Environment variables

## Names: `OPENGREP_*` and `SEMGREP_*`

Opengrep was forked from Semgrep. Semgrep's variables, whose names start with
`SEMGREP_`, still work, and each also has an `OPENGREP_` equivalent:
`SEMGREP_TIMEOUT` and `OPENGREP_TIMEOUT` mean the same. When both are set, the
`OPENGREP_*` one wins. Variables new to Opengrep, such as
[`OPENGREP_SKIN`](env/OPENGREP_SKIN.md), exist only under their `OPENGREP_*`
name. This reference names variables by their `OPENGREP_*` name and lists the
`SEMGREP_*` name as an alias. Variables that never had a
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
| [`ALL_PROXY`](env/ALL_PROXY.md) |  | The proxy for opengrep's downloads over both http and https, unless a scheme has its own. |
| [`CI`](env/CI.md) |  | Set by CI services; any value other than false or 0 turns off the status line of a scan. |
| [`COLUMNS`](env/COLUMNS.md) |  | The width the text report is laid out for, between 40 and 120. |
| [`GIT_SSH_COMMAND`](env/GIT_SSH_COMMAND.md) |  | The ssh command git uses to clone a git+ssh rule repository; opengrep sets a non-prompting one when it is unset. |
| [`HOME`](env/HOME.md) |  | The home directory; opengrep keeps only the language server's cache there. |
| [`HTTP_PROXY`](env/HTTP_PROXY.md) |  | The proxy for opengrep's plain http downloads; https downloads use HTTPS_PROXY. |
| [`HTTPS_PROXY`](env/HTTPS_PROXY.md) |  | The proxy for opengrep's https downloads, such as rules from a URL or the registry. |
| [`NO_COLOR`](env/NO_COLOR.md) |  | Turn off colour and other text styling in all output, even on a terminal. |
| [`NO_PROXY`](env/NO_PROXY.md) |  | Hosts that opengrep's downloads reach directly, without the proxy. |
| [`OPENGREP_APP_URL`](env/OPENGREP_APP_URL.md) |  | Another name for the registry base URL, used when OPENGREP_URL is not set. |
| [`OPENGREP_AUDIT_ON`](env/OPENGREP_AUDIT_ON.md) | `--audit-on` | Event names for --audit-on when the flag is not given, separated by whitespace. |
| [`OPENGREP_BASELINE_COMMIT`](env/OPENGREP_BASELINE_COMMIT.md) | `--baseline-commit` | The baseline for scan and ci when --baseline-commit is not given. |
| [`OPENGREP_BASELINE_REF`](env/OPENGREP_BASELINE_REF.md) | `--baseline-commit` | Another name for the baseline, used when OPENGREP_BASELINE_COMMIT is not set. |
| [`OPENGREP_FORCE_COLOR`](env/OPENGREP_FORCE_COLOR.md) | `--force-color` | Style the output even through a pipe, when neither --force-color nor --no-force-color is given. |
| [`OPENGREP_LOG_FILE`](env/OPENGREP_LOG_FILE.md) |  | Also write the logs to this file, at the same level as standard error. |
| [`OPENGREP_LOG_LEVEL`](env/OPENGREP_LOG_LEVEL.md) |  | Set the level of the logs on standard error, overriding --quiet, --verbose and --debug. |
| [`OPENGREP_LOG_SRCS`](env/OPENGREP_LOG_SRCS.md) |  | Hear from parts of opengrep and its libraries whose logs are silent by default. |
| [`OPENGREP_LOG_TAGS`](env/OPENGREP_LOG_TAGS.md) |  | Choose which debug messages are shown, by the tags they carry. |
| [`OPENGREP_PR_ID`](env/OPENGREP_PR_ID.md) |  | Mark an opengrep ci run as a pull request, which makes its event pull_request. |
| [`OPENGREP_REPO_NAME`](env/OPENGREP_REPO_NAME.md) |  | On GitHub Actions, the repository opengrep ci asks GitHub about for a pull request's merge base. |
| [`OPENGREP_RULES`](env/OPENGREP_RULES.md) | `--config` | Rule sources used when --config is not given, as a whitespace-separated list. |
| [`OPENGREP_SKIN`](env/OPENGREP_SKIN.md) | `--skin` | The layout of the text report, when --skin is not given. |
| [`OPENGREP_SUPPRESS_ERRORS`](env/OPENGREP_SUPPRESS_ERRORS.md) | `--suppress-errors` | Whether errors fail opengrep ci, when the flag is not given. |
| [`OPENGREP_TIMEOUT`](env/OPENGREP_TIMEOUT.md) | `--timeout` | The value of --timeout for scan and ci when the flag is not given. |
| [`OPENGREP_URL`](env/OPENGREP_URL.md) |  | The base URL of the rule registry, for configs such as p/python and auto. |
| [`TERM`](env/TERM.md) |  | When unset, dumb or unknown, a scan draws no status line on the terminal. |
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
