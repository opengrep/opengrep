<!-- reference
id: env-OPENGREP_LOG_LEVEL
kind: env
name: OPENGREP_LOG_LEVEL
aliases: [SEMGREP_LOG_LEVEL]
summary: Set the level of the logs on standard error, overriding --quiet, --verbose and --debug.
value: `none`, `app`, `error`, `warning`, `info` or `debug`
related: [flag-quiet, flag-verbose, flag-debug, env-OPENGREP_LOG_SRCS, env-OPENGREP_LOG_TAGS, env-OPENGREP_LOG_FILE]
-->
# `OPENGREP_LOG_LEVEL`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_LOG_LEVEL`
- **Value:** `none`, `app`, `error`, `warning`, `info` or `debug`
- **See also:** [`--quiet`](../flags/quiet.md), [`--verbose`](../flags/verbose.md), [`--debug`](../flags/debug.md), [`OPENGREP_LOG_SRCS`](OPENGREP_LOG_SRCS.md), [`OPENGREP_LOG_TAGS`](OPENGREP_LOG_TAGS.md), [`OPENGREP_LOG_FILE`](OPENGREP_LOG_FILE.md)
<!-- END GENERATED: facts -->

Sets the level of the log messages that opengrep writes to standard error,
and to the file named by `OPENGREP_LOG_FILE` when that is set. The variable
wins over the level chosen by `--quiet`, `--verbose` or `--debug`. Opengrep
ignores values not in the list above.

Several variables set the level, and the first one set to a recognized value
wins, in this order: `PYTEST_OPENGREP_LOG_LEVEL`, `OPENGREP_LOG_LEVEL`,
`PYTEST_SEMGREP_LOG_LEVEL`, `SEMGREP_LOG_LEVEL`. The `PYTEST_` names are meant
for test runners (see [Internal and debugging interfaces](../internal.md)).

## Examples

### Informational logs

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
eval(code)
```

**Command and result:**
```console
$ OPENGREP_LOG_LEVEL=info opengrep scan --quiet --config rule.yaml app.py 2>&1 >/dev/null | grep INFO | head -3
[00.04][INFO]: Opengrep version: X.Y.Z
[00.04][INFO]: Getting the rules
[00.04][INFO]: loading local config from rule.yaml
```
