<!-- reference
id: env-OPENGREP_LOG_FILE
kind: env
name: OPENGREP_LOG_FILE
aliases: [SEMGREP_LOG_FILE]
summary: Also write the logs to this file, at the same level as standard error.
value: a file path
related: []
-->
# `OPENGREP_LOG_FILE`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_LOG_FILE`
- **Value:** a file path
- **See also:** [`--debug`](../flags/debug.md), [`--verbose`](../flags/verbose.md), [`OPENGREP_LOG_LEVEL`](OPENGREP_LOG_LEVEL.md)
<!-- END GENERATED: facts -->

Writes a copy of what opengrep logs to standard error into this file, at the
same level. By default that is only the warnings and the summary, so the file
costs nothing unless [`--verbose`](../flags/verbose.md),
[`--debug`](../flags/debug.md) or [`OPENGREP_LOG_LEVEL`](OPENGREP_LOG_LEVEL.md)
turns the level up. Standard error is unchanged.

The file is emptied at the start of each run, and missing directories on its
path are created. When the file cannot be created, opengrep warns
`cannot write the log file of $OPENGREP_LOG_FILE` and scans without it.

## Examples

### Keeping the verbose logs

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
$ OPENGREP_LOG_FILE=logs/scan.log opengrep scan --config rule.yaml --verbose app.py > /dev/null 2>&1
$ head -2 logs/scan.log
[00.04][INFO]: Opengrep version: X.Y.Z
[00.04][INFO]: Getting the rules
$ OPENGREP_LOG_FILE=/dev/null/scan.log opengrep scan --config rule.yaml app.py 2>&1 >/dev/null | grep -oF 'cannot write the log file of $OPENGREP_LOG_FILE'
cannot write the log file of $OPENGREP_LOG_FILE
```
