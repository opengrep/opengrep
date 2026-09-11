<!-- reference
id: flag-verbose
kind: flag
name: --verbose
aliases: [-v]
summary: Log what the scan is doing, at the info level.
commands: [scan, ci, test, validate, show, lsp, install-ci]
related: [flag-quiet, flag-debug, env-OPENGREP_LOG_LEVEL, env-OPENGREP_LOG_FILE]
-->
# `--verbose`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md), [`opengrep validate`](../commands/validate.md), [`opengrep show`](../commands/show.md), [`opengrep lsp`](../commands/lsp.md), [`opengrep install-ci`](../commands/install-ci.md)
- **Also spelled:** `-v`
- **See also:** [`--quiet`](quiet.md), [`--debug`](debug.md), [`OPENGREP_LOG_LEVEL`](../env/OPENGREP_LOG_LEVEL.md), [`OPENGREP_LOG_FILE`](../env/OPENGREP_LOG_FILE.md), [`--max-log-list-entries`](max-log-list-entries.md)
<!-- END GENERATED: facts -->

Raises the log level on standard error from warnings to info: which rules were
loaded and from where, how many targets were found, which files were skipped
and why, and the same for the rest of the run. The findings on standard output
do not change.

Each line carries the time since the start and its level, as in
`[00.04][INFO]: Getting the rules`.
[`--debug`](debug.md) adds more still, and [`--quiet`](quiet.md) silences
standard error altogether.

## Examples

### What the scan is doing

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
$ opengrep scan --config rule.yaml --verbose app.py 2>&1 >/dev/null | head -3
[00.04][INFO]: Opengrep version: X.Y.Z
[00.04][INFO]: Getting the rules
[00.04][INFO]: loading local config from rule.yaml
```
