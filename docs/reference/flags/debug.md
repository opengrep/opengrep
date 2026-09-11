<!-- reference
id: flag-debug
kind: flag
name: --debug
summary: Log everything --verbose does and the engine's own diagnostics as well.
commands: [scan, ci, test, validate, show, lsp, install-ci]
related: [flag-verbose, flag-quiet, env-OPENGREP_LOG_LEVEL, env-OPENGREP_LOG_SRCS, env-OPENGREP_LOG_FILE]
-->
# `--debug`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md), [`opengrep validate`](../commands/validate.md), [`opengrep show`](../commands/show.md), [`opengrep lsp`](../commands/lsp.md), [`opengrep install-ci`](../commands/install-ci.md)
- **See also:** [`--verbose`](verbose.md), [`--quiet`](quiet.md), [`OPENGREP_LOG_LEVEL`](../env/OPENGREP_LOG_LEVEL.md), [`OPENGREP_LOG_SRCS`](../env/OPENGREP_LOG_SRCS.md), [`OPENGREP_LOG_FILE`](../env/OPENGREP_LOG_FILE.md)
<!-- END GENERATED: facts -->

Sets the log level on standard error to debug, which is everything
[`--verbose`](verbose.md) shows plus the internals: the targets as they are
chosen, the rules as they are prepared, and the engine's own progress. Expect
hundreds of lines from even a one-file scan, so send it to a file or a pager.

[`OPENGREP_LOG_SRCS`](../environment.md) narrows the output to named parts of
opengrep, which is more use than reading everything.
[`OPENGREP_LOG_FILE`](../environment.md) keeps a copy in a file.

## Examples

### Turning the level up

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
$ opengrep scan --config rule.yaml --verbose app.py 2>&1 >/dev/null | grep -c setup_logging
0
$ opengrep scan --config rule.yaml --debug app.py 2>&1 >/dev/null | grep -c setup_logging
1
```

The line counted here is one the engine logs while starting up, which
`--verbose` does not show and `--debug` does.
