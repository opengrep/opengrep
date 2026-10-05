<!-- reference
id: env-OPENGREP_LOG_SRCS
kind: env
name: OPENGREP_LOG_SRCS
aliases: [SEMGREP_LOG_SRCS]
summary: Hear from parts of opengrep and its libraries whose logs are silent by default.
value: comma-separated regular expressions
related: []
-->
# `OPENGREP_LOG_SRCS`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_LOG_SRCS`
- **Value:** comma-separated regular expressions
- **See also:** [`--debug`](../flags/debug.md), [`OPENGREP_LOG_LEVEL`](OPENGREP_LOG_LEVEL.md)
<!-- END GENERATED: facts -->

Log messages come from sources: opengrep's own application code, and named
parts such as `semgrep.targeting`, `networking.http`, `cohttp.lwt.client` or
`tls.config`. Only the application's messages are shown by default. The rest
are silenced, and [`--debug`](../flags/debug.md) lists them as
`Skipping logs for <name>`.

This variable names the sources to hear from, as comma-separated regular
expressions, each matched anywhere in a source's name. The log level still
applies: a source brought back this way shows only messages at or above the
level chosen by `--verbose`, `--debug` or
[`OPENGREP_LOG_LEVEL`](OPENGREP_LOG_LEVEL.md).

The first of `PYTEST_OPENGREP_LOG_SRCS`, `OPENGREP_LOG_SRCS`,
`PYTEST_SEMGREP_LOG_SRCS` and `SEMGREP_LOG_SRCS` that is set wins.

## Examples

### The logs of the targeting code

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
$ opengrep scan --config rule.yaml --debug app.py 2>&1 >/dev/null | grep -c '(semgrep.targeting)'
0
$ OPENGREP_LOG_SRCS=semgrep.targeting opengrep scan --config rule.yaml --debug app.py 2>&1 >/dev/null | grep '(semgrep.targeting)' | head -2
[00.04][DEBUG](default)(semgrep.targeting): group_scanning_roots_by_project [app.py]
[00.04][DEBUG](default)(semgrep.targeting): Find_target.get_targets_for_project
```
