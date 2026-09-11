<!-- reference
id: flag-quiet
kind: flag
name: --quiet
aliases: [-q]
summary: Print the findings and nothing else.
commands: [scan, ci, test, validate, show, lsp, install-ci]
related: [flag-verbose, flag-debug, env-OPENGREP_LOG_LEVEL]
-->
# `--quiet`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md), [`opengrep validate`](../commands/validate.md), [`opengrep show`](../commands/show.md), [`opengrep lsp`](../commands/lsp.md), [`opengrep install-ci`](../commands/install-ci.md)
- **Also spelled:** `-q`
- **See also:** [`--verbose`](verbose.md), [`--debug`](debug.md), [`OPENGREP_LOG_LEVEL`](../env/OPENGREP_LOG_LEVEL.md), [`--max-log-list-entries`](max-log-list-entries.md)
<!-- END GENERATED: facts -->

Silences everything opengrep writes to standard error: the progress, the
summary, and the warnings. The findings on standard output are unchanged, so
this is what a script wants when it reads the report from a pipe.

`--quiet`, [`--verbose`](verbose.md) and [`--debug`](debug.md) choose one log
level between them, the default being to show warnings and errors.
[`OPENGREP_LOG_LEVEL`](../env/OPENGREP_LOG_LEVEL.md) overrides whichever of
them is given.

## Examples

### Nothing but the findings

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
$ opengrep scan --config rule.yaml app.py 2>&1 >/dev/null
1 file · 1 rule

1 file · 1 finding
$ opengrep scan --config rule.yaml --quiet app.py 2>&1 >/dev/null
$ opengrep scan --config rule.yaml --quiet app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```

The second command prints nothing: with `--quiet` there is nothing on standard
error.
