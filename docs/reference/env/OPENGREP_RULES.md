<!-- reference
id: env-OPENGREP_RULES
kind: env
name: OPENGREP_RULES
aliases: [SEMGREP_RULES]
summary: Rule sources used when --config is not given, as a whitespace-separated list.
value: whitespace-separated list of rule sources
related: []
-->
# `OPENGREP_RULES`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_RULES`
- **Value:** whitespace-separated list of rule sources
- **Equivalent to:** [`--config`](../flags/config.md)
<!-- END GENERATED: facts -->

When no `--config` is given, `opengrep scan`, `opengrep ci` and
`opengrep test` read their rule sources from this variable. The value is split
at spaces, tabs and newlines. Each word is a source, as for
[`--config`](../flags/config.md). The value has no quoting, so it cannot name a
path that contains a space.

Any `--config` on the command line makes opengrep ignore the whole variable,
with a warning.

## Examples

### Rules from the environment

**`eval.yaml`**
```yaml title="eval.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`exec.yaml`**
```yaml title="exec.yaml"
rules:
  - id: find-exec
    pattern: exec(...)
    message: found exec
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(code)
exec(code)
```

**Command and result:**
```console
$ OPENGREP_RULES="eval.yaml exec.yaml" opengrep scan app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(code)

  warn  find-exec
  found exec

    2 │ exec(code)

$ OPENGREP_RULES="eval.yaml exec.yaml" opengrep scan --config eval.yaml app.py 2>&1 | grep WARNING
[00.05][WARNING]: -c/-f/--config is given; ignoring $OPENGREP_RULES
```
