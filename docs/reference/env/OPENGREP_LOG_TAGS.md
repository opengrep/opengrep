<!-- reference
id: env-OPENGREP_LOG_TAGS
kind: env
name: OPENGREP_LOG_TAGS
aliases: [SEMGREP_LOG_TAGS]
summary: Choose which debug messages are shown, by the tags they carry.
value: comma-separated tag names, or `all`
related: []
-->
# `OPENGREP_LOG_TAGS`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_LOG_TAGS`
- **Value:** comma-separated tag names, or `all`
- **See also:** [`OPENGREP_LOG_LEVEL`](OPENGREP_LOG_LEVEL.md)
<!-- END GENERATED: facts -->

Debug messages can carry tags, and opengrep's own carry the tag `default`. By
default, a debug message is shown only if it is tagged `default`. This variable
replaces that list: a debug message is shown when it carries one of the named
tags, and `all` shows every debug message whatever its tags.

Only debug messages are filtered this way. Info messages, warnings and errors
are shown or not by the level alone, and the level still decides whether debug
messages appear in the first place: tags narrow what
[`--debug`](../flags/debug.md) shows, they do not turn it on.

The first of `PYTEST_OPENGREP_LOG_TAGS`, `OPENGREP_LOG_TAGS`,
`PYTEST_SEMGREP_LOG_TAGS` and `SEMGREP_LOG_TAGS` that is set wins.

## Examples

### Hiding the debug messages, keeping the rest

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
$ opengrep scan --config rule.yaml --debug app.py 2>&1 >/dev/null | grep -q '\[DEBUG\]' && echo shown || echo hidden
shown
$ OPENGREP_LOG_TAGS=nosuchtag opengrep scan --config rule.yaml --debug app.py 2>&1 >/dev/null | grep -q '\[DEBUG\]' && echo shown || echo hidden
hidden
$ OPENGREP_LOG_TAGS=nosuchtag opengrep scan --config rule.yaml --debug app.py 2>&1 >/dev/null | grep -q '\[INFO\]' && echo shown || echo hidden
shown
```

No debug message carries the tag `nosuchtag`, so they all disappear, while the
info messages are untouched.
