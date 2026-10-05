<!-- reference
id: env-OPENGREP_SKIN
kind: env
name: OPENGREP_SKIN
summary: The layout of the text report, when --skin is not given.
value: `legacy`, `simple` or `vivid`
-->
# `OPENGREP_SKIN`

<!-- BEGIN GENERATED: facts -->
- **Value:** `legacy`, `simple` or `vivid`
- **Equivalent to:** [`--skin`](../flags/skin.md)
<!-- END GENERATED: facts -->

Sets the layout of the text report, as [`--skin`](../flags/skin.md) does, for
every `opengrep scan`, `opengrep ci` and `opengrep validate` that does not pass
the flag. The flag wins, with a warning that the variable is ignored. An empty
value counts as unset, and any other value outside the three names stops the
command before it starts, with exit status 2.

There is no `SEMGREP_SKIN` form.

## Examples

### The layout from the environment

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
$ OPENGREP_SKIN=legacy opengrep scan --config rule.yaml app.py
┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    app.py
    ❯❱ find-eval
          found eval

            1┆ eval(1)
$ OPENGREP_SKIN=legacy opengrep scan --config rule.yaml --skin simple app.py 2>&1 | grep WARNING
[00.00][WARNING]: --skin is given; ignoring $OPENGREP_SKIN
$ OPENGREP_SKIN=fancy opengrep scan --config rule.yaml app.py 2>&1 | grep -A1 'invalid value'
opengrep scan: environment variable OPENGREP_SKIN: invalid value "fancy",
               expected one of 'legacy', 'simple' or 'vivid'
$ OPENGREP_SKIN=fancy opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 2
```
