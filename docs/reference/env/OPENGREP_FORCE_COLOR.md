<!-- reference
id: env-OPENGREP_FORCE_COLOR
kind: env
name: OPENGREP_FORCE_COLOR
aliases: [SEMGREP_FORCE_COLOR]
summary: Style the output even through a pipe, when neither --force-color nor --no-force-color is given.
value: `true`, `yes`, `1` or `false`, `no`, `0`
related: [env-NO_COLOR]
-->
# `OPENGREP_FORCE_COLOR`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_FORCE_COLOR`
- **Value:** `true`, `yes`, `1` or `false`, `no`, `0`
- **Equivalent to:** [`--force-color`](../flags/force-color.md)
- **See also:** [`NO_COLOR`](NO_COLOR.md)
<!-- END GENERATED: facts -->

When neither [`--force-color`](../flags/force-color.md) nor `--no-force-color`
is given, opengrep reads this variable. A true value styles the output
even when it goes to a pipe or a file, and wins over
[`NO_COLOR`](NO_COLOR.md). A false value leaves the usual rule in place, under
which only a terminal gets colour.

The accepted spellings are `true`, `yes` and `1` for true, and `false`, `no`
and `0` for false. Any other value is rejected like a bad flag value, with exit
status 2.

## Examples

### Colour through a pipe, from the environment

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
$ opengrep scan --config rule.yaml app.py | grep -q $'\e\[' && echo styled || echo plain
plain
$ OPENGREP_FORCE_COLOR=1 opengrep scan --config rule.yaml app.py | grep -q $'\e\[' && echo styled || echo plain
styled
$ OPENGREP_FORCE_COLOR=1 opengrep scan --config rule.yaml --no-force-color app.py 2>/dev/null | grep -q $'\e\[' && echo styled || echo plain
plain
```
