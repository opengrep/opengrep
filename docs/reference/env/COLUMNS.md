<!-- reference
id: env-COLUMNS
kind: env
name: COLUMNS
summary: The width the text report is laid out for, between 40 and 120.
value: a positive whole number
related: [flag-max-chars-per-line, flag-skin]
-->
# `COLUMNS`

<!-- BEGIN GENERATED: facts -->
- **Value:** a positive whole number
- **See also:** [`--max-chars-per-line`](../flags/max-chars-per-line.md), [`--skin`](../flags/skin.md)
<!-- END GENERATED: facts -->

The text report is laid out for a width. Opengrep takes it from `COLUMNS` when
that holds a positive whole number, and otherwise from the terminal. Either way
the width is kept between 40 and 120: a smaller value counts as 40 and a larger
one as 120. Output that goes to a pipe or a file, with no `COLUMNS`, is laid out
for 120.

The width decides where long lines of code wrap, and how far the rules of the
[vivid skin](../flags/skin.md) reach. It affects the text report only.

The name is read as it is, with no `OPENGREP_` form.

## Examples

### A narrow report

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`long.py`**
```python title="long.py"
eval(some_extremely_long_variable_name_for_wrapping + other_long_name_here + more)
```

**Command and result:**
```console
$ COLUMNS=40 opengrep scan --config rule.yaml long.py
long.py

  warn  find-eval
  found eval

    1 │ eval(some_extremely_long_varia
        ble_name_for_wrapping +
        other_long_name_here + more)

$ diff <(COLUMNS=20 opengrep scan --config rule.yaml long.py) <(COLUMNS=40 opengrep scan --config rule.yaml long.py) > /dev/null && echo same
same
```

A width of 20 is below the minimum, so the report comes out exactly as it does
at 40.
