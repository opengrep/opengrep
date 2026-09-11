<!-- reference
id: key-paths
kind: rule-key
name: paths
summary: Limit a rule to files whose paths match, or do not match, glob patterns.
covers: [key-include, key-exclude]
related: [flag-exclude, flag-include]
-->
# `paths`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`--exclude`](../flags/exclude.md), [`--include`](../flags/include.md)
<!-- END GENERATED: facts -->

```yaml
paths:
  include: [GLOB, ...]
  exclude: [GLOB, ...]
```

A rule with `paths` runs only on the files that match at least one `include`
glob, when `include` is given, and no `exclude` glob. A glob is matched against
every directory in the file's path as well as the file name. So `generated/`
matches both `generated/y.py` and `app/generated/x.py`, and `*_test.py` matches
a file with that name in any directory.

`paths` applies to files named on the command line as well as to the files
found under directory targets. It accepts only `include` and `exclude`; any
other key makes the rule invalid.

To filter files for all rules at once, use [`--exclude`](../flags/exclude.md)
and `--include`.

## Examples

### Skip generated code and test files

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: no-print
    pattern: print(...)
    message: use logging instead of print
    languages: [python]
    severity: INFO
    paths:
      exclude:
        - "generated/"
        - "*_test.py"
```

**`app/main.py`**
```python title="app/main.py"
print("hello")
```

**`app/main_test.py`**
```python title="app/main_test.py"
print("hello")
```

**`app/generated/api.py`**
```python title="app/generated/api.py"
print("hello")
```

**`generated/schema.py`**
```python title="generated/schema.py"
print("hello")
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml .
app/main.py

  info  no-print
  use logging instead of print

    1 │ print("hello")
```
