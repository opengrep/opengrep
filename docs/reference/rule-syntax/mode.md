<!-- reference
id: key-mode
kind: rule-key
name: mode
summary: How the rule finds code: search, the default, or taint.
related: [key-mode-taint, key-pattern]
-->
# `mode`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`mode: taint`](taint-mode.md), [`pattern`](pattern.md)
<!-- END GENERATED: facts -->

`mode` chooses how a rule finds code.

- `search`, the default, which a rule without `mode` uses: the rule matches
  code with one of [`pattern`](pattern.md), [`pattern-either`](pattern-either.md),
  [`patterns`](patterns.md) or [`pattern-regex`](pattern-regex.md).
- `taint`: the rule follows data from sources to sinks; see
  [`mode: taint`](taint-mode.md).

Any other value makes the rule invalid.

## Examples

### `search` is the default

**`search.yaml`**
```yaml title="search.yaml"
rules:
  - id: find-eval-default
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
  - id: find-eval
    mode: search
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`search.py`**
```python title="search.py"
# ruleid: find-eval-default, find-eval
eval(expression)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### An unknown mode

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    mode: find
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(expression)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
```
