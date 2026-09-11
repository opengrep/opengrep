<!-- reference
id: opt-implicit_deep_exprstmt
kind: option
name: implicit_deep_exprstmt
summary: Let an expression statement in a statement pattern match a statement containing it.
value: `true` or `false`
default: true
related: []
-->
# `implicit_deep_exprstmt`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`go_deeper_expr`](go_deeper_expr.md)
<!-- END GENERATED: facts -->

In a statement pattern, an expression statement such as `finish()` also
matches a statement that contains the expression: `result = finish()` or
`log(finish())`. It works through [`go_deeper_expr`](go_deeper_expr.md), and
has no effect when that option is `false`.

With `implicit_deep_exprstmt: false`, it matches the statement `finish()`
only.

A pattern made of a single expression is not a statement pattern: it matches
the expression wherever it occurs, whatever this option says.

## Examples

### A call whose result is used

The rule `start-then-finish` turns the option off.

**`steps.yaml`**
```yaml title="steps.yaml"
rules:
  - id: start-then-finish-default
    pattern: |
      start()
      ...
      finish()
    message: finish() follows start()
    languages: [python]
    severity: WARNING
  - id: start-then-finish
    pattern: |
      start()
      ...
      finish()
    message: finish() follows start()
    languages: [python]
    severity: WARNING
    options:
      implicit_deep_exprstmt: false
```

**`steps.py`**
```python title="steps.py"
def assigned():
    # ruleid: start-then-finish-default
    start()
    result = finish()

def passed_on():
    # ruleid: start-then-finish-default
    start()
    log(finish())

def plain():
    # ruleid: start-then-finish-default, start-then-finish
    start()
    finish()
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
