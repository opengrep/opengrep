<!-- reference
id: opt-go_deeper_expr
kind: option
name: go_deeper_expr
summary: Let deep expression patterns and statements look inside expressions.
value: `true` or `false`
default: true
related: [opt-implicit_deep_exprstmt, opt-go_deeper_stmt]
-->
# `go_deeper_expr`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`implicit_deep_exprstmt`](implicit_deep_exprstmt.md), [`go_deeper_stmt`](go_deeper_stmt.md)
<!-- END GENERATED: facts -->

This option lets matching look inside expressions in two places:

- a deep expression pattern `<... e ...>` matches any expression that
  contains `e`;
- in a statement pattern, a statement such as `finish()` also matches a
  statement that contains it, such as `result = finish()`; see
  [`implicit_deep_exprstmt`](implicit_deep_exprstmt.md).

With `go_deeper_expr: false`, both match at the top only: `<... e ...>`
matches `e` itself, and `finish()` matches the statement `finish()`.

## Examples

### A condition that contains the check

The rule `admin-branch` turns the option off.

**`branch.yaml`**
```yaml title="branch.yaml"
rules:
  - id: admin-branch-default
    pattern: |
      if <... user.is_admin ...>:
          ...
    message: branch on admin rights
    languages: [python]
    severity: WARNING
  - id: admin-branch
    pattern: |
      if <... user.is_admin ...>:
          ...
    message: branch on admin rights
    languages: [python]
    severity: WARNING
    options:
      go_deeper_expr: false
```

**`branch.py`**
```python title="branch.py"
def view(user, ok):
    # ruleid: admin-branch-default, admin-branch
    if user.is_admin:
        pass
    # ruleid: admin-branch-default
    if ok and user.is_admin:
        pass
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
