<!-- reference
id: opt-attr_expr
kind: option
name: attr_expr
summary: Let a call pattern match a decorator or an annotation written like a call.
value: `true` or `false`
default: true
related: [opt-decorators_order_matters]
-->
# `attr_expr`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`decorators_order_matters`](decorators_order_matters.md)
<!-- END GENERATED: facts -->

A call pattern such as `route(...)` also matches an attribute written like a
call: a Python decorator `@route("/admin")`, or a Java annotation
`@RequestMapping("/admin")` for the pattern `RequestMapping(...)`. The
arguments of the attribute then count as inside a call, so
`pattern-inside: route(...)` covers them.

With `attr_expr: false`, attributes are not matched as calls. A Python
decorator with a dotted name, such as `@app.route(...)`, is the exception: it
is matched as a call whatever this option says.

## Examples

### A decorator and a call

The rule `route-call` turns the option off.

**`routes.yaml`**
```yaml title="routes.yaml"
rules:
  - id: route-call-default
    pattern: route(...)
    message: registers a route
    languages: [python]
    severity: WARNING
  - id: route-call
    pattern: route(...)
    message: registers a route
    languages: [python]
    severity: WARNING
    options:
      attr_expr: false
```

**`routes.py`**
```python title="routes.py"
# ruleid: route-call-default
@route("/admin")
def admin():
    pass

def health():
    pass

# ruleid: route-call-default, route-call
route("/health")(health)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
