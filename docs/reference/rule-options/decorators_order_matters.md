<!-- reference
id: opt-decorators_order_matters
kind: option
name: decorators_order_matters
summary: Require decorators and annotations to appear in the order the pattern gives.
value: `true` or `false`
default: false
related: []
-->
# `decorators_order_matters`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`attr_expr`](attr_expr.md)
<!-- END GENERATED: facts -->

The decorators of a pattern, or its annotations in a language such as Java,
match in any order by default. With `decorators_order_matters: true`, they
must appear in the order the pattern gives, though other decorators may come
between them.

Keywords such as `public` and `static` are not affected: they match in any
order either way.

## Examples

### Decorators in the wrong order

The rule `login-route` turns the option on.

**`views.yaml`**
```yaml title="views.yaml"
rules:
  - id: login-route-default
    pattern: |
      @app.route(...)
      @login_required
      def $F(...):
          ...
    message: route that requires a login
    languages: [python]
    severity: WARNING
  - id: login-route
    pattern: |
      @app.route(...)
      @login_required
      def $F(...):
          ...
    message: route that requires a login
    languages: [python]
    severity: WARNING
    options:
      decorators_order_matters: true
```

**`views.py`**
```python title="views.py"
# ruleid: login-route-default, login-route
@app.route("/a")
@login_required
def a():
    pass

# ruleid: login-route-default
@login_required
@app.route("/b")
def b():
    pass

# ruleid: login-route-default, login-route
@app.route("/c")
@cache
@login_required
def c():
    pass
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
