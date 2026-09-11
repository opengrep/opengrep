<!-- reference
id: opt-taint_unify_mvars
kind: option
name: taint_unify_mvars
summary: Require a metavariable used in both a source and a sink to bind the same code in both.
value: `true` or `false`
default: false
related: []
-->
# `taint_unify_mvars`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
<!-- END GENERATED: facts -->

In a [taint rule](../rule-syntax/taint-mode.md), the metavariables of the
sources and of the sinks are independent by default: a source
`session.get($KEY)` and a sink `response.set_cookie($KEY, ...)` match whatever
key each of them names.

With `taint_unify_mvars: true`, a metavariable that appears in both a source
and a sink must bind the same code in both, or the flow is not reported.

## Examples

### A session value written to a cookie of the same name

The rule `session-to-cookie` turns the option on.

**`cookies.yaml`**
```yaml title="cookies.yaml"
rules:
  - id: session-to-cookie-default
    mode: taint
    pattern-sources:
      - pattern: session.get($KEY)
    pattern-sinks:
      - pattern: response.set_cookie($KEY, ...)
    message: session value $KEY copied to a cookie
    languages: [python]
    severity: WARNING
  - id: session-to-cookie
    mode: taint
    pattern-sources:
      - pattern: session.get($KEY)
    pattern-sinks:
      - pattern: response.set_cookie($KEY, ...)
    message: session value $KEY copied to a cookie
    languages: [python]
    severity: WARNING
    options:
      taint_unify_mvars: true
```

**`cookies.py`**
```python title="cookies.py"
def view(session, response):
    user = session.get("user")
    # ruleid: session-to-cookie-default, session-to-cookie
    response.set_cookie("user", user)
    theme = session.get("theme")
    # ruleid: session-to-cookie-default
    response.set_cookie("lang", theme)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
