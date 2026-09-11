<!-- reference
id: opt-arrow_is_function
kind: option
name: arrow_is_function
summary: Match arrow functions and function expressions like other functions.
value: `true` or `false`
default: true
related: []
-->
# `arrow_is_function`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`flddef_assign`](flddef_assign.md)
<!-- END GENERATED: facts -->

Two function forms match each other while this option is on:

- a function pattern `function (...) { ... }` also matches an arrow function,
  with a block body, `(req) => { ... }`, or without, `(req) => 1`;
- a function declaration pattern `function $F(req) { ... }` also matches a
  variable holding an arrow function or a function expression, such as
  `const handler = (req) => { ... }`.

With `arrow_is_function: false`, each pattern matches only its own form.

## Examples

### Callbacks

The rule `route-handler` turns the option off.

**`handlers.yaml`**
```yaml title="handlers.yaml"
rules:
  - id: route-handler-default
    pattern: app.get($PATH, function (...) { ... })
    message: route handler
    languages: [javascript]
    severity: WARNING
  - id: route-handler
    pattern: app.get($PATH, function (...) { ... })
    message: route handler
    languages: [javascript]
    severity: WARNING
    options:
      arrow_is_function: false
```

**`handlers.js`**
```javascript title="handlers.js"
// ruleid: route-handler-default, route-handler
app.get("/", function (req, res) { res.send("hi"); });
// ruleid: route-handler-default
app.get("/a", (req, res) => { res.send("hi"); });
// ruleid: route-handler-default
app.get("/b", (req, res) => res.send("hi"));
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### Functions held in variables

**`declared.yaml`**
```yaml title="declared.yaml"
rules:
  - id: request-handler-default
    pattern: function $F(req) { ... }
    message: request handler
    languages: [javascript]
    severity: WARNING
  - id: request-handler
    pattern: function $F(req) { ... }
    message: request handler
    languages: [javascript]
    severity: WARNING
    options:
      arrow_is_function: false
```

**`declared.js`**
```javascript title="declared.js"
// ruleid: request-handler-default, request-handler
function a(req) { return 1; }
// ruleid: request-handler-default
const b = (req) => { return 1; };
// ruleid: request-handler-default
const c = function (req) { return 1; };
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
