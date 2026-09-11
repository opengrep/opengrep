<!-- reference
id: opt-flddef_assign
kind: option
name: flddef_assign
summary: Let a pattern assigning a function match methods and fields that hold functions.
value: `true` or `false`
default: false
related: [opt-arrow_is_function]
-->
# `flddef_assign`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`arrow_is_function`](arrow_is_function.md), [`vardef_assign`](vardef_assign.md)
<!-- END GENERATED: facts -->

With `flddef_assign: true`, a pattern that assigns a function, such as
`$X = function (...) { ... }`, also matches a function defined as a member:

- a class field holding a function, `onClick = function (e) { ... }`;
- a class method, `onKey(e) { ... }`;
- a method in an object literal, `{ onClick(e) { ... } }`;
- an object property holding a function, `{ onKey: function (e) { ... } }`.

A function declared at the top level, `function top(e) { ... }`, is not
matched.

## Examples

### Functions defined in a class

The rule `function-member` turns the option on.

**`widget.yaml`**
```yaml title="widget.yaml"
rules:
  - id: function-member-default
    pattern: $X = function (...) { ... }
    message: function assigned to a name
    languages: [javascript]
    severity: WARNING
  - id: function-member
    pattern: $X = function (...) { ... }
    message: function assigned to a name
    languages: [javascript]
    severity: WARNING
    options:
      flddef_assign: true
```

**`widget.js`**
```javascript title="widget.js"
class Widget {
  // ruleid: function-member
  onClick = function (e) { return e; };
  // ruleid: function-member
  onKey(e) { return e; }
}

// ruleid: function-member-default, function-member
obj.onClick = function (e) { return e; };

// ok: function-member
function top(e) { return e; }
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
