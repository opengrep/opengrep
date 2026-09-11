<!-- reference
id: opt-vardef_assign
kind: option
name: vardef_assign
summary: Let an assignment pattern match a variable declaration with an initial value.
value: `true` or `false`
default: true
related: [opt-flddef_assign, opt-let_is_var]
-->
# `vardef_assign`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`flddef_assign`](flddef_assign.md), [`let_is_var`](let_is_var.md)
<!-- END GENERATED: facts -->

An assignment pattern such as `password = "..."` also matches a declaration
that gives the variable an initial value: `var password = "hunter2";`,
`let ...` and `const ...` in JavaScript, or `password: str = "hunter2"` in
Python. It works in statement patterns too: `$X = source();` followed by
`...` and `sink($X);` matches `const data = source(); sink(data);`.

With `vardef_assign: false`, an assignment pattern matches assignments only.

## Examples

### Declarations and assignments

The rule `hardcoded-password` turns the option off.

**`password.yaml`**
```yaml title="password.yaml"
rules:
  - id: hardcoded-password-default
    pattern: password = "..."
    message: hard-coded password
    languages: [javascript]
    severity: WARNING
  - id: hardcoded-password
    pattern: password = "..."
    message: hard-coded password
    languages: [javascript]
    severity: WARNING
    options:
      vardef_assign: false
```

**`password.js`**
```javascript title="password.js"
function a() {
  // ruleid: hardcoded-password-default, hardcoded-password
  password = "hunter2";
}
function b() {
  // ruleid: hardcoded-password-default
  let password = "hunter2";
}
function c() {
  // ruleid: hardcoded-password-default
  const password = "hunter2";
}
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
