<!-- reference
id: opt-let_is_var
kind: option
name: let_is_var
summary: Let a var declaration pattern match let and const declarations.
value: `true` or `false`
default: true
related: []
-->
# `let_is_var`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`vardef_assign`](vardef_assign.md)
<!-- END GENERATED: facts -->

A `var` declaration pattern, such as `var $X = "...";`, also matches `let` and
`const` declarations. The reverse does not hold: a `let` pattern matches only
`let`.

With `let_is_var: false`, a `var` pattern matches `var` alone.

## Examples

### Three kinds of declaration

The rule `string-var` turns the option off.

**`decls.yaml`**
```yaml title="decls.yaml"
rules:
  - id: string-var-default
    pattern: var $X = "...";
    message: string declared
    languages: [javascript]
    severity: WARNING
  - id: string-var
    pattern: var $X = "...";
    message: string declared
    languages: [javascript]
    severity: WARNING
    options:
      let_is_var: false
```

**`decls.js`**
```javascript title="decls.js"
// ruleid: string-var-default, string-var
var a = "s";
// ruleid: string-var-default
let b = "s";
// ruleid: string-var-default
const c = "s";
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
