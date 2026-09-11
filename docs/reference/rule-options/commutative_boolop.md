<!-- reference
id: opt-commutative_boolop
kind: option
name: commutative_boolop
summary: Match the operands of && and || in any order.
value: `true` or `false`
default: false
related: []
-->
# `commutative_boolop`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`ac_matching`](ac_matching.md)
<!-- END GENERATED: facts -->

With `commutative_boolop: true`, the boolean operators `&&` and `||`, `and`
and `or` in Python, are matched like `|` is under
[`ac_matching`](ac_matching.md): the operands of the pattern may appear in any
order, among other operands. `user.isAdmin && req.secure` then matches
`req.secure && user.active && user.isAdmin`.

It works through `ac_matching`, and has no effect when that option is `false`.

## Examples

### Conditions written the other way round

The rule `admin-over-https` turns the option on.

**`admin.yaml`**
```yaml title="admin.yaml"
rules:
  - id: admin-over-https-default
    pattern: user.isAdmin && req.secure
    message: admin request over HTTPS
    languages: [javascript]
    severity: WARNING
  - id: admin-over-https
    pattern: user.isAdmin && req.secure
    message: admin request over HTTPS
    languages: [javascript]
    severity: WARNING
    options:
      commutative_boolop: true
```

**`admin.js`**
```javascript title="admin.js"
// ruleid: admin-over-https-default, admin-over-https
if (user.isAdmin && req.secure) {}
// ruleid: admin-over-https
if (req.secure && user.isAdmin) {}
// ruleid: admin-over-https
if (req.secure && user.active && user.isAdmin) {}
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
