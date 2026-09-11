<!-- reference
id: opt-ac_matching
kind: option
name: ac_matching
summary: Match chains of an associative operator as a flat list of operands.
value: `true` or `false`
default: true
related: [opt-commutative_boolop, opt-symmetric_eq]
-->
# `ac_matching`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`commutative_boolop`](commutative_boolop.md), [`symmetric_eq`](symmetric_eq.md)
<!-- END GENERATED: facts -->

`a | b | c` is parsed as `(a | b) | c`. With `ac_matching`, on by default, a
chain of one associative operator is matched as a flat list of operands, not
as the nested pairs the parser produces. Two cases follow.

- The bitwise operators `|`, `&` and `^` are also commutative: the operands of
  the pattern may appear in any order, among other operands.
  `os.O_CREAT | os.O_TRUNC` matches `os.O_TRUNC | os.O_WRONLY | os.O_CREAT`.
- For `&&` and `||`, the order is kept, but a metavariable can stand for
  several operands in a row: `a() && $X && c()` matches
  `a() && b() && d() && c()`.
  [`commutative_boolop`](commutative_boolop.md) drops the order too.

With `ac_matching: false`, operators are matched as the parser nests them.
[`commutative_boolop`](commutative_boolop.md) and
[`symmetric_eq`](symmetric_eq.md) then have no effect.

## Examples

### Flags combined in any order

The rule `trunc-create` turns the option off.

**`flags.yaml`**
```yaml title="flags.yaml"
rules:
  - id: trunc-create-default
    pattern: os.O_CREAT | os.O_TRUNC
    message: creates or truncates the file
    languages: [python]
    severity: WARNING
  - id: trunc-create
    pattern: os.O_CREAT | os.O_TRUNC
    message: creates or truncates the file
    languages: [python]
    severity: WARNING
    options:
      ac_matching: false
```

**`flags.py`**
```python title="flags.py"
import os

def create(path):
    # ruleid: trunc-create-default, trunc-create
    os.open(path, os.O_CREAT | os.O_TRUNC)
    # ruleid: trunc-create-default
    os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC)
    # ruleid: trunc-create-default
    os.open(path, os.O_TRUNC | os.O_WRONLY | os.O_CREAT)
    # ok: trunc-create-default
    os.open(path, os.O_CREAT | os.O_WRONLY)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### A metavariable for several conditions

**`guard.yaml`**
```yaml title="guard.yaml"
rules:
  - id: guard-default
    pattern: user.active && $COND && user.admin
    message: admin check with extra conditions
    languages: [javascript]
    severity: WARNING
  - id: guard
    pattern: user.active && $COND && user.admin
    message: admin check with extra conditions
    languages: [javascript]
    severity: WARNING
    options:
      ac_matching: false
```

**`guard.js`**
```javascript title="guard.js"
// ruleid: guard-default, guard
if (user.active && user.verified && user.admin) {}
// ruleid: guard-default
if (user.active && user.verified && req.secure && user.admin) {}
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
