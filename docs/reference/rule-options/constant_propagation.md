<!-- reference
id: opt-constant_propagation
kind: option
name: constant_propagation
summary: Let a literal in a pattern match a variable known to hold that value.
value: `true` or `false`
default: true
related: []
-->
# `constant_propagation`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`symbolic_propagation`](symbolic_propagation.md)
<!-- END GENERATED: facts -->

A variable that is assigned a constant is matched as that constant. After
`path = "/etc/passwd"`, the pattern `open("/etc/passwd")` also matches
`open(path)`. Constant expressions are evaluated first, so a variable holding
`"/etc/" + "passwd"` matches too. A variable that is assigned something else
afterwards no longer counts as constant.

With `constant_propagation: false`, a literal in a pattern matches only the
literal itself. The option also governs two things built on it:
`metavariable-regex` with `constant-propagation: true`, which then sees only
literals, and [`symbolic_propagation`](symbolic_propagation.md).

## Examples

### A path held in a variable

The rule `passwd-file-default` leaves the option on; `passwd-file` turns it
off, and matches only the literal.

**`passwd.yaml`**
```yaml title="passwd.yaml"
rules:
  - id: passwd-file-default
    pattern: open("/etc/passwd")
    message: reads the password file
    languages: [python]
    severity: WARNING
  - id: passwd-file
    pattern: open("/etc/passwd")
    message: reads the password file
    languages: [python]
    severity: WARNING
    options:
      constant_propagation: false
```

**`passwd.py`**
```python title="passwd.py"
# ruleid: passwd-file-default, passwd-file
open("/etc/passwd")

path = "/etc/passwd"
# ruleid: passwd-file-default
open(path)

joined = "/etc/" + "passwd"
# ruleid: passwd-file-default
open(joined)

other = "/etc/passwd"
other = input()
# ok: passwd-file-default
open(other)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
