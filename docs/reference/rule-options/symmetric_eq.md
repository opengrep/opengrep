<!-- reference
id: opt-symmetric_eq
kind: option
name: symmetric_eq
summary: Match the operands of == and != in either order.
value: `true` or `false`
default: false
related: []
covers: [opt-commutative_compop]
-->
# `symmetric_eq`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`ac_matching`](ac_matching.md)
<!-- END GENERATED: facts -->

With `symmetric_eq: true`, `request.method == "POST"` also matches
`"POST" == request.method`, and the same holds for `!=`. Other comparisons
keep their order: `limit < count` does not match `count > limit`.

It works through [`ac_matching`](ac_matching.md), and has no effect when that
option is `false`.

`commutative_compop` is an older name for this option. It still works, but it
prints a deprecation warning for every `==` and `!=` the rule is matched
against.

## Examples

### A comparison written the other way round

The rule `post-request` turns the option on.

**`post.yaml`**
```yaml title="post.yaml"
rules:
  - id: post-request-default
    pattern: request.method == "POST"
    message: handles a POST request
    languages: [python]
    severity: WARNING
  - id: post-request
    pattern: request.method == "POST"
    message: handles a POST request
    languages: [python]
    severity: WARNING
    options:
      symmetric_eq: true
```

**`post.py`**
```python title="post.py"
def handle(request):
    # ruleid: post-request-default, post-request
    if request.method == "POST":
        pass
    # ruleid: post-request
    if "POST" == request.method:
        pass
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### The old name

**`compop.yaml`**
```yaml title="compop.yaml"
rules:
  - id: post-request
    pattern: request.method == "POST"
    message: handles a POST request
    languages: [python]
    severity: WARNING
    options:
      commutative_compop: true
```

**`compop.py`**
```python title="compop.py"
def handle(request):
    if "POST" == request.method:
        pass
    if request.method == "GET":
        pass
    if user.role == "admin":
        pass
```

**Command and result:**
```console
$ opengrep scan --config compop.yaml compop.py 2>&1 >/dev/null | grep -m1 deprecated
[00.00][WARNING]: `commutative_compop` rule option has been deprecated. Please use `symmetric_eq` instead.
$ opengrep scan --config compop.yaml compop.py 2>&1 >/dev/null | grep -c deprecated
3
```

The three comparisons produce three warnings, although only the first matches.
