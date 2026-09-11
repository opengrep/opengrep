<!-- reference
id: opt-go_deeper_stmt
kind: option
name: go_deeper_stmt
summary: Let ... between statements reach statements nested in later blocks.
value: `true` or `false`
default: true
related: []
-->
# `go_deeper_stmt`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
- **See also:** [`go_deeper_expr`](go_deeper_expr.md)
<!-- END GENERATED: facts -->

In a statement pattern, `...` between two statements also reaches statements
nested in the blocks that follow. `lock.acquire()`, `...`, `lock.release()`
matches when `lock.release()` sits in an `if` or a `finally` after the call to
`acquire`.

With `go_deeper_stmt: false`, the statement after `...` must be in the same
block as the one before it.

## Examples

### A release in a nested block

The rule `lock-released` turns the option off.

**`locks.yaml`**
```yaml title="locks.yaml"
rules:
  - id: lock-released-default
    pattern: |
      lock.acquire()
      ...
      lock.release()
    message: lock acquired and released
    languages: [python]
    severity: WARNING
  - id: lock-released
    pattern: |
      lock.acquire()
      ...
      lock.release()
    message: lock acquired and released
    languages: [python]
    severity: WARNING
    options:
      go_deeper_stmt: false
```

**`locks.py`**
```python title="locks.py"
def conditional(lock, ok):
    # ruleid: lock-released-default
    lock.acquire()
    if ok:
        lock.release()

def guarded(lock):
    # ruleid: lock-released-default
    lock.acquire()
    try:
        work()
    finally:
        lock.release()

def flat(lock):
    # ruleid: lock-released-default, lock-released
    lock.acquire()
    lock.release()
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
