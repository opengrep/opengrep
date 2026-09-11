<!-- reference
id: opt-taint_only_propagate_through_assignments
kind: option
name: taint_only_propagate_through_assignments
summary: Let taint move only by assignment, not through operators or calls.
value: `true` or `false`
default: false
related: [opt-taint_assume_safe_functions]
-->
# `taint_only_propagate_through_assignments`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`taint_assume_safe_functions`](taint_assume_safe_functions.md)
<!-- END GENERATED: facts -->

With `taint_only_propagate_through_assignments: true`, taint moves from one
variable to another only by assignment, as in `copy = data`. Operators, calls
and method calls no longer spread it, so `data + "!"`, `quote(data)` and
`data.strip()` are clean, and neither does indexing with a tainted value:
`table[data]` is clean too. This goes further than
[`taint_assume_safe_functions`](taint_assume_safe_functions.md) and
[`taint_assume_safe_indexes`](taint_assume_safe_indexes.md) together.

A list literal still carries the taint of its elements: `[data]` is tainted.

## Examples

### What still carries taint

The rule `logged` turns the option on.

**`logged.yaml`**
```yaml title="logged.yaml"
rules:
  - id: logged-default
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: log(...)
    message: user input is logged
    languages: [python]
    severity: WARNING
  - id: logged
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: log(...)
    message: user input is logged
    languages: [python]
    severity: WARNING
    options:
      taint_only_propagate_through_assignments: true
```

**`logged.py`**
```python title="logged.py"
data = input()
copy = data
# ruleid: logged-default, logged
log(copy)
items = [data]
# ruleid: logged-default, logged
log(items)
# ruleid: logged-default
log(data + "!")
# ruleid: logged-default
log(quote(data))
stripped = data.strip()
# ruleid: logged-default
log(stripped)
# ruleid: logged-default
log(table[data])
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
