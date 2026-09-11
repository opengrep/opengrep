<!-- reference
id: opt-unify_ids_strictly
kind: option
name: unify_ids_strictly
summary: Require a metavariable bound twice to name the same variable, not just the same name.
value: `true` or `false`
default: true
related: []
-->
# `unify_ids_strictly`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `true`
<!-- END GENERATED: facts -->

When a metavariable that holds an identifier is met again, the two occurrences
must name the same variable. A parameter of an inner function that happens to
have the name of an outer variable is a different variable, so it does not
unify with it.

With `unify_ids_strictly: false`, having the same name is enough.

## Examples

### A parameter with the name of an outer variable

The rule `source-to-sink` turns the option off.

**`flow.yaml`**
```yaml title="flow.yaml"
rules:
  - id: source-to-sink-default
    patterns:
      - pattern-inside: |
          $X = source()
          ...
      - pattern: sink($X)
    message: value from source() reaches sink()
    languages: [python]
    severity: WARNING
  - id: source-to-sink
    patterns:
      - pattern-inside: |
          $X = source()
          ...
      - pattern: sink($X)
    message: value from source() reaches sink()
    languages: [python]
    severity: WARNING
    options:
      unify_ids_strictly: false
```

**`flow.py`**
```python title="flow.py"
def handler():
    data = source()
    # ruleid: source-to-sink-default, source-to-sink
    sink(data)

    def log(data):
        # ruleid: source-to-sink
        sink(data)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
