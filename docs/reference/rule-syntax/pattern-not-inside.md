<!-- reference
id: key-pattern-not-inside
kind: rule-key
name: pattern-not-inside
summary: Drop the matches that lie inside code matching this pattern.
related: [key-pattern-inside, key-pattern-not]
-->
# `pattern-not-inside`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`pattern-inside`](pattern-inside.md), [`pattern-not`](pattern-not.md), [`patterns`](patterns.md)
<!-- END GENERATED: facts -->

`pattern-not-inside` is an item of [`patterns`](patterns.md). It drops the
matches of the other items that lie inside code matched by its own pattern,
which describes the safe surroundings: a `with` block, a check that guards
the call, a function known to be harmless.

Like [`pattern-not`](pattern-not.md), it is a negative item: it cannot stand
at the top of a rule or among the alternatives of
[`pattern-either`](pattern-either.md).

## Examples

### A file opened outside a `with` block

**`open-without-with.yaml`**
```yaml title="open-without-with.yaml"
rules:
  - id: open-without-with
    patterns:
      - pattern: open(...)
      - pattern-not-inside: |
          with open(...) as $F:
              ...
    message: file opened without a with block
    languages: [python]
    severity: INFO
```

**`open-without-with.py`**
```python title="open-without-with.py"
# ruleid: open-without-with
f = open("report.txt")
data = f.read()

# ok: open-without-with
with open("report.txt") as f:
    data = f.read()
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
