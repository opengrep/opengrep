<!-- reference
id: opt-taint_assume_safe_indexes
kind: option
name: taint_assume_safe_indexes
summary: Assume that a tainted index does not taint the element it selects.
value: `true` or `false`
default: false
related: []
-->
# `taint_assume_safe_indexes`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`taint_assume_safe_functions`](taint_assume_safe_functions.md)
<!-- END GENERATED: facts -->

By default, `commands[choice]` is tainted when the index `choice` is. With
`taint_assume_safe_indexes: true`, a tainted index leaves the element clean.

An element of a tainted collection is still tainted: `words[0]` is tainted
when `words` is.

## Examples

### A tainted index and a tainted list

The rule `shell` turns the option on.

**`shell.yaml`**
```yaml title="shell.yaml"
rules:
  - id: shell-default
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: user input reaches os.system
    languages: [python]
    severity: ERROR
  - id: shell
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: user input reaches os.system
    languages: [python]
    severity: ERROR
    options:
      taint_assume_safe_indexes: true
```

**`shell.py`**
```python title="shell.py"
import os

commands = {"list": "ls", "where": "pwd"}
choice = input()
# ruleid: shell-default
os.system(commands[choice])

words = input()
# ruleid: shell-default, shell
os.system(words[0])
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
