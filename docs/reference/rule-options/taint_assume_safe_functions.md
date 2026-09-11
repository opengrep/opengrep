<!-- reference
id: opt-taint_assume_safe_functions
kind: option
name: taint_assume_safe_functions
summary: Assume that a call returns clean data, even when its arguments are tainted.
value: `true` or `false`
default: false
related: [key-mode-taint, opt-taint_assume_safe_indexes]
-->
# `taint_assume_safe_functions`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** [`mode: taint`](../rule-syntax/taint-mode.md), [`taint_assume_safe_indexes`](taint_assume_safe_indexes.md), [`taint_only_propagate_through_assignments`](taint_only_propagate_through_assignments.md)
<!-- END GENERATED: facts -->

By default, a [taint rule](../rule-syntax/taint-mode.md) assumes that a call
returns tainted data when one of its arguments is tainted, and that a method
called on a tainted object does too: `quote(name)` and `name.strip()` are
tainted when `name` is.

With `taint_assume_safe_functions: true`, the result of such a call is clean.
Operators still spread taint, so `"ls " + name` stays tainted, and a tainted
argument passed straight to a sink is still reported.

A function whose behaviour opengrep has worked out is the exception. With
[`taint_intrafile`](taint_intrafile.md), a call to a function defined in the
same file still returns the taint its body passes on.

## Examples

### Calls that take tainted data

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
      taint_assume_safe_functions: true
```

**`shell.py`**
```python title="shell.py"
import os

name = input()
# ruleid: shell-default, shell
os.system("ls " + name)
# ruleid: shell-default
os.system(quote(name))
stripped = name.strip()
# ruleid: shell-default
os.system(stripped)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### A function defined in the same file

**`wrapped.yaml`**
```yaml title="wrapped.yaml"
rules:
  - id: wrapped
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: user input reaches os.system
    languages: [python]
    severity: ERROR
    options:
      taint_intrafile: true
      taint_assume_safe_functions: true
```

**`wrapped.py`**
```python title="wrapped.py"
import os

def listing(path):
    return "ls " + path

name = input()
# ruleid: wrapped
os.system(listing(name))
# ok: wrapped
os.system(quote(name))
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

`listing` is defined in the file, so its result keeps the taint of `name`;
`quote` is not, so its result is assumed clean.
