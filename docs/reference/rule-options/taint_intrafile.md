<!-- reference
id: opt-taint_intrafile
kind: option
name: taint_intrafile
summary: Follow taint through calls to functions defined in the same file.
value: `true` or `false`
default: false
related: [flag-taint-intrafile, opt-taint_interfile, key-mode-taint]
-->
# `taint_intrafile`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `false`
- **See also:** `--taint-intrafile`, [`taint_interfile`](taint_interfile.md), [`mode: taint`](../rule-syntax/taint-mode.md)
<!-- END GENERATED: facts -->

By default a [taint rule](../rule-syntax/taint-mode.md) analyses each function
on its own. Taint passed as an argument to another function is not followed
into that function. With `taint_intrafile: true`, opengrep computes a summary
of each function in the file, recording how taint flows from its parameters to
its result and to sinks. It applies these summaries at call sites. The finding
is reported at the sink, inside the called function.

The flag `--taint-intrafile` turns this on for every taint rule of the scan.
The option can turn it on for one rule, but it cannot turn off the flag.
[`taint_interfile`](taint_interfile.md) implies `taint_intrafile`.

Supported languages are Apex, C, C++, C#, Clojure, Dart, Elixir, Go, Java,
JavaScript, Julia, Kotlin, Lua, Python, Ruby, Rust, Scala, Swift, TypeScript
and Visual Basic. For other languages opengrep logs a warning, and the
analysis may stay within single functions.

## Examples

### Taint through a helper function

```yaml title="user-input-to-system.yaml"
rules:
  - id: user-input-to-system
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
```

```python title="user-input-to-system.py"
import os

def run(cmd):
    # ruleid: user-input-to-system
    os.system(cmd)

run(input())
```

```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### The flag instead of the option

```yaml title="rule.yaml"
rules:
  - id: user-input-to-system
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: user input reaches os.system
    languages: [python]
    severity: ERROR
```

```python title="app.py"
import os

def run(cmd):
    os.system(cmd)

run(input())
```

```console
$ opengrep scan --config rule.yaml app.py
$ opengrep scan --config rule.yaml --taint-intrafile app.py


┌────────────────┐
│ 1 Code Finding │
└────────────────┘

    app.py
   ❯❯❱ user-input-to-system
          user input reaches os.system

            4┆ os.system(cmd)
```
