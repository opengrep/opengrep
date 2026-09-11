<!-- reference
id: flag-taint-intrafile
kind: flag
name: --taint-intrafile
summary: Follow taint through calls to functions defined in the same file, for every taint rule.
commands: [scan, ci, test]
related: [opt-taint_intrafile, key-mode-taint, flag-taint-interfile, flag-guarded-taint-signatures]
-->
# `--taint-intrafile`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md)
- **See also:** [`taint_intrafile`](../rule-options/taint_intrafile.md), [`mode: taint`](../rule-syntax/taint-mode.md), [`--taint-interfile`](taint-interfile.md), [`--guarded-taint-signatures`](guarded-taint-signatures.md)
<!-- END GENERATED: facts -->

A [taint rule](../rule-syntax/taint-mode.md) analyses one function at a time:
taint handed to another function is not followed into it. This flag turns on
cross-function analysis within each file for every taint rule of the scan.
Opengrep summarises what each function does with its arguments and applies
those summaries at the call sites, however long the chain.

The finding is reported at the sink, inside the function that contains it,
not at the call that started the chain.

The rule option [`taint_intrafile`](../rule-options/taint_intrafile.md) asks
for the same thing for one rule. The flag only ever turns the analysis on: a
rule cannot opt out of it.

Supported languages are Apex, C, C++, C#, Clojure, Dart, Elixir, Go, Java,
JavaScript, Julia, Kotlin, Lua, Python, Ruby, Rust, Scala, Swift, TypeScript
and Visual Basic. Elsewhere opengrep warns and stays within single functions.

## Examples

### A chain of calls in one file

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: input-to-system
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: input reaches os.system
    languages: [python]
    severity: ERROR
```

**`chain.py`**
```python title="chain.py"
import os

def a(x):
    os.system(x)

def b(x):
    a(x)

def c(x):
    b(x)

def d(x):
    c(x)

d(input())
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml chain.py
$ opengrep scan --config rule.yaml --taint-intrafile chain.py
chain.py

  error  input-to-system
  input reaches os.system

    4 │ os.system(x)
```

The first command prints nothing: without the flag the taint stops at the
call to `c`.
