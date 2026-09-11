<!-- reference
id: key-mode-taint
kind: rule-key
name: mode: taint
summary: Find data that flows from sources to sinks without passing through a sanitizer.
covers: [key-pattern-sources, key-pattern-sinks, key-pattern-sanitizers, key-pattern-propagators]
related: [opt-taint_intrafile, opt-taint_interfile, flag-taint-intrafile, flag-taint-interfile]
-->
# `mode: taint`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`taint_intrafile`](../rule-options/taint_intrafile.md), [`taint_interfile`](../rule-options/taint_interfile.md), `--taint-intrafile`, `--taint-interfile`
<!-- END GENERATED: facts -->

```yaml
mode: taint
pattern-sources: [...]
pattern-sanitizers: [...]    # optional
pattern-propagators: [...]   # optional
pattern-sinks: [...]
```

A taint rule reports code where data from a source reaches a sink. Each item
of the four lists is a pattern, written with the same keys as a search rule
(`pattern`, `patterns`, `pattern-either`, …). Some items take extra keys, such
as `label` and `requires` for tracking kinds of taint, or `by-side-effect`.

- `pattern-sources`: the code they match is tainted.
- Taint spreads through assignments, and to expressions and calls that take a
  tainted value.
- `pattern-sanitizers`: the code they match is clean, even when built from
  tainted values.
- `pattern-propagators`: patterns with `from: $A` and `to: $B` that spread
  taint through operations that the analysis cannot see, such as
  `$B.append($A)` moving taint from `$A` to `$B`.
- `pattern-sinks`: a finding is reported at each sink that tainted data
  reaches.

By default the analysis stays within one function: taint does not follow
calls into other functions. The option
[`taint_intrafile`](../rule-options/taint_intrafile.md), or the flag
`--taint-intrafile`, follows calls to functions in the same file.
[`taint_interfile`](../rule-options/taint_interfile.md), or the flag
`--taint-interfile`, follows calls across files.

## Examples

### User input reaching a shell

```yaml title="user-input-to-system.yaml"
rules:
  - id: user-input-to-system
    mode: taint
    pattern-sources:
      - pattern: input(...)
    pattern-sanitizers:
      - pattern: shlex.quote(...)
    pattern-sinks:
      - pattern: os.system(...)
    message: user input reaches os.system
    languages: [python]
    severity: ERROR
```

```python title="user-input-to-system.py"
import os, shlex

name = input()
# ruleid: user-input-to-system
os.system("ls " + name)
# ok: user-input-to-system
os.system("ls " + shlex.quote(name))
```

```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
