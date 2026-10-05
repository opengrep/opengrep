<!-- reference
id: key-pattern
kind: rule-key
name: pattern
summary: Match code that looks like the pattern, written in the rule's language.
related: [key-patterns, key-pattern-either, key-pattern-regex, flag-pattern]
-->
# `pattern`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md), [`pattern-either`](pattern-either.md), [`pattern-regex`](pattern-regex.md), [`--pattern`](../flags/pattern.md), [`mode`](mode.md)
<!-- END GENERATED: facts -->

A pattern is a piece of code in the rule's language. Opengrep matches it
against the syntax of the target, so line breaks, spacing and comments make no
difference. Two kinds of placeholders make a pattern general:

- a metavariable, a `$` followed by capital letters, digits and underscores
  such as `$URL`, matches any expression and binds it. A metavariable that
  appears more than once must match the same code each time.
- an ellipsis, `...`, matches any sequence: of arguments in a call, of
  statements in a block, and so on.

A pattern can stand on its own at the top of a rule, as an item of
[`patterns`](patterns.md), or as an alternative in
[`pattern-either`](pattern-either.md). A rule has exactly one of `pattern`,
`pattern-either`, `patterns` and [`pattern-regex`](pattern-regex.md) at its
top. A pattern of several lines is written as a YAML block, after `|`.

In a rule for `regex`, which has no syntax to match, `pattern` is an error;
use `pattern-regex` there.

## Examples

### A keyword argument anywhere in the call

**`no-verify.yaml`**
```yaml title="no-verify.yaml"
rules:
  - id: no-verify
    pattern: requests.get($URL, ..., verify=False, ...)
    message: TLS certificate verification is disabled
    languages: [python]
    severity: WARNING
```

**`no-verify.py`**
```python title="no-verify.py"
import requests

# ruleid: no-verify
requests.get(url, verify=False)
# ruleid: no-verify
requests.get(url, timeout=5, verify=False)
# ok: no-verify
requests.get(url, timeout=5)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### The same metavariable twice

**`self-compare.yaml`**
```yaml title="self-compare.yaml"
rules:
  - id: self-compare
    pattern: $X == $X
    message: $X is compared with itself
    languages: [python]
    severity: WARNING
```

**`self-compare.py`**
```python title="self-compare.py"
# ruleid: self-compare
if token == token:
    pass
# ok: self-compare
if token == expected:
    pass
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
