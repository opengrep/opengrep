<!-- reference
id: key-metavariable-regex
kind: rule-key
name: metavariable-regex
summary: Keep the matches where the code a metavariable matched fits a regular expression.
covers: [key-constant-propagation]
related: [key-patterns, key-pattern-regex, key-metavariable-pattern]
-->
# `metavariable-regex`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md), [`pattern-regex`](pattern-regex.md), [`metavariable-pattern`](metavariable-pattern.md), [`metavariable-comparison`](metavariable-comparison.md)
<!-- END GENERATED: facts -->

`metavariable-regex` is an item of [`patterns`](patterns.md). It keeps the
matches where the code bound to a metavariable fits a PCRE regular
expression. It takes a mapping:

- `metavariable`: the metavariable, bound by another item;
- `regex`: the regular expression;
- `constant-propagation`: `true` or `false`, by default `false`.

The regex must match at the start of the metavariable's text, but may stop
before its end: `md5` keeps `md5` and `md5_hex`, not `new_md5`. End it with
`$` to require the whole text.

By default the regex sees the code as written, so a string literal includes
its quotes. With `constant-propagation: true`, a metavariable bound to a
constant is matched by its value instead: a string without its quotes, and a
variable that holds a constant by the constant it holds.

## Examples

### Weak hash functions by name

**`weak-hash.yaml`**
```yaml title="weak-hash.yaml"
rules:
  - id: weak-hash
    patterns:
      - pattern: hashlib.$ALG(...)
      - metavariable-regex:
          metavariable: $ALG
          regex: md5|sha1
    message: $ALG is a weak hash
    languages: [python]
    severity: WARNING
```

**`weak-hash.py`**
```python title="weak-hash.py"
import hashlib

# ruleid: weak-hash
hashlib.md5(data)
# ruleid: weak-hash
hashlib.sha1(data)
# ok: weak-hash
hashlib.sha256(data)
# ok: weak-hash
hashlib.new_md5(data)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### Values through a variable

**`weak-alg.yaml`**
```yaml title="weak-alg.yaml"
rules:
  - id: weak-alg-default
    patterns:
      - pattern: hashlib.new($ALG, ...)
      - metavariable-regex:
          metavariable: $ALG
          regex: '"(md5|sha1)"'
    message: weak hash
    languages: [python]
    severity: WARNING
  - id: weak-alg
    patterns:
      - pattern: hashlib.new($ALG, ...)
      - metavariable-regex:
          metavariable: $ALG
          regex: md5|sha1
          constant-propagation: true
    message: weak hash
    languages: [python]
    severity: WARNING
```

**`weak-alg.py`**
```python title="weak-alg.py"
import hashlib

# ruleid: weak-alg-default, weak-alg
hashlib.new("md5", data)

alg = "sha1"
# ruleid: weak-alg
hashlib.new(alg, data)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

`weak-alg-default` matches the literal with its quotes and misses the
variable. `weak-alg` matches the value in both places.
