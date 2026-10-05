<!-- reference
id: key-pattern-either
kind: rule-key
name: pattern-either
summary: Match code that matches any one of a list of patterns.
related: [key-pattern, key-patterns]
-->
# `pattern-either`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`pattern`](pattern.md), [`patterns`](patterns.md)
<!-- END GENERATED: facts -->

`pattern-either` takes a list of alternatives and matches the code that
matches at least one of them. Each item is a mapping with a single key:
[`pattern`](pattern.md), [`patterns`](patterns.md),
[`pattern-regex`](pattern-regex.md), or another `pattern-either`. A bare
string is an error, with a hint to write `pattern:` in front of it.

`pattern-either` can stand at the top of a rule or as an item of
[`patterns`](patterns.md). Negative patterns, such as `pattern-not`, are not
allowed among its items: they belong directly under `patterns`.

## Examples

### Several weak hash functions

**`weak-hash.yaml`**
```yaml title="weak-hash.yaml"
rules:
  - id: weak-hash
    pattern-either:
      - pattern: hashlib.md5(...)
      - pattern: hashlib.sha1(...)
      - pattern: hashlib.new("md5", ...)
    message: weak hash
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
# ruleid: weak-hash
hashlib.new("md5", data)
# ok: weak-hash
hashlib.sha256(data)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### A negative pattern among the alternatives

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern-either:
      - pattern: eval(...)
      - pattern-not: eval("...")
    message: found eval
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(expression)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py 2>&1 | grep negate
you can only negate directly inside `patterns:` or `all:`
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
```
