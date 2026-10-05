<!-- reference
id: key-metavariable-comparison
kind: rule-key
name: metavariable-comparison
summary: Keep the matches where an expression over metavariables is true.
covers: [key-comparison, key-strip]
related: [key-patterns, key-metavariable-regex]
-->
# `metavariable-comparison`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md), [`metavariable-regex`](metavariable-regex.md)
<!-- END GENERATED: facts -->

`metavariable-comparison` is an item of [`patterns`](patterns.md). It keeps
the matches for which an expression over the metavariables is true. It takes
a mapping:

- `comparison`: the expression, in Python syntax. It can use the
  metavariables, numbers and strings, arithmetic (`+`, `-`, `*`, `%`),
  comparisons (`<`, `<=`, `>`, `>=`, `==`, `!=`), `and`, `or` and `not`, and
  the conversions `int()` and `str()`;
- `metavariable`: the metavariable the comparison is about. It is needed only
  with `strip`;
- `strip`: `true` to remove the quotes around that metavariable's value first,
  so that a string such as `"80"` compares as the number 80.

## Examples

### A short RSA key

**`short-rsa-key.yaml`**
```yaml title="short-rsa-key.yaml"
rules:
  - id: short-rsa-key
    patterns:
      - pattern: rsa.generate_private_key(..., key_size=$SIZE, ...)
      - metavariable-comparison:
          metavariable: $SIZE
          comparison: $SIZE < 2048
    message: RSA keys shorter than 2048 bits are weak
    languages: [python]
    severity: ERROR
```

**`short-rsa-key.py`**
```python title="short-rsa-key.py"
from cryptography.hazmat.primitives.asymmetric import rsa

# ruleid: short-rsa-key
rsa.generate_private_key(public_exponent=65537, key_size=1024)
# ok: short-rsa-key
rsa.generate_private_key(public_exponent=65537, key_size=4096)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### A number written as a string

**`low-port.yaml`**
```yaml title="low-port.yaml"
rules:
  - id: low-port-default
    patterns:
      - pattern: app.run(port=$PORT)
      - metavariable-comparison:
          metavariable: $PORT
          comparison: $PORT < 1024
    message: a privileged port
    languages: [python]
    severity: INFO
  - id: low-port
    patterns:
      - pattern: app.run(port=$PORT)
      - metavariable-comparison:
          metavariable: $PORT
          comparison: $PORT < 1024
          strip: true
    message: a privileged port
    languages: [python]
    severity: INFO
```

**`low-port.py`**
```python title="low-port.py"
# ruleid: low-port-default, low-port
app.run(port=80)
# ruleid: low-port
app.run(port="80")
app.run(port=8080)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```
