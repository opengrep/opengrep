<!-- reference
id: key-metavariable-analysis
kind: rule-key
name: metavariable-analysis
summary: Keep the matches where a metavariable passes an analysis: entropy for secrets, redos for regular expressions.
covers: [key-analyzer, key-entropy, key-redos]
related: [key-patterns]
-->
# `metavariable-analysis`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`patterns`](patterns.md)
<!-- END GENERATED: facts -->

`metavariable-analysis` is an item of [`patterns`](patterns.md). It keeps the
matches where the code bound to a metavariable passes an analysis. It takes a
mapping:

- `metavariable`: the metavariable, bound by another item;
- `analyzer`: the analysis to run:
  - `entropy`: the value looks random, as an API token or a key does, rather
    than like words;
  - `redos`: the value is a regular expression open to catastrophic
    backtracking, which an attacker can exploit with a crafted input.

Any other analyzer is an error, `Unsupported analyzer`.

## Examples

### A string that looks like a secret

**`high-entropy-secret.yaml`**
```yaml title="high-entropy-secret.yaml"
rules:
  - id: high-entropy-secret
    patterns:
      - pattern: $NAME = "$VALUE"
      - metavariable-analysis:
          metavariable: $VALUE
          analyzer: entropy
    message: a string that looks like a secret
    languages: [python]
    severity: WARNING
```

**`high-entropy-secret.py`**
```python title="high-entropy-secret.py"
# ruleid: high-entropy-secret
api_token = "ghp_7Hq2xV9kLmP4sT8wZ3nR6yB1cF5jD0aE"
# ok: high-entropy-secret
greeting = "hello world"
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### A regular expression open to ReDoS

**`redos.yaml`**
```yaml title="redos.yaml"
rules:
  - id: redos
    patterns:
      - pattern: re.compile("$RE")
      - metavariable-analysis:
          metavariable: $RE
          analyzer: redos
    message: a regex open to catastrophic backtracking
    languages: [python]
    severity: WARNING
```

**`redos.py`**
```python title="redos.py"
import re

# ruleid: redos
re.compile("^(a+)+$")
# ok: redos
re.compile("^[a-z]+$")
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```
