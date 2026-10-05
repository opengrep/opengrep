<!-- reference
id: key-fix-regex
kind: rule-key
name: fix-regex
summary: Fix each match by replacing what a regular expression matches inside it.
covers: [key-replacement, key-count]
related: [key-fix, flag-autofix]
-->
# `fix-regex`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`fix`](fix.md), [`--autofix`](../flags/autofix.md)
<!-- END GENERATED: facts -->

`fix-regex` fixes a match by editing its text rather than replacing it whole,
as [`fix`](fix.md) does. It takes a mapping:

- `regex`: a PCRE regular expression, run over the text of the match;
- `replacement`: the text that replaces each match of the regex. `\1`, `\2`
  and so on stand for the groups of the regex;
- `count`: the most matches to replace, from the start. Without it, every
  match is replaced.

[`--autofix`](../flags/autofix.md) applies it, and `opengrep test` checks it
against a `.fixed` file just as it checks `fix`.

## Examples

### Turning TLS verification back on

**`tls-verify.yaml`**
```yaml title="tls-verify.yaml"
rules:
  - id: tls-verify
    pattern: requests.$METHOD(..., verify=False, ...)
    fix-regex:
      regex: verify\s*=\s*False
      replacement: verify=True
    message: TLS certificate verification is disabled
    languages: [python]
    severity: WARNING
```

**`tls-verify.py`**
```python title="tls-verify.py"
import requests

# ruleid: tls-verify
requests.get("https://api.example.com/users", verify = False)
```

**`tls-verify.fixed.py`**
```python title="tls-verify.fixed.py"
import requests

# ruleid: tls-verify
requests.get("https://api.example.com/users", verify=True)
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
1/1: ✓ All fix tests passed
```

### Groups and `count`

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: rename-all
    pattern: f(...)
    fix-regex:
      regex: (\d+)
      replacement: n\1
    message: m
    languages: [python]
    severity: INFO
  - id: rename-first
    pattern: g(...)
    fix-regex:
      regex: (\d+)
      replacement: n\1
      count: 1
    message: m
    languages: [python]
    severity: INFO
```

**`app.py`**
```python title="app.py"
f(1, 2, 3)
g(1, 2, 3)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml --autofix --dryrun app.py | grep 'fix:'
    fix: f(n1, n2, n3)
    fix: g(n1, 2, 3)
```
