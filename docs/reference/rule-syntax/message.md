<!-- reference
id: key-message
kind: rule-key
name: message
summary: The text reported with each finding; metavariables in it are replaced by the code they matched.
related: [key-severity, key-rules]
-->
# `message`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`severity`](severity.md), [`rules`](rules.md)
<!-- END GENERATED: facts -->

Every rule needs a `message`: a rule without one is rejected with `Missing
required field message`. The message is printed under the rule id in the text
output, and is the `message` of each result in the JSON output.

A metavariable in the message, such as `$ALG`, is replaced by the code the
metavariable matched in that finding, so the message can name the function,
the argument or the value involved.

## Examples

### Naming the matched code

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: weak-hash
    pattern: hashlib.$ALG(...)
    message: $ALG is a weak hash for passwords
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
import hashlib

hashlib.md5(password)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  warn  weak-hash
  md5 is a weak hash for passwords

    3 │ hashlib.md5(password)

```
