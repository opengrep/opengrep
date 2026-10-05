<!-- reference
id: key-pattern-not-regex
kind: rule-key
name: pattern-not-regex
summary: Drop the matches that contain a match of this regular expression.
related: [key-pattern-regex, key-pattern-not]
-->
# `pattern-not-regex`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`pattern-regex`](pattern-regex.md), [`pattern-not`](pattern-not.md)
<!-- END GENERATED: facts -->

`pattern-not-regex` is an item of [`patterns`](patterns.md). It runs a PCRE
regular expression over the text of the file and drops each match of the other
items that contains a match of the regex. A regex match counts only when it
lies entirely within the finding: one that starts or ends outside it, such as
a comment later on the same line, drops nothing.

It is a negative item, like [`pattern-not`](pattern-not.md): `patterns` needs a
positive item next to it.

## Examples

### Placeholder passwords

**`hardcoded-password.yaml`**
```yaml title="hardcoded-password.yaml"
rules:
  - id: hardcoded-password
    patterns:
      - pattern: $NAME = "..."
      - metavariable-regex:
          metavariable: $NAME
          regex: (?i).*password
      - pattern-not-regex: (?i)example|changeme
    message: hard-coded password in $NAME
    languages: [python]
    severity: ERROR
```

**`hardcoded-password.py`**
```python title="hardcoded-password.py"
# ruleid: hardcoded-password
db_password = "hunter2"
# ok: hardcoded-password
db_password = "changeme"
# ruleid: hardcoded-password
db_password = "hunter2"  # not an example
# ok: hardcoded-password
db_user = "admin"
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

The third assignment is reported: the word `example` is in the comment, outside
the match.
