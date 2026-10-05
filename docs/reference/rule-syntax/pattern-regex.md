<!-- reference
id: key-pattern-regex
kind: rule-key
name: pattern-regex
summary: Match the text of the file with a PCRE regular expression.
related: [key-pattern-not-regex, key-pattern, key-metavariable-regex]
-->
# `pattern-regex`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`pattern-not-regex`](pattern-not-regex.md), [`pattern`](pattern.md), [`metavariable-regex`](metavariable-regex.md), [`patterns`](patterns.md)
<!-- END GENERATED: facts -->

`pattern-regex` matches a PCRE regular expression against the text of the
file, not its syntax, so it finds things a code pattern cannot: a token
format, a word in a comment, text in a file of any language.

A named group, `(?P<NAME>...)`, binds the metavariable `$NAME` to the text it
matched, for use in the message or in the other items of
[`patterns`](patterns.md).

`pattern-regex` can stand at the top of a rule, as an item of `patterns`, or
as an alternative in [`pattern-either`](pattern-either.md). It is also what a
rule for `regex`, which has no syntax to match, is written with.

## Examples

### An AWS access key in the source

**`aws-key.yaml`**
```yaml title="aws-key.yaml"
rules:
  - id: aws-key
    pattern-regex: AKIA[0-9A-Z]{16}
    message: an AWS access key id in the source
    languages: [python]
    severity: ERROR
```

**`aws-key.py`**
```python title="aws-key.py"
import os

# ruleid: aws-key
ACCESS_KEY = "AKIAIOSFODNN7EXAMPLE"
# ok: aws-key
ACCESS_KEY = os.environ["AWS_ACCESS_KEY_ID"]
```

**Command and result:**
```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### A named group in the message

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: aws-key
    pattern-regex: (?P<KEY>AKIA[0-9A-Z]{16})
    message: AWS access key id $KEY in the source
    languages: [python]
    severity: ERROR
```

**`app.py`**
```python title="app.py"
ACCESS_KEY = "AKIAIOSFODNN7EXAMPLE"
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  error  aws-key
  AWS access key id AKIAIOSFODNN7EXAMPLE in the source

    1 │ ACCESS_KEY = "AKIAIOSFODNN7EXAMPLE"

```
