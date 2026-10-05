<!-- reference
id: key-rules
kind: rule-key
name: rules
summary: The top-level list of a rule file, one entry per rule.
related: [flag-config, cmd-validate]
-->
# `rules`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`--config`](../flags/config.md), [`opengrep validate`](../commands/validate.md), [`message`](message.md)
<!-- END GENERATED: facts -->

A rule file is a YAML mapping with one key, `rules`, whose value is a list.
Each entry of the list is a rule with its own `id`, `message`, `severity`,
`languages` and patterns, so one file can hold any number of rules, for any
number of languages.

The file is rejected when `rules` is missing (`missing rules entry as
top-level key`), when its value is not a list (`expected a list of rules
following rules:`), or when the mapping has another key next to `rules`.

## Examples

### Two rules in one file

**`rules.yaml`**
```yaml title="rules.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
  - id: find-exec
    pattern: exec(...)
    message: found exec
    languages: [python]
    severity: ERROR
```

**`rules.py`**
```python title="rules.py"
# ruleid: find-eval
eval(expression)
# ruleid: find-exec
exec(source)
```

**Command and result:**
```console
$ opengrep test .
2/2: ✓ All tests passed
No tests for fixes found.
```

### A key next to `rules`

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
version: 2
```

**`app.py`**
```python title="app.py"
eval(expression)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py 2>&1 | grep properties
Unknown or duplicate properties found in YAML object: version
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
```
