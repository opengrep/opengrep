<!-- reference
id: key-severity
kind: rule-key
name: severity
summary: How serious a finding is: ERROR, WARNING or INFO, or CRITICAL, HIGH, MEDIUM or LOW.
related: [flag-severity, key-message]
-->
# `severity`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`--severity`](../flags/severity.md), [`message`](message.md)
<!-- END GENERATED: facts -->

Every rule needs a `severity`: a rule without one is rejected with `Missing
required field severity`. The value is one of `ERROR`, `WARNING` and `INFO`,
or one of the four levels `CRITICAL`, `HIGH`, `MEDIUM` and `LOW`, written in
capitals. Any other spelling, `error` or `Warning` included, makes the rule
invalid.

The text output labels each finding with its severity: `ERROR` and `HIGH`
show as `error`, `WARNING` and `MEDIUM` as `warn`, `INFO` and `LOW` as `info`,
and `CRITICAL` as `critical`. [`--severity`](../flags/severity.md) reports
only the findings of the severities it names.

## Examples

### Each severity in the text output

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: sev-critical
    pattern: critical()
    message: CRITICAL
    languages: [python]
    severity: CRITICAL
  - id: sev-error
    pattern: error()
    message: ERROR
    languages: [python]
    severity: ERROR
  - id: sev-high
    pattern: high()
    message: HIGH
    languages: [python]
    severity: HIGH
  - id: sev-warning
    pattern: warning()
    message: WARNING
    languages: [python]
    severity: WARNING
  - id: sev-medium
    pattern: medium()
    message: MEDIUM
    languages: [python]
    severity: MEDIUM
  - id: sev-info
    pattern: info()
    message: INFO
    languages: [python]
    severity: INFO
  - id: sev-low
    pattern: low()
    message: LOW
    languages: [python]
    severity: LOW
```

**`app.py`**
```python title="app.py"
critical()
error()
high()
warning()
medium()
info()
low()
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py
app.py

  critical  sev-critical
  CRITICAL

    1 │ critical()

  error  sev-error
  ERROR

    2 │ error()

  error  sev-high
  HIGH

    3 │ high()

  warn  sev-warning
  WARNING

    4 │ warning()

  warn  sev-medium
  MEDIUM

    5 │ medium()

  info  sev-info
  INFO

    6 │ info()

  info  sev-low
  LOW

    7 │ low()

```

### A severity in lower case

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: error
```

**`app.py`**
```python title="app.py"
eval(expression)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py 2>&1 | grep 'Bad severity'
Bad severity: error (expected ERROR, WARNING or INFO)
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
```
