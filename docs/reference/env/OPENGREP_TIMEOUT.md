<!-- reference
id: env-OPENGREP_TIMEOUT
kind: env
name: OPENGREP_TIMEOUT
aliases: [SEMGREP_TIMEOUT]
summary: The value of --timeout for scan and ci when the flag is not given.
value: a number of seconds
related: []
-->
# `OPENGREP_TIMEOUT`

<!-- BEGIN GENERATED: facts -->
- **Also spelled:** `SEMGREP_TIMEOUT`
- **Value:** a number of seconds
- **Equivalent to:** [`--timeout`](../flags/timeout.md)
<!-- END GENERATED: facts -->

When `--timeout` is not given, `opengrep scan` and `opengrep ci` take its value
from this variable. See [`--timeout`](../flags/timeout.md) for its meaning.

A value that is not a number is an error, and opengrep exits with status 2.

## Examples

### The flag wins

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(code)
```

**Command and result:**
```console
$ OPENGREP_TIMEOUT=10 opengrep scan --config rule.yaml --timeout 20 app.py 2>&1 | grep WARNING
[00.04][WARNING]: --timeout is given; ignoring $OPENGREP_TIMEOUT
```

### A value that is not a number

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**Command and result:**
```console
$ OPENGREP_TIMEOUT=soon opengrep scan --config rule.yaml . 2> errors.txt; echo "exit status: $?"
exit status: 2
$ tail -2 errors.txt
opengrep scan: environment variable OPENGREP_TIMEOUT: invalid value "soon",
               expected a number
```
