<!-- reference
id: opt-timeout
kind: option
name: timeout
summary: This rule's time limit on each file, in seconds; needs --allow-rule-timeout-control.
value: seconds, a number
related: []
-->
# `timeout`

<!-- BEGIN GENERATED: facts -->
- **Value:** seconds, a number
- **See also:** [`--timeout`](../flags/timeout.md), [`dynamic_timeout`](dynamic_timeout.md)
<!-- END GENERATED: facts -->

With [`--allow-rule-timeout-control`](../flags/allow-rule-timeout-control.md),
`timeout` replaces [`--timeout`](../flags/timeout.md) for this rule: the rule
may spend that many seconds on each file, whether that is more or less than
the scan's limit. Without the flag, the option is ignored.

Two cases differ from the flag:

- `--timeout 0` removes every time limit, and the option then has no effect.
- A rule's `timeout: 0`, or a negative value, does not remove the limit: the
  rule times out on every file at once.

With [`dynamic_timeout`](dynamic_timeout.md), the value is the base that the
size of the file multiplies.

## Examples

### A rule's own limit, allowed or not

**`slow.yaml`**
```yaml title="slow.yaml"
rules:
  - id: slow
    patterns:
      - pattern: $F(..., $X, ...) + $G(..., $Y, ...)
      - pattern-not: $F($X, $X, ...) + $G(...)
    message: slow
    languages: [python]
    severity: INFO
    options:
      timeout: 0.001
```

**`patient.yaml`**
```yaml title="patient.yaml"
rules:
  - id: patient
    patterns:
      - pattern: $F(..., $X, ...) + $G(..., $Y, ...)
      - pattern-not: $F($X, $X, ...) + $G(...)
    message: slow
    languages: [python]
    severity: INFO
    options:
      timeout: 100
```

**Command and result:**
```console
$ seq 3000 | sed 's/.*/x& = f(a, b, c) + g(d, e) + h(&)/' > big.py
$ opengrep scan --config slow.yaml big.py 2>&1 | grep -c 'timeout error'
0
$ opengrep scan --config slow.yaml --allow-rule-timeout-control big.py 2>&1 | grep 'timeout error'
[00.00][WARNING]: 1 timeout error(s) in big.py when running the following rules: [slow]
$ opengrep scan --config patient.yaml --allow-rule-timeout-control --timeout 0.001 big.py 2>&1 | grep -c 'timeout error'
0
```

Without the flag, `slow` gets the scan's 5 seconds and finishes. With it, its
own limit of a millisecond applies. `patient` finishes although the scan's
limit is a millisecond, because its own limit is 100 seconds.
