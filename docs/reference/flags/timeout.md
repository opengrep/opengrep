<!-- reference
id: flag-timeout
kind: flag
name: --timeout
summary: Maximum time in seconds for one rule on one file; 0 means no limit.
commands: [scan, ci, test]
value: `SECONDS`, a number
default: 5.0
env: [env-OPENGREP_TIMEOUT]
related: [flag-timeout-threshold, flag-interfile-timeout, flag-dynamic-timeout, flag-allow-rule-timeout-control, opt-timeout, opt-dynamic_timeout]
-->
# `--timeout`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), `ci`, [`opengrep test`](../commands/test.md)
- **Value:** `SECONDS`, a number
- **Default:** `5.0`
- **Environment:** [`OPENGREP_TIMEOUT`](../env/OPENGREP_TIMEOUT.md)
- **See also:** `--timeout-threshold`, `--interfile-timeout`, `--dynamic-timeout`, `--allow-rule-timeout-control`, `timeout`, [`dynamic_timeout`](../rule-options/dynamic_timeout.md)
<!-- END GENERATED: facts -->

Limits the time each rule may spend on each file. The limit applies to every
rule and file pair separately. `0` removes the limit.

When a rule runs out of time on a file, opengrep stops that rule on that file.
It drops the rule's findings for the file, records a timeout error and
continues with the other rules. The error appears in the `errors` of `--json`
output, and a warning names the file and the rules. Once
[`--timeout-threshold`](../flags.md) rules have timed out on a file, opengrep
skips the rest of the rules on that file.

Other limits:

- `--interfile-timeout` limits the interfile analysis of a rule (see
  [`taint_interfile`](../rule-options/taint_interfile.md)).
- With `--dynamic-timeout`, the limit grows with the size of the file.
- With `--allow-rule-timeout-control`, a rule can set its own limit with its
  `timeout` and [`dynamic_timeout`](../rule-options/dynamic_timeout.md)
  options. A rule's options have no effect when `--timeout` is `0`.

[`OPENGREP_TIMEOUT`](../env/OPENGREP_TIMEOUT.md) sets the value for `scan` and
`ci` when the flag is not given. `opengrep test` does not read it.

## Examples

### A rule that runs out of time

```yaml title="slow.yaml"
rules:
  - id: slow
    patterns:
      - pattern: $F(..., $X, ...) + $G(..., $Y, ...)
      - pattern-not: $F($X, $X, ...) + $G(...)
    message: slow
    languages: [python]
    severity: INFO
```

```console
$ seq 20000 | sed 's/.*/x& = f(a, b, c) + g(d, e) + h(&)/' > big.py
$ opengrep scan --config slow.yaml --timeout 0.001 big.py 2>&1 | grep timeout
[00.17][WARNING]: 1 timeout error(s) in big.py when running the following rules: [slow]
```
