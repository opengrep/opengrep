<!-- reference
id: flag-timeout-threshold
kind: flag
name: --timeout-threshold
summary: How many rules may time out on a file before opengrep gives up on that file.
commands: [scan, ci, test]
value: `INT`
related: [flag-timeout, flag-max-memory, flag-strict]
-->
# `--timeout-threshold`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md), [`opengrep test`](../commands/test.md)
- **Value:** `INT`
- **See also:** [`--timeout`](timeout.md), [`--max-memory`](max-memory.md), [`--strict`](strict.md)
<!-- END GENERATED: facts -->

When a rule runs out of its [`--timeout`](timeout.md) on a file, opengrep
drops that rule's findings for the file and carries on with the other rules.
A file that does this to rule after rule is usually pathological — generated,
minified, enormous — and going on costs time for nothing.

This is how many rules may time out on one file before opengrep stops running
rules on it altogether. It says so on standard error, and the file counts as
partially analysed. `0` removes the limit, so every rule is tried however many
have already timed out.

## Examples

### Giving up on a file after one timeout

**`slow.yaml`**
```yaml title="slow.yaml"
rules:
  - id: slow-a
    patterns:
      - pattern: $F(..., $X, ...) + $G(..., $Y, ...)
      - pattern-not: $F($X, $X, ...) + $G(...)
    message: slow a
    languages: [python]
    severity: INFO
```

**Command and result:**
```console
$ seq 20000 | sed 's/.*/x& = f(a, b, c) + g(d, e) + h(&)/' > big.py
$ opengrep scan --config slow.yaml --timeout 0.001 --timeout-threshold 1 big.py 2>&1 >/dev/null | grep 'stopped running'
Opengrep stopped running rules on big.py after 1 timeout error(s). See `--timeout-threshold` for more info.
```
