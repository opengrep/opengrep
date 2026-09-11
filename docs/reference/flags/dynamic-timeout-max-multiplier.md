<!-- reference
id: flag-dynamic-timeout-max-multiplier
kind: flag
name: --dynamic-timeout-max-multiplier
summary: The ceiling on how much a file's size may stretch the timeout.
commands: [scan, ci]
value: `INT`
default: 20
related: [flag-dynamic-timeout, flag-dynamic-timeout-unit-kb, flag-timeout]
-->
# `--dynamic-timeout-max-multiplier`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`
- **Default:** `20`
- **See also:** [`--dynamic-timeout`](dynamic-timeout.md), [`--dynamic-timeout-unit-kb`](dynamic-timeout-unit-kb.md), [`--timeout`](timeout.md), [`--allow-rule-timeout-control`](allow-rule-timeout-control.md), [`dynamic_timeout_max_multiplier`](../rule-options/dynamic_timeout_max_multiplier.md)
<!-- END GENERATED: facts -->

Caps the multiplier that [`--dynamic-timeout`](dynamic-timeout.md) works out
from a file's size, so that one enormous file cannot take an unbounded share
of a scan. With the default of 20 and a 5 second
[`--timeout`](timeout.md), no rule gets more than 100 seconds on a file,
however large it is.

The multiplier reaches this ceiling at
`max multiplier × ` [`--dynamic-timeout-unit-kb`](dynamic-timeout-unit-kb.md),
which with both defaults is 600 KB.

It does nothing unless `--dynamic-timeout` is on, or a rule asks for dynamic
timeouts and the scan allows it with
[`--allow-rule-timeout-control`](allow-rule-timeout-control.md).

## Examples

### Holding the ceiling down

<!-- not run: whether a rule reaches its limit depends on the machine -->
**Command:**
```console no-check
$ opengrep scan --config rules/ --dynamic-timeout --dynamic-timeout-max-multiplier 4 .
```
