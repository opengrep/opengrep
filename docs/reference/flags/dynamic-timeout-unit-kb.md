<!-- reference
id: flag-dynamic-timeout-unit-kb
kind: flag
name: --dynamic-timeout-unit-kb
summary: The file size, in KB, that counts as one step when scaling the timeout.
commands: [scan, ci]
value: `INT`
default: 30
related: [flag-dynamic-timeout, flag-dynamic-timeout-max-multiplier, flag-timeout]
-->
# `--dynamic-timeout-unit-kb`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `INT`
- **Default:** `30`
- **See also:** [`--dynamic-timeout`](dynamic-timeout.md), [`--dynamic-timeout-max-multiplier`](dynamic-timeout-max-multiplier.md), [`--timeout`](timeout.md), [`--allow-rule-timeout-control`](allow-rule-timeout-control.md), [`dynamic_timeout_unit_kb`](../rule-options/dynamic_timeout_unit_kb.md)
<!-- END GENERATED: facts -->

Sets the unit in the scaling that [`--dynamic-timeout`](dynamic-timeout.md)
does: the multiplier applied to [`--timeout`](timeout.md) is the file's size
in KB divided by this number, never below 1 and never above
[`--dynamic-timeout-max-multiplier`](dynamic-timeout-max-multiplier.md).

The default is 30, so a 30 KB file gets one timeout's worth of time, a 120 KB
file four, and so on. Lower it to hand out extra time sooner, raise it to keep
the limit closer to the flat `--timeout` for all but the largest files.

It does nothing unless `--dynamic-timeout` is on, or a rule asks for dynamic
timeouts and the scan allows it with
[`--allow-rule-timeout-control`](allow-rule-timeout-control.md).

## Examples

### A smaller unit, more generous limits

<!-- not run: whether a rule reaches its limit depends on the machine -->
**Command:**
```console no-check
$ opengrep scan --config rules/ --dynamic-timeout --dynamic-timeout-unit-kb 10 .
```
