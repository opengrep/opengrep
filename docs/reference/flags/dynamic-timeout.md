<!-- reference
id: flag-dynamic-timeout
kind: flag
name: --dynamic-timeout
summary: Scale each rule's time limit with the size of the file being scanned.
commands: [scan, ci]
default: false
related: [flag-timeout, flag-dynamic-timeout-unit-kb, flag-dynamic-timeout-max-multiplier, flag-allow-rule-timeout-control, opt-dynamic_timeout]
-->
# `--dynamic-timeout`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Default:** `false`
- **See also:** [`--timeout`](timeout.md), [`--dynamic-timeout-unit-kb`](dynamic-timeout-unit-kb.md), [`--dynamic-timeout-max-multiplier`](dynamic-timeout-max-multiplier.md), [`--allow-rule-timeout-control`](allow-rule-timeout-control.md), [`dynamic_timeout`](../rule-options/dynamic_timeout.md)
<!-- END GENERATED: facts -->

[`--timeout`](timeout.md) gives every rule the same number of seconds on every
file, which is either generous for small files or tight for large ones. With
`--dynamic-timeout` the limit grows with the file:

```
limit = timeout × clamp(file size in KB / unit, 1, max multiplier)
```

The unit is [`--dynamic-timeout-unit-kb`](dynamic-timeout-unit-kb.md), 30 by
default, and the ceiling is
[`--dynamic-timeout-max-multiplier`](dynamic-timeout-max-multiplier.md), 20 by
default. So with the default 5 second timeout, a file under 30 KB still gets 5
seconds, a 150 KB file gets 25, and anything from 600 KB up gets 100, the
ceiling.

A `--timeout` of 0 means no limit at all, and this flag then has nothing to
scale.

A rule can ask for the same thing with the
[`dynamic_timeout`](../rule-options/dynamic_timeout.md) option, which opengrep
honours only when the scan also passes
[`--allow-rule-timeout-control`](allow-rule-timeout-control.md).

## Examples

### Giving large files more time

<!-- not run: whether a rule reaches its limit depends on the machine -->
**Command:**
```console no-check
$ opengrep scan --config rules/ --timeout 5 --dynamic-timeout .
```
