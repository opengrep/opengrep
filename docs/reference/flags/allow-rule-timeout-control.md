<!-- reference
id: flag-allow-rule-timeout-control
kind: flag
name: --allow-rule-timeout-control
summary: Let rules set their own time limits, which opengrep otherwise ignores.
commands: [scan, ci]
default: false
related: [flag-timeout, flag-dynamic-timeout, flag-dynamic-timeout-unit-kb, flag-dynamic-timeout-max-multiplier, opt-dynamic_timeout]
-->
# `--allow-rule-timeout-control`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Default:** `false`
- **See also:** [`--timeout`](timeout.md), [`--dynamic-timeout`](dynamic-timeout.md), [`--dynamic-timeout-unit-kb`](dynamic-timeout-unit-kb.md), [`--dynamic-timeout-max-multiplier`](dynamic-timeout-max-multiplier.md), [`dynamic_timeout`](../rule-options/dynamic_timeout.md)
<!-- END GENERATED: facts -->

A rule may carry its own timeout settings in its `options:` block: `timeout`,
[`dynamic_timeout`](../rule-options/dynamic_timeout.md),
`dynamic_timeout_unit_kb` and `dynamic_timeout_max_multiplier`. Opengrep
ignores all four unless the scan passes this flag.

The point is who decides how long a scan may take. Rules are often written
elsewhere — a registry pack, a shared repository — and a rule that granted
itself a large timeout would spend the scan's time without being asked. The
budget stays with whoever runs the scan, until they hand it over with this
flag.

With the flag, each option replaces the scan's value for that rule alone:
`timeout` instead of [`--timeout`](timeout.md), and the others instead of the
matching [`--dynamic-timeout`](dynamic-timeout.md) flags. One thing it cannot
do is reinstate a limit the scan has removed: with `--timeout 0` nothing is
limited, and the rule's own settings never come into play.

## Examples

### A rule asking for a shorter limit

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
    options:
      timeout: 0.001
```

**Command and result:**
```console
$ seq 20000 | sed 's/.*/x& = f(&)/' > big.py && echo 'eval(1)' >> big.py
$ opengrep scan --config rule.yaml --files-with-matches big.py
big.py
$ opengrep scan --config rule.yaml --allow-rule-timeout-control --files-with-matches big.py
$ opengrep scan --config rule.yaml --allow-rule-timeout-control big.py 2>&1 >/dev/null | grep 'timeout error'
[00.16][WARNING]: 1 timeout error(s) in big.py when running the following rules: [find-eval]
```

Without the flag the rule keeps the scan's five second limit and finishes, so
the finding is reported. With it, the rule holds itself to a thousandth of a
second, runs out of time, and reports nothing.
