<!-- reference
id: opt-dynamic_timeout
kind: option
name: dynamic_timeout
summary: Scale this rule's time limit with the size of each file; needs --allow-rule-timeout-control.
value: `true` or `false`
default: unset, the scan's --dynamic-timeout applies
related: [flag-allow-rule-timeout-control, flag-dynamic-timeout, flag-timeout, opt-timeout, opt-dynamic_timeout_unit_kb, opt-dynamic_timeout_max_multiplier]
-->
# `dynamic_timeout`

<!-- BEGIN GENERATED: facts -->
- **Value:** `true` or `false`
- **Default:** `unset, the scan's --dynamic-timeout applies`
- **See also:** `--allow-rule-timeout-control`, `--dynamic-timeout`, [`--timeout`](../flags/timeout.md), `timeout`, `dynamic_timeout_unit_kb`, `dynamic_timeout_max_multiplier`
<!-- END GENERATED: facts -->

Opengrep ignores this option unless the scan runs with
`--allow-rule-timeout-control`. The same holds for the other timeout options
of a rule: `timeout`, `dynamic_timeout_unit_kb` and
`dynamic_timeout_max_multiplier`. When allowed, `dynamic_timeout: true` makes
the rule's time limit on a file grow with the size of the file:

```
limit = base × clamp(size in KB / unit, 1, max multiplier)
```

| Term | Rule option | Otherwise |
|---|---|---|
| base | `timeout` | [`--timeout`](../flags/timeout.md), default 5 s |
| unit | `dynamic_timeout_unit_kb` | `--dynamic-timeout-unit-kb`, default 30 |
| max multiplier | `dynamic_timeout_max_multiplier` | `--dynamic-timeout-max-multiplier`, default 20 |

For example, with the defaults, a 150 KB file gets 5 × 5 = 25 seconds, and a
10 KB file gets 5 seconds.

`dynamic_timeout: false` turns scaling off for the rule even when the scan
uses `--dynamic-timeout`. Without the option, `--dynamic-timeout` decides.
When `--timeout` is `0`, nothing has a time limit and these options have no
effect.

## Examples

### A rule allowed more time on large files

```yaml title="rule.yaml"
rules:
  - id: expensive-rule
    pattern: $X == $X
    message: comparison with itself
    languages: [python]
    severity: WARNING
    options:
      timeout: 10
      dynamic_timeout: true
      dynamic_timeout_max_multiplier: 6
```

With these options, the rule gets 10 seconds on files up to 30 KB, and up to
60 seconds on files of 180 KB or more. Without
`--allow-rule-timeout-control`, it gets the scan's 5 seconds.

```console no-check
$ opengrep scan --config rule.yaml --allow-rule-timeout-control .
```
