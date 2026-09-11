<!-- reference
id: opt-dynamic_timeout_unit_kb
kind: option
name: dynamic_timeout_unit_kb
summary: For this rule, how many KB of file earn one more time limit; needs --allow-rule-timeout-control.
value: an integer, in KB
related: [flag-dynamic-timeout-unit-kb]
-->
# `dynamic_timeout_unit_kb`

<!-- BEGIN GENERATED: facts -->
- **Value:** an integer, in KB
- **See also:** [`--dynamic-timeout-unit-kb`](../flags/dynamic-timeout-unit-kb.md), [`dynamic_timeout`](dynamic_timeout.md)
<!-- END GENERATED: facts -->

When [`dynamic_timeout`](dynamic_timeout.md) scales this rule's time limit
with the size of a file, the limit is multiplied by the size of the file in KB
(1024 bytes) divided by this unit, and kept between 1 and
[`dynamic_timeout_max_multiplier`](dynamic_timeout_max_multiplier.md). A
smaller unit gives large files more time.

The option needs [`--allow-rule-timeout-control`](../flags/allow-rule-timeout-control.md),
like the other timeout options of a rule. A rule that does not set it uses
[`--dynamic-timeout-unit-kb`](../flags/dynamic-timeout-unit-kb.md), whose
default is 30.

## Examples

### The limit a 3 KB file gets

Opengrep logs the limit it sets for each file, as an info message from the
`semgrep.engine` log source, which
[`OPENGREP_LOG_SRCS`](../env/OPENGREP_LOG_SRCS.md) turns on.

**`unit1.yaml`**
```yaml title="unit1.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
    options:
      timeout: 2
      dynamic_timeout: true
      dynamic_timeout_unit_kb: 1
```

**`unit2.yaml`**
```yaml title="unit2.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
    options:
      timeout: 2
      dynamic_timeout: true
      dynamic_timeout_unit_kb: 2
```

**Command and result:**
```console
$ python3 -c "open('three.py','w').write('eval(1)\n' + '#' * 3070 + '\n')"
$ OPENGREP_LOG_SRCS=semgrep.engine opengrep scan --config unit1.yaml --allow-rule-timeout-control --verbose three.py 2>&1 | grep -o 'for .* derived from the file size'
for three.py to 6.01s using a factor of 3.01 derived from the file size
$ OPENGREP_LOG_SRCS=semgrep.engine opengrep scan --config unit2.yaml --allow-rule-timeout-control --verbose three.py 2>&1 | grep -o 'for .* derived from the file size'
for three.py to 3.01s using a factor of 1.50 derived from the file size
```

The file is 3079 bytes, about 3 KB: three units of 1 KB, or one and a half
units of 2 KB, times the rule's base of 2 seconds.
