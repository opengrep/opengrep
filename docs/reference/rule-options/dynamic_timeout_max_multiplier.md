<!-- reference
id: opt-dynamic_timeout_max_multiplier
kind: option
name: dynamic_timeout_max_multiplier
summary: For this rule, the most that file size may multiply its time limit; needs --allow-rule-timeout-control.
value: an integer
related: [flag-dynamic-timeout-max-multiplier]
-->
# `dynamic_timeout_max_multiplier`

<!-- BEGIN GENERATED: facts -->
- **Value:** an integer
- **See also:** [`--dynamic-timeout-max-multiplier`](../flags/dynamic-timeout-max-multiplier.md), [`dynamic_timeout`](dynamic_timeout.md)
<!-- END GENERATED: facts -->

When [`dynamic_timeout`](dynamic_timeout.md) scales this rule's time limit
with the size of a file, the factor is the size divided by
[`dynamic_timeout_unit_kb`](dynamic_timeout_unit_kb.md), but never more than
this value. It keeps very large files from getting unbounded time.

The option needs [`--allow-rule-timeout-control`](../flags/allow-rule-timeout-control.md),
like the other timeout options of a rule. A rule that does not set it uses
[`--dynamic-timeout-max-multiplier`](../flags/dynamic-timeout-max-multiplier.md),
whose default is 20.

## Examples

### A 10 KB file with and without a cap

Opengrep logs the limit it sets for each file, as an info message from the
`semgrep.engine` log source, which
[`OPENGREP_LOG_SRCS`](../env/OPENGREP_LOG_SRCS.md) turns on.

**`uncapped.yaml`**
```yaml title="uncapped.yaml"
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

**`capped.yaml`**
```yaml title="capped.yaml"
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
      dynamic_timeout_max_multiplier: 4
```

**Command and result:**
```console
$ python3 -c "open('ten.py','w').write('eval(1)\n' + '#' * 10240 + '\n')"
$ OPENGREP_LOG_SRCS=semgrep.engine opengrep scan --config uncapped.yaml --allow-rule-timeout-control --verbose ten.py 2>&1 | grep -o 'for .* derived from the file size'
for ten.py to 20.02s using a factor of 10.01 derived from the file size
$ OPENGREP_LOG_SRCS=semgrep.engine opengrep scan --config capped.yaml --allow-rule-timeout-control --verbose ten.py 2>&1 | grep -o 'for .* derived from the file size'
for ten.py to 8.00s using a factor of 4.00 derived from the file size
```

The file is about 10 KB, ten units of 1 KB. The first rule's factor of 10 is
below the default cap of 20; the second rule caps it at 4.
