<!-- reference
id: flag-validate
kind: flag
name: --validate
summary: Check the rules and scan nothing; the older spelling of the validate command.
commands: [scan]
related: [cmd-validate, flag-test, flag-config]
-->
# `--validate`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`opengrep validate`](../commands/validate.md), [`--test`](test.md), [`--config`](config.md)
<!-- END GENERATED: facts -->

Checks the rules given with [`--config`](config.md) and performs no search,
printing the same verdict as [`opengrep validate`](../commands/validate.md).
It exists because older versions had no `validate` command, and scripts were
written against the flag.

Prefer the command: it takes its rule files as positional arguments, which
reads better, and it is where the behaviour is documented.

## Examples

### Checking a rule file

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**Command and result:**
```console
$ opengrep scan --validate --config rule.yaml 2>&1 >/dev/null
Configuration is valid - found 0 fatal errors, 0 skippable error(s), and 1 rule(s).
$ opengrep validate rule.yaml 2>&1 >/dev/null
Configuration is valid - found 0 fatal errors, 0 skippable error(s), and 1 rule(s).
```
