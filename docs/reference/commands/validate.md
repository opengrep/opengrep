<!-- reference
id: cmd-validate
kind: command
name: opengrep validate
summary: Check that rule files are valid, without scanning any code.
related: [cmd-test, cmd-scan, flag-config]
-->
# `opengrep validate`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep test`](test.md), [`opengrep scan`](scan.md), [`--config`](../flags/config.md), [`--skip-invalid-configs`](../flags/skip-invalid-configs.md), [`--strict`](../flags/strict.md), [`--validate`](../flags/validate.md)
<!-- END GENERATED: facts -->

```
opengrep validate [FLAGS] PATHS...
```

Reads the rules under PATHS and reports whether they are valid. PATHS are rule
files or directories holding them, given as positional arguments: unlike
[`scan`](scan.md) and [`test`](test.md), `validate` has no `--config`. Without
any path it stops with `Nothing to validate, pass a directory or rule file`
and exit status 2.

A rule is checked in three ways:

- the file must be YAML;
- each rule must fit the rule schema, which is the same parsing a scan does,
  so patterns are parsed as well;
- the rule files are then scanned with opengrep's own rules about rules, the
  metachecks, which look for mistakes that are valid syntax. A metacheck match
  is an error.

Metachecks run on rule *files*, so they only reach rules that came from a
local file or directory. Rules from the registry, a URL or a git repository
are parsed but not metachecked, and `validate` warns when that leaves nothing
to metacheck.

Everything `validate` prints, the verdict included, goes to standard error.

## Exit status

| Status | Meaning |
|---|---|
| 0 | Every rule is valid. |
| 2 | Fatal error, such as no path given. |
| 3 | A rule file could not be parsed as a target of the metachecks. |
| 4 | A rule does not fit the schema, or a pattern could not be parsed. |
| 5 | A rule file is not valid YAML. |
| 7 | No rules could be loaded at all. |
| 8 | A rule names an unsupported language. |
| 141 | The reader of the output closed the pipe. |

## Flags

<!-- BEGIN GENERATED: flags -->
| Flag | Summary |
|---|---|
| [`--debug`](../flags/debug.md) | Log everything --verbose does and the engine's own diagnostics as well. |
| `--develop` | *not yet documented* |
| [`--experimental`](../flags/experimental.md) | Accepted for compatibility; opengrep has only the one implementation. |
| [`--force-color`](../flags/force-color.md) | Style the output even when it is not going to a terminal. |
| `--profile` | *not yet documented* |
| [`--quiet`](../flags/quiet.md) | Print the findings and nothing else. |
| [`--verbose`](../flags/verbose.md) | Log what the scan is doing, at the info level. |
<!-- END GENERATED: flags -->

## Examples

### A valid rule file

**`good.yaml`**
```yaml title="good.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**Command and result:**
```console
$ opengrep validate good.yaml 2>&1 >/dev/null
Configuration is valid - found 0 fatal errors, 0 skippable error(s), and 1 rule(s).
$ opengrep validate good.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
```

### A rule that does not fit the schema

**`bad.yaml`**
```yaml title="bad.yaml"
rules:
  - id: broken
    patterns:
      - pattern-not: eval("...")
    message: no positive term
    languages: [python]
    severity: WARNING
```

**Command and result:**
```console
$ opengrep validate bad.yaml 2>&1 >/dev/null
[00.04][WARNING]: bad.yaml:3:5: Invalid rule schema in rule broken
  --> bad.yaml:3
2 |   - id: broken
3 |     patterns:
  |     ^^^^^^^^
4 |       - pattern-not: eval("...")
you need at least one positive term (not just negations or conditions)
Configuration is invalid - found 0 fatal errors, 1 skippable error(s), and 0 rule(s).
[00.12][ERROR]: Please fix the above errors and try again.
$ opengrep validate bad.yaml > /dev/null 2>&1; echo "exit status: $?"
exit status: 4
```
