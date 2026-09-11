<!-- reference
id: flag-exclude-rule
kind: flag
name: --exclude-rule
summary: Skip a rule by its id, whichever config it came from.
commands: [scan, ci]
value: `RULE_ID`, repeatable
related: [flag-config, flag-exclude]
-->
# `--exclude-rule`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md), [`opengrep ci`](../commands/ci.md)
- **Value:** `RULE_ID`, repeatable
- **See also:** [`--config`](config.md), [`--exclude`](exclude.md), [`--enable-nosem`](enable-nosem.md), [`--opengrep-ignore-pattern`](opengrep-ignore-pattern.md), [`--rewrite-rule-ids`](rewrite-rule-ids.md), [`--severity`](severity.md)
<!-- END GENERATED: facts -->

Drops the rule with this id from the run. Repeat the flag to drop several.
This is for a rule inside a config you do not control, such as a registry
pack or a shared directory: the rest of the pack still runs.

The id is the one reported in the findings, so it includes the prefix that
[`--config`](config.md) adds for rules loaded from a directory, as in
`rules.python.find-eval`.

To silence a rule on one line rather than everywhere, use a `nosem` comment.
To keep a rule from looking at certain files, give the rule a
[`paths`](../rule-syntax/paths.md) key.

## Examples

### Running a config without one of its rules

**`rules.yaml`**
```yaml title="rules.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
  - id: find-print
    pattern: print(...)
    message: found print
    languages: [python]
    severity: INFO
```

**`app.py`**
```python title="app.py"
eval(1)
print(2)
```

**Command and result:**
```console
$ opengrep scan --config rules.yaml app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)

  info  find-print
  found print

    2 │ print(2)

$ opengrep scan --config rules.yaml --exclude-rule find-print app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
