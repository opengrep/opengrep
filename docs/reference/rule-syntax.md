# Rule syntax

<!-- BEGIN GENERATED: stamp -->
> Reference for **opengrep 1.30.0** (commit `d094c70bb`).
<!-- END GENERATED: stamp -->

A rule file is a YAML document with a top-level `rules:` list. Each rule is a
mapping with at least `id`, `message`, `severity`, `languages`, and a way to
match code: a pattern key such as `pattern` or `patterns` in the default search
mode, or sources and sinks in [taint mode](rule-syntax/taint-mode.md).

```yaml
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

Opengrep skips unknown top-level keys of a rule and carries on, warning as it
goes: `Skipping unknown field 'nosuchkey' in rule "find-eval"`. It rejects
unknown keys where the syntax is closed, for example under
[`paths`](rule-syntax/paths.md) or in a rule's
[`options`](rule-options.md).

<!-- BEGIN GENERATED: index -->
| Name | Summary |
|---|---|
| [`mode: taint`](rule-syntax/taint-mode.md) | Find data that flows from sources to sinks without passing through a sanitizer. |
| [`paths`](rule-syntax/paths.md) | Limit a rule to files whose paths match, or do not match, glob patterns. |
| [`patterns`](rule-syntax/patterns.md) | Match code that satisfies every condition of a list. |

Not yet documented (58): `aliengrep`, `analyzer`, `at-exit`, `base`, `by-side-effect`, `comparison`, `concat`, `constant-propagation`, `control`, `count`, `dest-language`, `dest-rules`, `django-view`, `entropy`, `equivalences`, `exact`, `extract`, `fix`, `fix-regex`, `focus-metavariable`, `from`, `kind`, `label`, `language`, `message`, `metavariable`, `metavariable-analysis`, `metavariable-comparison`, `metavariable-pattern`, `metavariable-regex`, `metavariable-regexp`, `metavariable-type`, `mode`, `module`, `modules`, `pattern`, `pattern-either`, `pattern-inside`, `pattern-not`, `pattern-not-inside`, `pattern-not-regex`, `pattern-regex`, `pattern-where-python`, `redos`, `reduce`, `regex`, `replace-labels`, `replacement`, `requires`, `rules`, `separate`, `severity`, `steps`, `strip`, `to`, `transform`, `type`, `types`
<!-- END GENERATED: index -->

## Examples

### An unknown key only warns

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
    nosuchkey: whatever
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep scan --config rule.yaml app.py 2>&1 | grep Skipping
[00.04][WARNING]: Skipping unknown field 'nosuchkey' in rule "find-eval"
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 0
```
