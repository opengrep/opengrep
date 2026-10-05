# Rule syntax

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
| [`fix`](rule-syntax/fix.md) | Replacement code for each match, applied by --autofix. |
| [`fix-regex`](rule-syntax/fix-regex.md) | Fix each match by replacing what a regular expression matches inside it. |
| [`focus-metavariable`](rule-syntax/focus-metavariable.md) | Report only the code a metavariable matched, not the whole match. |
| [`message`](rule-syntax/message.md) | The text reported with each finding; metavariables in it are replaced by the code they matched. |
| [`metavariable-analysis`](rule-syntax/metavariable-analysis.md) | Keep the matches where a metavariable passes an analysis: entropy for secrets, redos for regular expressions. |
| [`metavariable-comparison`](rule-syntax/metavariable-comparison.md) | Keep the matches where an expression over metavariables is true. |
| [`metavariable-pattern`](rule-syntax/metavariable-pattern.md) | Keep the matches where the code a metavariable matched also matches a further pattern. |
| [`metavariable-regex`](rule-syntax/metavariable-regex.md) | Keep the matches where the code a metavariable matched fits a regular expression. |
| [`metavariable-type`](rule-syntax/metavariable-type.md) | Keep the matches where a metavariable's expression has a given type. |
| [`mode`](rule-syntax/mode.md) | How the rule finds code: search, the default, or taint. |
| [`mode: taint`](rule-syntax/taint-mode.md) | Find data that flows from sources to sinks without passing through a sanitizer. |
| [`paths`](rule-syntax/paths.md) | Limit a rule to files whose paths match, or do not match, glob patterns. |
| [`pattern`](rule-syntax/pattern.md) | Match code that looks like the pattern, written in the rule's language. |
| [`pattern-either`](rule-syntax/pattern-either.md) | Match code that matches any one of a list of patterns. |
| [`pattern-inside`](rule-syntax/pattern-inside.md) | Keep only the matches that lie inside code matching this pattern. |
| [`pattern-not`](rule-syntax/pattern-not.md) | Drop the matches that also match this pattern. |
| [`pattern-not-inside`](rule-syntax/pattern-not-inside.md) | Drop the matches that lie inside code matching this pattern. |
| [`pattern-not-regex`](rule-syntax/pattern-not-regex.md) | Drop the matches that contain a match of this regular expression. |
| [`pattern-regex`](rule-syntax/pattern-regex.md) | Match the text of the file with a PCRE regular expression. |
| [`patterns`](rule-syntax/patterns.md) | Match code that satisfies every condition of a list. |
| [`rules`](rule-syntax/rules.md) | The top-level list of a rule file, one entry per rule. |
| [`severity`](rule-syntax/severity.md) | How serious a finding is: ERROR, WARNING or INFO, or CRITICAL, HIGH, MEDIUM or LOW. |

Not yet documented (29): `aliengrep`, `at-exit`, `base`, `by-side-effect`, `concat`, `control`, `dest-language`, `dest-rules`, `django-view`, `equivalences`, `exact`, `extract`, `from`, `kind`, `label`, `language`, `metavariable`, `metavariable-regexp`, `module`, `modules`, `pattern-where-python`, `reduce`, `regex`, `replace-labels`, `requires`, `separate`, `steps`, `to`, `transform`
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
