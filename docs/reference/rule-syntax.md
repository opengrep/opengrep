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

Opengrep ignores unknown top-level keys of a rule, and logs them at the debug
level. It rejects unknown keys where the syntax is closed, for example under
[`paths`](rule-syntax/paths.md).

<!-- BEGIN GENERATED: index -->
| Name | Summary |
|---|---|
| [`mode: taint`](rule-syntax/taint-mode.md) | Find data that flows from sources to sinks without passing through a sanitizer. |
| [`paths`](rule-syntax/paths.md) | Limit a rule to files whose paths match, or do not match, glob patterns. |
| [`patterns`](rule-syntax/patterns.md) | Match code that satisfies every condition of a list. |

Not yet documented (72): `aliengrep`, `all`, `analyzer`, `any`, `anywhere`, `as`, `at-exit`, `base`, `by-side-effect`, `comparison`, `concat`, `constant-propagation`, `control`, `count`, `dest-language`, `dest-rules`, `django-view`, `entropy`, `equivalences`, `exact`, `extract`, `fix`, `fix-regex`, `focus`, `focus-metavariable`, `from`, `inside`, `kind`, `label`, `language`, `match`, `message`, `metavariable`, `metavariable-analysis`, `metavariable-comparison`, `metavariable-pattern`, `metavariable-regex`, `metavariable-regexp`, `metavariable-type`, `mode`, `module`, `modules`, `not`, `pattern`, `pattern-either`, `pattern-inside`, `pattern-not`, `pattern-not-inside`, `pattern-not-regex`, `pattern-regex`, `pattern-where-python`, `propagators`, `redos`, `reduce`, `regex`, `replace-labels`, `replacement`, `requires`, `rules`, `sanitizers`, `separate`, `severity`, `sinks`, `sources`, `steps`, `strip`, `taint`, `to`, `transform`, `type`, `types`, `where`
<!-- END GENERATED: index -->
