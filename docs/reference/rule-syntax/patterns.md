<!-- reference
id: key-patterns
kind: rule-key
name: patterns
summary: Match code that satisfies every condition of a list.
related: [key-pattern, key-pattern-either, key-pattern-inside, key-pattern-not, key-pattern-not-inside, key-pattern-regex, key-focus-metavariable, key-metavariable-regex]
-->
# `patterns`

<!-- BEGIN GENERATED: facts -->
- **See also:** `pattern`, `pattern-either`, `pattern-inside`, `pattern-not`, `pattern-not-inside`, `pattern-regex`, `focus-metavariable`, `metavariable-regex`
<!-- END GENERATED: facts -->

`patterns` takes a list of conditions and matches the code that satisfies all
of them. The items can be:

- positive patterns: `pattern`, `pattern-regex`, `pattern-either`, or a nested
  `patterns`;
- negative patterns: `pattern-not`, `pattern-not-regex`;
- context: `pattern-inside` and `pattern-not-inside`;
- filters on metavariables: `metavariable-regex`, `metavariable-pattern`,
  `metavariable-comparison`, `metavariable-type`, `metavariable-analysis`, and
  `focus-metavariable`.

Code matches when it:

- matches every positive pattern;
- lies inside code matched by each `pattern-inside` and outside code matched by
  any `pattern-not-inside`;
- matches no negative pattern;
- passes every filter.

The order of the items does not matter. A metavariable bound in one item must
stand for the same code in the others.

A `patterns` list made only of negative patterns and filters is an invalid
rule.

## Examples

### Inside a function, and not a string literal

```yaml title="eval-in-function.yaml"
rules:
  - id: eval-in-function
    patterns:
      - pattern-inside: |
          def $F(...):
              ...
      - pattern: eval($X)
      - pattern-not: eval("...")
    message: eval of a non-literal inside a function
    languages: [python]
    severity: WARNING
```

```python title="eval-in-function.py"
# ok: eval-in-function
eval(user_input)

def handler(expr):
    # ruleid: eval-in-function
    eval(expr)
    # ok: eval-in-function
    eval("1 + 1")
```

```console
$ opengrep test .
1/1: ✓ All tests passed
No tests for fixes found.
```

### Only negative patterns

```yaml title="rule.yaml"
rules:
  - id: only-negative
    patterns:
      - pattern-not: eval("...")
    message: a rule that cannot work
    languages: [python]
    severity: WARNING
```

```python title="app.py"
eval(x)
```

```console
$ opengrep scan --config rule.yaml app.py 2>&1 | grep positive
you need at least one positive term (not just negations or conditions)
$ opengrep scan --config rule.yaml app.py > /dev/null 2>&1; echo "exit status: $?"
exit status: 7
```
