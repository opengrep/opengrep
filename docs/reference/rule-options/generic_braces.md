<!-- reference
id: opt-generic_braces
kind: option
name: generic_braces
summary: With aliengrep, replace the pairs of braces that an ellipsis keeps balanced.
value: a list of pairs of one-character strings
default: parentheses, square brackets and curly braces
related: [opt-generic_extra_braces]
-->
# `generic_braces`

<!-- BEGIN GENERATED: facts -->
- **Value:** a list of pairs of one-character strings
- **Default:** `parentheses, square brackets and curly braces`
- **See also:** [`generic_extra_braces`](generic_extra_braces.md)
<!-- END GENERATED: facts -->

With [`generic_engine: aliengrep`](generic_engine.md), an ellipsis `...` or
`$...X` keeps braces balanced: it does not stop inside a pair it has opened.
In `f(g(x))`, the pattern `f($...ARGS)` binds `$...ARGS` to `g(x)`, not to
`g(x`.

The pairs are `()`, `[]` and `{}`. `generic_braces` replaces that list, so a
pair left out becomes ordinary text;
[`generic_extra_braces`](generic_extra_braces.md) adds pairs to it instead.
Each brace must be a single character.

The option has no effect with the default engine, spacegrep.

## Examples

### Parentheses no longer counted

The rule `call-args` keeps only angle brackets as braces.

**`calls.yaml`**
```yaml title="calls.yaml"
rules:
  - id: call-args-default
    pattern: f($...ARGS)
    message: "arguments: $...ARGS"
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
  - id: call-args
    pattern: f($...ARGS)
    message: "arguments: $...ARGS"
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
      generic_braces: [["<", ">"]]
```

**`calls.txt`**
```text title="calls.txt"
f(g(x))
```

**Command and result:**
```console
$ opengrep scan --config calls.yaml calls.txt
calls.txt

  info  call-args
  arguments: g(x

    1 │ f(g(x))

  info  call-args-default
  arguments: g(x)

    1 │ f(g(x))
```

Without parentheses among the braces, the ellipsis stops at the first `)`.
