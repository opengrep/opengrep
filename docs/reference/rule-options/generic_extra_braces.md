<!-- reference
id: opt-generic_extra_braces
kind: option
name: generic_extra_braces
summary: With aliengrep, add pairs of braces that an ellipsis keeps balanced.
value: a list of pairs of one-character strings
default: []
related: []
-->
# `generic_extra_braces`

<!-- BEGIN GENERATED: facts -->
- **Value:** a list of pairs of one-character strings
- **See also:** [`generic_braces`](generic_braces.md)
<!-- END GENERATED: facts -->

With [`generic_engine: aliengrep`](generic_engine.md), an ellipsis keeps the
braces `()`, `[]` and `{}` balanced (see
[`generic_braces`](generic_braces.md)). `generic_extra_braces` adds pairs to
that list, such as `["<", ">"]` for generic types or tags.

Each brace must be a single character: a longer one makes the rule invalid,
with the error `Multibyte opening braces aren't supported`.

The option has no effect with the default engine, spacegrep.

## Examples

### Angle brackets as braces

The rule `type-args` adds angle brackets.

**`types.yaml`**
```yaml title="types.yaml"
rules:
  - id: type-args-default
    pattern: List<$...ARGS>
    message: "type arguments: $...ARGS"
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
  - id: type-args
    pattern: List<$...ARGS>
    message: "type arguments: $...ARGS"
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
      generic_extra_braces: [["<", ">"]]
```

**`types.txt`**
```text title="types.txt"
List<Map<K, V>> m
```

**Command and result:**
```console
$ opengrep scan --config types.yaml types.txt
types.txt

  info  type-args-default
  type arguments: Map<K, V

    1 │ List<Map<K, V>> m

  info  type-args
  type arguments: Map<K, V>

    1 │ List<Map<K, V>> m
```

Without the extra pair, the ellipsis stops at the first `>`.
