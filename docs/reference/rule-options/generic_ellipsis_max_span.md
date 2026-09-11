<!-- reference
id: opt-generic_ellipsis_max_span
kind: option
name: generic_ellipsis_max_span
summary: With spacegrep, the most line breaks an ellipsis may span.
value: an integer
default: 10
related: []
-->
# `generic_ellipsis_max_span`

<!-- BEGIN GENERATED: facts -->
- **Value:** an integer
- **Default:** `10`
<!-- END GENERATED: facts -->

With `languages: [generic]` and the default engine, spacegrep, an ellipsis
`...` matches at most this many line breaks. `0` keeps each match of `...`
within one line.

The option has no effect with
[`generic_engine: aliengrep`](generic_engine.md), whose ellipsis is limited by
[`generic_multiline`](generic_multiline.md) instead.

## Examples

### Blocks within one line

The rule `block` keeps its ellipsis within a line.

**`blocks.yaml`**
```yaml title="blocks.yaml"
rules:
  - id: block-default
    pattern: begin ... end
    message: a block
    languages: [generic]
    severity: INFO
  - id: block
    pattern: begin ... end
    message: a block
    languages: [generic]
    severity: INFO
    options:
      generic_ellipsis_max_span: 0
```

**`blocks.txt`**
```text title="blocks.txt"
begin x end
begin
x
end
```

**Command and result:**
```console
$ opengrep scan --config blocks.yaml blocks.txt
blocks.txt

  info  block
  a block

    1 │ begin x end

  info  block-default
  a block

    1 │ begin x end

  info  block-default
  a block

    2 │ begin
    3 │ x
    4 │ end
```
