<!-- reference
id: opt-generic_extra_word_characters
kind: option
name: generic_extra_word_characters
summary: With aliengrep, add characters that a metavariable may capture as part of a word.
value: a list of one-character strings
default: []
related: []
-->
# `generic_extra_word_characters`

<!-- BEGIN GENERATED: facts -->
- **Value:** a list of one-character strings
<!-- END GENERATED: facts -->

With [`generic_engine: aliengrep`](generic_engine.md), a metavariable such as
`$NAME` captures one word: letters, digits and `_`. It cannot capture part of
a longer word, so `$NAME` does not match `Content-Type`, where `-` ends the
word.

`generic_extra_word_characters` adds characters to words. With `["-"]`,
`$NAME` captures `Content-Type` whole. Each character must be a single byte.

The option has no effect with the default engine, spacegrep.

## Examples

### Header names with dashes

The rule `header` adds `-` to word characters.

**`headers.yaml`**
```yaml title="headers.yaml"
rules:
  - id: header-default
    pattern: "header $NAME:"
    message: "header $NAME"
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
  - id: header
    pattern: "header $NAME:"
    message: "header $NAME"
    languages: [generic]
    severity: INFO
    options:
      generic_engine: aliengrep
      generic_extra_word_characters: ["-"]
```

**`request.txt`**
```text title="request.txt"
header Content-Type: text/plain
header Host: example.com
```

**Command and result:**
```console
$ opengrep scan --config headers.yaml request.txt
request.txt

  info  header
  header Content-Type

    1 │ header Content-Type: text/plain

  info  header
  header Host

    2 │ header Host: example.com

  info  header-default
  header Host

    2 │ header Host: example.com
```
