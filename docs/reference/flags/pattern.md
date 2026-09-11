<!-- reference
id: flag-pattern
kind: flag
name: --pattern
aliases: [-e]
summary: Search with a single pattern given on the command line, instead of a rule file.
commands: [scan]
value: `PATTERN`
related: [flag-lang, flag-replacement, flag-config, key-patterns]
-->
# `--pattern`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Also spelled:** `-e`
- **Value:** `PATTERN`
- **See also:** [`--lang`](lang.md), [`--replacement`](replacement.md), [`--config`](config.md), [`patterns`](../rule-syntax/patterns.md)
<!-- END GENERATED: facts -->

Runs one pattern without writing a rule, which is what you want when grepping
a codebase for a shape rather than a string. The pattern syntax is the one a
rule's `pattern:` key uses, so `eval(...)` matches any call to `eval`.

`-e` and [`-l`/`--lang`](lang.md) must be given together: the pattern has to be
parsed as some language. Without `-l`, opengrep stops with
`-e/--pattern and -l/--lang must both be specified` and exit status 2.

A finding from a command-line pattern has no rule behind it, so it is reported
with `-` as its rule id, the pattern as its message, and error severity.
[`--replacement`](replacement.md) adds a fix to it.

## Examples

### Searching for a call

**`app.py`**
```python title="app.py"
eval(1)
print(2)
```

**Command and result:**
```console
$ opengrep scan -e 'eval(...)' -l python app.py
app.py

  error  -
  eval(...)

    1 │ eval(1)

$ opengrep scan -e 'eval(...)' app.py 2>&1 >/dev/null | tail -1
[00.04][ERROR]: -e/--pattern and -l/--lang must both be specified
```
