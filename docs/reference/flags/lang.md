<!-- reference
id: flag-lang
kind: flag
name: --lang
aliases: [-l]
summary: The language of a command-line pattern, and of the files it is run on.
commands: [scan]
value: `LANGUAGE`
related: [flag-pattern, flag-show-supported-languages, flag-scan-unknown-extensions]
-->
# `--lang`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **Also spelled:** `-l`
- **Value:** `LANGUAGE`
- **See also:** [`--pattern`](pattern.md), [`--show-supported-languages`](show-supported-languages.md), [`--scan-unknown-extensions`](scan-unknown-extensions.md)
<!-- END GENERATED: facts -->

Says which language the pattern given with [`-e`/`--pattern`](pattern.md) is
written in. The pattern and the files are parsed as that language, so the same
text means different things under `-l python` and `-l js`.

The two flags go together: neither works without the other. LANGUAGE is any
name [`--show-supported-languages`](show-supported-languages.md) prints,
aliases included, so `python`, `py` and `python3` all do the same.

With a rule file there is no need for `--lang`: each rule names its own
`languages:`.

## Examples

### The same pattern, a different language

**`app.js`**
```javascript title="app.js"
eval(1)
```

**Command and result:**
```console
$ opengrep scan -e 'eval(...)' -l js app.js
app.js

  error  -
  eval(...)

    1 │ eval(1)
```
