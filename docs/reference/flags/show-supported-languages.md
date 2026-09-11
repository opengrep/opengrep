<!-- reference
id: flag-show-supported-languages
kind: flag
name: --show-supported-languages
summary: Print the languages opengrep can parse, and exit.
commands: [scan]
related: [flag-lang, cmd-show, flag-version]
-->
# `--show-supported-languages`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep scan`](../commands/scan.md)
- **See also:** [`--lang`](lang.md), [`opengrep show`](../commands/show.md), [`--version`](version.md)
<!-- END GENERATED: facts -->

Prints the language names opengrep accepts and exits, scanning nothing. These
are the names a rule's `languages:` key takes and the ones
[`-l`/`--lang`](lang.md) accepts, aliases included, so `python`, `py`,
`python3` all name the same language.

[`opengrep show supported-languages`](../commands/show.md) prints the same
list and is the tidier way to ask.

## Examples

### The languages this build knows

**Command and result:**
```console
$ opengrep scan --show-supported-languages
supported languages are: apex, bash, c, c#, c++, cairo, circom, clojure, cpp, crystal, csharp, dart, docker, dockerfile, elixir, ex, generic, go, golang, hack, hcl, html, java, javascript, js, json, jsonnet, julia, kotlin, kt, lisp, lua, move_on_aptos, move_on_sui, none, ocaml, php, promql, proto, proto3, protobuf, py, python, python2, python3, ql, r, regex, ruby, rust, scala, scheme, sh, sol, solidity, swift, terraform, tf, ts, typescript, vb, vue, xml, yaml
```
