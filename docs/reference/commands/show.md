<!-- reference
id: cmd-show
kind: command
name: opengrep show
summary: Print the version, the supported languages, or internal representations.
related: []
-->
# `opengrep show`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`--html`](../flags/html.md), [`--show-supported-languages`](../flags/show-supported-languages.md), [`--version`](../flags/version.md)
<!-- END GENERATED: facts -->

```
opengrep show KIND [ARGUMENTS...]
```

| KIND | Prints |
|---|---|
| `version` | The opengrep version, the same as `opengrep --version`. |
| `supported-languages` | The language names accepted in a rule's `languages` and by `--lang`, aliases included. |
| `dump-*` | Internal representations of rules, patterns and code. See [Internal and debugging interfaces](../internal.md#opengrep-show-dump-commands). |

An unknown KIND is an error: `opengrep show` exits with status 2.

## Flags

<!-- BEGIN GENERATED: flags -->
| Flag | Summary |
|---|---|
| [`--debug`](../flags/debug.md) | Log everything --verbose does and the engine's own diagnostics as well. |
| `--develop` | *not yet documented* |
| [`--experimental`](../flags/experimental.md) | Accepted for compatibility; opengrep has only the one implementation. |
| [`--html`](../flags/html.md) | Render a show dump as an HTML page instead of plain text. |
| [`--json`](../flags/json.md) | Print the findings as a JSON document instead of the text report. |
| `--profile` | *not yet documented* |
| [`--quiet`](../flags/quiet.md) | Print the findings and nothing else. |
| [`--verbose`](../flags/verbose.md) | Log what the scan is doing, at the info level. |
<!-- END GENERATED: flags -->

## Examples

### Version

**Command and result:**
```console
$ opengrep show version
X.Y.Z
```

### Supported languages

**Command and result:**
```console
$ opengrep show supported-languages
supported languages are: apex, bash, c, c#, c++, cairo, circom, clojure, cpp, crystal, csharp, dart, docker, dockerfile, elixir, ex, generic, go, golang, hack, hcl, html, java, javascript, js, json, jsonnet, julia, kotlin, kt, lisp, lua, move_on_aptos, move_on_sui, none, ocaml, php, promql, proto, proto3, protobuf, py, python, python2, python3, ql, r, regex, ruby, rust, scala, scheme, sh, sol, solidity, swift, terraform, tf, ts, typescript, vb, vue, xml, yaml
```
