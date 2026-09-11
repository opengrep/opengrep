<!-- reference
id: flag-html
kind: flag
name: --html
summary: Render a show dump as an HTML page instead of plain text.
commands: [show]
related: [cmd-show, flag-json]
-->
# `--html`

<!-- BEGIN GENERATED: facts -->
- **Accepted by:** [`opengrep show`](../commands/show.md)
- **See also:** [`opengrep show`](../commands/show.md), [`--json`](json.md)
<!-- END GENERATED: facts -->

Wraps the output of [`opengrep show`](../commands/show.md) in an HTML page,
with the tree indented and styled so it can be read in a browser. It is meant
for the `dump-*` kinds, whose plain output is a dense OCaml-style tree.

Kinds that print ordinary text, such as `version` and `supported-languages`,
ignore the flag. See
[Internal and debugging interfaces](../internal.md#opengrep-show-dump-commands)
for what the dumps contain; they are development aids, not a stable format.

## Examples

### A pattern dump as a page

**Command and result:**
```console
$ opengrep show --html dump-pattern python 'eval(...)' | grep -c '<html>'
1
$ opengrep show dump-pattern python 'eval(...)' | grep -c '<html>'
0
```

The second command prints the same tree as text, so it holds no `<html>` tag.
