<!-- reference
id: cmd-lsp
kind: command
name: opengrep lsp
summary: Run opengrep as a language server, speaking LSP on standard input and output.
related: [cmd-scan]
-->
# `opengrep lsp`

<!-- BEGIN GENERATED: facts -->
- **See also:** [`opengrep scan`](scan.md), [`HOME`](../env/HOME.md)
<!-- END GENERATED: facts -->

```
opengrep lsp [FLAGS]
```

Starts a [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
server that scans the files an editor opens and reports the findings as
diagnostics. It is meant to be started by an editor, not by hand: it reads
JSON-RPC messages with `Content-Length` framing on standard input and answers
on standard output.

The transport is fixed to standard input and output; there are no `--stdio` or
`--socket` flags. Beyond the flags every command takes, `lsp` has none of its
own, so the rules and the scan settings come from the client, which sends them
as LSP configuration when the session starts.

## Exit status

0 when the client ends the session, 2 on a fatal error, 141 when the reader of
the output closed the pipe.

## Flags

<!-- BEGIN GENERATED: flags -->
| Flag | Summary |
|---|---|
| [`--debug`](../flags/debug.md) | Log everything --verbose does and the engine's own diagnostics as well. |
| `--develop` | *not yet documented* |
| [`--experimental`](../flags/experimental.md) | Accepted for compatibility; opengrep has only the one implementation. |
| `--profile` | *not yet documented* |
| [`--quiet`](../flags/quiet.md) | Print the findings and nothing else. |
| [`--verbose`](../flags/verbose.md) | Log what the scan is doing, at the info level. |
<!-- END GENERATED: flags -->

## Examples

### Starting the server

An editor runs the command and then drives the session. There is nothing to
see when starting it by hand: the server waits for the client's `initialize`
request and answers on standard output.

<!-- not run: the server needs a client and never exits on its own -->
**Command:**
```console no-check
$ opengrep lsp
```
