# Commands

<!-- BEGIN GENERATED: stamp -->
> Reference for **opengrep 1.30.0** (commit `d094c70bb`).
<!-- END GENERATED: stamp -->

Opengrep is run as `opengrep <command> [flags] [arguments]`. When the first
argument is not a command, `scan` is assumed, so `opengrep --config rule.yaml .`
is `opengrep scan --config rule.yaml .`. The flags `--experimental`, `--debug`
and `--profile` may also be given before the command.

`opengrep --help` lists the commands. `opengrep <command> --help` shows the
built-in help of a command.

<!-- BEGIN GENERATED: index -->
| Name | Summary |
|---|---|
| [`opengrep scan`](commands/scan.md) | Run rules on files and report what they match. |
| [`opengrep show`](commands/show.md) | Print the version, the supported languages, or internal representations. |
| [`opengrep test`](commands/test.md) | Check rules against example files annotated with the lines they must and must not match. |

Not yet documented (4): `ci`, `install-ci`, `lsp`, `validate`
<!-- END GENERATED: index -->
