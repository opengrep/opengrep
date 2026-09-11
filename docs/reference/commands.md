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
| [`opengrep ci`](commands/ci.md) | Scan a repository the way a CI job wants it: repository metadata, blocking findings, and a non-zero status when they appear. |
| [`opengrep install-ci`](commands/install-ci.md) | Add a GitHub Actions workflow that runs opengrep ci on pull requests. |
| [`opengrep lsp`](commands/lsp.md) | Run opengrep as a language server, speaking LSP on standard input and output. |
| [`opengrep scan`](commands/scan.md) | Run rules on files and report what they match. |
| [`opengrep show`](commands/show.md) | Print the version, the supported languages, or internal representations. |
| [`opengrep test`](commands/test.md) | Check rules against example files annotated with the lines they must and must not match. |
| [`opengrep validate`](commands/validate.md) | Check that rule files are valid, without scanning any code. |
<!-- END GENERATED: index -->

## Examples

### `scan` is the default command

**`rule.yaml`**
```yaml title="rule.yaml"
rules:
  - id: find-eval
    pattern: eval(...)
    message: found eval
    languages: [python]
    severity: WARNING
```

**`app.py`**
```python title="app.py"
eval(1)
```

**Command and result:**
```console
$ opengrep --config rule.yaml app.py
app.py

  warn  find-eval
  found eval

    1 │ eval(1)
```
