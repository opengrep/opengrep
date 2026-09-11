# Opengrep reference

<!-- BEGIN GENERATED: stamp -->
> Reference for **opengrep 1.30.0** (commit `d094c70bb`).
<!-- END GENERATED: stamp -->

This reference covers the command line and the rule format of opengrep.

- [Commands](commands.md): `opengrep scan`, `opengrep test` and the other subcommands.
- [Flags](flags.md): every command-line flag, with the commands that accept it.
- [Rule syntax](rule-syntax.md): the keys of a rule.
- [Rule options](rule-options.md): the keys of a rule's `options:` block.
- [Environment variables](environment.md): the variables opengrep reads.
- [Internal and debugging interfaces](internal.md): interfaces meant for opengrep's
  own development, listed but not documented in detail.

## Reading an entry

Each entry has its own page. Below the title, a short list of facts gives the
commands that accept a flag, its default, the environment variable that has
the same effect, and related entries.

The examples at the end of a page are complete. Create the files shown, run
the command in the same directory, and you will see the output shown. Every
block in an example is labelled: with the name of the file to create, or with
**Command and result:** for the commands to run and what they print. Only
standard output is shown unless the command redirects standard error
(`2>&1`). Opengrep prints its progress and summary on standard error.

In example targets, a `# ruleid: <id>` comment marks the next line as one that
rule `<id>` must report, and `# ok: <id>` marks one it must not report.
[`opengrep test`](commands/test.md) checks these annotations, so an example
that runs `opengrep test` and prints `✓ All tests passed` shows exactly where
the rule matches.
