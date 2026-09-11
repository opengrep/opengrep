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
the command in the same directory, and you will see the output shown. The
examples are run against opengrep each time this reference is updated. Only
standard output is shown unless the command redirects standard error
(`2>&1`). Opengrep prints its progress and summary on standard error.

In example targets, a `# ruleid: <id>` comment marks the next line as one that
rule `<id>` must report, and `# ok: <id>` marks one it must not report.
[`opengrep test`](commands/test.md) checks these annotations, so an example
that runs `opengrep test` and prints `✓ All tests passed` shows exactly where
the rule matches.

## Maintaining this reference

Each entry page starts with a metadata comment:

```
<!-- reference
id: flag-timeout
kind: flag
name: --timeout
summary: One line, shown in the indexes.
commands: [scan, ci, test]
env: [env-OPENGREP_TIMEOUT]
related: [opt-timeout]
-->
```

Ids are `cmd-`, `flag-`, `key-`, `opt-` or `env-` followed by the name. Blocks
between `BEGIN GENERATED` and `END GENERATED` markers are rewritten by
`scripts/docs/reference.py index` from the metadata of all pages: the indexes,
the facts under each title, the flag list of each command, and the version
stamp. `scripts/docs/reference.py check` runs every example and checks the
pages against the binary and the sources. It reports undocumented flags,
options and environment variables, and entries for things that no longer
exist.
