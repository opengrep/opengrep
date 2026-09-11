# Maintaining the opengrep reference

`docs/reference/` is user documentation. Everything about how it is made lives
here and in `.claude/skills/opengrep-reference/SKILL.md`, never in the pages
themselves.

## Metadata

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
`reference.py index` from the metadata of all pages: the indexes, the facts
under each title, the flag list of each command, and the version stamp. The
same command writes the label above each example block, from the block's
`title="..."` attribute, and it replaces any bold-only line that stands
directly above a block under `## Examples`.

## Checking

`reference.py check` runs every example and checks the pages against the
binary and the sources. It reports undocumented flags, options and environment
variables, entries for things that no longer exist, broken links, and prose
that mentions the tooling.

Before comparing, both the expected and the actual output are normalised: the
example's directory becomes `<tmp>`, the version `X.Y.Z` and log times
`[00.00]`, and timestamps, long timing numbers and `max_memory_bytes` are
masked, so pages can show those values as printed.

`reference.py check --bless` writes what the commands of failing examples
really print into their pages. Use it to fill in a new example's output, or
after confirming that a change in behaviour is intended, and read the diff:
it records whatever the binary does, right or wrong.

An example block marked `console no-check` is shown but not run. The reason
goes in an HTML comment above the block's label, where readers do not see it:

```
<!-- not run: needs an authenticated GitHub CLI and a repository on GitHub -->
**Command:**
```

The examples show the output of the simple skin, which prints the findings
alone, and they do not pass `--skin`. While `scan` still defaults to another
skin, `check` runs them through a wrapper that adds `--skin simple` to the
scan commands. Delete that wrapper from `reference.py` once the default has
changed.

## Updating

`update-reference.sh` drives both jobs through Claude Code: with no arguments
it updates the reference for the built opengrep, and with `--fill` it
documents a batch of entries that are still missing. It refuses to run while
`bin/opengrep` is older than `src/core/Version.ml`, since the examples run
against that binary. The conventions both jobs follow are in
`.claude/skills/opengrep-reference/SKILL.md`.

Behavioural bugs found while documenting go to `BUG.md` at the repo root. The
pages describe such behaviour as it is, without pointing there.
