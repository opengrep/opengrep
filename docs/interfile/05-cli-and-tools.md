# 5. CLI surface and debugging tools

Everything in this document is user-facing.  If you're integrating
opengrep into a CI pipeline or debugging a missing finding, this is
what you have to work with.

## Running an interfile scan

The minimum invocation:

```
opengrep scan --taint-interfile <rules> <target>
```

That's it.  No external indexer.  opengrep groups the scan's
targets by the `project_root` carried on each target (set during
target discovery; targets without a discovered root fall back to
`cwd`), builds one interfile call graph per `(language,
project_root)` group in-process via projidx, and runs the analysis.
The standalone `opengrep-interfile-graph` tool takes `--project-root
DIR` explicitly.

The flag also accepts per-rule control via `options.taint_interfile:
true` in the rule YAML.  The CLI flag and the per-rule option combine
by **OR**: a rule runs interfile when the flag is on *or* its own
option is set.  So with the flag on, every rule is interfile; with
the flag off, only rules carrying `options.taint_interfile: true` go
through interfile dispatch.  The two settings never cancel each other
— neither side can turn the other off.  `--taint-interfile-depth` and
the per-rule `taint_interfile_depth` combine by **max** (the deeper of
the two wins).

Implies `--taint-intrafile` because interfile builds on the
intrafile per-function dataflow.

### Finding deduplication

The taint source is part of a finding's dedup key **only when the
rule is interfile** (flag or per-rule option, per the OR above).  The
practical effect:

- **Interfile:** two findings at the same sink reached from different
  sources are kept as distinct findings.
- **Intrafile:** findings at the same sink collapse to one regardless
  of source.

So the same rule and target can report more findings under
`--taint-interfile` than without it — that is the dedup key widening,
not new dataflow.  (`core_unique_key` in
`src/reporting/Core_json_output.ml`.)

### `--taint-interfile-depth N` (default 3)

Bounds the BFS that computes the relevant subgraph (see [§ 3](03-dispatch.md)
"Phase E").  Higher values include longer call chains; runtime grows
with the chain depth.  Default 3 is enough for almost every rule;
chains of length 5+ are rare in practice.

### `--taint-intrafile`

Independent flag.  Enables the intrafile cross-function analysis
without enabling interfile.  Required for interfile; can be used
alone for single-file interprocedural taint.

## The interfile call graph as a standalone product

The same call graph that the taint engine consumes is available
through one diagnostic tool, `opengrep-interfile-graph`, with
subcommands covering both the raw index and the engine's view of it:

```
$ bin/opengrep-interfile-graph index      -r /path/to/repo -l go --dump-edges > edges.tsv
$ bin/opengrep-interfile-graph full-graph -r /path/to/repo -l go
$ bin/opengrep-interfile-graph lookup -r /path/to/repo -l go -p "Authenticate"
$ bin/opengrep-interfile-graph edges  -r /path/to/repo -l go -p "Service.Get"
$ bin/opengrep-interfile-graph relevant-graph --rules my-rule.yaml /path/to/code
$ bin/opengrep-interfile-graph topo-order   --rules my-rule.yaml /path/to/code --signatures
```

- `index` — build the raw project index for one language and dump
  it.  This shows the graph *as projidx builds it* (paths may be
  relative to the project root), before the engine absolutifies it.
  Options:
  - `--dump-edges` — TSV of every edge in the call graph
    (source, destination, call site, kind).
  - `--dump-all` — TSV of every entry (function/method/class).
  - `--sample N` — print the first N entries for quick inspection
    (default 10).
  - `--list-files` — print the files that would be indexed, then exit.
  - `--include <glob>` / `--exclude <glob>` — filter the file set
    (repeatable).
- `full-graph` — vertex/edge/file counts and (with `-v`) a
  file-by-file breakdown plus a TSV edge dump to stderr, in the same
  format as `index --dump-edges`.  This shows the graph *as the
  engine consumes it* (absolute paths).
- `dump-interfile-ast -l LANG FILE [-r DIR]` — dump a file's generic
  AST after call-graph callee resolution, so each callee's
  `id_resolved` shows the resolved definition.  With `-r` the projidx
  pipeline resolves callees across files; without it resolution is
  single-file.
- `lookup -p PATTERN` — find vertices whose name matches the regex,
  optionally with their callers/callees (`-v`).
- `edges -p PATTERN [--kind call|dispatch]` — show callers and
  callees of matching vertices, optionally filtered by edge kind.
- `relevant-graph --rules R TARGET...` — compute the per-rule
  relevant subgraph as the taint engine would (§ 3), and report
  vertices/edges/files for each rule.
- `topo-order --rules R TARGET... [--signatures] [--json]` — emit
  the topological order each rule would walk; with `--signatures`
  also run the signature-extraction fold and show the final signature
  for each function.

This is the right tool when a taint finding is missing and you
suspect the call graph or the relevant subgraph.

### `opengrep show dump-interfile-graph`

Quick textual dump of the graph for one language under one project
root, embedded in the main `opengrep show` namespace.  Prints the
vertex/edge counts, then every vertex, then every edge as
`callee <- caller @ call_site, kind`, all on stdout (contrast
`opengrep-interfile-graph full-graph -v`, which prints per-file
vertex counts on stdout and a TSV edge dump on stderr).  Used by
tests and quick interactive checks.

```
$ opengrep show dump-interfile-graph go /path/to/repo
```

## Configuration knobs you don't usually need

These exist for testing and rare production tuning (on
`opengrep-interfile-graph index`; `--ncores` is also accepted by
`relevant-graph` and `topo-order`):

- `--ncores N` (also `-j N`) — cap projidx parallelism.  Defaults to
  the CPU count.
- `--include <glob>` / `--exclude <glob>` — filter the file set.
  Both are purely additive filters on top of discovery, which runs
  with an empty semgrepignore set and no per-language default
  excludes (the only language-supplied excludes are TypeScript's
  tsconfig `exclude` arrays).
- `--pyrefly-toml <pyrefly.toml>` — read `project-includes` /
  `project-excludes` arrays from a pyrefly config file.  Useful when
  a project already maintains those for another tool.

[Next: § 6, the subtle correctness and performance situations the
code handles.](06-subtleties.md)
