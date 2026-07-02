# Opengrep Cross-Function Tainting (Interfile)

This directory describes how opengrep's interfile taint analysis works.
"Interfile" means cross-file: taint that starts in one file (a source)
flows through function calls into another file (a sink).  The
implementation hinges on three layers:

1. A project-wide **call graph** built in-process from source by
   `projidx` (the `opengrep_project_index` library).  No external
   indexer.
2. A per-rule **dispatch** layer that carves a relevant subgraph out
   of the project-wide graph and walks it in topological order,
   building up a signature database.
3. The existing **taint engine** (`Dataflow_tainting`, `Sig_inst`,
   `Match_tainting_mode`) that extracts and instantiates function
   signatures, now consuming signatures across the whole subgraph
   instead of one file at a time.

If you have never read this code before, read these documents in
order.  Each is short and self-contained; later documents assume the
earlier ones.

| # | Document | What it covers |
|---|----------|----------------|
| 1 | [01-architecture.md](01-architecture.md) | The end-to-end pipeline.  How a `--taint-interfile` run goes from a project root to a list of findings. |
| 2 | [02-call-graph.md](02-call-graph.md) | **projidx** — how the project-wide call graph is built from source.  Discovery, the `Type_state` lattice, the per-file resolver, dispatch edges. |
| 3 | [03-dispatch.md](03-dispatch.md) | **Interfile_dispatch** — how the taint engine consumes the call graph: relevant subgraph, topological fold, the shared signature database, FBDecl dispatch merge. |
| 4 | [04-language-quirks.md](04-language-quirks.md) | Per-language hooks: Python `__init__`, Go interface embedding, Ruby `attr_reader` / `include`, TypeScript `tsconfig`, Rust `impl`. |
| 5 | [05-cli-and-tools.md](05-cli-and-tools.md) | User-facing CLI surface: `--taint-interfile`, `opengrep-project-index`, `opengrep-interfile-graph`, `opengrep show dump-interfile-graph`. |
| 6 | [06-subtleties.md](06-subtleties.md) | Subtle correctness and performance situations the code handles.  Read this **after** the architecture chapters — it presupposes the vocabulary. |

## Design at a glance

- **The call graph is built in-process.**  Edge emission is in
  source, language-by-language, behind one `Lang_config.t` per
  supported language.  No external indexer, no separate index file,
  no `--scip-index-dir`.  When recall is missing for a language, the
  fix is a few lines in this repo.
- **Per-rule, not per-target.**  Interfile rules don't run once per
  target file; they run once per rule, processing the entire relevant
  call subgraph in topological order.  Per-target rules run alongside
  in the same parmap.
- **Functional signature database.**  The per-rule signature database
  is threaded as a fold accumulator through topological order — no
  shared mutable state across rules, no locks.
- **No long-lived caches.**  The interfile call graph is built
  exactly once per `(language, project_root)` per scan —
  `Interfile_dispatch` groups all of a language's rules onto one
  build — so there is no cache to invalidate.  Diff scans simply
  build the baseline and head graphs independently.

## What this builds on, what it doesn't change

- **Builds on:** the intrafile cross-function story
  ([docs/INTRA_FUNCTION_IMPLEMENTATION.md](../INTRA_FUNCTION_IMPLEMENTATION.md)).
  Interfile uses the same per-function dataflow and signature
  extractor.  Interfile decides **which** functions get analysed,
  **in what order**, and **what signatures they see for their
  callees**.
- **Doesn't change:** per-rule semantics for the user.  A rule with
  `taint_interfile: true` (or a scan invoked with
  `--taint-interfile`) gets cross-file source→sink discovery.
  Nothing else.
