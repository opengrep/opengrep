# 1. Architecture: How an interfile scan runs end-to-end

Read this first.  Everything else in this directory references the
pipeline laid out here.

## The five-line summary

1. The user runs `opengrep --taint-interfile <rules> <target>`.
2. For every language with interfile taint rules, **projidx** walks
   the project tree once and builds a project-wide call graph.
3. **Interfile_dispatch** takes that graph and, for each rule,
   carves out a "relevant subgraph" containing only the functions
   that sit between the rule's sources and its sinks.
4. The relevant subgraph is processed in **topological order** —
   callees first, callers last — building up a **signature database**
   that records how taint flows through each function.
5. Findings emit at the **sink target**: when a sink in a target file
   inherits taint via the signature database from a source in any
   file, we record the trace.

The whole thing runs in one opengrep process.  No external indexer.
No `--scip-index-dir`.

## Where each stage lives

| Stage | Module | Output |
|---|---|---|
| CLI parse, decide `taint_interfile = true` | `src/osemgrep/cli_scan/Scan_CLI.ml` | `Core_scan_config.taint_interfile : bool` (per-rule: `Rule_options.taint_interfile`) |
| Group rules by language and targets by `project_root` | `src/engine/Interfile_dispatch.ml` `build_rule_states` | `lang_context` list (one per `(lang, root)` group) |
| Build the project-wide call graph | `src/engine/Interfile_graph.ml` → `tools/project_index/` (projidx) | `Call_graph.G.t` |
| Filter targets to those present in the graph | `Interfile_dispatch.targets_in_interfile_graph` | `interfile_target list` |
| Parse target files + companion files | `Interfile_dispatch.parse_companion_files` | `(Lang, AST table)` |
| Extract sources/sinks per rule | `Match_taint_spec.taint_config_of_rule` | `taint_inst` per rule, per file |
| Compute relevant subgraph | `Graph_reachability.compute_relevant_subgraph` | `Call_graph.G.t` |
| Topological fold | `Interfile_dispatch.topo_fold` | `Shape_and_sig.signature_database` + findings |
| Run per-rule tasks | `Interfile_dispatch.run_rule`, scheduled by `Core_scan` | `Core_match.t list` |

## What "interfile" buys you over intrafile

The intrafile engine (see [`docs/INTRA_FUNCTION_IMPLEMENTATION.md`](../INTRA_FUNCTION_IMPLEMENTATION.md))
already extracts per-function taint signatures and replays them at
call sites *inside one file*.  Interfile lifts that across the whole
project.  Concretely:

- A taint **source** in `app/handlers.py` can taint a parameter that
  flows through `lib/util.py` and lands in a **sink** in
  `app/db.py`.  Intrafile would miss this because `util.process()` is
  defined in another file; interfile sees the cross-file edge in the
  call graph and uses the signature extracted for `util.process` when
  analysing the handler.
- An **interface declaration** in `internal/auth/iface.go` has no
  body.  The interface method's effective signature is the merge of
  every concrete implementation.  Interfile traverses the
  `Dispatch` edges in the call graph and merges those signatures
  ([§ 3](03-dispatch.md)).
- A **higher-order function** receives a callback and invokes it.
  If the callback is defined in another file, the chain
  `caller → HOF → callback` only resolves with a project-wide call
  graph.

## Two modes of rule processing

Inside `Core_scan` there are two parallel task pools:

- **Per-target tasks**: for every target file, run every applicable
  rule.  This is what runs for plain semgrep rules and for intrafile
  taint rules.
- **Per-rule tasks** (the interfile path): for every interfile taint
  rule, run *one* task that processes the entire relevant subgraph.

Both pools feed into the same `Domainslib_.parmap` so per-target and
per-rule tasks execute in parallel.  Interfile tasks are placed
**first** in the work list so the long-running ones start early.

The two pools must not double-count findings on the same `(rule,
target)`, but they CAN overlap on the same rule when graph build
partially fails.  `Interfile_dispatch.interfile_taint_rule_ids`
returns the IDs of all rules that go through per-rule dispatch;
`build_rule_states` additionally returns, per rule, the list of
project_roots where its graph build failed.  `Core_scan` then gates
per target: an interfile rule runs in the per-target queue only on
targets whose `project_root` is in that rule's failed-roots list
(intrafile fallback) — for targets under roots whose graph
succeeded, per-rule dispatch handles them and the per-target queue
skips them.

## The signature database, in one paragraph

When a function `F` is analysed (whether for intrafile or interfile),
the dataflow engine produces a **taint signature** for `F`: a
description of how every parameter flow ends up in every return
position, plus any side effects on globals/fields.  Signatures are
keyed by `Function_id.t` (a `(name, file, line, col)` tuple) and
stored in a `Shape_and_sig.signature_database`.  When `F`'s caller
`G` is then analysed and hits a call site `F(x)`, the engine looks
up `F`'s signature in the database and **instantiates** it: it
substitutes `G`'s actual argument taint for `F`'s parameter
abstractions and propagates the result.  Interfile is the same
idea, just with the database populated from across the whole
project.

`Sig_inst.ml` is where instantiation happens.  Its
`instantiate_function_signature` has a subtle interfile case
(unresolved-callback effect preservation) described in
[§ 6](06-subtleties.md).

## State lifetimes

There are no long-lived caches; every structure's lifetime is tied to
the scan that built it:

- **The interfile graph** is built exactly once per `(language,
  normalised absolute project_root)` per scan:
  `Interfile_dispatch.build_rule_states` groups all of a language's
  interfile rules onto one `Interfile_graph.load_interfile_build`
  call, and every rule's subgraph is sliced from that shared graph.
  Diff scans build the baseline and head graphs independently — the
  projidx output for a different commit is different.
- **Signatures, info maps, glob envs** are built per-rule per-file
  during `build_rule_states` and held immutable on the `rule_state`
  record.  The topological fold threads the signature database
  *functionally*; no mutation, no locks.
- **Per-file epilogue CFGs** (top-level + class-init) are built
  inline per rule for the files whose epilogue actually runs — the
  topo-universe gate prunes the rest, and the dataflow check
  dominates the build cost.

## Parallelism rules of thumb

- Each interfile rule = one independent task.  Rules share read-only
  state (the graph, the rule list, the AST table) but each rule has
  its own signature database that nobody else reads.
- Inside a rule, the topological fold is serial: callees must finish
  before callers.
- Per-file work (parsing, building `taint_inst`, building IL+CFG) is
  done **once per rule** during `build_rule_states` and reused across
  the topological fold.  Companion files (files in the subgraph but
  not in the target list) are batch-parsed in parallel before
  per-rule init.

The next document, [02-call-graph.md](02-call-graph.md), explains how
projidx actually builds the graph that this whole pipeline rides on.
