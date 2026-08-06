# 3. Dispatch: from graph to findings

This document is about `src/engine/Interfile_dispatch.ml`.  Read
[02-call-graph.md](02-call-graph.md) first.

`Interfile_dispatch` is the layer between projidx and the taint
engine.  It takes the project-wide call graph, builds per-rule state,
runs the per-rule signature fixpoint, and emits findings.

## The two entry points

- `build_rule_states` is called once per scan, before any parmap
  task fires.  It loads the graph, parses target and companion files,
  builds per-rule subgraphs, and returns the prepared `rule_state`
  list together with the xlangs it used, the per-rule target paths
  its dispatch does not cover (routed to per-target intrafile
  fallback), and the per-file index/parse failures that `Core_scan`
  surfaces as scan errors.  Each `rule_state` is a fully-prepared
  work item.
- `run_rule rs` is the task body.  Given a `rule_state`, it runs the
  signature fixpoint and returns a `Core_match.t list` of findings.

`Core_scan` calls `build_rule_states` once and then schedules each
returned `rule_state` as a separate parmap task.

## What's in a rule_state

```ocaml
type rule_state = {
  rule : R.taint_rule;
  lang : Lang.t;
  relevant_graph : Call_graph.G.t;
  topo_order : Function_id.t list;
  info_map : Match_tainting_mode.fun_info FunctionMap.t;
  file_envs : file_env FpathMap.t;
  builtin_signature_db : Shape_and_sig.builtin_signature_database option;
  match_on : [ `Sink | `Source ];
  target_root_map : Fpath.t option FpathMap.t;
}
```

Everything in here is **immutable after construction**.  The
signature database is threaded *functionally* through the fold; no
field is mutated during `run_rule`.

`info_map` is a `Function_id → fun_info` map containing, for every
function in the subgraph, its parsed AST, its IL+CFG, and the rule's
`taint_inst` (source/sink predicates specialised to that file); the
file-level `glob_env` lives on the `file_env` and is fetched via
`glob_env_of_fid`.  Building it is the expensive part of
setup, which is why `build_rule_states` runs eagerly under one
parmap rather than lazily inside `run_rule`.

`topo_order` is the list of `Function_id`s in topological order:
leaves (deepest callees) first, roots (callers with no incoming
calls) last.  Produced by `Call_graph.Topo.fold` on
`relevant_graph`.  It drives the Phase 2 emission pass (order there is
immaterial anyway); Phase 1 instead walks the SCC condensation of
`relevant_graph` so cycles are handled as a unit (see "The signature
fixpoint" below).  Before the order is taken,
`prune_impl_interface_cycles` removes direct interface→impl `Call`
edges so implementations precede their interfaces; only indirect
cycles survive as SCCs.  Source/sink fids that are not vertices of
the subgraph are appended to `topo_order` so their functions are
still analysed.

## How a rule_state is built

`build_rule_states` runs as a sequence of phases.  Most are parallel
(parmap inside one phase), but the phases themselves are sequential.

### Phase A: Group rules by language

`interfile_taint_rules_by_lang` walks every rule with `taint_interfile
= true` (either via the CLI flag or the rule's own `options`) and
groups them by their target language.  Rules of different languages
are independent; we'll process each group separately.

### Phase B: Group targets by project root, load one graph per group

Targets are grouped by the `project_root` carried on each
`Target.t` (set during target discovery in `Find_targets`).  Targets
without a discovered root fall back to `cwd`.  Multi-root scans
produce multiple groups, each producing its own projidx graph.  For
each `(language, project_root)` group with rules, call
`Interfile_graph.load_interfile_build ~ncores ~targeting_conf`,
which builds the graph (Phase 0–3 of projidx, see
[§ 2](02-call-graph.md)) and also returns the projidx-stamped ASTs
(reused for the dispatch parse) and the per-file build failures.
There is no cache, and no need for one: dispatch groups all of a
language's rules onto this one call, so the build runs once per
`(lang, project_root)` per scan.
`targeting_conf` is the same `Find_targets.conf` the scan used, so
projidx's file universe matches Semgrep's target selection
(`--include`/`--exclude`, `.semgrepignore`, size/minified filtering).

`targets_in_interfile_graph` then keeps only those targets whose
absolute path appears as a vertex in the graph.  A target that
projidx didn't index falls back to per-target intrafile via the
fallback gate in `Core_scan` (see fallback-rule semantics below) so
the rule still produces findings on it.

### Phase C: Parse target files

All targets across all languages are concatenated into parse
batches.  Two parses run: the dispatch parse reuses the
projidx-stamped ASTs where available, and a separate fresh
Naming-only parse produces the ASTs that spec extraction matches
against — the projidx-stamped `id_info` payloads make pattern
traversal roughly two orders of magnitude slower, so the matcher
gets clean ASTs.  Each batch returns a `Hashtbl (Fpath.t,
G.program)` of file → AST.

### Phase D: Extract per-rule specs

For every `(rule, lang_context)` pair, run
`Match_taint_spec.spec_matches_of_taint_rule` against each target
file's AST.  This produces the source / sink / sanitiser /
propagator matches that drive the rest of the analysis.  From those
matches we extract `Function_id.t` lists for sources and sinks — the
boundary that defines the relevant subgraph.

Spec extraction is parallel: work items are `(rule, target chunk)`
pairs, with chunks capped at 2000 targets, so one expensive rule
spreads across domains.  Per-rule per-file `Formula_cache.t` is
created fresh each time (the cache is a mutable `Hashtbl` and isn't
thread-safe).

### Phase E: Compute relevant subgraph

For each rule, call
`Graph_reachability.compute_relevant_subgraph` with the rule's
`sources` and `sinks` lists.  The algorithm:

1. **Forward BFS** from sources through the graph (edges are
   callee → caller, so forward = toward callers).  Cap reachability
   at `taint_interfile_depth` hops.
2. **Forward BFS** from sinks similarly.
3. **Common ancestors** = intersection of the two reachable sets.
4. **Relevant set** within the common ancestors: a vertex is
   relevant if it's a source/sink itself, if it has a predecessor
   that's a source/sink or that is reachable from only one of the
   two sets (an entry point), or if it has multiple predecessors in
   the common set (it's a bridge between groups).
5. **Reverse BFS** from the relevant set to pull in the callee
   subtrees so each function's callees are present.
6. **Dispatch closure**: pull in `Dispatch` predecessors
   (implementations) for any interface vertex in the set, along with
   each added implementation's `Call` predecessors.  Without this, a
   bodiless interface method that lands in the subgraph would have
   no implementations to dispatch to during signature extraction,
   yielding an empty signature that poisons callers.  Repeat up to
   `max_depth` times since newly added implementations may
   themselves call other interface methods.

The relevant subgraph is typically a tiny slice of the project-wide
graph: a single rule's sources and sinks rarely span more than a few
hundred functions.

### Phase F: Parse companion files

The relevant subgraph references functions in files that may not be
among the scan targets (an interfile chain `app/handler.py → util.py
→ db.py` only has `handler.py` as a target if the user pointed the
scan at `app/`).  We collect every file that appears in any rule's
subgraph but not in the target AST table, batch them by language, and
parse them in parallel.

### Phase G: Initialise per-rule per-file state

For each rule + file in the subgraph, build a `file_env`:

- `ast`: the parsed program.
- `taint_inst`: the rule's source/sink/sanitiser/propagator
  predicates, specialised to this file.  When the file has no rule
  matches we still build a `taint_inst` with empty predicates so
  signature extraction runs but no spurious source/sink matches arise
  from byte-position collisions with another file.
- `glob_env`: file-level taint state from top-level statements
  (module-scope assignments etc.).
- `glob_effects`: any taint effects emitted at module scope.

Function ids are absolutified at this boundary so they compare equal
to the absolute-path ids in the graph (see [§ 2](02-call-graph.md)
"Output").

## The signature fixpoint

`run_rule rs` ultimately calls `topo_fold ~detect_findings:true rs`,
which runs in two phases:

- **Phase 1 — converge signatures.**  The relevant subgraph is
  condensed into strongly-connected components (`Call_graph.SCC`) and
  the SCCs are processed callees-first.  Acyclic functions are
  singleton SCCs and get summarised exactly once; a cyclic SCC (mutual
  recursion, or an indirect impl↔interface dispatch loop) is *iterated*
  to a fixpoint — its members are re-summarised until every member's
  `SignatureSet` stops changing.  A plain topological walk has no valid
  order *inside* a cycle, so a member can be summarised before its
  cyclic callee and emit an incomplete, order-dependent signature; the
  fixpoint removes that dependence.  The generic
  driver is `Graph_fixpoint.Make(Call_graph.G)(Sig_lattice)(Sig_store)`
  (`run`, bounded by `max_iter` with an `on_max_iter` escape that keeps
  the current database rather than looping on a pathological rule).
- **Phase 2 — emit findings.**  A single pass over `rs.topo_order`
  against the converged database.  Each function is re-extracted
  against the converged database before checking; the re-extraction
  is deliberate — dropping it silently loses findings on large
  corpora, so it must not be optimised away without an A/B run.
  Every function's callees already carry their final signatures, so
  order no longer matters and the database is not threaded through
  this pass.

Within Phase 1, each function is summarised by its body shape:

### Case 1: bodiless declarations (`FBDecl` / `FBNothing`)

A bodiless interface or abstract method (Java abstract methods lower
to `FBNothing`).  We do **not** extract a signature from the empty
body — that would store an empty sig that
makes the function look like a no-op effects-wise, which is unsound
(callers would see "no taint propagation" instead of falling back to
conservative propagation).

Instead:

1. Look up the function's implementations: `dispatch_impls` first
   reads the AST mirror `id_info.id_resolved_alternatives`, falling
   back to the graph's `Dispatch` predecessors in
   `rs.relevant_graph` when the mirror is empty.  Skip self-edges —
   the interface declaration carries a Dispatch edge to itself;
   including its own empty body as an implementation pollutes the
   merge.
2. Filter to implementations whose signature is already in `db`
   (which it should be — they're earlier in topo order).
3. If none, skip this vertex entirely: `db` stays unchanged.  No
   empty signature gets stored.  Callers will fall back to
   conservative propagation when they hit a call to this vertex.
4. If some, extract a skeleton signature from the declaration via
   `extract_replace`, then call `dispatch_merge_fbdecl` to merge the
   implementation signatures into a single rich signature and
   `replace` the skeleton in `db`.

`Sig_inst.merge_dispatch_signatures` does the merge:

- Strip leading "receiver" params from each implementation when its
  param count exceeds the interface's (Go method values carry the
  receiver as an extra leading param).
- Filter `BGlob`-dependent effects: implementations reference their
  own globals; those would resolve incorrectly at the interface
  call site.
- Remap each implementation's `BArg` indices to canonical parameter
  positions — the canonical params are the *first implementation's*
  (the interface signature only supplies the param count used for
  receiver stripping, and is the fallback when the impl list is
  empty).  An implementation with an incompatible param count is
  skipped with a warning, not remapped.
- Union all the effect sets.

### Case 2: normal function body

Call `extract_signatures` (factored out of `extract_and_check`):

- Extract a taint signature from the function body, replaying any
  callee signatures from `db` (intrafile cross-function machinery,
  see [`docs/INTRA_FUNCTION_IMPLEMENTATION.md`](../INTRA_FUNCTION_IMPLEMENTATION.md)).
- **Replace** this function's entry in `db` with just the freshly
  extracted signature(s) — replace, not accumulate.  A cyclic SCC
  re-summarises the same function on every fixpoint lap; accumulating
  would pile up several same-arity signatures for one function and
  make `find_by_arity` give up.

Finding detection does *not* happen here — it is Phase 2's job, run
once over the converged database.

`detect_findings` in Phase 2 is gated by `is_target_file
rs.target_root_map`: findings emit at sinks in target files, not in
companion files.  This avoids reporting the same finding twice when
both the source file and the sink file are scan targets — the sink
target sees the flow via the signature database.

## What runs in parallel and what doesn't

- **Across rules:** parallel.  `Core_scan` schedules each
  `rule_state` as a separate task.  Rules don't share mutable state.
- **Within a rule, Phase 1 (signature fixpoint):** serial.  The
  database must converge callees-first; a cyclic SCC iterates
  internally until stable.  You can't extract a caller's signature
  before its (acyclic) callees'.
- **Within a rule, Phase 2 (finding emission):** order-independent —
  every callee already carries its final signature — but run serially
  as a plain fold for simplicity.
- **Within `build_rule_states`:** the *setup* phases (the two
  target parses — dispatch and extraction — spec extract, companion
  parse, rule-state init) are each one parmap.  Setup is more
  parallel than the fold because it has no topological dependency.

## Why SCCs, not a plain `Call_graph.Topo.fold`

A topological order is only well defined on a DAG.  `ocamlgraph`'s
`Topological.Make` still *produces* an order for a cyclic graph, but
the members of a cycle come out in an arbitrary order fixed by graph
construction — and since the project index is built in parallel, that
order is not even stable between runs.  Summarising a cyclic caller
before its mutual callee yields an incomplete, nondeterministic
signature.

Condensing to SCCs sidesteps this: the SCC condensation *is* a DAG, so
processing SCCs callees-first is always well ordered, and each cyclic
SCC is iterated to a fixpoint so its result no longer depends on the
within-cycle order.  `Graph_fixpoint` threads the signature database as
its fold accumulator (input `db`, output the updated `db`), so the pass
stays functional and lets us share the rule's state across rules
without locks ([§ 1](01-architecture.md) "State lifetimes").

## A note on bodiless / fallback signatures

When Phase 1 reaches a function whose body is not analysable (FBDecl
with no implementations, or a function the engine cannot extract a
useful signature for), `db` simply does not gain an entry for it.  Callers that subsequently hit a call to
that function fall back to **conservative propagation** in
`Sig_inst.instantiate_function_signature`: if the actual argument expression
maps to one of the caller's own parameters (a `BArg`), the
incoming effect is preserved with the *caller's* parameter index;
otherwise it is dropped.  Dropping is the only sound choice —
preserving with the inner function's parameter index would alias
the effect to an unrelated parameter and propagate phantom taint
through every subsequent call frame.  See [§ 6](06-subtleties.md).

[Next: § 4, the per-language hooks that make this work for Python,
Go, Ruby, TypeScript, and the rest.](04-language-quirks.md)
