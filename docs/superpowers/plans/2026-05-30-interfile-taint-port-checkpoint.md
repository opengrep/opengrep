# Interfile Taint — Port onto main's new engine (checkpoint)

Branch: `port/taint-interfile-on-main` (off `main`, the rewritten taint engine).
Goal: Semgrep-Pro-parity interfile taint for every taint-capable language.

## Status: 81 / 88 e2e interfile fixtures passing (was 5 at session start)

(+2 since the 79 checkpoint: `map_service_container` via `new Map([[k,v]])`
detection, and `provider_spec_registration_container` via object-form provider
specs `register("k", { useValue: ... })`.)

All progress is regression-free against intra-file taint, search, and the
single-file cross-file probes (Go multi-hop, JS object methods, etc.).

## How interfile works here

Files are parsed once, grouped into **import-connected components**
(same-directory union + resolved import edges), and the interprocedural taint
engine runs once per component over its concatenated AST (original per-file
positions preserved), then findings are distributed back per file.

## Commits this session (in order)

1. `cfce12e` resolve cross-file imported object-method signatures in calls
   (consult `imported_global_cell_of_lval` in `check_function_call`'s shape
   fallback).
2. `40db72b` key top-level method-call graph edges at the method token
   (`extract_toplevel_calls` matched `extract_calls`).
3. `cdec093` resolve nested instance-method calls `this.field.method()`
   (added the multi-offset case to `lookup_signature_with_object_context`).
   **+23 fixtures** — the DI backbone.
4. `77e2b00` disambiguate interfile match dedup by real file location
   (`PM.uniq` digest now includes `range_loc` file).
5. `f8de42a` prepend receiver for Python method calls with explicit `self`.
6. `37025ca` group interfile files into import-connected components.
   **+40 fixtures** — fixes multi-module conflation.

## Remaining 9 failures (precise root causes)

- **Cross-file sanitizers** (`python_sanitizer`, `python_side_effect_sanitizer`,
  want 1 got 2): a sanitizing helper `def sanitize(v): return clean(v)` extracts
  an **empty** signature (`v => {}`) because the `ToReturn` is dropped when the
  sanitized return carries no taint (`Taint_signature_extractor.extract_signature`
  keeps `ToReturn` only when `has_data_or_shape || has_control`). An empty sig
  then falls back to argument propagation. Interacts with callee arity in a way
  still not fully understood (a same-arity sibling helper triggers it; a
  different-arity sibling does not).
- **Merged-AST global-name conflation** (`imported_value_package_collision`
  want 2 got 4; `imported_python_module_value` want 3 got 2): two module globals
  with the same name in different files of one component share taint in the env
  (global/`BGlob` keyed by name, not file). Import *resolution* is correct
  (`find_exported_global_cell` is file-qualified) but the value is already
  tainted upstream.
- **`python_module_imports`** (want 2 got 1): `import source; source.f()` (plain
  module name) resolves; `import source as s; s.f()` works — the non-alias direct
  module-qualified call drops one finding.
- **`commonjs_require`** (want 3 got 1): `const x = require("./m")` /
  `module.exports = function(){}` whole-module export binding not resolved (only
  `module.exports.api = {...}` object-member path works).
- **`language_matrix`** (want 28 got 23): 5 languages still missing cross-file
  (move_on_aptos, ocaml, r, scheme, swift, vue — each its own quirk).
- **DI edge cases** (`map_service` want 4 got 3; `provider_spec` want 6 got 3):
  specific provider-registration shapes still partial.

## KEY ROOT CAUSE: merged-AST byte-range collisions in spec matching

`Match_taint_spec.any_is_in_matches_OSS` decides if an IL node is a
source/sanitizer/sink by **pure byte-range containment** (`Range.($<=$) r
rwm.r`), with **no file check**. The merged component AST concatenates files
that each keep their own 0-based byte offsets, so a node in file A can fall
inside a spec match's range in file B. Consequences:

- **False positives** (the sanitizer + `package_collision` failures): a
  helper's body byte-aligns with an unrelated `sink(...)` in another file, so
  its signature gets a spurious `ToSink`. Confirmed by padding one file (which
  shifts offsets) — the false finding disappears.
- **Spurious passes** (currently masking real gaps): `elixir`, `java_unqualified_field`,
  and `python_duplicate_names` only pass because a `source()`/sink byte-aligns
  across files. Their *real* cross-file flows do not actually produce the
  finding.

A file-aware guard (require `node_file = spec_file`, comparing the node's token
file to `rwm.origin.range_loc` file) is the correct fix and removes the false
positives — but it is **net −2 on the suite right now** because it also removes
the 3 spurious passes, and it needs two follow-ons before it is net-positive:
1. Make the source/sink **spec-match dedup file-aware** too (identical files
   like `duplicate_names` collapse their two same-range sinks into one).
2. Fix the **real** cross-file flows for `elixir` (module-qualified calls) and
   `java_unqualified_field` (constructor-set field read through a method), which
   were only collision-passing.

## Turn update: elixir fixed genuinely; byte-collision is multi-point; java pinned

- **Elixir cross-file module calls now resolve genuinely** (commit after `32e6e5f`):
  `Elixir_to_generic` emits `G.ModuleDef`, not `G.ClassDef`, so
  `Visit_function_defs` never registered module functions under the module
  name. Treating `ModuleDef` as a class boundary fixes `Module.func()` resolution
  (elixir was previously only passing via the byte-collision). Also resolved
  `move_on_aptos` in the matrix. No regression.
- **The byte-collision is multi-point.** Beyond `any_is_in_matches_OSS`, the
  relevant-graph builder `Graph_from_AST.find_functions_containing_ranges` is
  also byte-only, so `duplicate_names`' two identical `main`s collapse to one
  analyzed function. A complete fix must make **both** file-aware. Measured: the
  file-aware spec-match alone is net −1 at the 81 base (fixes sanitizer; breaks
  duplicate + java; elixir no longer regresses).
- **java unqualified field pinned**: `value` resolves to `G.EnclosedVar` (Java
  naming marks it a field). Modeling `EnclosedVar` as `this.field` in
  `AST_to_IL.lval_of_id_info` fires but is **inert** — the remaining gap is
  sid-consistent offset matching between the constructor's `this.value` and the
  method's `this.value`, plus the constructor→instance field-shape flow. Deep.

## Next highest-leverage work

1. The byte-collision file-aware fix above (correctness; fixes the 2
   false-positive fixtures) plus its 2 follow-ons (recovers the 3 spurious
   passes properly). This is the single most important correctness item.
2. Cross-file sanitizers also relate to (1): once spec matching is file-aware,
   `sanitize`'s spurious `ToSink` disappears (verified locally).
3. Remaining per-feature/per-language gaps: commonjs `require`, the 5 matrix
   languages, the direct+alias module-import naming conflation, and the two
   partial DI registration shapes.
