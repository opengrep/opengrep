# Interfile Taint — Port onto main's new engine (checkpoint)

Branch: `port/taint-interfile-on-main` (off `main`, the rewritten taint engine).
Goal: Semgrep-Pro-parity interfile taint for every taint-capable language.

## Status: 79 / 88 e2e interfile fixtures passing (was 5 at session start)

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

## Next highest-leverage work

1. Cross-file sanitizers (2 fixtures) — make `extract_signature` retain an
   explicit "returns untainted" `ToReturn` so an analyzed sanitizer does not
   fall back to propagation; investigate the arity interaction.
2. File-qualify global taint keying for merged ASTs (2–3 fixtures) — the deepest
   item; risk of regressing the value tier that matches globals across files.
