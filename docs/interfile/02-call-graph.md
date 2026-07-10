# 2. The call graph: projidx

`projidx` (formal name: `opengrep_project_index`) is opengrep's
in-process call-graph builder.  It lives in `src/project_index/`.

For interfile taint analysis it is invoked **as a library**, never as
a subprocess — see `src/engine/Interfile_graph.ml`.  For debugging
and offline graph inspection, `opengrep-interfile-graph index`
exposes the raw index ([§ 5](05-cli-and-tools.md)).

## What the call graph is

A `Call_graph.G.t` — a bidirectional labelled directed graph from
`ocamlgraph`.

- **Vertices** are `Function_id.t`, a record carrying the function
  name and the file/line/column of its declaration.  Two
  `Function_id.t` compare equal iff `(name, file, line, col)` agree;
  byte offsets are not part of identity.  This matters: AST tokens
  and graph-derived ids must compare equal even when one has a real
  byte offset and the other has bytepos=0.
- **Edges** are *callee → caller* (note the direction!) labelled with
  a `Call_graph.edge`:
  ```
  type edge = { call_site : Pos.t; kind : edge_kind }
  type edge_kind = Call | Dispatch
  ```
  `Call` edges are concrete call expressions; `Dispatch` edges link
  interface declarations to their concrete implementations
  (interface ↔ impl).  The taint engine processes the two kinds
  differently — see [§ 3](03-dispatch.md) "FBDecl dispatch merge".

## The four-phase build

projidx's main entry point is `Opengrep_project_index.Main.collect`
(see `src/project_index/Main.ml`).  It runs four phases in order:

### Phase 0: Discover

`Discover.discover_files ~targeting_conf ~lang ~project_root
~includes ~excludes` returns every source file of the requested
language under the root.

Always delegates to `Find_targets.get_target_fpaths` — the same
discovery Semgrep itself uses — so projidx's file universe matches
the scan's target selection (gitignore, semgrepignore, size/minified
filtering, CLI `--include`/`--exclude`).  The interfile engine
passes the scan's actual `Find_targets.conf`; the
`opengrep-interfile-graph` tool uses
`Discover.projidx_default_targeting_conf`, a permissive variant that
disables size caps and `.semgrepignore` so large source files (e.g.
pytorch's 1.3 MB `common_methods_invocations.py`) still get indexed.

Per-language exclude globs come from `Lang_config.discover_excludes`.
TypeScript walks `tsconfig*.json` files and accumulates their
`exclude` arrays (matching what `scip-typescript` honours); other
languages return `[]`.

### Phase 1: Vertices and types

Phase 1 visits every AST and records two things:

1. **Vertex info** — every callable target (`Func_info.t` containing
   the function's `fn_id`, AST entity, and parsed `function_definition`).
   "Every callable" includes real `FuncDef` / `MethodDef` entries
   plus language-specific synthesised methods (Ruby
   `attr_reader :foo` declares an accessor with no AST FuncDef;
   projidx synthesises a `func_info` for it so call sites resolve).

   `fn_id` is `IL.name option list` — a path from outermost scope to
   the function's leaf name (`[None; Some foo]` for a free function,
   `[Some Cls; Some meth]` for a method, longer for nested defs).
   Consumers don't pattern-match the list directly; they go through
   `Func_info.as_method` / `Func_info.as_free`, which return the
   typed pair / leaf when the shape matches and `None` otherwise.
   This keeps the wire shape compatible with upstream
   `Visit_function_defs` while removing the silent-drop
   `| _ -> false` arms from every consumer site.

2. **The `Type_state.t` lattice** — a project-wide read-only
   structure mapping class names to methods, parents, fields, and
   return types.  Keys are typed wrappers from `Names`
   (`Class_name.t`, `Method_name.t`, `Field_name.t`, `Class_qn.t`,
   `Module_qn.t`) over private strings.

`Type_state.t` lives in `src/tainting/Type_state.ml` (not in
`src/project_index/`!) because both `Graph_from_AST`'s callee
resolver and the taint engine consume it.  Its shape:

```ocaml
type t = {
  inherited_methods : Func_info.t list Class_name_map.t;
  parent_class      : Class_name.t Class_name_map.t;
  module_singletons : G.name Module_qn_map.t;
  method_returns    : G.name Method_map.t;
  fields            : (Fpath.t * G.name) list Field_map.t;
  methods           : Func_info.t list Class_name_map.t;
  function_returns  : G.name Method_name_map.t;
  function_return_tuples : G.name option list Method_name_map.t;
  method_return_tuples   : G.name option list Method_map.t;
}
```

The `fields` map holds *all* `(defining_file, type)` pairs for each
`(class, field)` pair, not just one — Go's flat namespace lets many
packages declare `type Service struct { store ... }` with different
field types, and the chained resolver picks the entry from the
caller's package.

Population happens via two mechanisms:

- **Direct collection:** Phase 1 walks each file's AST via
  `Visit_function_defs.fold_with_parent_path` (for fdefs) and
  `Walker.walk_file` (for class defs, type defs, var defs, other
  defs).  Each observation is funnelled into the right `Type_state`
  setter.
- **Augmentation:** After the initial collection, `Main.ml` runs
  augmentation passes that extend the lattice with derived
  information.  Examples: walk class hierarchies for Python and
  attach inherited methods; build Go embedded-interface inheritance;
  stamp a var's class onto its occurrences' `id_type` when
  `x = f()` returns a known class (variable classes live on the AST,
  not in a side table).

**Constructor type inference is language-gated.**  When
`Type_infer.infer_expr_type` sees a bare call `foo()` with no known
return type, what it concludes depends on whether the language
constructs objects with a `new` keyword (`Lang_config.uses_new_keyword`,
re-exported as `Graph_from_AST.uses_new_keyword`):

- **No `new` keyword** (Python, Kotlin, Scala): `Foo()` *can* be a
  constructor, so the result is typed as an instance of `foo`.  This is
  what lets `Foo().bar()` resolve `bar` on class `Foo`.  The guess is
  self-limiting — if no class named `foo` exists the fabricated type
  resolves to no methods and is inert.
- **`new` keyword** (Java, C#, JS/TS, C++, PHP): a bare `foo()` is never
  a constructor (construction is `new Foo()`, a `G.New` node handled
  separately), so no type is fabricated — `infer_expr_type` returns
  *unknown*.  Fabricating one here would otherwise resolve `x.m()` to a
  same-named class's method and produce spurious cross-file edges.

The residual case in no-`new` languages — a function and a class that
share a name — is genuinely scope-dependent (which one `foo()` means
depends on the file's imports) and is left to resolve by registration
order; correctly disambiguating it would require import-aware inference.

`set_parent` / `set_module_singleton` are **last-wins**: when a
simple class name appears in many files, the last write determines
which entry the lattice carries.  This matches `Hashtbl.replace`
semantics and avoids losing edges when (e.g.) `Unsupported` is
declared in many files but only one is the "real" base.

`Type_state.equal` (used by the augment-pass fixpoint to detect
convergence) compares each binding's values structurally, including
the **full qualified path** of `G.name` values, not just their leaf
strings.  Two distinct qualified types with the same leaf (Go's
`pkg_a.Store` vs `pkg_b.Store`) must register as a change, otherwise
the fixpoint would converge prematurely on a stale type.

### Phase 2: Per-file edges (the parallel phase)

Phase 2 runs in parallel across `ncores` Domains via
`Domainslib_.parmap` with `chunksize = 1`; each work unit is a batch
of at most 500 files, which amortises Domainslib dispatch overhead
while keeping one task per thread (so the `Memprof_limits`-based
memory limit and timeout stay sound).  For each file the task calls
`Pipeline.edges_for_file ctx fi` which:

1. **Augments per-file object mappings** — turns `x = SomeClass()`
   and `x = factory_func()` (where `factory_func` is known to return
   a `SomeClass`) into `(x, SomeClass)` pairs so subsequent
   `x.method()` calls can resolve to `SomeClass.method`.

2. **Walks every function definition** and extracts call edges via
   `Graph_from_AST.extract_calls`.  Each callee resolution goes
   through `identify_callee`, which uses seven indexes bundled in
   `Func_lookup.t`:

   ```
   funcs_by_name             (* per-file: project-wide leaf → funcs, narrowed to visible names *)
   project_funcs_by_name     (* project-wide leaf → funcs, no visibility narrowing *)
   funcs_by_module_qn        (* M.foo qualified resolution *)
   alias_to_module_qn        (* per-file: import M as X *)
   same_file_funcs_by_name   (* per-file: defs in this file *)
   funcs_by_package          (* Go pkg.Func() resolution *)
   local_imports             (* per-file: import set *)
   ```

   The split between `funcs_by_name` (visible) and
   `project_funcs_by_name` (everything) matters: structural queries
   like "does this callee declare a function-typed param?" need the
   unfiltered set; in-scope name resolution needs the
   visibility-narrowed one.  Lookups are O(1) hashtable hits.

3. **Extracts top-level edges** — calls at module scope, decorators,
   and HOF callbacks registered at top level (`wire.NewSet(...)` in
   Go, route registrations in TS, etc.).

4. **Emits edges** through `Edge_emitter.t`.  The emitter handles
   the "top-level lambda lift": when the caller fdef is a
   module-scope anonymous lambda, the edge is duplicated with the
   file's synthetic `top_level` vertex as a second destination, so
   calls inside `(func() { ... })()` IIFEs still show up in the
   module's outgoing-edge set.

### Phase 3: Interface dispatch

The final phase adds `Dispatch` edges.  For each interface in the
project, we look up the concrete classes that implement it and emit
an `interface_method ↔ impl_method` Dispatch edge for each method.

The naive shape — for each interface I, for each concrete class C,
for each method M of I, ask "does C have M?" — is O(I × C × |I|),
which is millions of probes on large projects.  Two optimisations
make it tractable:

- **Memoise `methods_in_file`** per `(class_qn, file)`.  The inner
  loop becomes hashtable hits.
- **Rarity pivot.**  Build a project-wide
  `(method_name, arity) → concretes` index.  For each interface I,
  pick I's rarest method as a pivot; the candidate concretes are
  exactly `index[pivot]`, typically 1–2 entries.

Together these make Phase 3 effectively O(I + C).

## Output: graph plus relative paths, then absolutified

projidx emits vertices with file paths **as Find_targets returns
them**, which may be relative to the project root.  The taint engine
indexes graph vertices by absolute path.  The bridge is
`Call_graph.make_paths_absolute`, called once at the boundary in
`Interfile_graph.load_interfile_graph`:

```ocaml
let graph = Call_graph.make_paths_absolute project_root_abs graph in
```

After this every vertex in the cached graph has `Fpath.is_abs = true`
and target lookups (which use absolute paths) match without surprise.

## Per-language hooks: Lang_config.t

projidx is language-agnostic; everything language-specific lives in
the `Lang_config.t` value for that language.  See
[04-language-quirks.md](04-language-quirks.md) for the full list of
hooks and what each language overrides.

The key entries:

- `discover_excludes` — Phase 0 extra excludes.
- `class_def_reshape` — coerce Ruby `module M ... end` and Go
  `type T interface { ... }` into `ClassDef` shape so the visitor
  can treat them uniformly.
- `class_body_synth_methods` — Ruby `attr_reader` / `attr_accessor`
  declare accessor methods that have no AST FuncDef.
- `class_body_extra_parents` — Ruby `include Foo`, `extend Foo`,
  `prepend Foo` add Foo's methods to the class's MRO.
- `walks_inheritance` — gate the inheritance pass (Python yes,
  Go no).
- `include_anonymous_funcs` — gate adding lambdas/blocks as
  call-graph vertices.  Ruby: false (every `do |x| ... end` block
  becomes a function; vertex count and per-file work explode by 10×
  while adding little signal).
- `unqualified_scope` — `Per_file` (Python, Ruby, JS, Clojure),
  `Per_directory` (Go, C — sibling files share a scope keyed on
  the parent directory), or `Per_package` (Java, Kotlin, C#, C++ —
  sibling files share a scope keyed on the `(package x.y.z)` /
  `(namespace X.Y)` declaration in their AST, regardless of disk
  layout).
- `module_path_from_ast` — extract the file's module/namespace
  identifier from the AST as an override to the file-path-derived
  module key.  Java/Kotlin `package x.y.z;`, C# file-scoped
  `namespace X.Y;`, C++ outermost `namespace X { ... }`, and Clojure
  `(ns x.y.z)` all plug in here.  When `Some path`, two files with
  the same package end up under the same module key regardless of
  which directory they sit in.
- `normalize_import_specifier` — rewrite a raw import specifier
  before it becomes a module key.  C/C++ use this to strip
  `.h`/`.hpp`/`.hh`/`.hxx` from `#include "handler.h"` so it
  matches the `handler` module key derived from `handler.h`.

## The standalone view

The same `Main.collect` is exposed through the `index` subcommand of
`tools/opengrep-interfile-graph`.  Typical invocation:

```
$ bin/opengrep-interfile-graph index --lang go \
    --project-root /path/to/repo \
    --dump-edges > edges.tsv
```

This dumps every edge in TSV form (`callee_file:line:col<tab>caller_file:line:col<tab>call_site<tab>kind`)
where `kind` is `call` or `dispatch`.  Useful for spot-checking the
graph against a known corpus and diffing two runs.

[Next: § 3, how Interfile_dispatch consumes this graph.](03-dispatch.md)
