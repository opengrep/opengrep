# 6. Subtleties

This document catalogues situations the code handles that are not
obvious from a casual read.  Read [§ 1–4](README.md) first — the
vocabulary here assumes you know what `FBDecl`, the relevant
subgraph, `Type_state.t`, and the signature fixpoint are.

The subtleties group into four buckets:

- **A. Signature instantiation.**  Cases in `Sig_inst` and the
  signature fixpoint where the naive thing is wrong.
- **B. Callee resolution.**  Cases in projidx's `identify_callee`
  and friends where bare name matching gives the wrong answer.
- **C. Subgraph construction.**  Why the relevant subgraph is
  shaped the way it is.
- **D. Performance shape.**  Algorithms that look quadratic and
  aren't.

---

## A. Signature instantiation

### A1. Dropping vs preserving `ToSinkInCall` for unresolved callbacks

**Where:** `src/tainting/Sig_inst.ml`, the unresolved-callback branch of
`instantiate_function_signature`.

When `Sig_inst` instantiates a function signature and hits a
`ToSinkInCall { callee = fun_exp; arg = fun_arg; ... }` effect, the
callback `fun_exp` may or may not resolve to a parameter of the
enclosing function.

- If it resolves to a parameter, the effect should be preserved
  using the **enclosing function's** parameter index (the `BArg`
  the callback is bound to).
- If it does not resolve to a parameter (it's a lambda, a local
  variable, or an unresolvable name), the effect must be
  **dropped**.  The naive alternative — preserving with the inner
  function's `fun_arg.T.index` — would alias the effect to a
  completely unrelated parameter of the enclosing function and
  propagate phantom taint through every subsequent call frame.  In
  interfile mode, where the effect chain spans many functions,
  this accumulates exponentially.

Concretely:

```ocaml
let arg_opt = (* try to find a BArg the callback is bound to *) in
match arg_opt with
| Some updated_arg ->
    [ ToSinkInCall { callee = exp; arg = updated_arg; args_taints } ]
| None ->
    []   (* drop the effect — preserving would be unsound *)
```

This case is what makes intrafile and interfile diverge here:
intrafile usually has resolvable callbacks (everything is in one
function and `BArg`s match), interfile routinely hits unresolvable
callbacks at the seams between files.

### A2. Pre-merge dispatch, not on-the-fly

**Where:** `src/engine/Interfile_dispatch.ml`,
`dispatch_merge_fbdecl`.

When the signature fixpoint reaches a bodiless interface method
(`FBDecl`), it does **not** extract a signature from the empty
body.  An empty signature stored in `db` makes the function look
like a no-op effects-wise; callers would see "no taint propagation"
instead of falling back to conservative propagation.  That is
unsound.

Instead, Phase 1:

1. Looks up the function's `Dispatch` predecessors (implementations)
   in `rs.relevant_graph`.  Filters out self-edges — the interface
   declaration carries a Dispatch edge to itself; including its own
   empty body as an implementation would pollute the merge.
2. If none of the implementations have signatures in `db` yet, this
   vertex is skipped for now.  `db` stays unchanged.  Callers fall
   back to conservative propagation (A1) when they hit a call to this
   vertex — and if the interface and its impls sit in the same cyclic
   SCC, a later fixpoint lap revisits it once the impls are ready.
3. If some implementations are available, `Sig_inst.merge_dispatch_signatures`
   merges them into a single signature, keyed at the interface's
   `Function_id.t`, and the result replaces the empty skeleton.

The merge itself:

- Strips leading "receiver" params from each implementation when
  its param count exceeds the interface's (Go method values carry
  the receiver as an extra leading param).
- Filters effects depending on `BGlob`: implementations reference
  their own globals; those would resolve incorrectly at the
  interface call site.
- Remaps each implementation's `BArg` indices to the interface's
  canonical parameter positions.
- Unions all the effect sets.

The reason for **pre-merge** (vs an on-the-fly lookup at every call
site) is that the signature fixpoint makes the implementations'
signatures available in `db` before callers of the interface are
summarised (for an acyclic dispatch, on the callees-first pass; for a
cyclic one, once the SCC converges).  One merge per interface beats
one merge per call site, and composes cleanly with the
conservative-propagation fallback.

### A3. Recursive instantiation cache

**Where:** `src/tainting/Sig_inst.ml` `recursive_cache`.

The same callback expression can appear in many `ToSinkInCall`
entries across a single signature being instantiated.  Without
memoisation, each occurrence independently walks the full nested
call chain, driving exponential blow-up.  `recursive_cache` keys on
`(callee_str, args_taints)` (structural equality on args_taints)
and lives on the per-call instantiation context so it does not leak
across unrelated calls.

### A4. Enclosing-receiver field writes via a callee (`BThis`)

**Where:** `src/tainting/Dataflow_tainting.ml`, the `ToLvalThis`
effect branch; `src/tainting/Sig_inst.ml`.

A method can taint a field of its *own* receiver indirectly, by
calling another method on the same instance — `self.helper()` or
`super().__init__()` whose body does `this.f = tainted`.  The write
lands on the enclosing receiver, not on a parameter or a global, so
it can't be expressed as a `BArg`/`BGlob` effect.

`Sig_inst` carries it as a `ToLvalThis` effect, which
`Dataflow_tainting` records against the `Taint.BThis` base.  That
composes the field write into the *calling* method's own signature,
so a later call to the caller on a same-instance receiver sees
`this.f` as tainted.  Without `BThis` the write would be dropped at
the callee boundary and the taint would vanish across the
`self.helper()` / `super().__init__()` seam.

---

## B. Callee resolution

### B1. Cross-package interface dispatch (Go)

Large Go projects routinely have many packages each defining
`type Service struct { ... }` with a `Get(...)` method.  When a
caller writes `s.Get(...)` where `s` is `*foo.Service`, the
resolver must pick the right `Get` — not "any function named Get
in the project".

The shared disambiguation pipeline:

1. **Same-file filter:** prefer candidates in the caller's file.
2. **Same-dir filter:** prefer candidates in the caller's
   directory (= same Go package).
3. **Interface match:** if `s`'s declared type is an interface,
   route to its FBDecl.
4. **`pick_by_arity` fallback:** match on parameter count.

This pipeline runs at both shapes that produce method calls in Go:

- The catchall `DotAccess` branch in `identify_callee`
  (e.g. `obj.method()` where `obj` is a dotted expression).
- The bare-`Id` receiver branch (e.g. `s.Get(...)` where `s` is a
  plain identifier).

Both branches must apply the same disambiguation, otherwise one of
them silently drops candidates as ambiguous via `pick_by_arity`.

### B2. Go interface method bodies

Go's tree-sitter parser emits

```
DefStmt(entity, TypeDef { tbody = NewType (TyRecordAnon (Interface, fields)) })
```

for `type Foo interface { Bar(); Baz(); }`.  The default
`iter_no_id_info` visitor does **not** recurse into the
`TyRecordAnon` field list, so without intervention the interface's
methods never get `current_class = Some Foo` set.

`Visit_function_defs` has a branch that, when it sees a Go interface
TypeDef, manually walks the field list and calls
`self#visit_field f field` for each, with `current_class`
save-excursion'd to `Some iface_name`.  This makes interface
methods get `fn_id = [Some Iface; Some method]` and land in
`Type_state.methods[Iface]`.

### B3. Go interface embedding

```go
type Authenticator interface {
    Authenticate(ctx, req) error
}

type Client interface {
    Authenticator        // embed
    Name() string
}
```

The embed parses as a Spread expression field, **not** a FuncDef.
Without extra work, `Client`'s own methods are only `Name` — the
inherited `Authenticate` is missing from `Type_state.methods[Client]`.

projidx builds a project-wide `embeds : interface_name → embedded_iface_name list`
map by walking Type_def observations and BFS-transitively gathers
every inherited `func_info`, re-emitting them under the embedder's
class name (with `fn_id` rewritten to `[Some Embedder; Some method]`).
After this, `find_methods(Client, Authenticate)` resolves to
`Authenticator`'s FBDecl.

### B4. PHP `Class::staticMethod()` without name-only fallback

PHP's static-call syntax `Class::staticMethod(...)` must resolve to
a static method on `Class` specifically.  The obj.method() site in
`identify_callee` therefore returns `None` (rather than a bare-name
project-wide fallback) when the imported-name lookup fails.  A
bare-name fallback would let `Class::staticMethod(...)` aliases to
methods on completely unrelated classes that happen to share the
leaf name, which is precision-destroying in practice.

### B5. Top-level HOF callbacks are collected structurally

`extract_toplevel_hof_callbacks` must classify a call site as
module scope (feeding the file's `top_level` vertex) rather than
inside a function body.  The classification is structural: it walks
the program with `Walker.fold_exprs_in_program
~skip_nested_fdefs:true`, so expressions inside function
definitions are never visited and every call it reaches is module
scope by construction — no position arithmetic is involved.
Operator pseudo-calls (`IdSpecial (Op _)` callees, e.g. PEP 604
`int | None` unions) are filtered out; they would otherwise emit
spurious callback edges.

Result: patterns like Go's `wire.NewSet(handler1, handler2)` at
module scope emit as top-level edges to each handler.

### B6. Lambda VarDef visibility

Lambda VarDefs (`const X = () => ...` in TS/JS, `x = lambda: ...`
in Python) are valid call targets.  They have call-graph vertices
(Phase 1 includes them), but a callsite-by-name resolver also needs
them in the per-file `funcs_by_name` visibility set.  The visitor
that builds the visibility set treats `Func_def`, `Class_def`, and
`Var_def` (with a lambda RHS) uniformly via the
`Walker.Observation.t` variant — there is no `FuncDef`-only branch.

### B7. `set_parent` / `set_module_singleton` are last-wins

When a simple class name appears in many files (Go's flat
namespace, or Python codebases that reuse `Unsupported` /
`Manager` / `Error` as class names across files), the
`Type_state.set_parent` setter is **last-wins** — the most recently
processed file's binding takes precedence.  This matches the
`Hashtbl.replace` semantics that the baselines were captured
against; a first-wins setter silently loses edges by retaining an
earlier binding that doesn't reflect the eventual reality.

Same reasoning applies to `Type_state.set_module_singleton`.

### B8. Constructor calls resolve to the constructor method

**Where:** `src/tainting/Graph_from_AST.ml`, `ctor_candidate_funcs`
and `resolve_constructor_from_type`.

A constructor call names the *class*, never the constructor method:
`Foo()` (Python/Kotlin/Scala), `new Foo()` (Java/C#/JS/TS),
`Foo.new()` (Ruby/Crystal).  The callee leaf is `Foo` or `new`, so
the usual leaf-narrowed candidate list never contains the
constructor (`__init__`, `initialize`, the C++ class-named ctor,
…).

`identify_callee` re-narrows by the language's
`constructor_names` (from `Lang_config`) to recover the candidate
set, then resolves to the constructor declared on the named class.
The Ruby/Crystal `ClassName.new(args)` shape is handled at the
`obj.method()` site: when the method leaf is `new`, it synthesises
the receiver type and routes to the same constructor resolution.
This is what lets taint flow into a constructor's body and out
through `this`/`self` fields (see A4).

### B9. Block-syntax calls resolve the inner callee

**Where:** `src/tainting/Graph_from_AST.ml`, the `Call` branch that
unwraps `Call(Call(f, args), [block])`.

Ruby/Crystal/Scala `f(real_args) do |x| ... end` parses as
`Call(Call(f, real_args), [block_lambda])` — the outer call's callee
is itself a `Call`.  Resolving the edge against the outer node would
fail to link `f`.  The branch detects the
`(inner Call) applied to a single lambda arg` shape (for Ruby,
Crystal, Scala) and resolves the call edge against the **inner**
call, so `f` gets its edge while the `do |x|` block is still
collected as a HOF callback.  (Bare generics `foo<T>(...)` in
C++/Rust are handled nearby by rerouting to the plain `N (Id)`
resolution path, which re-narrows from the project-wide index.)

---

## C. Subgraph construction

### C1. Dispatch closure

A bodiless interface method can end up in the relevant subgraph
through the source→sink BFS.  Without further work, signature
extraction at that vertex would yield an empty signature that
"poisons" callers (they see no effects instead of conservative
fallback).

The fix is in the BFS itself: after the source/sink-driven
common-ancestor computation, iterate up to `max_depth` times — for
every interface vertex in the current set, add its `Dispatch`
predecessors (concrete implementations).  Repeat because newly
added implementations may themselves call other interface methods.

Lives in `Graph_reachability.compute_relevant_subgraph`'s
`with_impls` augment loop.

### C2. Dispatch-free depth budgeting during BFS

The two edge kinds (`Call_graph.edge_kind` is just `Call | Dispatch`)
are not charged the same way against the BFS depth budget.  A
`Dispatch` edge (concrete implementation ↔ interface method) is a
choice of body, not an extra call frame, so charging it would exhaust
the budget prematurely on chains like
`Impl → Iface → Impl' → Iface' → …`.

`iter_succ_e_either` / `iter_pred_e_either` in `Graph_reachability`
walk both the per-file and project-wide graphs; the BFS then expands
`Dispatch` neighbours "for free" (depth unchanged) and only charges
depth for `Call` edges, stopping Call expansion once the budget runs
out.  The subgraph stays bounded; runtime stays sane.

### C3. `taint_inst` per file, not per scan

Inside `init_file` we build a `Taint_rule_inst.t` **per file**, not
per scan.  When a file has no source/sink/sanitiser/propagator
matches for the rule, we still create a `taint_inst` for it — but
with empty predicates, not the predicates from another file.

The reason is byte-position collision.  `Range.t` carries only byte
positions, no file path.  If we shared one file's `taint_inst`
across all files, the wrong file's match ranges would collide with
unrelated code at the same byte offsets in other files and produce
spurious source/sink hits.  Empty predicates for non-matching files
let signature extraction still run (parameter flow is tracked)
without inventing matches.

---

## D. Performance shape

These are places where the naive shape would be too slow to be
usable.  The implementation hits the listed shape via explicit
algorithm choices.

### D1. File discovery: single `Find_targets` path, scan-policy aware

`Discover.discover_files` always delegates to
`Find_targets.get_target_fpaths`.  An earlier revision had a `git
ls-files`-based fast path that skipped `Find_targets` for git roots
to avoid `lstat`-per-file overhead, but it diverged from Semgrep's
target-discovery semantics (it ignored `.semgrepignore`, size caps,
explicit-target behaviour, etc.) and was removed.  The interfile
engine passes the scan's own `Find_targets.conf` to projidx so the
two systems agree on which files exist; the cost of the extra
walking is paid once per `(lang, root)` per scan —
`Interfile_dispatch` groups all of a language's rules onto one
build, so nothing needs caching.

### D2. `identify_callee` lookups are O(1)

Three sites in `Graph_from_AST.identify_callee` need to ask
"which project-wide functions match this leaf name?":

- HOF candidate filter in `extract_hof_callbacks_from_call`.
- Bare-name uniqueness fallback in `try_unique_callee`.
- Method-name uniqueness fallback in `try_unique_method_call`.

Each is a structural query (does the callee declare a
function-typed param? is there exactly one project-wide candidate?)
that needs the unfiltered candidate set — not the per-file
visibility-narrowed `funcs_by_name`.

`Func_lookup.t` therefore carries **two** name indexes:

- `funcs_by_name` — project-wide, narrowed per-file to visible names.
- `project_funcs_by_name` — project-wide, no narrowing.

The three lookup sites use `project_funcs_by_name` and get O(1)
hashtable hits instead of linear scans over tens of thousands of
project funcs.

### D3. Interface dispatch matching is O(I + C)

The dispatch phase needs to find, for every interface `I`, the
concrete classes that implement it.  The naive shape is
O(I × C × |I|) — for each interface, for each concrete class, for
each method of the interface, ask "does the class have this
method?".  On a project with hundreds of interfaces and thousands
of concrete classes this is millions of probes.

The implementation uses two optimisations:

- **`methods_in_file` memoised** per `(class_qn, file)`.  The inner
  "does class C have method M?" probe becomes a hashtable hit.
- **Rarity-pivot match.**  Build a project-wide
  `(method_name, arity) → concretes` index once.  For each
  interface `I`, pick `I`'s rarest method as a pivot; the candidate
  concretes are exactly `index[pivot]`, typically 1–2 entries.

Together the dispatch phase is effectively O(I + C).

### D4. Parallel phases via `Domainslib_.parmap`

projidx Phase 2 (per-file edges) and `Interfile_dispatch`'s setup
phases (target parse, spec extract, companion parse, rule-state
init) all run as parmaps.  The unit of parallelism is one file in
Phase 2 and one rule in `build_rule_states`; both grain sizes give
good throughput on multi-core boxes.

The signature fixpoint inside `run_rule` is the one explicitly serial
phase — the signature database must converge callees-first (with cyclic
SCCs iterating internally).
Parallelism across rules within `run_rule` execution is provided by
the outer parmap scheduling each `rule_state` as its own task
alongside per-target tasks.

---

## Cross-references

If you got here looking for a specific subtlety and need broader
context first, the bullets above reference these concepts; they're
defined in:

- `FBDecl`, dispatch merge, signature fixpoint → [§ 3](03-dispatch.md).
- `Type_state.t`, `Func_lookup.t`, projidx phases → [§ 2](02-call-graph.md).
- Per-language reshapes (Go interfaces, Ruby `attr_reader`, Rust
  `impl`) → [§ 4](04-language-quirks.md).
- The flags and tools (`opengrep-interfile-graph` `lookup` /
  `edges` / `relevant-graph`) you'd use to debug any of the above
  → [§ 5](05-cli-and-tools.md).
