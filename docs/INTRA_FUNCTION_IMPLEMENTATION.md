# Opengrep Cross-Function Tainting (Intrafile)

## Overview

This document explains how Opengrep performs intrafile cross-function taint
analysis when the `--taint-intrafile` flag (or per-rule option) is enabled.
The implementation hinges on four cooperating subsystems:

1. **Object initialization detection** (`src/tainting/Object_initialization.ml`)
   discovers constructor calls so we can treat freshly created instances as
   trackable taint carriers.
2. **Call graph construction and SCC ordering**
   (`src/tainting/Graph_from_AST.ml`) builds a dependency graph of
   function calls; the engine then orders its strongly-connected components
   so that callees are processed before callers.
3. **Signature extraction** (`src/tainting/Taint_signature_extractor.ml`)
   runs a focused dataflow on each function/constructor in topological order
   to capture how taint flows through parameters, fields, globals, and return
   values.
4. **Signature instantiation** (`src/tainting/Sig_inst.ml`) replays the stored
   signatures at every call site so the engine knows how taint should be
   propagated interprocedurally within the same file.

These components are wired together from
`Match_tainting_mode.check_rule` (`src/engine/Match_tainting_mode.ml`) when the
current rule or run configuration sets `taint_intrafile = true`. The analysis
proceeds in two passes: first extracting all function signatures in topological
order, then running the full taint analysis with the complete signature database.

## Object Initialization Detection

File: `src/tainting/Object_initialization.ml`

- **Purpose:** Identify expressions that construct objects (e.g., `new Foo()`)
  so we can attach taint to instance fields consistently across method calls.
- **Key exports:**
  - `detect_object_initialization : G.program -> Lang.t -> object_mapping list`
    scans the AST and returns `(variable_name, class_name)` pairs.
  - `is_constructor : Lang.t -> string -> string option -> bool` determines if a
    given function name acts as a constructor for the current language.
- **How it works:**
  1. Collect all class/struct names in the file (`collect_class_names`).
  2. Traverse variable declarations and assignments looking for constructor
     shapes. Each language registers its own `constructor_pattern` containing a
     matcher (e.g., `G.New` for Java/C#, `Call` for Python) plus metadata such
     as recognized constructor method names.
  3. Emit mappings whenever an l-value is initialized using a recognized
     constructor. These mappings are stamped onto the AST as `id_type`
     annotations so that instance variables (`this.x`, `self.y`) can be
     associated with the correct class context during analysis.

The object mappings are computed and stamped onto the AST before the call
graph is built:

```ocaml
Object_initialization.(
  stamp_id_types (detect_object_initialization ast lang) ast);
let call_graph = Graph_from_AST.build_call_graph ~lang ast in
```

## Signature Extraction

File: `src/tainting/Taint_signature_extractor.ml`

- **Goal:** Build a reusable summary (`Shape_and_sig.Signature.t`) describing
  how taint entering a function can reach sinks, returns, globals, or fields.
- **Entry point:**
  `extract_signature_with_file_context` — wraps `extract_signature` with
  additional AST context (class name, method properties) and stores the result
  in a `Shape_and_sig.signature_database`.

### Call Graph and Topological Ordering

To ensure that function signatures are available when needed (i.e., when a
function calls another function, the callee's signature should already be
computed), we use a **call graph** to determine the analysis order:

1. **Build the call graph** (`Graph_from_AST.build_call_graph`):
   - Traverse the AST to identify all function definitions and their calls
   - Create a directed graph where an edge from `f` to `g` means "`f` calls `g`"
   - Each node is identified by a `Function_id.t`
   - Handle method calls, direct function calls, and constructor invocations

2. **Condense into SCCs and order callees-first**:
   - Condense the call graph into its strongly-connected components and
     process them bottom-up (leaf callees first, callers last)
   - Acyclic functions form singleton SCCs analyzed exactly once
   - A cyclic SCC (mutual / indirect recursion) has no valid internal
     callees-first order, so it is *iterated* to a fixpoint: its members
     are re-analyzed until every member's signature stops changing.  This
     removes the order-dependence that a plain topological walk would have
     inside a cycle, and is driven by the same generic
     `Graph_fixpoint` engine as the interfile loop

3. **Analyze in that order**:
   - Extract each function's signature, replacing (not accumulating) its
     entry so repeated fixpoint laps don't pile up several same-arity sigs
   - By the time a function is analyzed its callees carry final signatures
     (for a cycle, after the SCC converges), so signature instantiation
     resolves correctly during the dataflow analysis

The call graph building happens in `Match_tainting_mode.check_rule` before the
signature extraction loop:

```ocaml
let call_graph = Graph_from_AST.build_call_graph ~lang ast in
(* ... narrowed to the source/sink-relevant subgraph [relevant_graph] ... *)
let sccs_callees_first =
  List.rev (Call_graph.SCC.scc_list relevant_graph)
in
let signature_db_after_order =
  Engine.run ~sccs:sccs_callees_first ~graph:relevant_graph ~analyze
    initial_signature_db
```

Here `Engine` is an instance of `Graph_fixpoint.Make`, and `analyze` extracts
a single function's signature(s) and replaces that function's entry in the
database.

### Signature Extraction Algorithm

Once we have the proper analysis order, for each function we:

1. **Assume taint sources for analysis:**
   - `mk_param_assumptions` marks each parameter as tainted using `BArg` base
     values so the fixed-point can discover flows from arguments. Additionally,
     it checks if any parameter matches a source pattern (e.g., function parameters
     themselves can be sources) and creates `Taint.Src` taints accordingly.
   - `mk_method_property_assumptions` adds taint for `this./self.` fields that
     were previously discovered via `extract_method_properties`.
   - Optional `in_env` provides additional seed taints (e.g., globals).
2. **Run intraprocedural dataflow:** call `Dataflow_tainting.fixpoint` with
   the constructed environment, producing effect sets (`ToSink`, `ToReturn`,
   `ToLval`, etc.) and the final l-value mapping.
3. **Filter and annotate effects:**
   - `extract_param_labels_from_sink` gathers which parameters must be tainted
     for a sink effect to fire; this becomes a precondition list stored in the
     signature.
   - `ToReturn` and `ToLval` effects are filtered inline: taints that cannot
     materialize a value (shape variables) are dropped, and effects left with
     no relevant data, shape, or control taint are discarded.
4. **Persist signature:** the finalized signature and mapping are added to
   the database keyed by `Function_id.t` (a `Shape_and_sig.FunctionMap`).

### Why class properties matter

For methods that read/write instance fields, we extract expressions such as
`this.password` or `self.token` so that the fixed-point can see taint entering
or leaving through those properties even when they are set in constructors.

## Signature Instantiation (Function Instantiation)

File: `src/tainting/Sig_inst.ml`

- **Purpose:** When the engine encounters a function call, we consult the
  signature database to determine how taint should propagate from actual
  arguments to the callee's effects.
- **Main routine:** `instantiate_function_signature`.
- **Key steps:**
  1. Retrieve the pre-recorded signature using the callee's
     `Function_id.t`.
  2. Evaluate the signature's preconditions (`Precondition.solve`) against the
     current call-site taint state. If they fail, the signature simply does not
     contribute any effects.
  3. Substitute formal parameter references (`BArg index`, `BThis`, globals)
     with the actual call arguments, leveraging helpers such as
     `instantiate_lval_using_actual_exps` and `instantiate_lval_using_shape`.
  4. Emit concrete `Effect.t` instances (sink hits, tainted returns, tainted
     l-values) which are merged into the surrounding dataflow state.

### Interaction with the main engine

During intrafile analysis, `Match_tainting_mode.check_rule` uses the call graph
to determine the order in which to extract signatures and analyze functions:

```ocaml
(* Step 1: Build the call graph *)
let call_graph = Graph_from_AST.build_call_graph ~lang ast in

(* Step 2: Converge the signature database over SCCs, callees first *)
let sccs_callees_first =
  List.rev (Call_graph.SCC.scc_list relevant_graph) in
let signature_db_after_order =
  Engine.run ~sccs:sccs_callees_first ~graph:relevant_graph ~analyze
    initial_signature_db in

(* Step 3: Emit matches by re-checking each function in topological
   order against the converged database *)
List.fold_left
  (fun matches node ->
    match Shape_and_sig.FunctionMap.find_opt node info_map with
    | None -> matches
    | Some info ->
        let _db, findings =
          extract_and_check ~db:signature_db_after_order
            ~detect_findings:true (* ... *) info
        in
        List.rev_append findings matches)
  [] analysis_order
```

The key insight is that signature extraction and taint analysis are
**two separate passes**:

1. **First pass (signature extraction):** Analyze all functions in SCC
   callees-first order — cyclic SCCs iterated to a fixpoint — to build their
   signatures. By the time function `f` is analyzed the signatures of all
   functions `f` calls are available (for a cycle, once the SCC converges).

2. **Second pass (taint analysis):** Run the full taint analysis with all
   signatures available. Each function is re-checked (`extract_and_check`,
   which runs `check_fundef_with_cfg`) against the complete `signature_db`,
   instantiating signatures at call sites via
   `Sig_inst.instantiate_function_signature`.

This two-pass approach ensures that cross-function taint flows are correctly
captured, as the signature database is fully populated before we look for
actual taint violations.

## Summary

The `--taint-intrafile` implementation consists of several cooperating phases:

1. **Object detection** stamps constructor knowledge onto the AST (as
   `id_type` annotations) so that instance fields map to concrete classes.

2. **Call graph construction** builds a directed graph of function calls to
   determine dependencies between functions.

3. **SCC ordering** condenses the call graph into strongly-connected
   components and orders them callees-first, so signatures are available when
   needed; cyclic SCCs are iterated to a fixpoint rather than ordered
   arbitrarily.

4. **Signature extraction** (first pass) performs a dedicated fixed-point per
   function in that order to learn how taint moves through arguments, returns,
   globals, and fields. This includes checking if function parameters
   themselves match source patterns.

5. **Taint analysis** (second pass) runs the full taint analysis with the
   complete signature database, instantiating signatures at call sites to
   propagate taint across function boundaries.

6. **Signature instantiation** reuses the pre-computed summaries at call sites,
   providing a lightweight intrafile interprocedural analysis without
   re-analyzing the callee each time.

Together these components deliver the behavior behind `--taint-intrafile`,
allowing Opengrep to capture cross-function taint flows within a single file
while maintaining predictable performance.

## TODO

1. Inheritance is not yet supported, for example the following gives a false negative:
```python
class BaseClass:
    def __init__(self):
        self.name = source()

class ChildClass(BaseClass):
    def method(self):
        sink(self.name)
 
```
