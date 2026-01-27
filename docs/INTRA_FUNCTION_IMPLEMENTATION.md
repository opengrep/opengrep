# Opengrep Cross-Function Tainting (Intrafile)

## Overview

This document explains how Opengrep performs intrafile cross-function taint
analysis when the `--taint-intrafile` flag (or per-rule option) is enabled.
The implementation hinges on four cooperating subsystems:

1. **Object initialization detection** (`src/tainting/Object_initialization.ml`)
   discovers constructor calls so we can treat freshly created instances as
   trackable taint carriers.
2. **Call graph construction and topological sorting**
   (`src/tainting/Taint_signature_extractor.ml`) builds a dependency graph of
   function calls and orders functions for analysis such that callees are
   processed before callers.
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
     constructor. These mappings seed the signature database so that instance
     variables (`this.x`, `self.y`) can be associated with the correct class
     context during analysis.

The object mappings are inserted into the signature database at the start of
`check_rule`, before we iterate over the file's function definitions:

```ocaml
let object_mappings =
  Taint_signature_extractor.detect_object_initialization ast taint_inst.lang in
let initial_signature_db =
  Shape_and_sig.add_object_mappings
    (signature_db ||| Shape_and_sig.empty_signature_database ())
    object_mappings
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

1. **Build the call graph** (`build_call_graph`):
   - Traverse the AST to identify all function definitions and their calls
   - Create a directed graph where an edge from `f` to `g` means "`f` calls `g`"
   - Each node is identified by `(class_name option, function_name)`
   - Handle method calls, direct function calls, and constructor invocations

2. **Compute topological order** (`topological_sort`):
   - Perform topological sorting on the call graph to get a bottom-up analysis order
   - Functions that don't call others (leaf functions) are analyzed first
   - Functions that call others are analyzed after their callees
   - For cyclic dependencies, we use arbitrary ordering (mutual recursion not fully supported)

3. **Analyze in sorted order**:
   - Process functions in topological order, extracting signatures
   - When analyzing function `f`, all functions it calls have their signatures ready
   - This allows signature instantiation to work correctly during the dataflow analysis

The call graph building happens in `Match_tainting_mode.check_rule` before the
signature extraction loop:

```ocaml
(* Build call graph and compute topological order *)
let call_graph = Taint_signature_extractor.build_call_graph ast in
let sorted_functions = Taint_signature_extractor.topological_sort call_graph in

(* Extract signatures in topological order *)
let signature_db =
  sorted_functions
  |> List.fold_left
       (fun db (class_name, func_name, fdef) ->
          Taint_signature_extractor.extract_signature_with_file_context
            ~db ~class_name func_name fdef ...)
       initial_signature_db
```

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
   - `filter_effects_with_real_sources` (implemented inline) discards effects
     that only exist because of synthetic seed taint.
4. **Persist signature:** the finalized signature and mapping are added to
   the database keyed by `(class_name, function_name)`.

### Why class properties matter

For methods that read/write instance fields, we extract expressions such as
`this.password` or `self.token` so that the fixed-point can see taint entering
or leaving through those properties even when they are set in constructors.

## Signature Instantiation (Function Instantiation)

File: `src/tainting/Sig_inst.ml`

- **Purpose:** When the engine encounters a function call, we consult the
  signature database to determine how taint should propagate from actual
  arguments to the callee's effects.
- **Main routine:** `instantiate_sig ~env ~call_args signature`.
- **Key steps:**
  1. Retrieve the pre-recorded signature using the callee's
     `(class_name, function_name)` pair.
  2. Evaluate the signature's preconditions (`Precondition.solve`) against the
     current call-site taint state. If they fail, the signature simply does not
     contribute any effects.
  3. Substitute formal parameter references (`BArg index`, `BThis`, globals)
     with the actual call arguments, leveraging helpers such as
     `instantiate_lval` and `instantiate_effect` (see lines ~640–870).
  4. Emit concrete `Effect.t` instances (sink hits, tainted returns, tainted
     l-values) which are merged into the surrounding dataflow state.

### Interaction with the main engine

During intrafile analysis, `Match_tainting_mode.check_rule` uses the call graph
to determine the order in which to extract signatures and analyze functions:

```ocaml
(* Step 1: Build call graph and compute topological order *)
let call_graph = Taint_signature_extractor.build_call_graph ast in
let sorted_functions = Taint_signature_extractor.topological_sort call_graph in

(* Step 2: Extract signatures in topological order *)
let signature_db =
  sorted_functions
  |> List.fold_left
       (fun db (class_name, func_name, fdef) ->
          Taint_signature_extractor.extract_signature_with_file_context
            ~db ~taint_inst ~class_name func_name fdef)
       initial_signature_db
in

(* Step 3: Run actual taint analysis with all signatures available *)
Visit_function_defs.fold_with_class_context
  (fun () opt_ent class_name fdef ->
     let _flow, effects, _ =
       check_fundef taint_inst name !ctx ~glob_env ~signature_db fdef
     in
     record_matches effects;
     ())
  () ast
```

The key insight is that signature extraction and taint analysis are now
**two separate passes**:

1. **First pass (signature extraction):** Analyze all functions in topological
   order to build their signatures. At this point, when analyzing function `f`,
   the signatures of all functions that `f` calls are already available.

2. **Second pass (taint analysis):** Run the full taint analysis with all
   signatures available. Now `check_fundef` can use the complete `signature_db`
   to instantiate signatures at call sites via `Sig_inst.instantiate_sig`.

This two-pass approach ensures that cross-function taint flows are correctly
captured, as the signature database is fully populated before we look for
actual taint violations.

## Summary

The `--taint-intrafile` implementation consists of several cooperating phases:

1. **Object detection** seeds the database with constructor knowledge so that
   instance fields map to concrete classes.

2. **Call graph construction** builds a directed graph of function calls to
   determine dependencies between functions.

3. **Topological sorting** orders functions such that callees are analyzed
   before their callers, ensuring signatures are available when needed.

4. **Signature extraction** (first pass) performs a dedicated fixed-point per
   function in topological order to learn how taint moves through arguments,
   returns, globals, and fields. This includes checking if function parameters
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
