# OpenGrep Cross-Function Tainting (Intrafile)

## Overview

This document explains how OpenGrep performs intrafile cross-function taint
analysis when the `--taint-intrafile` flag (or per-rule option) is enabled.
The implementation hinges on three cooperating subsystems:

1. **Object initialization detection** (`src/tainting/Object_initialization.ml`)
   discovers constructor calls so we can treat freshly created instances as
   trackable taint carriers.
2. **Signature extraction** (`src/tainting/Taint_signature_extractor.ml`)
   runs a focused dataflow on each function/constructor to capture how taint
   flows through parameters, fields, globals, and return values.
3. **Signature instantiation** (`src/tainting/Sig_inst.ml`) replays the stored
   signatures at every call site so the engine knows how taint should be
   propagated interprocedurally within the same file.

These components are wired together from
`Match_tainting_mode.check_rule` (`src/engine/Match_tainting_mode.ml`) when the
current rule or run configuration sets `taint_intrafile = true`.

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
- **Algorithm outline:**
  1. **Assume taint sources for analysis:**
     - `mk_param_assumptions` marks each parameter as tainted using `BArg` base
       values so the fixed-point can discover flows from arguments.
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

During intrafile analysis, `Match_tainting_mode.check_rule` threads the
signature database through each function definition:

```ocaml
let signature_db =
  Visit_function_defs.fold_with_class_context
    (fun signature_db opt_ent class_name fdef ->
       ...
       let signature_db =
         if taint_inst.options.taint_intrafile then
           Taint_signature_extractor.extract_signature_with_file_context
             ~db:signature_db ...
         else
           signature_db
       in
       let _flow, effects, _ =
         check_fundef taint_inst name !ctx ~glob_env ~signature_db fdef
       in
       record_matches effects;
       signature_db)
    initial_signature_db ast
```

`check_fundef` passes the evolving `signature_db` into the dataflow fixpoint,
enabling `Sig_inst.instantiate_sig` to materialize cross-function effects
whenever a tracked call occurs.

## Summary

- **Object detection** seeds the database with constructor knowledge so that
  instance fields map to concrete classes.
- **Signature extraction** performs a dedicated fixed-point per function to
  learn how taint moves through arguments, returns, globals, and fields.
- **Signature instantiation** reuses those summaries at call sites, providing a
  lightweight intrafile interprocedural analysis without re-analyzing the
  callee each time.

Together these components deliver the behavior behind `--taint-intrafile`,
allowing OpenGrep to capture cross-function taint flows within a single file
while maintaining predictable performance.
## TODO:
0. We might want to consider raising the taint_FIXPOINT_TIMEOUT for the case of --taint-intrafile. For some files the computations take longer and
limiting it to 200ms might stop the taint computation before it finishes. 

1. The assumption is that functions are defined in order they are used. In particular support for recursive and mutually recursive functions
is unreliable and might produce false positives. 

2. Inheritance is not yet supported, for example the following gives a false negative:
```python
class BaseClass:
    def __init__(self):
        self.name = source()

class ChildClass(BaseClass):
    def method(self):
        sink(self.name)
 
```

3. Nested functions are not properly supported, for example the following does not find a taint.
```python
def outer():
    def inner():
        return source()

    x = inner()
    sink(x)

```

4. functions defined as arrows. For example
```javascript
const getTainted = () => {
  return source();
};

const x = getTainted();
sink(x);
```

5 Higher order functions. For example this will fail:
```javascript

async function graphql(ghtoken, query, variables) {
  const results = await fetch('https://api.github.com/graphql', {
    method: 'POST',
    body: JSON.stringify({ query, variables }),
  });
  const res = await results.json();
  return res.data;
}

async function getHistory(name, owner) {
  const result = await graphql(ghtoken, query, { name, owner });
  return result.repository.commits.nodes;
}

const history = await getHistory(name, owner);
const items = history.flatMap((node) => {
  const changes = node.associatedPullRequests.nodes;
  const item = changes[0];
  return item;
});
```

