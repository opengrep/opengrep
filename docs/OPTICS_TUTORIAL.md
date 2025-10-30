# Optics Tutorial for Opengrep

## Table of Contents
1. [Introduction](#introduction)
2. [What are Optics?](#what-are-optics)
3. [The Three Types of Optics](#the-three-types-of-optics)
4. [Using Optics](#using-optics)
5. [How the PPX Works](#how-the-ppx-works)
6. [Examples from Opengrep](#examples-from-opengrep)
7. [Comparison with Visitors](#comparison-with-visitors)

## Introduction

Optics provide a functional, composable way to access and traverse data structures. This tutorial shows how we use optics in Opengrep to replace the visitor pattern for AST traversal.

## What are Optics?

Optics are functional programming tools for working with data structures. Instead of using inheritance-based visitors, optics provide:

- **Immutability**: No mutation, just pure functions
- **Composability**: Small optics compose into larger ones
- **Type safety**: Leverages OCaml's type system
- **Simplicity**: No need to subclass visitors or override methods

## The Three Types of Optics

Our PPX generates three types of optics for each type:

### 1. Prisms - Accessing Variant Constructors

A **prism** focuses on one constructor of a variant type.

```ocaml
type 'a prism = {
  preview : 's -> 'a option;  (* Try to extract the value *)
  review  : 'a -> 's;          (* Build the constructor *)
}
```

**Example**: For `type expr_kind = Add of expr * expr | ...`, we generate:

```ocaml
let _Add : (expr_kind, expr * expr) prism = {
  preview = (function Add (e1, e2) -> Some (e1, e2) | _ -> None);
  review = (fun (e1, e2) -> Add (e1, e2));
}
```

**Usage**:
```ocaml
(* Extract the arguments from an Add expression *)
match _Add.preview expr.e with
| Some (left, right) -> (* process left and right *)
| None -> (* not an Add *)

(* Build an Add expression *)
let add_expr = _Add.review (e1, e2)
```

### 2. Traversals - Mapping Over Children

A **traversal** maps a function over all direct children of the same type.

```ocaml
type 's traversal = {
  map_children : ('s -> 's) -> 's -> 's
}
```

**Example**: For `type expr = { e: expr_kind }`, we generate:

```ocaml
let expr_kind_children : expr_kind traversal = {
  map_children = fun f -> function
    | Add (e1, e2) -> Add (f e1, f e2)
    | Call (func, args) -> Call (f func, List.map f args)
    | Id _ as x -> x  (* no children *)
}
```

**Usage**:
```ocaml
(* Double all integer literals in an expression *)
let double_ints = expr_kind_children.map_children (function
  | { e = Int n } -> { e = Int (n * 2) }
  | e -> e
)
```

### 3. Folds - Collecting Elements

A **fold** collects all elements of a target type using a MONOID.

```ocaml
type ('s, 'a) fold = {
  fold_map : 'm. (module MONOID with type t = 'm) ->
             ('a -> 'm) -> 's -> 'm
}
```

**Example**: For collecting all `expr` nodes from a `stmt_kind`:

```ocaml
let fold_stmt_kind_to_expr : (stmt_kind, expr) fold = {
  fold_map = fun (module M : MONOID) f -> function
    | ExprStmt e -> f e
    | Block stmts ->
        List.fold_left (fun acc s ->
          M.append acc (fold_stmt_to_expr.fold_map (module M) f s)
        ) M.empty stmts
    | If (cond, then_, else_) ->
        M.append (f cond)
          (M.append (fold_stmt_to_expr ...) (fold_stmt_to_expr ...))
}
```

**Usage**:
```ocaml
(* Collect all expressions from a statement *)
module ListMonoid(T : sig type t end) : MONOID with type t = T.t list = struct
  type t = T.t list
  let empty = []
  let append = (@)
end

let collect_exprs stmt =
  let module M = ListMonoid(struct type t = expr end) in
  fold_stmt_kind_to_expr.fold_map (module M) (fun e -> [e]) stmt.s
```

## Using Optics

### Step 1: Add `[@@deriving optics]` to Your Type

```ocaml
type expr_kind =
  | Add of expr * expr
  | Mul of expr * expr
  | Int of int
  | Call of expr * expr list
[@@deriving optics]
```

### Step 2: The PPX Generates Optics

The PPX automatically generates:
- Prisms: `_Add`, `_Mul`, `_Int`, `_Call`
- Traversals: `expr_kind_children`
- Folds: `fold_expr_kind_to_expr` (for collecting expr nodes)

### Step 3: Use the Generated Optics

```ocaml
(* Example: Collect all function calls *)
let collect_calls expr =
  let module M = ListMonoid(struct type t = expr end) in
  fold_expr_kind_to_expr.fold_map (module M) (fun e ->
    match e.e with
    | Call _ -> [e]
    | _ -> []
  ) expr.e
```

## How the PPX Works

### Two-Pass Architecture

The PPX uses a **purely functional two-pass transformation**:

#### Pass 1: Collect Type Aliases Globally
```ocaml
let collect_aliases_from_structure (str : structure) = ...
```

Builds a map of all type aliases in the file:
```ocaml
type 'a bracket = tok * 'a * tok
(* Maps to: ("bracket", (["a"], <tok * 'a * tok>)) *)
```

#### Pass 2: Generate Optics with Full Alias Map

For each type with `[@@deriving optics]`:
1. Merge file-global aliases with local aliases
2. **Expand type aliases** with parameter substitution
3. Generate prisms, traversals, and folds

### Type Alias Expansion

**The Problem**: `stmt list bracket` is opaque to the PPX

**The Solution**: Expand it to `tok * stmt list * tok`

```ocaml
let rec expand_type (alias_map : ...) (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "bracket" }, [param]) ->
      (* bracket is: type 'a bracket = tok * 'a * tok *)
      (* Substitute 'a with param *)
      let expanded_params = List.map (expand_type alias_map) [param] in
      let subst = [("a", List.hd expanded_params)] in
      substitute_in_type subst (tok, 'a, tok)
      (* Results in: tok * <expanded param> * tok *)
```

**Example**:
- Input: `Block of stmt list bracket`
- After expansion: `Block of (tok * stmt list * tok)`
- Now the PPX can see the `stmt list` inside and generate correct folds!

### Fold Generation Example

For `Block of stmt list bracket`:

```ocaml
| Block x0 ->
    let acc = ref M.empty in
    (* Expand 'stmt list bracket' to 'tok * stmt list * tok' *)
    match x0 with (t0, t1, t2) ->
      (* t0: tok - ignored *)
      (* t1: stmt list - traverse! *)
      List.iter (fun x ->
        acc := M.append !acc (fold_stmt_to_<target>.fold_map (module M) f x)
      ) t1;
      (* t2: tok - ignored *)
    !acc
```

## Examples from Opengrep

### Example 1: Collecting All Statements Recursively

```ocaml
(* Collect all statements recursively from a statement using optics *)
let collect_all_stmts (stmt : G.stmt) : G.stmt list =
  let module M = ListMonoid(struct type t = G.stmt end) in
  (* Use the generated fold to collect all nested statements *)
  let nested = G.fold_stmt_kind_to_stmt.fold_map (module M) (fun s -> [s]) stmt.s in
  stmt :: nested
```

**What this does**:
1. Creates a list monoid for collecting `stmt` values
2. Uses the generated `fold_stmt_kind_to_stmt` to collect all nested statements
3. Prepends the current statement to the results

**Visitor equivalent**:
```ocaml
class ['acc] collect_stmts = object
  inherit ['acc] G.iter as super
  method! visit_stmt acc stmt =
    acc := stmt :: !acc;
    super#visit_stmt acc stmt
end
```

### Example 2: Collecting Expressions from Statements

```ocaml
(* Collect all expressions from a statement kind *)
let collect_exprs_from_stmt_kind (sk : G.stmt_kind) : G.expr list =
  let module M = ListMonoid(struct type t = G.expr end) in
  G.fold_stmt_kind_to_expr.fold_map (module M) (fun e -> [e]) sk
```

**What this does**:
- Uses the generated `fold_stmt_kind_to_expr`
- Collects ALL `expr` nodes directly contained in the statement
- Returns a flat list of expressions

### Example 3: Building a Call Graph (Complex)

```ocaml
(* Extract all function definitions recursively *)
let rec extract_funcs_from_stmt (stmt : G.stmt) =
  match stmt.s with
  | G.DefStmt (ent, G.FuncDef fdef) ->
      (* Found a function - collect it AND search its body *)
      let this_func = [(Some ent, None, fdef)] in
      let nested_funcs = match fdef.fbody with
        | G.FBStmt body_stmt -> extract_funcs_from_stmt body_stmt
        | _ -> []
      in
      this_func @ nested_funcs
  | G.DefStmt (ent, G.ClassDef { cbody = (_, fields, _); _ }) ->
      (* Extract methods from class, preserving class context *)
      let class_name = match ent.name with G.EN n -> Some n | _ -> None in
      fields |> List.concat_map (fun (G.F stmt) ->
        extract_funcs_from_stmt stmt |> List.map (fun (method_ent, _, fdef) ->
          (method_ent, class_name, fdef))
      )
  | _ ->
      (* Recursively search nested statements *)
      let module M = ListMonoid(struct type t = G.stmt end) in
      let all_stmts = G.fold_stmt_kind_to_stmt.fold_map (module M) (fun s -> [s]) stmt.s in
      all_stmts |> List.concat_map extract_funcs_from_stmt
```

**Key features**:
1. **Pattern matching** to identify function and class definitions
2. **Recursion** into function bodies to find nested functions
3. **Context preservation** - tracks class name for methods
4. **Optics for traversal** - uses `fold_stmt_kind_to_stmt` to find nested statements

### Example 4: Extracting All Calls from a Function

```ocaml
let extract_calls (fdef : G.function_definition) : fn_id list =
  (* Step 1: Collect all expressions from the function body *)
  let all_exprs = match fdef.fbody with
    | G.FBStmt stmt ->
        let all_stmts = collect_all_stmts stmt in
        let top_level_exprs =
          all_stmts |> List.concat_map (fun s ->
            collect_exprs_from_stmt_kind s.s) in
        top_level_exprs |> List.concat_map collect_all_exprs
    | _ -> []
  in

  (* Step 2: Extract function calls from all expressions *)
  all_exprs |> List.concat_map (fun e ->
    match e.e with
    | G.Call ({ e = G.N (G.Id (id, id_info)); _ }, _) ->
        let callee = AST_to_IL.var_of_id_info id id_info in
        [{ class_name = None; name = Some callee }]
    | _ -> []
  )
```

**Completely functional**:
- ✅ No mutable refs
- ✅ No visitor inheritance
- ✅ Clear data flow: stmt → stmts → exprs → calls
- ✅ Uses only optics folds for traversal

## Comparison with Visitors

### Visitor Pattern (OO)

```ocaml
class ['acc] my_visitor = object
  inherit ['acc] VisitorsRuntime.map as super

  method! visit_expr acc expr =
    (* Do something with expr *)
    acc := expr :: !acc;
    super#visit_expr acc expr
end

let visitor = new my_visitor
let results = ref []
visitor#visit_program results ast
```

**Downsides**:
- ❌ Mutable state (`ref`)
- ❌ Inheritance complexity
- ❌ Hard to compose visitors
- ❌ Need to override correct methods

### Optics Pattern (Functional)

```ocaml
let collect_exprs program =
  let module M = ListMonoid(struct type t = expr end) in
  program |> List.concat_map (fun stmt ->
    fold_stmt_kind_to_expr.fold_map (module M) (fun e -> [e]) stmt.s
  )
```

**Benefits**:
- ✅ Pure functions
- ✅ No inheritance
- ✅ Easy to compose
- ✅ Type-safe

## Advanced Topics

### Handling Polymorphic Types

The PPX handles polymorphic type aliases like `'a bracket = tok * 'a * tok`:

1. **Collect aliases globally** from the entire file
2. **Expand with parameter substitution**: `stmt list bracket` → `tok * stmt list * tok`
3. **Generate folds** that traverse into the expanded structure

### Custom Monoids

You can define custom monoids for different aggregations:

```ocaml
(* Count how many times something appears *)
module CountMonoid : MONOID with type t = int = struct
  type t = int
  let empty = 0
  let append = (+)
end

let count_calls expr =
  fold_expr_kind_to_expr.fold_map (module CountMonoid) (fun e ->
    match e.e with Call _ -> 1 | _ -> 0
  ) expr.e
```

### Composing Folds

Folds compose naturally through function composition:

```ocaml
(* Collect all expressions from a program *)
let all_exprs program =
  let module M = ListMonoid(struct type t = expr end) in
  program |> List.concat_map (fun stmt ->
    (* First fold: stmt_kind -> definition_kind *)
    fold_stmt_kind_to_definition_kind.fold_map (module M) (fun def_kind ->
      (* Second fold: definition_kind -> function_definition *)
      fold_definition_kind_to_function_definition.fold_map (module M) (fun fdef ->
        (* Third fold: function_definition contains exprs *)
        match fdef.fbody with
        | FBExpr e -> [e]
        | _ -> []
      ) def_kind
    ) stmt.s
  )
```

## Summary

Optics provide a functional, composable alternative to the visitor pattern:

- **Prisms** for accessing variant constructors
- **Traversals** for mapping over children
- **Folds** for collecting elements

The PPX automatically generates these optics with:
- Global type alias expansion
- Polymorphic type support
- Clean, functional code

This enables writing AST traversals that are:
- Pure and immutable
- Composable and modular
- Type-safe and concise
