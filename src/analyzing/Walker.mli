(* AST traversal helpers — a fold over expressions in a stmt, plus the
   observation-driven file walker that projidx uses to amortise its
   per-file passes.  Sits alongside [Visit_function_defs] so callers
   in [src/tainting/] and [src/project_index/] can both reach it. *)

module G = AST_generic

module Observation : sig
  type t =
    | Func_def of {
        opt_ent : G.entity option;
        parent_path : IL.name option list;
        fdef : G.function_definition;
      }
    | Class_def of {
        ent : G.entity;
        cdef : G.class_definition;
      }
    | Type_def of {
        ent : G.entity;
        tdef : G.type_definition;
      }
    | Other_def of {
        ent : G.entity;
        kind : string;
        anys : G.any list;
      }
    | Var_def of {
        ent : G.entity;
        vdef : G.variable_definition;
      }
end

(* Walk a file's AST and return the full observation list in forward
   (visitor-traversal) order. *)
val walk_file : lang:Lang.t -> G.program -> Observation.t list

(* Fold over every expression reachable from a stmt in visitor-traversal
   order.  Encapsulates the [iter_no_id_info] + ref pattern so callers
   write pure folds.

   [~skip_nested_fdefs:true] suppresses descent into inner function
   bodies — for analyses that must attribute exprs to the enclosing
   fdef only. *)
val fold_exprs_in_stmt :
  ?skip_nested_fdefs:bool ->
  ('acc -> G.expr -> 'acc) -> 'acc -> G.stmt -> 'acc

(* Like [fold_exprs_in_stmt] but folds over sub-statements rather than
   sub-expressions.  Used by analyses that match on stmt kinds. *)
val fold_stmts_in_stmt :
  ?skip_nested_fdefs:bool ->
  ('acc -> G.stmt -> 'acc) -> 'acc -> G.stmt -> 'acc

(* Program-level variants — fold every stmt / expr reachable from any
   top-level stmt in the program. *)
val fold_stmts_in_program :
  ?skip_nested_fdefs:bool ->
  ('acc -> G.stmt -> 'acc) -> 'acc -> G.program -> 'acc

val fold_exprs_in_program :
  ?skip_nested_fdefs:bool ->
  ('acc -> G.expr -> 'acc) -> 'acc -> G.program -> 'acc

(* Fold over every expression reachable from a function definition —
   parameter defaults, return-type annotation, and body.

   [~skip_nested_fdefs:true] skips inner function bodies; the outer
   fdef's own exprs are always emitted. *)
val fold_exprs_in_fdef :
  ?skip_nested_fdefs:bool ->
  ('acc -> G.expr -> 'acc) -> 'acc -> G.function_definition -> 'acc
