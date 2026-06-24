(* Higher-order-function callback extraction.
 * Resolves function references passed as arguments (directly, nested in
 * record/list literals, or reached via [id_svalue]) to callee [fn_id]s.
 * See [Graph_from_AST] for the orchestration that consumes these. *)

(* Extract HOF callbacks from a single call expression.
   Returns list of (fn_id, tok, tmp_opt) where tmp_opt is the _tmp node for ShortLambda. *)
val extract_hof_callbacks_from_call :
  lang:Lang.t ->
  method_hofs:string list ->
  function_hofs:(string list * int) list ->
  all_funcs:Callee_resolution.func_info list ->
  caller_parent_path:IL.name option list ->
  AST_generic.expr ->
  AST_generic.arguments ->
  (Callee_resolution.fn_id * Tok.t * IL.name option) list

(* Extract HOF callbacks from a whole function body, returning
   (fn_id, tok, tmp_opt) tuples. tmp_opt is Some IL.name for ShortLambda
   callbacks that need a _tmp intermediate node. *)
val extract_hof_callbacks :
  ?_object_mappings:(AST_generic.name * AST_generic.name) list ->
  ?all_funcs:Callee_resolution.func_info list ->
  ?caller_parent_path:IL.name option list ->
  lang:Lang.t ->
  AST_generic.function_definition ->
  (Callee_resolution.fn_id * Tok.t * IL.name option) list
