(* Higher-order-function callback extraction.
 * Resolves function references passed as arguments (directly, nested in
 * record/list literals, or reached via [id_svalue]) to callee [fn_id]s.
 * See [Graph_from_AST] for the orchestration that consumes these. *)

(* Resolved name a [fn_id] writes back onto the AST, when it carries a real
   (non-fake) token. *)
val resolved_name_of_fn_id :
  ?allow_located_fake:bool ->
  Callee_resolution.fn_id ->
  AST_generic.resolved_name option

(* Sets [ii.id_resolved]; mutating the ref mutates the shared AST. *)
val set_id_resolved_to_def :
  ?allow_located_fake:bool ->
  AST_generic.id_info ->
  Callee_resolution.fn_id ->
  unit

(* Identify callback candidates from a single call argument. Returns a list
   because an argument may carry several function references (record/list
   literal, or a variable whose [id_svalue] wraps such a container). *)
val try_identify_callback_args :
  lang:Lang.t ->
  all_funcs:Callee_resolution.func_info list ->
  ?func_lookup:Func_lookup.t ->
  caller_parent_path:IL.name option list ->
  AST_generic.argument ->
  (Callee_resolution.fn_id * Tok.t * IL.name option) list

(* Extract HOF callbacks from a single call expression.
   Returns list of (fn_id, tok, tmp_opt) where tmp_opt is the _tmp node for ShortLambda. *)
val extract_hof_callbacks_from_call :
  lang:Lang.t ->
  method_hofs:string list ->
  function_hofs:(string list * int) list ->
  all_funcs:Callee_resolution.func_info list ->
  ?func_lookup:Func_lookup.t ->
  caller_parent_path:IL.name option list ->
  AST_generic.expr ->
  AST_generic.arguments ->
  (Callee_resolution.fn_id * Tok.t * IL.name option) list
