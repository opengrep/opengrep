(* Higher-order-function callback extraction.
 * Resolves function references passed as arguments (directly, nested in
 * record/list literals, or reached via [id_svalue]) to callee [fn_id]s.
 * See [Graph_from_AST] for the orchestration that consumes these. *)

(* The result is the resolved name of a [fn_id], that is the definition's kind
   and sid, and it is present when the bare name carries a real (non-fake)
   token. [set_callee_definition] stamps that sid onto the AST. *)
val resolved_name_of_fn_id :
  ?allow_located_fake:bool ->
  Callee_resolution.fn_id ->
  AST_generic.resolved_name option

(* Sets [ii.id_callee_definition] to the sids of the given definitions, and
   leaves it unchanged when there are none; mutating the ref mutates the
   shared AST. *)
val set_callee_definition :
  ?allow_located_fake:bool ->
  AST_generic.id_info ->
  Callee_resolution.fn_id list ->
  unit

type reference =
  | Bound of AST_generic.expr
  | Written of AST_generic.expr

type callback_resolver =
  caller:Function_id.t option -> reference -> Symbol_table.resolution

(* Identify callback candidates from a single call argument. Returns a list
   because an argument may carry several function references (record/list
   literal, or a variable whose [id_svalue] wraps such a container). *)
val try_identify_callback_args :
  lang:Lang.t ->
  resolve_callback:callback_resolver ->
  caller_parent_path:IL.name option list ->
  AST_generic.argument ->
  (Callee_resolution.fn_id * Tok.t * IL.name option) list

(* Extract HOF callbacks from a single call expression.
   Returns list of (fn_id, tok, tmp_opt) where tmp_opt is the _tmp node for ShortLambda. *)
val extract_hof_callbacks_from_call :
  lang:Lang.t ->
  method_hofs:string list ->
  function_hofs:(string list * int) list ->
  resolve_callback:callback_resolver ->
  caller_parent_path:IL.name option list ->
  AST_generic.expr ->
  AST_generic.arguments ->
  (Callee_resolution.fn_id * Tok.t * IL.name option) list
