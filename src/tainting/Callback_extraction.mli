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

(* Sets [ii.id_callee_definition] to the definition's sid; mutating the ref
   mutates the shared AST. *)
val set_callee_definition :
  ?allow_located_fake:bool ->
  AST_generic.id_info ->
  Callee_resolution.fn_id ->
  unit

type callback_scope =
  | Unscoped
  | In_module of Names.Module_qn.t
  | Method_of of string
  | Method_by_bare_name

type callback_site_resolver =
  ?func_lookup:Func_lookup.t ->
  ?caller_parent_path:IL.name option list ->
  ?scope:callback_scope ->
  ?arg:AST_generic.expr ->
  IL.name ->
  Callee_resolution.fn_id option

val identify_callback :
  ?all_funcs:Callee_resolution.func_info list ->
  ?func_lookup:Func_lookup.t ->
  ?caller_parent_path:IL.name option list ->
  ?scope:callback_scope ->
  IL.name ->
  Callee_resolution.fn_id option

val identify_callback_interfile :
  lang:Lang.t ->
  type_state:Type_state.t ->
  ?func_lookup:Func_lookup.t ->
  ?caller_parent_path:IL.name option list ->
  ?scope:callback_scope ->
  ?arg:AST_generic.expr ->
  IL.name ->
  Callee_resolution.fn_id option

(* Identify callback candidates from a single call argument. Returns a list
   because an argument may carry several function references (record/list
   literal, or a variable whose [id_svalue] wraps such a container). *)
val try_identify_callback_args :
  lang:Lang.t ->
  identify_callback:callback_site_resolver ->
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
  identify_callback:callback_site_resolver ->
  ?func_lookup:Func_lookup.t ->
  caller_parent_path:IL.name option list ->
  AST_generic.expr ->
  AST_generic.arguments ->
  (Callee_resolution.fn_id * Tok.t * IL.name option) list
