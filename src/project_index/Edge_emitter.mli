(* Turns resolver [(fn_id, tok, …)] tuples into concrete graph edges, resolving
   [fn_id -> Function_id.t] (skipping on lookup failure). When the caller is a
   top-level lambda the edge is duplicated to the file's [top_level] node so
   module-scope-lambda calls join the module's outgoing-edge set. *)

type edge = Function_id.t * Function_id.t * Tok.t

type t

val create : top_level:Function_id.t -> t

val emit_call :
  t ->
  caller_node:Function_id.t option ->
  is_toplevel_lambda:bool ->
  callee:Func_info.fn_id ->
  call_tok:Tok.t ->
  edge list

(* [tmp = Some name] inserts the ShortLambda intermediate node AST_to_IL emits. *)
val emit_callback :
  t ->
  caller_node:Function_id.t option ->
  is_toplevel_lambda:bool ->
  callback:Func_info.fn_id ->
  call_tok:Tok.t ->
  tmp:IL.name option ->
  edge list

val emit_toplevel :
  t ->
  callee:Func_info.fn_id ->
  call_tok:Tok.t ->
  edge list
