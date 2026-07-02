module G = AST_generic

(* Element type's leaf class of an iterable ([[]T] / arrays). *)
val slice_element_of_ty : G.type_ -> G.name option

(* The class declared on the name's [id_type]. *)
val declared_class_of_name : G.name -> G.name option

(* [current_class] is [None] outside a method. *)
type ctx = {
  function_return : string -> G.name option;
  method_return : class_name:string -> method_name:string -> G.name option;
  field_type : class_name:string -> field_name:string -> G.name option;
  parent_of : string -> string option;
  has_class : string -> bool;
  current_class : G.name option;
  uses_new_keyword : bool;
}

val method_call_target :
  type_recv:(G.expr -> G.name option) -> G.expr -> (G.name * string) option

val type_of_expr :
  ?max_depth : int -> ctx : ctx -> G.expr -> G.name option

(* [uses_new_keyword] (per [Lang_config]) suppresses the bare-[foo()]-as-constructor fallback. *)
val infer_expr_type :
  ?max_depth : int ->
  uses_new_keyword : bool ->
  type_state : Type_state.t ->
  G.expr ->
  G.name option
