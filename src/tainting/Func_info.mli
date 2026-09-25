(* Scope path outermost->innermost: [Some cls; Some meth] method, [] anonymous. *)
type fn_id = IL.name option list
[@@deriving show]

(* Paths name definitions by where they are: two definitions rebinding one
   name share a sid but are distinct functions. *)
val compare_fn_id : fn_id -> fn_id -> int
val equal_fn_id : fn_id -> fn_id -> bool

(* [entity = None] for anonymous funcs. *)
type t = {
  fn_id : fn_id;
  entity : AST_generic.entity option;
  fdef : AST_generic.function_definition;
}

val entity_qualifier : t -> string option

val as_method : fn_id -> (IL.name * IL.name) option
val as_free : fn_id -> IL.name option
val is_method_of : class_name:string -> method_name:string -> fn_id -> bool
val bare_name : fn_id -> IL.name option
val has_body : AST_generic.function_definition -> bool
val enclosing_class : fn_id -> IL.name option
val method_id : cls:IL.name -> meth:IL.name -> fn_id
val free_id : IL.name -> fn_id

(* File of the def's [fkind] token; [None] only for location-less tokens. *)
val def_file_opt : t -> Fpath.t option

(* The candidates [keep] accepts, or all of them when it accepts none: a
   file test that matches nothing must not erase a function. *)
val prefer : keep:(t -> bool) -> t list -> t list

