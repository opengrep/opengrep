(* Scope path outermost->innermost: [Some cls; Some meth] method, [] anonymous. *)
type fn_id = IL.name option list
[@@deriving show, eq, ord]

(* [entity = None] for anonymous funcs. *)
type t = {
  fn_id : fn_id;
  entity : AST_generic.entity option;
  fdef : AST_generic.function_definition;
}

val as_method : fn_id -> (IL.name * IL.name) option
val as_free : fn_id -> IL.name option
val is_method_of : class_name:string -> method_name:string -> fn_id -> bool
val leaf_name : fn_id -> IL.name option
val enclosing_class : fn_id -> IL.name option
val method_id : cls:IL.name -> meth:IL.name -> fn_id
val free_id : IL.name -> fn_id

(* File of the def's [fkind] token; [None] only for location-less tokens. *)
val def_file_opt : t -> Fpath.t option

(* Drop non-[keep] methods, but only within method-name groups that hold
   several entries and would keep at least one survivor.  Uniquely named
   methods and groups [keep] would empty are left alone, so a failed match
   degrades to the un-narrowed set.  [None] = nothing changed. *)
val narrow_colliding_groups :
  keep:(t -> bool) -> t list -> t list option
