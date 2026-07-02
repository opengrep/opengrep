type t

val empty : t

val add_inherited : t -> Names.Class_name.t -> Func_info.t list -> t

(* Single direct parent; first parent wins on multiple inheritance. *)
val set_parent : t -> Names.Class_name.t -> Names.Class_name.t -> t

val get_parent : t -> Names.Class_name.t -> Names.Class_name.t option

val set_module_singleton :
  t -> Names.Module_qn.t -> AST_generic.name -> t

val get_module_singleton :
  t -> Names.Module_qn.t -> AST_generic.name option

val set_method_return :
  t -> Names.Class_name.t -> Names.Method_name.t -> AST_generic.name -> t

val get_method_return :
  t -> Names.Class_name.t -> Names.Method_name.t -> AST_generic.name option

(* Appends; [field_type_for_caller] disambiguates by caller directory. *)
val set_field :
  t -> Names.Class_name.t -> Names.Field_name.t -> Fpath.t ->
  AST_generic.name -> t

val get_field :
  t -> Names.Class_name.t -> Names.Field_name.t -> AST_generic.name option

(* [set_methods] OVERWRITES on conflict. *)
val set_methods : t -> Names.Class_name.t -> Func_info.t list -> t

val add_method : t -> Names.Class_name.t -> Func_info.t -> t

val get_methods : t -> Names.Class_name.t -> Func_info.t list option

val set_function_return :
  t -> Names.Method_name.t -> AST_generic.name -> t

val get_function_return :
  t -> Names.Method_name.t -> AST_generic.name option

val set_function_return_tuple :
  t -> Names.Method_name.t -> AST_generic.name option list -> t

val get_function_return_tuple :
  t -> Names.Method_name.t -> AST_generic.name option list option

val set_method_return_tuple :
  t -> Names.Class_name.t -> Names.Method_name.t ->
  AST_generic.name option list -> t

val get_method_return_tuple :
  t -> Names.Class_name.t -> Names.Method_name.t ->
  AST_generic.name option list option

(* Value-aware (not physical): fixpoint "no change" check, setters are last-wins. *)
val equal : t -> t -> bool

(* String-keyed class views for the engine's callee resolver; [empty] misses. *)
val has_class : t -> string -> bool

val find_methods :
  t -> fallback:Func_info.t list -> class_name:string -> method_name:string ->
  Func_info.t list

val parent : t -> string -> string option

val super_class : t -> string -> string

val method_return :
  t -> class_name:string -> method_name:string -> AST_generic.name option

val field_type_for_caller :
  t -> class_name:string -> field_name:string -> caller_dir:string option ->
  AST_generic.name option
