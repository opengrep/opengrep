type t

val empty : t

(* Every map below is keyed by the bare class name, with one entry per
   defining file; a reader takes the entries of the files this state sees
   the class in ([add_class_file], narrowed by [narrow]), else all. *)

val add_class_file : t -> Names.Class_name.t -> Fpath.t -> t

val add_inherited :
  t -> Names.Class_name.t -> Fpath.t -> Func_info.t list -> t

(* Single direct parent; first parent wins on multiple inheritance. *)
val set_parent :
  t -> Names.Class_name.t -> Fpath.t -> Names.Class_name.t -> t

val get_parent : t -> Names.Class_name.t -> Names.Class_name.t option

val set_module_singleton :
  t -> Names.Module_qn.t -> AST_generic.name -> t

val get_module_singleton :
  t -> Names.Module_qn.t -> AST_generic.name option

(* One entry per defining file, so two same-named classes keep their own
   return types; a re-set of one file with an equal type changes nothing. *)
val set_method_return :
  t -> Names.Class_name.t -> Names.Method_name.t -> Fpath.t ->
  AST_generic.name -> t

(* The entry of the file that defines the class as this state sees it
   (its [methods]), else the first. *)
val get_method_return :
  t -> Names.Class_name.t -> Names.Method_name.t -> AST_generic.name option

(* Whether the method of the class defined in this file has an entry. *)
val has_method_return :
  t -> Names.Class_name.t -> Names.Method_name.t -> Fpath.t -> bool

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

val fold_methods :
  (Names.Class_name.t -> Func_info.t list -> 'a -> 'a) -> t -> 'a -> 'a

(* The result is [t] with every class of [classes] narrowed to the defining
   files that [keep_file] accepts and to the methods that
   [Func_info.narrow_colliding_groups] keeps. A method group that [keep_file]
   would empty keeps every entry. A file list that [keep_file] would empty
   keeps every file. A class outside [classes] is unchanged. *)
val narrow :
  classes:Names.Class_name.t list ->
  keep_file:(Names.Class_name.t -> string -> bool) ->
  file_of_func:(Func_info.t -> string option) ->
  t ->
  t

(* The result lists the classes whose method list or list of defining
   files the narrowing can change: a class with two methods of one bare
   name, and a class with more than one defining file. A caller that
   narrows every class of the project passes this list as [classes]. *)
val narrowable_classes : t -> Names.Class_name.t list

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
