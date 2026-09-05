(* Function-resolution bundle for [Graph_from_AST.identify_callee]. Queries default
   to nothing-here ([], false, None) when an index is absent; do not mutate a wrapped [Hashtbl.t] (no snapshot). *)

type t

val empty : t

type leaf_index
type module_index
type alias_index
type file_module_index
type name_set

(* Local import name -> the exported name it binds and the files that
   export it; [import { C as Alias }] names its origin exactly. *)
type class_alias_index

val leaf_index_of_hashtbl :
  (string, Func_info.t list) Hashtbl.t -> leaf_index
val module_index_of_hashtbl :
  (Names.Module_qn.t, Func_info.t list) Hashtbl.t -> module_index
val alias_index_of_hashtbl :
  (string, Names.Module_qn.t) Hashtbl.t -> alias_index
val file_module_index_of_hashtbl :
  (string, Names.Module_qn.t) Hashtbl.t -> file_module_index
val name_set_of_hashtbl : (string, unit) Hashtbl.t -> name_set

val class_alias_index_of_hashtbl :
  (string, string * name_set) Hashtbl.t -> class_alias_index

val name_set_mem : name_set -> string -> bool

val create :
  ?funcs_by_name : leaf_index ->
  ?project_funcs_by_name : leaf_index ->
  ?funcs_by_module_qn : module_index ->
  ?alias_to_module_qn : alias_index ->
  ?same_file_funcs_by_name : leaf_index ->
  ?funcs_by_package : leaf_index ->
  ?file_module_qn : file_module_index ->
  ?local_imports : name_set ->
  ?class_aliases : class_alias_index ->
  ?overload_groups : bool ->
  unit -> t

(* Whether the index widened overload groups to their union, so that a
   same-arity tie resolves to the group's representative. *)
val overload_groups : t -> bool

(* [None] when the name is not an import alias bound to a class. *)
val resolve_class_alias : t -> string -> (string * name_set) option

val with_local_imports :
  t -> name_set option -> t

val is_locally_imported : t -> string -> bool

(* Falls back to filtering [all_funcs] when unindexed. *)
val funcs_with_leaf :
  t -> all_funcs:Func_info.t list -> string -> Func_info.t list

(* [None] when no index means keep [all_funcs] (vs [Some []]). *)
val narrow_candidates_by_leaf :
  t -> string -> Func_info.t list option

val nested_in_same_file : t -> string -> Func_info.t list

val resolve_alias : t -> string -> Names.Module_qn.t option

(* False (both indexes unpopulated) means skip the import path entirely. *)
val imports_indexed : t -> bool

val funcs_in_module : t -> Names.Module_qn.t -> Func_info.t list

val funcs_in_package : t -> string -> Func_info.t list

val module_qn_of_file : t -> string -> Names.Module_qn.t option
