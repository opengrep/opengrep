(* Function-resolution bundle for [Graph_from_AST.identify_callee]. Queries default
   to nothing-here ([], false, None) when an index is absent; do not mutate a wrapped [Hashtbl.t] (no snapshot). *)

type t

val empty : t

type bare_name_index
type module_index
type alias_index
type file_module_index
type name_set

(* Local import name -> the exported name it binds and the files that
   export it; [import { C as Alias }] names its origin exactly. *)
type class_alias_index

(* A [constructor_index] maps the bare class name to the constructor
   functions of that class, each list in the order in which the functions
   were given. *)
type constructor_index

type module_attribute =
  | Attr_functions of Func_info.t list
  | Attr_class of Names.Class_qn.t
  | Attr_module of Names.Module_qn.t

type module_attributes = module_attribute Common.SMap.t Common.SMap.t

type resolution_orders = Names.Class_qn.t list Common.SMap.t

module Class_qn_map : Map.S with type key = Names.Class_qn.t

type methods_by_class = Func_info.t list Common.SMap.t Class_qn_map.t

type class_qn_by_definition =
  (Function_id.t * Names.Class_qn.t) list Common.SMap.t

val attributes_of_module :
  module_attributes -> Names.Module_qn.t -> module_attribute Common.SMap.t

type scope_kind =
  | Scope_function of Func_info.t
  | Scope_class of Names.Class_qn.t
  | Scope_extension of Func_info.t

type scope_entry = {
  kind : scope_kind;
  parent_path : IL.name option list;
}

type scope_table

val scope_table_of_map : scope_entry list Common.SMap.t -> scope_table

val class_of_entries : scope_entry list -> Names.Class_qn.t option

val functions_of_entries : scope_entry list -> Func_info.t list

val extensions_of_entries : scope_entry list -> Func_info.t list

val empty_scope_table : scope_table

val bare_name_index_layered : front:bare_name_index -> back:bare_name_index -> bare_name_index
val bare_name_index_override : front:bare_name_index -> back:bare_name_index -> bare_name_index
val bare_name_index_of_hashtbl :
  (string, Func_info.t list) Hashtbl.t -> bare_name_index
val module_index_of_hashtbl :
  (Names.Module_qn.t, Func_info.t list) Hashtbl.t -> module_index
val alias_index_of_hashtbl :
  (string, Names.Module_qn.t) Hashtbl.t -> alias_index
val file_module_index_of_hashtbl :
  (string, Names.Module_qn.t) Hashtbl.t -> file_module_index
val name_set_of_hashtbl : (string, unit) Hashtbl.t -> name_set

val class_alias_index_of_hashtbl :
  (string, string * name_set) Hashtbl.t -> class_alias_index

(* The result holds the constructors that a file's bare-name table
   contains under the constructor names of [lang], keyed by class. The
   result is [None] when the table contains no entry under any of those
   names, and a lookup then reads the project-wide index. *)
val constructor_index_of_hashtbl :
  lang:Lang.t ->
  (string, Func_info.t list) Hashtbl.t -> constructor_index option

(* The result holds the constructors among the given functions, keyed by
   class. The project index builds the map once over every function of
   the project, and the single-file call graph builds one over the file's
   functions. *)
val constructor_index_of_funcs :
  lang:Lang.t -> Func_info.t list -> constructor_index

val name_set_mem : name_set -> string -> bool

val create :
  ?funcs_by_name : bare_name_index ->
  ?project_funcs_by_name : bare_name_index ->
  ?funcs_by_module_qn : module_index ->
  ?alias_to_module_qn : alias_index ->
  ?same_file_funcs_by_name : bare_name_index ->
  ?funcs_by_package : bare_name_index ->
  ?file_module_qn : file_module_index ->
  ?local_imports : name_set ->
  ?class_aliases : class_alias_index ->
  ?constructors : constructor_index ->
  ?project_constructors : constructor_index ->
  ?overload_groups : bool ->
  module_attributes : module_attributes ->
  resolution_orders : resolution_orders ->
  class_qn_by_definition : class_qn_by_definition ->
  methods_by_class : methods_by_class ->
  scope_table : scope_table ->
  unit -> t

(* Whether the index widened overload groups to their union, so that a
   same-arity tie resolves to the group's representative. *)
val overload_groups : t -> bool

val resolve_in_scope : t -> string -> scope_entry list

val module_attribute :
  t -> Names.Module_qn.t -> string -> module_attribute option

val resolution_order : t -> Names.Class_qn.t -> Names.Class_qn.t list

val is_known_class : t -> Names.Class_qn.t -> bool

val is_known_module : t -> Names.Module_qn.t -> bool

val class_qn_of_definition : t -> IL.name -> Names.Class_qn.t option

val find_along_order :
  t ->
  Names.Class_qn.t list ->
  (Names.Class_qn.t -> string list) ->
  Func_info.t list

(* [None] when the name is not an import alias bound to a class. *)
val resolve_class_alias : t -> string -> (string * name_set) option

val with_local_imports :
  t -> name_set option -> t

val is_locally_imported : t -> string -> bool

(* Falls back to filtering [all_funcs] when unindexed. *)
val funcs_with_bare_name :
  t -> all_funcs:Func_info.t list -> string -> Func_info.t list

(* [None] when no index means keep [all_funcs] (vs [Some []]). *)
val narrow_candidates_by_bare_name :
  t -> string -> Func_info.t list option

(* The result lists the constructors of the class. The lookup reads the file's
   own constructor index when it carries one, and the project-wide index
   otherwise; the result is the empty list when the index that is read holds
   no constructor for the class. *)
val constructors_of_class : t -> string -> Func_info.t list

val nested_in_same_file : t -> string -> Func_info.t list

val resolve_alias : t -> string -> Names.Module_qn.t option

(* False (both indexes unpopulated) means skip the import path entirely. *)
val imports_indexed : t -> bool

val funcs_in_module : t -> Names.Module_qn.t -> Func_info.t list

val funcs_in_package : t -> string -> Func_info.t list

val module_qn_of_file : t -> string -> Names.Module_qn.t option
