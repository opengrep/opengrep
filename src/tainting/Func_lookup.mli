(* The project index's tables that [Callee_resolution.identify_callee_interfile]
   reads. Queries default to nothing-here ([], false, None) when an index is absent; do not mutate a wrapped [Hashtbl.t] (no snapshot). *)

type t

type alias_index

type module_attribute =
  | Attr_functions of Func_info.t list
  | Attr_class of Names.Class_qn.t
  | Attr_class_with_companion of Names.Class_qn.t * Names.Class_qn.t
  | Attr_module of Names.Module_qn.t

type module_attributes = module_attribute Common.SMap.t Common.SMap.t

module Class_qn_map : Map.S with type key = Names.Class_qn.t

type companion_index = Names.Class_qn.t Class_qn_map.t

val attributes_of_module :
  module_attributes -> Names.Module_qn.t -> module_attribute Common.SMap.t

type scope_kind =
  | Scope_function of Func_info.t
  | Scope_class of Names.Class_qn.t
  | Scope_companion of Names.Class_qn.t
  | Scope_extension of Func_info.t
  | Scope_object of Func_info.t list Common.SMap.t
  | Scope_local_value

type scope_entry = {
  kind : scope_kind;
  parent_path : IL.name option list;
}

type scope_table

val scope_table_of_map : scope_entry list Common.SMap.t -> scope_table

val scope_table_union : front:scope_table -> back:scope_table -> scope_table

val scope_table_shadowing :
  front:scope_table -> back:scope_table -> scope_table

val class_of_entries : scope_entry list -> Names.Class_qn.t option

val companion_of_entries : scope_entry list -> Names.Class_qn.t option

val functions_of_entries : scope_entry list -> Func_info.t list

val extensions_of_entries : scope_entry list -> Func_info.t list

val object_of_entries :
  scope_entry list -> Func_info.t list Common.SMap.t option

val empty_scope_table : scope_table

val alias_index_of_hashtbl :
  (string, Names.Module_qn.t) Hashtbl.t -> alias_index

val create :
  ?alias_to_module_qn : alias_index ->
  ?own_modules : Names.Module_qn.t list ->
  ?companions : companion_index ->
  ?member_classes : Names.Class_qn.t list ->
  module_attributes : module_attributes ->
  class_of_qn : (Names.Class_qn.t -> Class_table.cls option) ->
  is_import : (AST_generic.SId.t -> bool) ->
  definition : (string -> module_attribute option) ->
  scope_table : scope_table ->
  unit -> t

val own_modules : t -> Names.Module_qn.t list

val resolve_in_scope :
  t -> caller_parent_path:IL.name option list -> string -> scope_entry list

val module_attribute :
  t -> Names.Module_qn.t -> string -> module_attribute option

val companion_of : t -> Names.Class_qn.t -> Names.Class_qn.t option

val class_of_qn : t -> Names.Class_qn.t -> Class_table.cls option

val is_known_module : t -> Names.Module_qn.t -> bool

val is_import : t -> AST_generic.SId.t -> bool

val member_classes : t -> Names.Class_qn.t list

val definition : t -> string -> module_attribute option

val resolve_alias : t -> string -> Names.Module_qn.t option
