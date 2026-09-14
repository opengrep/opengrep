type positioned_binding = {
  pb_pos : Pos.t option;
  pb_name : string;
  pb_parent_path : IL.name option list;
  pb_kinds : Func_lookup.scope_kind list;
}

val function_binding_of :
  pos:Pos.t option ->
  parent_path:IL.name option list ->
  string ->
  Func_info.t list ->
  positioned_binding list

val class_binding_of :
  pos:Pos.t option ->
  parent_path:IL.name option list ->
  string ->
  Names.Class_qn.t ->
  positioned_binding

val bindings_of_positioned :
  positioned_binding list -> Func_lookup.scope_entry list Common.SMap.t

val class_il_name_of : Types.class_info -> IL.name

val classes_by_qn : Types.class_info list -> Types.class_info Common.SMap.t

val bindings_in_class :
  Types.class_info ->
  (pos:Pos.t option -> parent_path:IL.name option list ->
   positioned_binding list) ->
  positioned_binding list

val class_member_bindings :
  members_of:(Names.Class_qn.t -> (string * Func_info.t list) list) ->
  Types.class_info list ->
  positioned_binding list

val own_class_bindings :
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  binds_at_file_scope:(Names.Class_qn.t -> bool) ->
  scope_of_owner:(Names.Class_qn.t -> IL.name option list option) ->
  Types.class_info list ->
  positioned_binding list

val own_definitions_of_file :
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  fi_file_str:string ->
  positioned_binding list

val position_of_tok : Tok.t -> Pos.t option

val own_alias_bindings :
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  fi_file_str:string ->
  positioned_binding list

val bindings_of_attributes :
  pos:Pos.t option ->
  keep:(string -> Func_lookup.module_attribute -> bool) ->
  Func_lookup.module_attribute Common.SMap.t ->
  positioned_binding list

val bindings_of_every_attribute :
  pos:Pos.t option ->
  Func_lookup.module_attribute Common.SMap.t ->
  positioned_binding list

type region_bindings = {
  rb_in_region : positioned_binding list;
  rb_names : unit Common.SMap.t;
}

val bindings_in_region : region_bindings -> positioned_binding list

val build_region_bindings :
  attributes_by_module:Func_lookup.module_attributes ->
  file_infos:Types.file_info list ->
  region_bindings Common.SMap.t

val top_level_bindings :
  definitions_by_qn:Types.definition Common.SMap.t ->
  positioned_binding list
