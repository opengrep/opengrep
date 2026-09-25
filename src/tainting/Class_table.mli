module SId_tbl : Hashtbl.S with type key = AST_generic.SId.t
module Field_path_map : Map.S with type key = string list

type kind =
  | Class_kind of AST_generic.class_kind
  | Module_kind

type role =
  | Definition of Function_id.t
  | Singleton_object
  | Trait_impl of Function_id.t

type parent =
  | Bound of AST_generic.SId.t
  | Unbound of AST_generic.type_
  | Impl of Function_id.t

type class_scope = {
  binding : AST_generic.SId.t;
  role : role;
  members : Func_info.t list Common.SMap.t;
  fields : Func_info.t list Field_path_map.t;
  parents : (parent * Linearisation.placement) list;
  class_side_parents : parent list;
  kind : kind;
  singleton_exposure : Class_parents.singleton_exposure;
  bound_functions : Func_info.t list;
  object_fields : Func_info.t list Field_path_map.t;
  extensions : Func_info.t list Common.SMap.t;
  reopens : bool;
}

type scope_id = {
  scope_binding : AST_generic.SId.t;
  scope_role : role;
}

module Scope_tbl : Hashtbl.S with type key = scope_id

val compare_scope : class_scope -> class_scope -> int
val scope_id_of : class_scope -> scope_id
val definition_scope : AST_generic.SId.t -> scope_id
val binding_of_id_info : AST_generic.id_info -> AST_generic.SId.t option
val id_info_of_name : AST_generic.name -> AST_generic.id_info
val name_of_type : AST_generic.type_ -> AST_generic.name option
val written_path : AST_generic.expr -> (AST_generic.name * string list) option
val path_of_type : AST_generic.type_ -> (AST_generic.name * string list) option
val qualified_path : AST_generic.name -> string list
val site_of_name : AST_generic.name -> AST_generic.SId.t option
val definition_binding : AST_generic.name -> AST_generic.SId.t option

type cls
type t

type position =
  | Term_position
  | Type_position

val build :
  lang:Lang.t ->
  classes:class_scope list list ->
  compiled_together:(Func_info.t list -> bool) ->
  defined:(class_scope -> bool) ->
  link:(class_scope -> parent -> scope_id option) ->
  outside:
    (position ->
    scope_id option ->
    AST_generic.name * string list ->
    scope_id option) ->
  may_implement:(interface:cls -> cls -> bool) ->
  t

val same : cls -> cls -> bool
val hash : cls -> int
val index : cls -> int
val scopes : cls -> class_scope list
val classes : t -> cls list
val class_of_scope : t -> scope_id -> cls option
val class_of_binding : t -> AST_generic.SId.t -> cls option
val object_of_binding : t -> AST_generic.SId.t -> cls option
val class_of_name :
  t ->
  position:position ->
  context:scope_id option ->
  AST_generic.name ->
  cls option
val class_of_path :
  t ->
  position:position ->
  context:scope_id option ->
  AST_generic.name * string list ->
  cls option
val order : t -> cls -> cls Linearisation.linearisation
val parents : t -> cls -> cls option list
val subclasses : t -> cls -> cls list
val class_side_parents : t -> cls -> cls option list
val own_members : cls -> string -> Func_info.t list
val member_table : cls -> Func_info.t list Common.SMap.t
val members_along : cls list -> Func_info.t list Common.SMap.t
val instance_fields : cls -> string list -> Func_info.t list
val object_fields : cls -> string list -> Func_info.t list
val extensions : cls -> string -> Func_info.t list
val bound_functions : cls -> Func_info.t list
val exposes : cls -> string -> bool
val is_abstraction : cls -> bool
val is_interface : cls -> bool
val is_trait_impl : cls -> bool
val name_of_class : t -> cls -> AST_generic.name option
val equal_type : t -> Structural_typing.equal_type
val descendants : t -> cls -> cls list
