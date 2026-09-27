module Fdef_tbl : Hashtbl.S with type key = AST_generic.function_definition
module SId_tbl : Hashtbl.S with type key = AST_generic.SId.t
module Field_path_map : Map.S with type key = string list

type kind =
  | Class_kind of AST_generic.class_kind
  | Module_kind

type declaration_kind =
  | Definition of Function_id.t
  | Singleton_object
  | Trait_impl of Function_id.t

type parent =
  | Bound of AST_generic.SId.t
  | Unbound of AST_generic.type_
  | Impl of Function_id.t

type parent_clause = {
  parent : parent;
  relation : Member_lookup.relation;
  written : AST_generic.type_ option;
  arguments : AST_generic.arguments option;
  delegate : AST_generic.expr option;
}

type member_import = {
  source : parent option;
  members : (string * string) list;
}

type class_scope = {
  binding : AST_generic.SId.t;
  declaration_kind : declaration_kind;
  members : Func_info.t list Common.SMap.t;
  fields : Func_info.t list Field_path_map.t;
  parents : parent_clause list;
  class_side_parents : parent list;
  member_imports : member_import list;
  type_members : (string * parent) list;
  requirements : string list;
  kind : kind;
  declaration : Lang_config.class_declaration;
  module_functions : Class_parents.module_functions;
  constructor_functions : Func_info.t list;
  object_fields : Func_info.t list Field_path_map.t;
  extensions : Func_info.t list Common.SMap.t;
  reopens : bool;
}

type scope_id = {
  scope_binding : AST_generic.SId.t;
  scope_declaration_kind : declaration_kind;
}

val equal_scope_id : scope_id -> scope_id -> bool
val hash_scope_id : scope_id -> int

module Scope_tbl : Hashtbl.S with type key = scope_id

val compare_scope : class_scope -> class_scope -> int
val scope_id_of : class_scope -> scope_id
val definition_scope : AST_generic.SId.t -> scope_id
val binding_of_id_info : AST_generic.id_info -> AST_generic.SId.t option
val id_info_of_name : AST_generic.name -> AST_generic.id_info
val name_of_type : AST_generic.type_ -> AST_generic.name option
val parent_of_type : AST_generic.type_ -> parent
val written_path : AST_generic.expr -> (AST_generic.name * string list) option
val path_of_type : AST_generic.type_ -> (AST_generic.name * string list) option
val qualified_path : AST_generic.name -> string list
val site_of_name : AST_generic.name -> AST_generic.SId.t option
val definition_binding : AST_generic.name -> AST_generic.SId.t option

val is_dereference_trait : Lang.t -> AST_generic.type_ -> bool

type cls
type t
type declared_type

type definition = {
  func : Func_info.t;
  signature : declared_type Structural_typing.signature;
}

type selected = {
  definitions : (cls, definition) Member_lookup.selection;
  functions : (cls, Func_info.t) Member_lookup.selection;
}

module Selection_key : sig
  type t = {
    build_configuration : int;
    side : Class_parents.side;
    name : string;
    importing : int list;
    levels : (int * int) list list;
  }
end

module Selection_tbl : Hashtbl.S with type key = Selection_key.t

module Overriding_key : sig
  type t = {
    build_configuration : int;
    cls : int;
    name : string;
  }
end

module Overriding_tbl : Hashtbl.S with type key = Overriding_key.t

type memo = {
  selections : selected Selection_tbl.t;
  overriding : definition list Overriding_tbl.t;
  dispatched : (definition list * Func_info.t list) list Overriding_tbl.t;
}

val create_memo : unit -> memo

type position =
  | Term_position
  | Type_position

val build :
  lang:Lang.t ->
  classes:class_scope list list ->
  compiled_together:(Func_info.t list -> bool) ->
  compiled_in:(int -> Func_info.t -> bool) ->
  defined:(class_scope -> bool) ->
  link:(class_scope -> parent -> scope_id option) ->
  cross_file_resolver:
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
val order : t -> cls -> cls Member_lookup.lookup_order
val parents : t -> cls -> cls option list
val parent_clauses : t -> cls -> (cls option * parent_clause) list

type import_origin =
  | Imported_from of cls
  | Imported_from_mixins
  | Imported_from_unknown

val imports : t -> cls -> string -> (import_origin * string) list
val delegations : t -> cls -> (cls option * cls option) list
val requirements : cls -> string list
val subclasses : t -> cls -> cls list
val class_side_parents : t -> cls -> cls option list
val distinct_by : ('item -> Func_info.t) -> 'item list -> 'item list
val distinct_definitions : Func_info.t list -> Func_info.t list
val own_members : cls -> string -> Func_info.t list
val member_table : cls -> Func_info.t list Common.SMap.t
val instance_fields : cls -> string list -> Func_info.t list
val object_fields : cls -> string list -> Func_info.t list
val extensions : cls -> string -> Func_info.t list
val constructor_functions : cls -> Func_info.t list
val is_module_function : cls -> string -> bool
val is_abstract_type : cls -> bool
val declarations : cls -> Lang_config.class_declaration list
val is_interface : cls -> bool
val is_trait : cls -> bool
val is_trait_impl : cls -> bool
val name_of_class : t -> cls -> AST_generic.name option
val satisfies : required:definition -> definition -> bool
val overrides : lang:Lang.t -> nearer:definition -> farther:definition -> bool

val select_member :
  lang:Lang.t ->
  cls Member_lookup.level list ->
  defines:(cls -> definition list) ->
  (cls, definition) Member_lookup.selection

val definition_table : t -> cls -> definition list Common.SMap.t
val own_definitions : t -> cls -> string -> definition list
val member_definitions : t -> cls -> definition list Common.SMap.t
val members : t -> cls -> Func_info.t list Common.SMap.t
val compiled_in : t -> build_configuration:int -> Func_info.t -> bool
val memo : t -> memo
val selected_of : (cls, definition) Member_lookup.selection -> selected
val level_classes : cls Member_lookup.level list -> cls list

val nearest :
  cls Member_lookup.level list ->
  defines:(cls -> 'found list) ->
  (cls, 'found) Member_lookup.selection

val find_nearest : cls Member_lookup.level list -> (cls -> 'found option) -> 'found option
val descendants : t -> cls -> cls list
