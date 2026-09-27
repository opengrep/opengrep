module Field_path_map = Class_table.Field_path_map

type kind = Class_table.kind =
  | Class_kind of AST_generic.class_kind
  | Module_kind

type declaration_kind = Class_table.declaration_kind =
  | Definition of Function_id.t
  | Singleton_object
  | Trait_impl of Function_id.t

type parent = Class_table.parent =
  | Bound of AST_generic.SId.t
  | Unbound of AST_generic.type_
  | Impl of Function_id.t

type parent_clause = Class_table.parent_clause = {
  parent : parent;
  relation : Member_lookup.relation;
  written : AST_generic.type_ option;
  arguments : AST_generic.arguments option;
  delegate : AST_generic.expr option;
}

type member_import = Class_table.member_import = {
  source : parent option;
  members : (string * string) list;
}

type class_scope = Class_table.class_scope = {
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

type referent_class =
  | Of_class of Class_table.cls
  | Of_external_class
  | Of_unknown_class

type referent = {
  variable : AST_generic.SId.t;
  path : string list;
  referent_class : referent_class;
}

type receiver_class =
  | Class of Class_table.cls
  | Exact of Class_table.cls
  | Class_object of Class_table.cls
  | Ancestors_of of Class_table.cls
  | Object_of of referent
  | External_class
  | Top_level_object
  | Unknown

type resolution =
  | Defined of Func_info.t list
  | External

type dispatch

type lookup_result = {
  resolution : resolution;
  dispatches : dispatch list;
}

val static_selection : resolution -> lookup_result

type receiver_relation =
  | Method_of
  | Extension_of

type t

val top_level_defs_are_methods_of_object : Lang.t -> bool
val member_name : Func_info.t -> string option
val create : lang:Lang.t -> AST_generic.program -> Func_info.t list -> t
val functions_of_binding : t -> AST_generic.SId.t -> Func_info.t list
val function_of_node : t -> Function_id.t -> Func_info.t option
val class_of_binding : t -> AST_generic.SId.t -> class_scope option
val classes : t -> class_scope list
val defined_here : t -> class_scope -> bool
val owner : t -> class_scope -> class_scope option

val unbound_receivers :
  t -> (receiver_relation * AST_generic.name * Func_info.t list) list

type use =
  | Called
  | Referenced

val class_table : t -> Class_table.t
val is_subclass : t -> Class_table.cls -> Class_table.cls -> bool option

val with_project :
  t ->
  Class_table.t ->
  extension_visible:(string -> Func_info.t -> bool) ->
  build_configuration:int ->
  cross_file_resolver:(t -> caller:Function_id.t option -> AST_generic.expr -> lookup_result) ->
  t

val with_own_memo : t -> Class_table.memo -> t

val with_types : t -> Type_state.t -> t

val class_definitions :
  t -> (Class_table.scope_id * AST_generic.definition_kind) list

val node_of_function : Func_info.t -> Function_id.t option

val class_of_expr :
  t -> caller:Function_id.t option -> AST_generic.expr -> Class_table.cls option

val class_of_declared_type :
  t ->
  context:Class_table.scope_id option ->
  AST_generic.type_ ->
  Class_table.cls option

val class_of_type_written_in :
  t ->
  written_in:Function_id.t option ->
  AST_generic.type_ ->
  Class_table.cls option

val external_type_path :
  t ->
  written_in:Function_id.t option ->
  AST_generic.type_ ->
  string list option

val accepts_external : t -> Class_table.cls -> string list -> bool option
val this_class : t -> caller:Function_id.t option -> Class_table.cls option
val class_of_function : t -> Func_info.t -> Class_table.cls option
val constructors : t -> class_scope -> resolution
val constructors_of_class : t -> Class_table.cls -> resolution
val constructs : t -> use -> bool
val self_receiver : t -> caller:Function_id.t option -> receiver_class

val receiver_class :
  t -> caller:Function_id.t option -> AST_generic.expr -> receiver_class

val resolve_member :
  t -> caller:Function_id.t option -> receiver_class -> string -> lookup_result

val resolve_callee :
  t -> caller:Function_id.t option -> AST_generic.expr -> lookup_result

val resolve_reference :
  t -> caller:Function_id.t option -> AST_generic.expr -> lookup_result

val resolve_construction : t -> AST_generic.type_ -> resolution
val resolve_qualified : t -> AST_generic.name -> resolution

val resolve_call :
  t -> caller:Function_id.t option -> AST_generic.expr -> lookup_result

val class_of_member_call :
  t ->
  caller:Function_id.t option ->
  AST_generic.expr ->
  (Class_table.cls option * lookup_result Lazy.t) option

val dispatch : t -> lookup_result -> Func_info.t list -> Func_info.t list
val dispatched : t -> lookup_result -> resolution

val reaching_values :
  t -> caller:Function_id.t option -> AST_generic.SId.t -> AST_generic.expr list
