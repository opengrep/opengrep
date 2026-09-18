(* Per-language hooks for project_index; each language is a [t], looked up via [for_lang]. *)

module G = AST_generic

type wrapper = {
  w_simple_name : string;
  w_frozen_default : bool;
}

type project_discovery = {
  excludes : string list;
  module_paths : (string * string list) list;
}

type parent_position = Prepended | Appended

type class_parent = {
  cp_path : string list;
  cp_position : parent_position;
}

type superclass_position =
  | Superclass_before_mixins
  | Superclass_after_mixins

type singleton_exposure =
  | No_singleton_exposure
  | Every_method_is_a_singleton
  | Named_singleton_methods of string list

type parent_resolution =
  | Parent_in_own_scope
  | Parent_by_lexical_scope
  | Parent_by_lexical_scope_then_simple_name

type relative_module =
  | Root_module
  | Own_module
  | Parent_module

type reexport_source =
  | Reexports_from_init_file
  | Reexports_from_public_directives

type unaliased_import_local =
  | First_segment_binds
  | Last_segment_binds

type binding_kind =
  | Wildcard_import
  | Single_import
  | Own_definition
  | Package_member

type t = {
  is_init_file : Fpath.t -> bool;
  is_stub_file : Fpath.t -> bool;
  project_scope_admits : G.entity option -> bool;
  rewrite_module_path : string -> string;
  module_path_from_ast : G.program -> string option;
  normalize_import_specifier : string -> string;
  specifiers_name_files : bool;
  specifiers_are_uris : bool;
  specifier_candidates : Fpath.t -> string list;
  class_dunders_from_decorators : G.attribute list -> string list;
  class_dunders_from_extends : G.class_definition -> string list;
  synth_call_dunders : G.expr -> string list option;
  inner_class_from_call : G.expr -> (string * string list) option;
  class_body_synth_methods : G.class_definition -> (string * Tok.t) list;
  class_body_extra_parents : G.class_definition -> class_parent list;
  superclass_position : superclass_position;
  class_body_singleton_methods : G.class_definition -> singleton_exposure;
  extract_wrapper : G.entity -> wrapper option;
  wrapper_dunders : wrapper -> string list;
  walks_inheritance : bool;
  has_reexports : bool;
  reexport_source : reexport_source;
  include_anonymous_funcs : bool;
  unqualified_scope :
    [ `Per_file | `Per_directory | `Per_package | `Per_namespace
    | `Per_module | `Per_go_package | `Per_constant_path | `Per_crate
    | `Per_translation_unit | `Per_project ];
  precedence : binding_kind -> int;
  own_package_members_kind : binding_kind;
  namespaces_nest : bool;
  relative_module_names : (string * relative_module) list;
  import_head_may_be_own_module : bool;
  (* This language's [Package]/[PackageEnd] directives are qn scopes (namespace
     blocks / package clauses), not the file's module identity (contrast Go). *)
  package_directive_is_namespace : bool;
  module_definition_is_namespace : bool;
  object_members_bind_in_namespace : bool;
  dict_literal_is_object_definition : bool;
  module_is_returned_value : bool;
  companion_object_has_own_name : bool;
  unaliased_import_binds : unaliased_import_local;
  hiding_alias : string option;
  (* Class identity is its constant path, file-independent (Ruby reopening):
     drops the file-path prefix from class qns. *)
  class_identity_is_constant_path : bool;
  discover_project : project_root:Fpath.t -> project_discovery;
  class_def_reshape :
    G.entity -> G.definition_kind -> (G.entity * G.definition_kind) option;
  strip_field_sigil : string -> string;
  class_constructor_synth_fields :
    G.function_definition -> (string * G.type_) list;
  (* PHP 8 ctor property promotion: typed ctor params are candidate fields. *)
  ctor_param_promotion : bool;
  interface_dispatch_uses_export_visibility : bool;
  parent_resolution : parent_resolution;
  package_clause_of_ast : G.program -> string option;
  method_owner_of_funcdef : G.function_definition -> string option;
  name_is_exported : string -> bool;
}

val equal_parent_position : parent_position -> parent_position -> bool

val decorator_simple_name : G.attribute -> string option
val entity_simple_name : G.entity -> string option
val name_to_path : G.name -> string list

val python : t
val ruby : t
val go : t
val typescript : t
val php : t
val rust : t
val java : t
val kotlin : t
val csharp : t
val cpp : t
val c : t
val clojure : t
val elixir : t
val apex : t
val swift : t
val vb : t
val lua : t
val dart : t
val julia : t
val crystal : t
val default : t

val for_lang : Lang.t -> t

val top_level_scope_is_project : t -> bool

val forms_overload_groups : lang:Lang.t -> cfg:t -> bool
