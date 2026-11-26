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

(* A [Constraint] parent is a class that a mixin may be applied to: it is
   in the mixin's own resolution order, not in the order of a class that
   applies the mixin. *)
type parent_position = Prepended | Appended | Constraint

type class_parent = {
  cp_path : string list;
  cp_position : parent_position;
  (* The parent is in the superclass position of a class or an object. *)
  cp_is_superclass : bool;
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
  (* The import binds every top-level name of the module (QL [import M]). *)
  | Every_name_binds

(* Which members of a class a bare name in one of its methods refers to. *)
type method_member_scope =
  | Members_not_in_method_scope
  (* Every member along the class's resolution order binds in the class's
     scope (Solidity). *)
  | Members_in_class_scope
  (* The class's own members bind in its scope; an inherited member binds
     only when no lexical binding exists, and neither a static member nor a
     member not exported from another file is inherited (Dart). *)
  | Inherited_members_after_lexical_scope

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
  (* A specifier without a leading dot refers to a file relative to the
     importing file before one on the library paths (Circom [include]). *)
  bare_specifiers_relative_first : bool;
  (* The specifiers of the files that the [part] directives of a file add to
     its library (Dart [part 'x.dart';]). *)
  library_parts_of_ast : G.program -> string list;
  specifier_candidates : Fpath.t -> string list;
  (* [import "path";] without an alias binds every top-level name of the
     file (Solidity). *)
  path_import_binds_all_names : bool;
  (* A wildcard import without an explicit export list ([__all__]) leaves out
     the names that begin with an underscore: Python, and Dart, where such
     names are private to their library. It holds by default. *)
  wildcard_import_omits_underscore_names : bool;
  (* An import binds its names from its statement onward and rebinds a name
     that an earlier import bound, so of the imports before a class that
     bind its parent, the last one binds it (Python). Elsewhere two imports
     that bind the name to different classes bind it through neither. *)
  imports_bind_in_statement_order : bool;
  (* The manifest of a crate whose [package] name heads the module path of
     its files (Rust's [Cargo.toml], Cairo's [Scarb.toml]). *)
  crate_manifest : string option;
  (* The stems of the files under [src] that are the root module of a crate
     (Rust's [lib.rs] and [main.rs], Scarb's [lib.cairo]). *)
  crate_root_stems : string list;
  class_dunders_from_decorators : G.attribute list -> string list;
  class_dunders_from_extends : G.class_definition -> string list;
  synth_call_dunders : G.expr -> string list option;
  inner_class_from_call : G.expr -> (string * string list) option;
  class_body_synth_methods : G.class_definition -> (string * Tok.t) list;
  class_body_extra_parents : G.class_definition -> class_parent list;
  superclass_position : superclass_position;
  (* Solidity: [is A, B] lists the most base-like contract first, so the
     linearisation reads the bases from the right. *)
  bases_listed_most_base_first : bool;
  (* The extends clause of a mixin ([G.Trait]) lists the classes it may be
     applied to (Dart [mixin M on B]), not superclasses. *)
  mixin_extends_lists_constraints : bool;
  (* The superclass position takes a class, never a mixin ([G.Trait]); a
     mixin is applied in the [with] position (Dart). *)
  superclass_is_never_a_mixin : bool;
  class_body_singleton_methods : G.class_definition -> singleton_exposure;
  extract_wrapper : G.entity -> wrapper option;
  wrapper_dunders : wrapper -> string list;
  walks_inheritance : bool;
  method_member_scope : method_member_scope;
  has_reexports : bool;
  reexport_source : reexport_source;
  include_anonymous_funcs : bool;
  (* Assigning a function value to an identifier ([f <- function(x) ..])
     defines the function under that identifier in the scope of the
     assignment, in the per-file and per-project scopes (R). *)
  assigned_function_defines_identifier : bool;
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
