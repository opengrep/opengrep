(* Per-language hooks for project_index; each language is a [t], looked up via [for_lang]. *)

module G = AST_generic

type wrapper = {
  w_simple_name : string;
  w_frozen_default : bool;
}

type t = {
  is_init_file : Fpath.t -> bool;
  rewrite_module_path : string -> string;
  module_path_from_ast : G.program -> string option;
  normalize_import_specifier : string -> string;
  class_dunders_from_decorators : G.attribute list -> string list;
  class_dunders_from_extends : G.class_definition -> string list;
  synth_call_dunders : G.expr -> string list option;
  inner_class_from_call : G.expr -> (string * string list) option;
  class_body_synth_methods : G.class_definition -> (string * Tok.t) list;
  class_body_extra_parents : G.class_definition -> string list list;
  extract_wrapper : G.entity -> wrapper option;
  wrapper_dunders : wrapper -> string list;
  walks_inheritance : bool;
  has_reexports : bool;
  include_anonymous_funcs : bool;
  unqualified_scope : [ `Per_file | `Per_directory | `Per_package ];
  (* This language's [Package]/[PackageEnd] directives are qn scopes (namespace
     blocks / package clauses), not the file's module identity (contrast Go). *)
  package_directive_is_namespace : bool;
  (* Class identity is its constant path, file-independent (Ruby reopening):
     drops the file-path prefix from class qns. *)
  class_identity_is_constant_path : bool;
  discover_excludes : project_root:Fpath.t -> string list;
  class_def_reshape :
    G.entity -> G.definition_kind -> (G.entity * G.definition_kind) option;

  (* Narrow the project [Type_state] by per-file import hints (Rust crate homonyms). *)
  narrow_methods_by_imports :
    fi_imports:(string * Names.Module_qn.t) list ->
    file_of_func:(Func_info.t -> string option) ->
    Type_state.t ->
    Type_state.t;

  strip_field_sigil : string -> string;
  class_constructor_synth_fields :
    G.function_definition -> (string * G.type_) list;
  (* PHP 8 ctor property promotion: typed ctor params are candidate fields. *)
  ctor_param_promotion : bool;
  interface_dispatch_uses_export_visibility : bool;
}

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
val default : t

val for_lang : Lang.t -> t
