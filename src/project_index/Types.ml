module G = AST_generic

type def_kind = K_function | K_method | K_class | K_companion

type entry = {
  id : Function_id.t;
  name : string;
  qn : Names.Def_qn.t;
  kind : def_kind;
  file : Fpath.t;
  range : Range.t option;
  defining_class_id : Function_id.t option;
  entity : G.entity option;
}

type definition =
  | Function_definitions of Func_info.t list
  | Class_definition of {
      class_file : Fpath.t;
      class_qn : Names.Class_qn.t;
      class_name : string;
      class_companion : Names.Class_qn.t option;
      class_entity : G.entity option;
    }

type import_binds =
  | Binds_any
  | Binds_function
  | Binds_constant
  | Binds_type
  | Binds_module

type import_role = Role_binds | Role_reexports

type import = {
  im_local : string;
  im_alias : string option;
  im_target : Names.Module_qn.t;
  im_tok : Tok.t;
  im_static : bool;
  im_global : bool;
  im_binds : import_binds;
  im_role : import_role;
  im_hidden : string list;
}

type class_info = {
  ci_id : Function_id.t;
  ci_qn : Names.Class_qn.t;
  ci_name : string;
  ci_class_kind : G.class_kind;
  ci_file : Fpath.t;
  ci_range : Range.t option;
  ci_parent_paths : Index_lang_rules.class_parent list;
  ci_singleton_exposure : Index_lang_rules.singleton_exposure;
  ci_imports : import list;
  ci_decorator_names : string list;
  ci_entity : G.entity option;
}

(* A class paired with functions attributed to it, e.g. the methods it
   inherits via the C3 linearisation. *)
type class_fun_info = class_info * Func_info.t list

type dataclass_wrapper = Index_lang_rules.wrapper

type file_info = {
  fi_file : Fpath.t;
  fi_module_path : Names.Module_qn.t;
  fi_package_clause : string option;
  fi_namespace_scopes : Names.Module_qn.t list;
  fi_imports : import list;
  fi_dataclass_wrappers : dataclass_wrapper list;
  fi_ast : G.program;
  fi_observations : Walker.Observation.t list;
  fi_module_object : Names.Class_qn.t option;
}
