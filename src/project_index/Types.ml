module G = AST_generic

type def_kind = K_function | K_method | K_class

type entry = {
  id : Function_id.t;
  name : string;
  kind : def_kind;
  file : Fpath.t;
  range : Range.t option;
  defining_class_id : Function_id.t option;
}

type class_info = {
  ci_id : Function_id.t;
  ci_qn : Names.Class_qn.t;
  ci_class_kind : G.class_kind;
  ci_file : Fpath.t;
  ci_range : Range.t option;
  ci_parent_paths : string list list;
  ci_imports : (string * Names.Module_qn.t) list;
  ci_decorator_names : string list;
}

type dataclass_wrapper = Index_lang_rules.wrapper

type import_kind =
  | I_default
  | I_named of string
  | I_namespace

type file_info = {
  fi_file : Fpath.t;
  fi_module_path : Names.Module_qn.t;
  fi_imports : (string * Names.Module_qn.t) list;
  fi_import_specifiers : (string * string * import_kind) list;
  fi_dataclass_wrappers : dataclass_wrapper list;
  fi_ast : G.program;
  fi_observations : Walker.Observation.t list;
}
