open Types

(* No simple-name fallback (unsound: floods false positives). *)
val resolve_parent_qn :
  imports:(string * Names.Module_qn.t) list ->
  reexport_map:(Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t ->
  known_class_qns:(Names.Class_qn.t, unit) Hashtbl.t ->
  string list -> Names.Module_qn.t option

(* [cross_module_parents]: whether a parent may bind to a class sharing
   no leading qn part with the child — legitimate for namespace/package
   scoped languages (C++/C#/Java/Go), a homonym trap for module-scoped
   ones (Python-style: cross-file names need imports). *)
val resolve_parent_by_scope :
  cross_module_parents:bool ->
  by_qn:(Names.Class_qn.t, class_info) Hashtbl.t ->
  qns_by_leaf:(Names.Class_name.t, Names.Class_qn.t list) Hashtbl.t ->
  class_info -> string list -> Names.Module_qn.t option

(* C3 linearisation of every class hierarchy: extends the lattice with each
   class's inherited methods and also returns the per-class inherited lists
   (the source for the diagnostic inherited-method entry rows). *)
val inherit_into_type_state :
  cross_module_parents:bool ->
  reexport_map:(Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t ->
  class_infos:class_info list ->
  func_def_file:(Graph_from_AST.func_info -> string option) ->
  Type_state.t -> Type_state.t * class_fun_info list
