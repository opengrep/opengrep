type t

val empty : t

val discover : project_root:Fpath.t -> Fpath.t list -> t

val module_qn_of_file : t -> Fpath.t -> Names.Module_qn.t option
