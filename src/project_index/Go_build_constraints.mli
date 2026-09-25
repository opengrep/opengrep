type t

val empty : t
val of_files : (Fpath.t * AST_generic.program) list -> t
val visible_from : t -> Fpath.t -> Func_info.t -> bool
val file_visible_from : t -> Fpath.t -> Fpath.t -> bool
val files_compiled_together : t -> Fpath.t list -> bool
val compiled_together : t -> Func_info.t list -> bool
