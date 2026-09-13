type t

val empty : t

val build :
  file_infos:Types.file_info list ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  t

val bindings_of_file : t -> string -> Scope_binding.positioned_binding list
