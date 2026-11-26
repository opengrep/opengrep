type t

val empty : t

val build :
  file_infos:Types.file_info list ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  t

val closure_of_file : t -> string -> unit Common.SMap.t

val files_in_closure : unit Common.SMap.t -> string list

(* The functions of a name visible in a file: its own declarations, then those
   of the files of its include closure by increasing path. *)
val visible_functions_of_name :
  t -> closure:unit Common.SMap.t -> string -> string -> Func_info.t list
