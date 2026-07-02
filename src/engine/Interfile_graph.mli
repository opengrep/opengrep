type interfile_graph = Call_graph.G.t

type resolved_asts = (string, AST_generic.program) Hashtbl.t

val load_interfile_graph :
  < Cap.fork > ->
  ?ncores:int ->
  targeting_conf:Find_targets.conf ->
  Lang.t -> Fpath.t -> interfile_graph option

val load_interfile_build :
  < Cap.fork > ->
  ?ncores:int ->
  targeting_conf:Find_targets.conf ->
  Lang.t -> Fpath.t -> (interfile_graph * resolved_asts) option

val absolutify_fid :
  Fpath.t option -> Function_id.t -> Function_id.t

val files_of_graph :
  Call_graph.G.t -> Fpath.t list
(** Sorted, unique file paths of all vertices. *)
