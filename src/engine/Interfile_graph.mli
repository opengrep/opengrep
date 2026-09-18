type interfile_graph = Call_graph.G.t

type resolved_asts = (string, AST_generic.program) Hashtbl.t

type skipped_tokens = (string, Tok.location list) Hashtbl.t

val load_interfile_graph :
  < Cap.fork > ->
  ?ncores:int ->
  targeting_conf:Find_targets.conf ->
  Lang.t -> Fpath.t -> interfile_graph option

val load_interfile_build :
  < Cap.fork > ->
  ?ncores:int ->
  targeting_conf:Find_targets.conf ->
  Lang.t -> Fpath.t ->
  (interfile_graph * resolved_asts * skipped_tokens * Core_error.t list)
  option
(** The third component is the skipped tokens of each partially parsed file.
    The fourth is the files the index failed to process, each with its error;
    their functions and edges are missing from the graph, so the caller
    reports them as scan errors. *)

val absolutify_fid :
  Fpath.t option -> Function_id.t -> Function_id.t

val files_of_graph :
  Call_graph.G.t -> Fpath.t list
(** Sorted, unique file paths of all vertices. *)
