(* ([entries], [graph], [scanned], [skipped]); [graph] vertices are
   [Function_id.t], paths may be relative (see [Call_graph.make_paths_absolute]). *)
val collect :
  < Cap.fork > ->
  ?targeting_conf:Find_targets.conf ->
  lang:Lang.t ->
  project_root:Fpath.t ->
  ncores:int ->
  includes:string list ->
  excludes:string list ->
  unit ->
  Types.entry list * Call_graph.G.t * int * int

(* Resolved AST (cross-file [id_callee_definition]) of [target], or [None] if
   not discovered. *)
val resolve_ast_for_file :
  < Cap.fork > ->
  ?targeting_conf:Find_targets.conf ->
  lang:Lang.t ->
  project_root:Fpath.t ->
  ncores:int ->
  target:Fpath.t ->
  unit ->
  AST_generic.program option

(* The result is the call graph, the resolved AST map (abs path to AST with
   cross file [id_callee_definition]), the skipped tokens by file and the
   files the index failed to process, each with its error. A failed file's
   functions and edges are missing from the graph, so the caller reports the
   failures as scan errors. *)
val collect_resolved :
  < Cap.fork > ->
  ?targeting_conf:Find_targets.conf ->
  lang:Lang.t ->
  project_root:Fpath.t ->
  ncores:int ->
  includes:string list ->
  excludes:string list ->
  unit ->
  Call_graph.G.t * (string, AST_generic.program) Hashtbl.t
  * (string, Tok.location list) Hashtbl.t
  * Core_error.t list
