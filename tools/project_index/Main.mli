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

(* Resolved AST (cross-file [id_resolved]) of [target], or [None] if not discovered. *)
val resolve_ast_for_file :
  < Cap.fork > ->
  ?targeting_conf:Find_targets.conf ->
  lang:Lang.t ->
  project_root:Fpath.t ->
  ncores:int ->
  target:Fpath.t ->
  unit ->
  AST_generic.program option

(* Call graph + resolved-AST map (abs path -> AST with cross-file [id_resolved]). *)
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

val cmd : unit Cmdliner.Cmd.t
