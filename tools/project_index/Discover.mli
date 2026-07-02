(* Read [project-includes]/[project-excludes] arrays from [pyrefly.toml]. *)
val read_pyrefly_includes_excludes : string -> string list * string list

(* [file] unchanged when it is not under [project_root]. *)
val relative_to : project_root:Fpath.t -> Fpath.t -> Fpath.t

(* Standalone-CLI conf: no size cap, ignores [.semgrepignore] but not [.gitignore]. *)
val projidx_default_targeting_conf : Find_targets.conf

val discover_files :
  targeting_conf : Find_targets.conf ->
  lang : Lang.t ->
  project_root : Fpath.t ->
  includes : string list ->
  excludes : string list ->
  Fpath.t list
