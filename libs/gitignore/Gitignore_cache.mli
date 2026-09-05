(* The path of the ignore file of a folder, or the file itself when the
   name given on the command line is an absolute path. *)
val ignore_file_path : filename:string -> Fpath.t -> Fpath.t

(* The contents of an ignore file, or None when there is no such file or
   when it cannot be read as a file. The latter is reported with a warning,
   once per file for the life of the cache. *)
val read_ignore_file_opt : Gitignore.gitignores_cache -> Fpath.t -> string option

(* Initialize the cache for a project defined by the project root folder.
   See the doc in Gitignore.ml about gitignore_filenames for more information
   on the ?gitignore_filenames parameter below.
*)
val create :
  ?gitignore_filenames:Gitignore.gitignore_filename list ->
  project_root:Fpath.t ->
  unit ->
  Gitignore.gitignores_cache

(*
   Load (or get it back from the cache) the .gitignore files applicable to
   target files in the given folder.
*)
val load :
  Gitignore.gitignores_cache ->
  Ppath.t (* directory *) ->
  Gitignore.level option
