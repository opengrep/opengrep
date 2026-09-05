(* An ignore file that cannot be read as a file is reported once, whichever
   reader meets it first: the '.semgrepignore' of the working directory is
   read both as its own level and by the walk that enters that directory.
   [forget_warnings] starts a new run; [warn_unreadable] is what the other
   reader reports through. *)
val forget_warnings : unit -> unit
val warn_unreadable : Fpath.t -> string (* reason *) -> unit

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
