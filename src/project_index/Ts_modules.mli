(* TypeScript/JavaScript module handling. *)

(* Walk tsconfig.build.json / tsconfig.json files under [project_root] once
   and return their "exclude" globs, normalised relative to the root, with
   the "paths" entries of the configs at the root.  tsconfig files are
   JSONC; comments and trailing commas are stripped before parsing. *)
val discover :
  project_root:Fpath.t -> string list * (string * string list) list
