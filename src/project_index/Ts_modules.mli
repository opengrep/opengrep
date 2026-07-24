(* TypeScript/JavaScript module handling. *)

(* Walk tsconfig.build.json / tsconfig.json files under [project_root] and
   return their "exclude" globs, normalised relative to the root.  tsconfig
   files are JSONC; comments and trailing commas are stripped before
   parsing. *)
val discover_excludes : project_root:Fpath.t -> string list

(* Index the trailing path-segment suffixes of every file (TS/JS extensions
   and "/index" stripped) so a non-relative import specifier can be matched
   against project files.  Only suffixes up to [max_suffix_segs] segments are
   indexed — the length of the longest bare specifier in the project — since a
   longer suffix can never be looked up. *)
val build_path_suffix_index :
  max_suffix_segs:int -> string list -> (string, string list) Hashtbl.t

(* Candidate file paths for an import specifier: a relative specifier
   expands to sibling-path candidates with TS/JS extensions and /index
   variants; a bare specifier consults the path-suffix index. *)
val resolve_specifier :
  ?path_suffix_index:(string, string list) Hashtbl.t option ->
  current_file:Fpath.t ->
  string ->
  string list
