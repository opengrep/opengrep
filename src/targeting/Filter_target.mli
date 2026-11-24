(* Select the file if it belongs to the language using Guess_lang.ml *)
val filter_target_for_xlang : Xlang.t -> Fpath.t -> bool

(* Select the file if it satisfies the include: exclude: constraints
 * in a rule paths: field *)
val filter_paths : Rule.paths -> string -> bool

(* The paths a rule's paths: constraints are matched against: the path
 * relative to the project root for a file found under a directory target,
 * the origin for a file given as a target (at each commit for a git blob). *)
val matched_paths : Target_and_root.target_proj_path -> Origin.t -> string list

(* A rule's paths: constraints on the paths of [matched_paths]; no
 * constraints, every target. *)
val rule_applies : Rule.paths option -> string list -> bool
