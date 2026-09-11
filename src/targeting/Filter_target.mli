(* Select the file if it belongs to the language using Guess_lang.ml *)
val filter_target_for_xlang : Xlang.t -> Fpath.t -> bool

(* Select the file if it satisfies the include: exclude: constraints
 * in a rule paths: field *)
val filter_paths : Rule.paths -> Fpath.t -> bool

(* A rule's paths: constraints on a target's origin: the path as the user
 * names it, at the commit for a git blob; no constraints, every origin. *)
val rule_applies_to_origin : Rule.paths option -> Origin.t -> bool
