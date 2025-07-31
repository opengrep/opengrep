(* See Core_match.ml for more info *)

type range_loc = Tok.location * Tok.location
[@@deriving show, eq, ord]

type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id;
  engine_of_match : Engine_kind.engine_of_finding;
  env : Metavariable.bindings;
  (* location info *)
  path : Target.path;
  range_loc : Tok.location * Tok.location;
  enclosure : Enclosure.t option;
  ast_node : AST_generic.any option;
  tokens : Tok.t list Lazy.t;
  (* trace *)
  taint_trace : Taint_trace.t Lazy.t option;
  (* for SCA *)
  sca_match : SCA_match.t option;
  (* for Secrets *)
  validation_state : Rule.validation_state;
  severity_override : Rule.severity option;
  metadata_override : JSON.t option;
  (* A field to be populated based on intra-formula `fix` keys.
     This is _prior_ to AST-based autofix and interpolation, which occurs in
     Autofix.ml.
  *)
  fix_text : string option;
  (* facts (known truths derived from the cfg) of a match.
     it is used for filtering out matches that do not satisfy the
     comparison condition. this field is added here so that
     it can be passed into and used in Match_search_mode.filter_ranges.
  *)
  facts : AST_generic.facts;
}

(* a record but really only the [id] field should matter *)
and rule_id = {
  id : Rule_ID.t;
  (* extra info useful for Core_json_output *)
  message : string;
  metadata : JSON.t option;
  fix : string option;
  fix_regexp : Rule.fix_regexp option;
  langs : Lang.t list;
  pattern_string : string;
  options : rule_id_options option
}
[@@deriving show, eq]

and rule_id_options  = {
  max_match_per_file : int option;
  (* maximum number of matches per file. *)
}
[@@deriving show, eq]

val rule_id_options_of_rule_options: Rule_options_t.t -> rule_id_options
val rule_id_options_of_rule_options_opt: Rule_options_t.t option -> rule_id_options option

val rule_ids_to_map: rule_id list -> rule_id Rule_ID.Map.t
val rule_ids_to_rule_id_options_map: rule_id list -> rule_id_options Rule_ID.Map.t
val to_rule_id_options_map: t list -> rule_id_options Rule_ID.Map.t

(* remove duplicate *)
val uniq : t list -> t list

(* set the engine_kind to `PRO in the match *)
val to_proprietary : t -> t

(* Remove matches that are strictly inside another match *)
val no_submatches : t list -> t list
val range : t -> Range.t
