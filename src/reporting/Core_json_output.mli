module Out = Semgrep_output_v1_j

(* entry point *)
val core_output_of_matches_and_errors : ?inline:bool -> Core_result.t -> Out.core_output

(* Can return an Error when we get a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
 *)
val match_to_match :
 ?inline:bool -> Core_result.processed_match -> (Out.core_match, Core_error.t) result

(* now used also in osemgrep *)
val error_to_error : Core_error.t -> Out.core_error
val dedup_and_sort :
  Core_match.rule_id_options Rule_ID.Map.t -> Out.core_match list -> Out.core_match list

(* For unit testing *)
type key [@@deriving show]

val test_core_unique_key : Out.core_match -> key
