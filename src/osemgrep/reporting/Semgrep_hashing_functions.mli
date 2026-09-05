module Out = Semgrep_output_v1_t

val syntactic_id : Out.cli_match -> string
(** A unique key designed with notification user experience in mind.
    Results in fewer unique findings than core_unique_key.

    This uses the Murmur3 128 hash, and is used e.g. in Gitlab_sast and
    Gitlab_secrets output. *)

(** The fingerprint used to uniquely identify a match. Users depend on it for
    the deduplication of findings, so the default scheme reproduces pysemgrep's
    exactly. *)
module Match_based_id : sig
  (** How the rule part of the id is derived:
      - [Pysemgrep]: rule.py formula_string() over the raw rule, see
        Formula_string.ml. The default.
      - [Parsed_formula]: the pattern strings of the parsed formula only. *)
  type scheme = Pysemgrep | Parsed_formula

  val formula : scheme -> Rule.t -> Out.metavars option -> string
  (** The rule part of the id with the metavariables substituted; the string
      that gets hashed together with the path and the rule id. *)

  val partial :
    ?scheme:scheme -> Rule.t -> Rule_ID.t -> Out.metavars option -> string -> string
  (** Everything but the trailing match index of the id. *)
end
