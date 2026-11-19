(* Per-rule interfile taint dispatch: each rule computes the signatures of
   its call-graph subgraph in one task, then analyses its files in chunks,
   in parallel, against those signatures. *)

type rule_state
(** Built by [build_rule_states], consumed by [signature_fixpoint]. *)

val rule_id_of : rule_state -> Rule_ID.t

(* The three below are consumed by tools/opengrep-interfile-graph. *)

val relevant_graph_of : rule_state -> Call_graph.G.t

val topo_order_of : rule_state -> Function_id.t list
(** Leaves first. *)

val extract_signatures : rule_state -> Shape_and_sig.signature_database

type rule_signatures
(** A rule's state with the stable signatures of its functions. *)

val signature_fixpoint : rule_state -> rule_signatures

(* An exception of a rule, which spans many files: the error carries the rule
   and no file. *)
val rule_error : Rule_ID.t -> Exception.t -> Core_error.t

(* A rule whose time limit ran out reports an Out.Timeout error. *)
val rule_timeout_error : Rule_ID.t -> Core_error.t

(* Runs a task of a rule under the limits of the rule; None when the rule's
   time limit runs out. *)
type task_limit = {
  run :
    'a.
    Time_limit.shared_limit option -> Rule_ID.t -> (unit -> 'a) -> 'a option;
}

val findings_of_rules :
  < Cap.fork > ->
  ncores:int ->
  limit:task_limit ->
  on_findings:(Core_match.t list -> unit) ->
  (rule_signatures * Time_limit.shared_limit option) list ->
  (Rule_ID.t * (Core_match.t list, Core_error.t) result) list
(** The findings of every rule, from its files in chunks that run in one
    parallel map, against the stable signatures, each under the limits of its
    rule. The merge of a rule's chunks runs under the same limits, and
    [on_findings] receives a rule's findings once the merge has completed. A
    rule with a failed or timed-out chunk or merge gives one error. *)

val rule_is_interfile : Rule.t -> bool
(** A taint rule the interfile task runs: its options turn interfile on. *)

val interfile_taint_rule_ids :
  Rule.t list ->
  Rule_ID.t list
(** IDs of taint rules that are interfile. *)

val build_rule_states :
  < Cap.fork ; Cap.time_limit ; Cap.memory_limit > ->
  ncores:int ->
  max_memory_mb:int ->
  valid_rules:Rule.t list ->
  targets:Target.t list ->
  respect_rule_paths:bool ->
  targeting_conf:Find_targets.conf ->
  scanning_roots:Scanning_root.directory list ->
  xconf:Match_env.xconfig ->
  rule_state list * Xlang.t list * Core_error.t list
  * (Fpath.t -> Tok.location list)
(** Returns rule_states, the interfile languages, and the errors of the
    files the analysis lost or left out and of the rules that did not run,
    as a per-target failure reports them; they go with the scan's errors. *)
