(* Per-rule interfile taint dispatch: each rule folds its call-graph
   subgraph in topo order as one sequential task, sharing a signature db. *)

type rule_state
(** Built by [build_rule_states], consumed by [run_rule]. *)

val rule_id_of : rule_state -> Rule_ID.t

(* The three below are consumed by tools/opengrep-interfile-graph. *)

val relevant_graph_of : rule_state -> Call_graph.G.t

val topo_order_of : rule_state -> Function_id.t list
(** Leaves first. *)

val extract_signatures : rule_state -> Shape_and_sig.signature_database

val run_rule : rule_state -> Core_match.t list

val interfile_taint_rule_ids :
  taint_interfile:bool ->
  Rule.t list ->
  Rule_ID.t list
(** IDs of taint rules that are interfile (global flag or per-rule option). *)

val build_rule_states :
  < Cap.fork > ->
  ncores:int ->
  taint_interfile:bool ->
  valid_rules:Rule.t list ->
  targets:Target.t list ->
  targeting_conf:Find_targets.conf ->
  xconf:Match_env.xconfig ->
  rule_state list * Xlang.t list * (Rule_ID.t * Fpath.t list) list
(** Returns rule_states, the interfile languages, and per-rule target
    abs_paths dispatch doesn't cover (to run in per-target intrafile mode). *)
