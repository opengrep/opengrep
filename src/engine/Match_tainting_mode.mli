type fun_info = {
  name : IL.name;
  class_name_str : string option;
  method_properties : AST_generic.expr list;
  cfg : IL.fun_cfg;
  fdef : AST_generic.function_definition;
  is_static : bool;
  is_lambda_assignment : bool;
  taint_inst : Taint_rule_inst.t option;
}

val build_info_map :
  lang:Lang.t ->
  ?fid_filter:(Function_id.t -> bool) ->
  AST_generic.program ->
  fun_info Shape_and_sig.FunctionMap.t

(* Whether findings anchor on the taint source or the sink, from the
   [taint_focus_on] / [taint_match_on] rule options. *)
val match_on_of_xconf : Match_env.xconfig -> [ `Sink | `Source ]

type reanalysis = {
  lang : Lang.t;
  cfg_of : Function_id.t -> IL.fun_cfg option;
  index_of : IL.fun_cfg -> Path_feasibility.index;
  check_path :
    IL.fun_cfg ->
    entry:Path_feasibility.state ->
    Path_feasibility.trace_step list ->
    Path_feasibility.verdict * Path_feasibility.state option list;
  reanalyse_once :
    IL.fun_cfg ->
    Shape_and_sig.signature_database option ->
    (Shape_and_sig.signature_database option -> Shape_and_sig.Effects.t) ->
    Shape_and_sig.Effects.t;
  signatures_with_all_traces :
    Function_id.t list -> Shape_and_sig.signature_database option;
  tables_with_all_traces : Taint_shared_tables.t;
}

type checked_function = {
  reanalysis : reanalysis;
  cfg : IL.fun_cfg;
  reanalyse :
    (Shape_and_sig.signature_database option -> Shape_and_sig.Effects.t) option;
}

val with_all_traces : Taint_rule_inst.t -> Taint_rule_inst.t

val mk_reanalysis :
  lang:Lang.t ->
  cfg_of:(Function_id.t -> IL.fun_cfg option) ->
  shared_tables:Taint_shared_tables.t ->
  signature_with_all_traces:
    (Taint_shared_tables.t ->
    Function_id.t ->
    Shape_and_sig.signature_database ->
    Shape_and_sig.signature_database) ->
  Shape_and_sig.signature_database option ->
  reanalysis

val pms_of_effect :
  lang:Lang.t ->
  match_on:[ `Sink | `Source ] ->
  checked_function:checked_function option ->
  Shape_and_sig.Effect.t ->
  Core_match.t list

val pms_of_effects :
  lang:Lang.t ->
  match_on:[ `Sink | `Source ] ->
  checked_function:checked_function option ->
  Shape_and_sig.Effects.t ->
  Core_match.t list

val force_traces : Core_match.t list -> Core_match.t list

val get_arity :
  AST_generic.parameter list ->
  fun_info ->
  Lang.t ->
  int
(** Effective arity, filtering language-specific implicit parameters. *)

val extract_signatures :
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  lang:Lang.t ->
  db:Shape_and_sig.signature_database ->
  taint_inst:Taint_rule_inst.t ->
  shared_tables:Taint_shared_tables.t ->
  fun_info ->
  Shape_and_sig.signature_database * Shape_and_sig.extended_sig list
(** Extract a function's taint signature(s) into the db, returning the freshly
    extracted signatures.  The SCC signature fixpoint replaces a function's
    entry with these each iteration (accumulating them breaks [find_by_arity]).
    No finding detection. *)

val extract_and_check :
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  ?glob_env:Taint_lval_env.t ->
  lang:Lang.t ->
  db:Shape_and_sig.signature_database ->
  match_on:[ `Sink | `Source ] ->
  taint_inst:Taint_rule_inst.t ->
  shared_tables:Taint_shared_tables.t ->
  detect_findings:bool ->
  reanalysis:reanalysis option ->
  fun_info ->
  Shape_and_sig.signature_database * Core_match.t list
(** Shared signature-extraction + finding-detection logic. *)

val build_class_init_cfgs :
  initialisers_are_functions:bool ->
  Lang.t ->
  AST_generic.program ->
  (IL.name option * IL.fun_cfg) list

val check_class_inits_prebuilt :
  Taint_rule_inst.t ->
  Taint_shared_tables.t ->
  (IL.name option * IL.fun_cfg) list ->
  ?signature_db:Shape_and_sig.signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  unit ->
  Shape_and_sig.Effects.t

val build_top_level_cfg :
  Lang.t ->
  AST_generic.program ->
  IL.name * IL.fun_cfg

val check_top_level_prebuilt :
  Taint_rule_inst.t ->
  Taint_shared_tables.t ->
  IL.name * IL.fun_cfg ->
  ?signature_db:Shape_and_sig.signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  unit ->
  Shape_and_sig.Effects.t

val check_fundef :
  Taint_rule_inst.t ->
  Taint_shared_tables.t ->
  IL.name (** entity being analyzed *) ->
  ?glob_env:Taint_lval_env.t ->
  ?class_name:string ->
  ?signature_db:Shape_and_sig.signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  AST_generic.function_definition ->
  IL.fun_cfg * Shape_and_sig.Effects.t * Dataflow_tainting.mapping
(** Check a function definition using a [Dataflow_tainting.config] (which can
  * be obtained with [taint_config_of_rule]). Findings are passed on-the-fly
  * to the [handle_findings] callback in the dataflow config.
  *
  * This is a low-level function exposed for debugging purposes (-dfg_tainting).
  *)

val check_rule :
  Formula_cache.t ->
  Rule.taint_rule ->
  (Core_match.t list -> Core_match.t list) ->
  shared_tables:Taint_shared_tables.t ->
  is_value_type:(AST_generic.type_ -> bool) Lazy.t ->
  ?signature_db:Shape_and_sig.signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  ?local_ast_call_graph:Call_graph.G.t option ->
  Match_env.xconfig ->
  Xtarget.t ->
  Core_profiling.rule_profiling Core_result.match_result option * Shape_and_sig.signature_database option
(** Check a single taint rule on a target. Returns both the match result and the
  * computed signature database (when taint_intrafile is enabled).
  *)

val check_rules :
  match_hook:(Core_match.t list -> Core_match.t list) ->
  per_rule_boilerplate_fn:
    (Rule.rule ->
    (unit -> Core_profiling.rule_profiling Core_result.match_result option) ->
    Core_profiling.rule_profiling Core_result.match_result option) ->
  Rule.taint_rule list ->
  Match_env.xconfig ->
  Xtarget.t ->
  (* timeout function *)
  Core_profiling.rule_profiling Core_result.match_result list
(** Runs the engine on a group of taint rules, which should be for the
  * same language. Running on multiple rules at once enables inter-rule
  * optimizations.
  *)
