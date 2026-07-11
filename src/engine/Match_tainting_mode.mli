type fun_info = {
  name : IL.name;
  class_name_str : string option;
  method_properties : AST_generic.expr list;
  cfg : IL.fun_cfg;
  fdef : AST_generic.function_definition;
  is_static : bool;
  is_lambda_assignment : bool;
  file_ast : AST_generic.program option;
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

val pms_of_effect :
  lang:Lang.t ->
  match_on:[ `Sink | `Source ] ->
  Shape_and_sig.Effect.t ->
  Core_match.t list

val pms_of_effects :
  lang:Lang.t ->
  match_on:[ `Sink | `Source ] ->
  Shape_and_sig.Effects.t ->
  Core_match.t list

val get_arity :
  AST_generic.parameter list ->
  fun_info ->
  Lang.t ->
  int
(** Effective arity, filtering language-specific implicit parameters. *)

val extract_and_check :
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  ?call_graph:Call_graph.G.t ->
  ?glob_env:Taint_lval_env.t ->
  lang:Lang.t ->
  db:Shape_and_sig.signature_database ->
  match_on:[ `Sink | `Source ] ->
  taint_inst:Taint_rule_inst.t ->
  ast:AST_generic.program ->
  detect_findings:bool ->
  fun_info ->
  Shape_and_sig.signature_database * Core_match.t list
(** Shared signature-extraction + finding-detection logic. *)

val build_class_init_cfgs :
  Lang.t ->
  AST_generic.program ->
  (IL.name option * IL.fun_cfg) list

val check_class_inits_prebuilt :
  Taint_rule_inst.t ->
  (IL.name option * IL.fun_cfg) list ->
  ?signature_db:Shape_and_sig.signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  ?call_graph:Call_graph.G.t ->
  unit ->
  Shape_and_sig.Effects.t

val build_top_level_cfg :
  Lang.t ->
  AST_generic.program ->
  IL.name * IL.fun_cfg

val check_top_level_prebuilt :
  Taint_rule_inst.t ->
  IL.name * IL.fun_cfg ->
  ?signature_db:Shape_and_sig.signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  ?call_graph:Call_graph.G.t ->
  unit ->
  Shape_and_sig.Effects.t

val check_fundef :
  Taint_rule_inst.t ->
  IL.name (** entity being analyzed *) ->
  ?glob_env:Taint_lval_env.t ->
  ?class_name:string ->
  ?signature_db:Shape_and_sig.signature_database ->
  ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
  ?call_graph:Call_graph.G.t ->
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
