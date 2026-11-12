(* Yoann Padioleau
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module R = Rule
module PM = Core_match
module RP = Core_result
module E = Core_error
module OutJ = Semgrep_output_v1_t
module Log = Log_engine.Log
module TLS = Thread_local_storage

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Match_search_mode and Match_tainting_mode *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* This can be captured in Run_semgrep.ml *)
exception File_timeout of Rule_ID.t list

(* TODO make this one of the Semgrep_error_code exceptions *)
exception Multistep_rules_not_available

(* TODO: add here the new factor parameters. *)
type timeout_config = {
  timeout : float;
  threshold : int;
  allow_rule_timeout_control : bool;
  dynamic_timeout : bool;
  dynamic_timeout_max_multiplier : int;
  dynamic_timeout_unit_kb : int;
  caps : < Cap.time_limit >;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The time limit for work on [file] under the rule options [options]. *)
let time_limit (options : Rule_options.t option) (file : Fpath.t)
    (timeout : timeout_config option) : (float * < Cap.time_limit >) option =
    match timeout with
    | None -> None
    | Some { timeout; caps; threshold = _;
             allow_rule_timeout_control; dynamic_timeout;
             dynamic_timeout_max_multiplier; dynamic_timeout_unit_kb } ->
        if timeout <= 0. then None
        else

          let dynamic_timeout =
            match allow_rule_timeout_control, options with
            | true, Some { dynamic_timeout = Some dt; _ } -> dt
            | _ -> dynamic_timeout
          in

          let timeout =
            match allow_rule_timeout_control, options with
            | true, Some { timeout = Some tm; _ } -> tm
            | _ -> timeout
          in

          if not dynamic_timeout
          then
            Some (timeout, caps)
          else

            let dynamic_timeout_unit_kb =
              match allow_rule_timeout_control, options with
              | true, Some { dynamic_timeout_unit_kb = Some dtk; _ } -> dtk
              | _ -> dynamic_timeout_unit_kb
            in

            let dynamic_timeout_max_multiplier =
              match allow_rule_timeout_control, options with
              | true, Some { dynamic_timeout_max_multiplier = Some dtm; _ } -> dtm
              | _ -> dynamic_timeout_max_multiplier
            in

            (* We use Spacegrep's [stat] because it's cached, and we don't want to repeat the call
             * for each rule. Otherwise we could use [UFile.filesize]. *)
            let filesize_kb = float_of_int (Spacegrep.Find_files.stat !!file).st_size /. 1024.
            in
            (* Multiply timeout by the number of [dynamic_timeout_unit_kb] blocks in the file. *)
            let timeout_multiplier =
              Float.div filesize_kb (float_of_int dynamic_timeout_unit_kb)
              |> max 1.
              |> min (float_of_int dynamic_timeout_max_multiplier)
            in
            Some (timeout *. timeout_multiplier, caps)

let timeout_function (rule : Rule.t) (file : Fpath.t)
    (timeout : timeout_config option) f =
  let limit = time_limit rule.Rule.options file timeout in
  (match (limit, timeout) with
  | Some (limit_s, _), Some { timeout = default_s; _ }
    when not (Float.equal limit_s default_s) ->
      Log.info (fun m ->
          m "setting timeout for rule %s on %s to %.2fs"
            (Rule_ID.to_string (fst rule.id))
            !!file limit_s)
  | _ -> ());
  match
    Time_limit.set_timeout_opt ~name:"Match_rules.timeout_function" limit f
  with
  | Some res -> Some res
  | None ->
      (* Note that we could timeout while testing the equality of two ASTs and
       * `busy_with_equal` will then erroneously have a `<> Not_busy` value. *)
      Log.err (fun m ->
          m "timeout for rule %s on file %s"
            (Rule_ID.to_string (fst rule.id))
            !!file);
      None

let is_relevant_rule_for_xtarget r xconf xtarget =
  let { path = { internal_path_to_content; _ }; lazy_content; _ } : Xtarget.t =
    xtarget
  in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf r.R.options in
  let interfile_taint =
    xconf.config.taint_interfile
    && (match r.R.mode with `Taint _ -> true | _ -> false)
  in
  let is_relevant =
    match xconf.filter_irrelevant_rules with
    | NoPrefiltering -> true
    | PrefilterWithCache cache -> (
        (* Under taint_interfile a file may hold only a source OR only a
           sink (companion files supply the rest), so the stock same-file
           AND prefilter would wrongly drop it. The OR prefilter keeps
           source-only / sink-only files but still drops files with
           neither — a file that can't seed or match any spec. *)
        let prefilter =
          if interfile_taint then
            Analyze_rule.regexp_prefilter_of_interfile_taint_rule
              ~cache:(Some cache) r
          else Analyze_rule.regexp_prefilter_of_rule ~cache:(Some cache) r
        in
        match prefilter with
        | None -> true
        | Some (prefilter_formula, func) ->
            (* NOTE: If [lazy_content] is shared in > 1 thread, then this is not
             * thread-safe. However, each [Xtarget.t] is only accessed in 1 worker
             * task, so there should be no race. *)
            let content = Lazy.force lazy_content in
            Log.info (fun m ->
                let s = Semgrep_prefilter_j.string_of_formula prefilter_formula in
                m "looking for %s in %s" s !!internal_path_to_content);
            func content)
  in
  if not is_relevant then
    Log.info (fun m ->
        m "skipping rule %s for %s"
          (Rule_ID.to_string (fst r.R.id))
          !!internal_path_to_content);
  is_relevant

type search_or_extract_rule = [ R.search_mode | R.extract_mode ] R.rule_info

(* Whether matching [rule] converts byte offsets of the target into lines and
   columns: a regexp or aliengrep pattern, or a metavariable-regex condition. A
   nested formula matches a temporary file, which has a converter of its own. *)
let reads_target_positions (rule : R.t) : bool =
  let rec has_regexp_condition (formula : R.formula) : bool =
    List.exists
      (fun ((_, condition) : _ * R.metavar_cond) ->
        match condition with
        | CondRegexp _ -> true
        | CondEval _
        | CondNestedFormula _
        | CondName _
        | CondType _
        | CondAnalysis _ ->
            false)
      formula.conditions
    ||
    match formula.f with
    | P _ -> false
    | Anywhere (_, sub)
    | Inside (_, sub)
    | Not (_, sub) ->
        has_regexp_condition sub
    | Or (_, subs)
    | And (_, subs) ->
        List.exists has_regexp_condition subs
  in
  List.exists
    (fun (xpattern : Xpattern.t) ->
      match xpattern.pat with
      | Regexp _
      | Aliengrep _ ->
          true
      | Sem _
      | Spacegrep _ ->
          false)
    (Visit_rule.xpatterns_of_rule rule)
  || List.exists has_regexp_condition (R.formulas_of_mode rule.mode)

(* The comment style that the spacegrep patterns of a run of search rules
   parse the target with. *)
type run_style =
  | No_spacegrep_pattern
  | Comment_style of Rule_options_t.generic_comment_style option

(* Whether matching [rule] parses the target as a spacegrep document. *)
let has_spacegrep_pattern (rule : R.t) : bool =
  List.exists
    (fun (xpattern : Xpattern.t) ->
      match xpattern.pat with
      | Spacegrep _ -> true
      | Sem _
      | Regexp _
      | Aliengrep _ ->
          false)
    (Visit_rule.xpatterns_of_rule rule)

(* This function separates out rules into groups of taint rules by languages,
   all of the nontaint rules, and the rules which we skip due to prefiltering.
*)
let group_rules xconf rules xtarget =
  let relevant_taint_rules, relevant_nontaint_rules, skipped_rules =
    rules
    |> Either_.partition_either3 (fun r ->
           let relevant_rule = is_relevant_rule_for_xtarget r xconf xtarget in
           match r.R.mode with
           | _ when not relevant_rule -> Right3 r
           | `Taint _ as mode -> Left3 { r with mode }
           | (`Extract _ | `Search _) as mode -> Middle3 { r with mode }
           | `SCA _ ->
               (* alt: failwith "SCA rule not available in core." *)
               Right3 r
           | `Steps _ ->
               Log.warn (fun m ->
                   m "Step rule not handled: %s" (Rule.show_rule r));
               raise Multistep_rules_not_available)
  in
  (* Taint rules are only relevant to each other if they are meant to be
     analyzing the same language.
     So we group the taint rules by common language, before passing them
     to [Match_tainting_mode.check_rules].
  *)
  let relevant_taint_rules_groups =
    relevant_taint_rules
    |> List_.map (fun r -> (r.R.target_analyzer, r))
    |> Assoc.group_assoc_bykey_eff |> List_.map snd
  in
  (relevant_taint_rules_groups, relevant_nontaint_rules, skipped_rules)

(* Given a thunk [f] that computes the results of running the engine on a
 * single rule, this function simply instruments the computation on a single
 * rule with some boilerplate logic, like setting the last matched rule,
 * timing out if it takes too long, and producing a faulty match result in
 * that case.
 *
 * In particular, we need this to call [Match_tainting_mode.check_rules],
 * which will iterate over each rule in a different place, and so needs
 * access to this logic.
 *)
(* [timed_out] holds the rules of the target that timed out, the most recent
   first; the target is abandoned when they reach the timeout threshold.
   [of_timeout] gives the result of a rule that timed out. *)
let per_rule_boilerplate (timeout : timeout_config option)
    (timed_out : Rule_ID.t list ref)
    ~(of_timeout : Core_profiling.rule_profiling RP.match_result -> 'a)
    (file : Fpath.t) (rule : Rule.t) (attempt : (unit -> 'a) option) : 'a =
  let rule_id = fst rule.R.id in
  TLS.set Rule.last_matched_rule (Some rule_id);
  let res_opt =
    match attempt with
    (* the rule timed out before it ran, while a value it reads was built *)
    | None -> None
    | Some f ->
        Profiling.profile_code
          (spf "real_rule:%s" (Rule_ID.to_string rule_id))
          (fun () ->
            (* here we handle the rule! *)
            timeout_function rule file timeout f)
  in
  match res_opt with
  | Some res -> res
  | None ->
      timed_out := rule_id :: !timed_out;
      (match timeout with
      | Some { threshold; _ }
        when threshold > 0 && List.length !timed_out >= threshold ->
          raise (File_timeout !timed_out)
      | _else_ -> ());
      let loc = Tok.first_loc_of_file file in
      let error = E.mk_error ~rule_id ~loc OutJ.Timeout in
      of_timeout
        (RP.mk_match_result []
           (Core_error.ErrorSet.singleton error)
           (Core_profiling.empty_rule_profiling rule))

let scc_match_hook (match_hook : Core_match.t -> unit)
    (dependency_match_table : Match_SCA_mode.dependency_match_table option) :
    PM.t list -> PM.t list =
  let get_dep_matches =
    match dependency_match_table with
    | Some table -> Hashtbl.find_opt table
    | None -> fun _ -> None
  in
  fun pms ->
    pms
    |> List.concat_map (fun (pm : Core_match.t) ->
           let dependency_matches = get_dep_matches pm.rule_id.id in
           let pms' =
             Match_SCA_mode.annotate_pattern_match dependency_matches pm
           in
           pms' |> List.iter match_hook;
           pms')

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check
    ?(dependency_match_table : Match_SCA_mode.dependency_match_table option)
    ~match_hook ~(timeout : timeout_config option) (xconf : Match_env.xconfig)
    (rules : Rule.rules) (xtarget : Xtarget.t) : Core_result.matches_single_file
    =
  let match_hook = scc_match_hook match_hook dependency_match_table in

  let {
    path = { internal_path_to_content = file; _ };
    lazy_ast_and_errors;
    xlang;
    _;
  } : Xtarget.t =
    xtarget
  in
  Log.info (fun m -> m "checking %s with %d rules" !!file (List.length rules));
  (match (!Profiling.profile, xlang) with
  (* coupling: see Run_semgrep.xtarget_of_file() *)
  | Profiling.ProfAll, Xlang.L (_lang, []) ->
      Log.debug (fun m ->
          m "forcing parsing of AST outside of rules, for better profile");
      (* XXX: Can result in [CamlinternalLazy.Undefined] error.
       * But it should not be the case, as we parse in the worker thread, which
       * is running alone in the domain. *)
      Lazy.force lazy_ast_and_errors |> ignore
  | _else_ -> ());

  (* One count of timed-out rules for the target, taint and search rules
     alike. *)
  let timed_out : Rule_ID.t list ref = ref [] in
  let per_rule_boilerplate_fn =
    per_rule_boilerplate timeout timed_out ~of_timeout:Fun.id file
  in
  let per_rule_boilerplate_fn_opt =
    per_rule_boilerplate timeout timed_out ~of_timeout:Option.some file
  in

  (* We separate out the taint rules specifically, because we may want to
     do some rule-wide optimizations, which require analyzing more than
     just one rule at once.

     The taint rules are "grouped", see [group_rule] for more.

     TODO: use skipped_rules to call the commented skipped_target_of_rule?
  *)
  let taint_rules_groups, nontaint_rules, _skipped_rules =
    group_rules xconf rules xtarget
  in
  (* The converter and the spacegrep documents of the target are built before
     the rules that read them run, outside the rules' own time limits: a time
     limit that interrupts a lazy value leaves it raising for every later
     rule. A build that times out gives a Timeout to each rule
     that needs it, which does not run. *)
  (* A build runs under the largest time limit of the rules that need it, and
     with no limit when one of them has none. *)
  let build (type a) (rules : R.rule list) (f : unit -> a) : a option =
    let limits =
      List_.map (fun (r : R.rule) -> time_limit r.R.options file timeout) rules
    in
    let limit =
      if List.exists Option.is_none limits then None
      else
        List.fold_left
          (fun (largest : (float * < Cap.time_limit >) option)
               (limit : (float * < Cap.time_limit >) option) ->
            match (largest, limit) with
            | Some (largest_s, _), Some (limit_s, _)
              when Float.compare limit_s largest_s > 0 ->
                limit
            | None, _ -> limit
            | Some _, _ -> largest)
          None limits
    in
    (* An error while building belongs to no rule. *)
    TLS.set Rule.last_matched_rule None;
    Time_limit.set_timeout_opt ~name:"Match_rules.target_build" limit f
  in
  let xtarget, linecol_timed_out =
    match
      List.filter reads_target_positions
        (List.concat_map (List_.map (fun (r : R.taint_rule) -> (r :> R.rule)))
           taint_rules_groups
        @ List_.map (fun (r : search_or_extract_rule) -> (r :> R.rule))
            nontaint_rules)
    with
    | [] -> (xtarget, false)
    | readers -> (
      match build readers (fun () -> Pos.full_converters_large file) with
      | Some converters ->
          ({ xtarget with lazy_linecol = Lazy.from_val converters }, false)
      | None -> (xtarget, true))
  in
  let runs (r : R.rule) : bool =
    not (linecol_timed_out && reads_target_positions r)
  in
  (* A rule that cannot run goes through its boilerplate as a timeout, which
     counts it toward the file's timeout threshold. *)
  let res_taint_rules =
    taint_rules_groups
    |> List.concat_map (fun (taint_rules : R.taint_rule list) ->
           let running, blocked =
             List.partition (fun (r : R.taint_rule) -> runs (r :> R.rule))
               taint_rules
           in
           List_.filter_map
             (fun (r : R.taint_rule) ->
               per_rule_boilerplate_fn_opt (r :> R.rule) None)
             blocked
           @ Match_tainting_mode.check_rules ~match_hook
               ~per_rule_boilerplate_fn:per_rule_boilerplate_fn_opt running
               xconf xtarget)
  in
  let check_search_rule
      (spacegrep_doc : Xpattern_match_spacegrep.parsed option)
      (r : search_or_extract_rule) =
    let xconf = Match_env.adjust_xconfig_with_rule_options xconf r.R.options in
    per_rule_boilerplate_fn
      (r :> R.rule)
      (Some
         (fun () ->
           Logs_.with_debug_trace ~__FUNCTION__
             ~pp_input:(fun _ ->
               "target: " ^ !!file ^ "\nruleid: "
               ^ (r.id |> fst |> Rule_ID.to_string))
             (fun () ->
               (* dispatching *)
               match r.R.mode with
               | `Search _formula as mode ->
                   Match_search_mode.check_rule ~spacegrep_doc { r with mode }
                     match_hook xconf xtarget
               | `Extract extract_spec ->
                   Match_search_mode.check_rule ~spacegrep_doc
                     { r with mode = `Search extract_spec.R.formula }
                     match_hook xconf xtarget)))
  in
  let comment_style_of (r : search_or_extract_rule) =
    (Match_env.adjust_xconfig_with_rule_options xconf r.R.options).config
      .generic_comment_style
  in
  (* The rules run in their order, in runs of consecutive rules whose spacegrep
     patterns share one comment style, carried with the run; a rule without a
     spacegrep pattern joins the run it is in. Each run that has a spacegrep
     pattern parses the target once, so that one spacegrep document is live at
     a time. *)
  let style_runs : (run_style * search_or_extract_rule list) list =
    List.fold_right
      (fun (r : search_or_extract_rule)
           (runs_after : (run_style * search_or_extract_rule list) list) ->
        let style =
          if has_spacegrep_pattern (r :> R.rule) then
            Comment_style (comment_style_of r)
          else No_spacegrep_pattern
        in
        match (style, runs_after) with
        | No_spacegrep_pattern, (run_style, run) :: rest ->
            (run_style, r :: run) :: rest
        | Comment_style _, (No_spacegrep_pattern, run) :: rest ->
            (style, r :: run) :: rest
        | Comment_style r_style, (Comment_style run_style, run) :: rest
          when Xpattern_match_spacegrep.equal_comment_style r_style run_style ->
            (style, r :: run) :: rest
        | _ -> (style, [ r ]) :: runs_after)
      nontaint_rules []
  in
  let res_nontaint_rules =
    style_runs
    |> List.concat_map
         (fun
           ((run_style, run) : run_style * search_or_extract_rule list)
         ->
           let parsers =
             List.filter has_spacegrep_pattern
               (List_.map (fun (r : search_or_extract_rule) -> (r :> R.rule)) run)
           in
           let spacegrep_doc, doc_timed_out =
             match run_style with
             | Comment_style style -> (
                 match
                   build parsers (fun () ->
                       Xpattern_match_spacegrep.parse_target
                         ~nested_formula:false style file)
                 with
                 | Some parsed -> (Some parsed, false)
                 | None -> (None, true))
             | No_spacegrep_pattern -> (None, false)
           in
           List_.map
             (fun (r : search_or_extract_rule) ->
               if
                 runs (r :> R.rule)
                 && not (doc_timed_out && has_spacegrep_pattern (r :> R.rule))
               then check_search_rule spacegrep_doc r
               else per_rule_boilerplate_fn (r :> R.rule) None)
             run)
  in
  let res_total = res_taint_rules @ res_nontaint_rules in
  (* TODO: detect if a target was fully skipped because no rule
   * were irrelevant.
   * We used to report that for each rule independently via
   *
   * let skipped_target_of_rule (file_and_more : Xtarget.t) (rule : R.rule) :
   *     OutJ.skipped_target =
   *   let rule_id, _ = rule.id in
   *   let details =
   *     Some
   *       (spf
   *          "No need to perform matching because target does not contain \
   *           some elements necessary for the rule to match '%s'"
   *          (Rule_ID.to_string rule_id))
   *   in
   *   {
   *     path = file_and_more.path.internal_path_to_content;
   *     reason = Irrelevant_rule;
   *     details;
   *     rule_id = Some rule_id;
   *   }
   *
   * when skipped_target was part of profiling info, but now
   * skipped_target is only in the Core_result.t (and not in the
   * intermediate match_result).
   *)
  RP.collate_rule_results xtarget.path.internal_path_to_content res_total
