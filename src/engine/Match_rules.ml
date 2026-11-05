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

(* The time limit for work on [file] under the rule options [options]: a
   rule's own, or [None] for work that belongs to no rule. *)
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
            | true, Some { timeout = Some tm; _ } ->
              Log.info (fun m ->
                  m "setting timeout for %s to %.2fs using a rule override"
                    !!file tm);
              tm
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
            let size_adjusted_timeout = timeout *. timeout_multiplier in
            Log.info (fun m ->
                m "setting timeout for %s to %.2fs using a factor of %.2f derived from the file size"
                  !!file size_adjusted_timeout timeout_multiplier);
            Some (size_adjusted_timeout, caps)

let timeout_function (rule : Rule.t) (file : Fpath.t)
    (timeout : timeout_config option) f =
  match
    Time_limit.set_timeout_opt ~name:"Match_rules.timeout_function"
      (time_limit rule.Rule.options file timeout)
      f
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

(* This function separates out rules into groups of taint rules by languages,
   all of the nontaint rules, and the rules which we skip due to prefiltering.
*)
type search_or_extract_rule = [ R.search_mode | R.extract_mode ] R.rule_info

(* Whether matching [rule] converts byte offsets of the target into lines and
   columns: a regexp or aliengrep pattern, or a metavariable-regex or
   metavariable-comparison condition. A nested formula matches a temporary
   file, which has a converter of its own. *)
let reads_target_positions (rule : R.t) : bool =
  let rec of_formula (formula : R.formula) : bool =
    (match formula.f with
    | P { pat = Regexp _ | Aliengrep _; _ } -> true
    | P { pat = Sem _ | Spacegrep _; _ } -> false
    | Anywhere (_, sub)
    | Inside (_, sub)
    | Not (_, sub) ->
        of_formula sub
    | Or (_, subs)
    | And (_, subs) ->
        List.exists of_formula subs)
    || List.exists
         (fun ((_, condition) : _ * R.metavar_cond) ->
           match condition with
           | CondRegexp _
           | CondEval _ ->
               true
           | CondNestedFormula _
           | CondName _
           | CondType _
           | CondAnalysis _ ->
               false)
         formula.conditions
  in
  List.exists of_formula (R.formulas_of_mode rule.mode)

(* Whether matching [rule] parses the target as a spacegrep document. *)
let has_spacegrep_pattern (rule : R.t) : bool =
  let rec of_formula (formula : R.formula) : bool =
    match formula.f with
    | P { pat = Spacegrep _; _ } -> true
    | P { pat = Sem _ | Regexp _ | Aliengrep _; _ } -> false
    | Anywhere (_, sub)
    | Inside (_, sub)
    | Not (_, sub) ->
        of_formula sub
    | Or (_, subs)
    | And (_, subs) ->
        List.exists of_formula subs
  in
  List.exists of_formula (R.formulas_of_mode rule.mode)

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
let per_rule_boilerplate_fn (timeout : timeout_config option) =
  let cnt_timeout = ref 0 in
  let rule_timeouts = ref [] in
  fun (file : Fpath.t) (rule : Rule.t) f ->
    let rule_id = fst rule.R.id in
    TLS.set Rule.last_matched_rule (Some rule_id);
    let res_opt =
      Profiling.profile_code
        (spf "real_rule:%s" (Rule_ID.to_string rule_id))
        (fun () ->
          (* here we handle the rule! *)
          timeout_function rule file timeout f)
    in
    match res_opt with
    | Some res -> res
    | None ->
        incr cnt_timeout;
        Stack_.push rule_id rule_timeouts;
        (match timeout with
        | Some { threshold; _ } when threshold > 0 && !cnt_timeout >= threshold
          ->
            raise (File_timeout !rule_timeouts)
        | _else_ -> ());
        let loc = Tok.first_loc_of_file file in
        let error = E.mk_error ~rule_id ~loc OutJ.Timeout in
        RP.mk_match_result []
          (Core_error.ErrorSet.singleton error)
          (Core_profiling.empty_rule_profiling rule)

let per_rule_boilerplate_fn_opt (timeout : timeout_config option) =
  let cnt_timeout = ref 0 in
  let rule_timeouts = ref [] in
  fun (file : Fpath.t) (rule : Rule.t) f ->
    let rule_id = fst rule.R.id in
    TLS.set Rule.last_matched_rule (Some rule_id);
    let res_opt =
      Profiling.profile_code
        (spf "real_rule:%s" (Rule_ID.to_string rule_id))
        (fun () ->
          (* here we handle the rule! *)
          timeout_function rule file timeout f)
    in
    match res_opt with
    | Some res -> res
    | None ->
        incr cnt_timeout;
        Stack_.push rule_id rule_timeouts;
        (match timeout with
        | Some { threshold; _ } when threshold > 0 && !cnt_timeout >= threshold
          ->
            raise (File_timeout !rule_timeouts)
        | _else_ -> ());
        let loc = Tok.first_loc_of_file file in
        let error = E.mk_error ~rule_id ~loc OutJ.Timeout in
        Some (RP.mk_match_result []
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

  let per_rule_boilerplate_fn = per_rule_boilerplate_fn timeout file in
  let per_rule_boilerplate_fn_opt = per_rule_boilerplate_fn_opt timeout file in

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
     the rules that read them run, under the file's time limit and outside the
     rules' own: a time limit that interrupts a lazy value leaves it raising
     for every later rule. A build that times out gives a Timeout to each rule
     that needs it, which does not run. *)
  let build (type a) (f : unit -> a) : a option =
    (* An error while building belongs to no rule. *)
    TLS.set Rule.last_matched_rule None;
    Time_limit.set_timeout_opt ~name:"Match_rules.target_build"
      (time_limit None file timeout)
      f
  in
  let timed_out_results (rules : R.rule list) =
    (match timeout with
    | Some { threshold; _ } when threshold > 0 && List.length rules >= threshold
      ->
        raise (File_timeout (List_.map (fun (r : R.rule) -> fst r.id) rules))
    | _else_ -> ());
    let loc = Tok.first_loc_of_file file in
    List_.map
      (fun (r : R.rule) ->
        RP.mk_match_result []
          (Core_error.ErrorSet.singleton
             (E.mk_error ~rule_id:(fst r.id) ~loc OutJ.Timeout))
          (Core_profiling.empty_rule_profiling r))
      rules
  in
  let xtarget, linecol_timed_out =
    if
      List.exists reads_target_positions
        (List.concat_map (List_.map (fun (r : R.taint_rule) -> (r :> R.rule)))
           taint_rules_groups
        @ List_.map (fun (r : search_or_extract_rule) -> (r :> R.rule))
            nontaint_rules)
    then
      match build (fun () -> Pos.full_converters_large file) with
      | Some converters ->
          ({ xtarget with lazy_linecol = Lazy.from_val converters }, false)
      | None -> (xtarget, true)
    else (xtarget, false)
  in
  let runs (r : R.rule) : bool =
    not (linecol_timed_out && reads_target_positions r)
  in
  let res_taint_rules =
    taint_rules_groups
    |> List.concat_map (fun (taint_rules : R.taint_rule list) ->
           let running, timed_out =
             List.partition (fun (r : R.taint_rule) -> runs (r :> R.rule))
               taint_rules
           in
           timed_out_results
             (List_.map (fun (r : R.taint_rule) -> (r :> R.rule)) timed_out)
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
                  match_hook xconf xtarget))
  in
  let comment_style_of (r : search_or_extract_rule) =
    (Match_env.adjust_xconfig_with_rule_options xconf r.R.options).config
      .generic_comment_style
  in
  (* The rules run in groups of one comment style, and each group parses the
     target once, so that one spacegrep document is live at a time. The
     results keep the order of the rules. *)
  let res_nontaint_rules =
    let indexed_rules = List.mapi (fun (index : int) r -> (index, r)) nontaint_rules in
    [ None; Some `C; Some `Cpp; Some `Shell ]
    |> List.concat_map
         (fun (style : Rule_options_t.generic_comment_style option) ->
           let group =
             List.filter
               (fun ((_, r) : int * search_or_extract_rule) ->
                 Xpattern_match_spacegrep.equal_comment_style style
                   (comment_style_of r))
               indexed_rules
           in
           let spacegrep_doc, doc_timed_out =
             if
               List.exists
                 (fun ((_, r) : int * search_or_extract_rule) ->
                   has_spacegrep_pattern (r :> R.rule))
                 group
             then
               match
                 build (fun () ->
                     Xpattern_match_spacegrep.parse_target
                       ~nested_formula:false style file)
               with
               | Some parsed -> (Some parsed, false)
               | None -> (None, true)
             else (None, false)
           in
           let running, timed_out =
             List.partition
               (fun ((_, r) : int * search_or_extract_rule) ->
                 runs (r :> R.rule)
                 && not (doc_timed_out && has_spacegrep_pattern (r :> R.rule)))
               group
           in
           List.combine (List_.map fst timed_out)
             (timed_out_results
                (List_.map
                   (fun ((_, r) : int * search_or_extract_rule) -> (r :> R.rule))
                   timed_out))
           @ List_.map
               (fun ((index, r) : int * search_or_extract_rule) ->
                 (index, check_search_rule spacegrep_doc r))
               running)
    |> List.sort (fun ((index1, _) : int * _) ((index2, _) : int * _) ->
           Int.compare index1 index2)
    |> List_.map snd
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
