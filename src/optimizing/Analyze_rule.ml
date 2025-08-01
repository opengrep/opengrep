(* Yoann Padioleau
 *
 * Copyright (C) 2021-2022 Semgrep Inc.
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
module R = Rule
module XP = Xpattern
module MV = Metavariable
module SP = Semgrep_prefilter_t
module MvarSet = Common2.StringSet
module Log = Log_optimizing.Log

[@@@warning "-32"] (* for the unused pp_ coming from deriving show *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Analyzing a semgrep rule for optimization purpose.
 *
 * Analyze_pattern.ml tries to extract a regexp from a pattern
 * in order to skip certain target files. However, it processes only
 * one pattern at a time and is not aware of the context in which this
 * pattern is used. For example in:
 *
 *  id: eval-not-in-foo
 *  patterns:
 *    - pattern: eval(...)
 *    - pattern-not:
 *        require("foo.js")
 *        ...
 *
 * the current Mini_rules_filter used in Match_patterns will just see a flat
 * list of patterns, and will look separately for 'eval' and 'foo.js'
 * and filter certain patterns; But really, even if 'foo.js' is mentioned
 * in a file, we should completly skip the file if 'eval' is not in the file
 * because after all 'foo.js' is mentioned in a pattern-not context!
 * The goal of this file is to process a rule holistically to
 * overcome the limitations of Analyze_pattern.ml
 *
 * There are many optimization opportunities when semgrep-core can see
 * the whole rule:
 *  - skip the pattern-not when computing the regexp
 *  - TODO if a pattern is very general (e.g., $FOO()), but is
 *    also mentioned in a metavariable-regexp, then we can use this
 *    regexp to filter the rule/target file.
 *  - TODO if a pattern is very general (e.g., $PROP), but reference
 *    metavariables used in all the patterns of a disjunction, then you
 *    can skip this pattern
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Conjunctive normal form (CNF).
 *
 * Why a CNF instead of a DNF (Disjunctive normal form)?
 * Because in the context of producing a regexp, regexps are good at
 * representing Or, but not so great for And.
 *
 * todo? the evaluation engine prefers to work on DNF where negation
 * must be inside a And, so maybe better to work also on DNF here?
 * and skip correctly the negations?
 *
 * update: I now use run_cnf_step2 to eval a cnf, so I could do it
 * on a DNF too because I don't anymore reduce everything to a single regexp
 * (it's fast enough to run many regexps on a file).
 *
 *)
type 'a cnf = And of 'a disj list

(* no need for negation, they are filtered *)
and 'a disj = Or of 'a list [@@deriving show]

(* can't filter a file if there's no specific identifier in the pattern *)
exception GeneralPattern
exception EmptyAnd
exception EmptyOr
exception CNF_exploded

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* NOTE "AND vs OR and filter_map":
 * We cannot use `List_.filter_map` for `R.Or`, because it has the wrong
 * semantics. We use `None` to say "we can't handle this", or in other words,
 * "we assume this pattern can match", or just "true"! So in an AND we can
 * remove those "true" terms, but in an OR we need to reduce the entire OR to
 * "true". Therefore, `List_.filter_map` works for AND-semantics, but for
 * OR-semantics we need `option_map`. *)
let option_map f xs =
  List.fold_left
    (fun acc x ->
      let* ys = acc in
      let* y = f x in
      Some (y :: ys))
    (Some []) xs

(*****************************************************************************)
(* Step0: a complex formula to a CNF *)
(*****************************************************************************)
(* Transforming a complex formula to a simple CNF formula.
 *
 * old: I used to do the CNF transformation while still having negations in the
 * formula, and later in step1 remove the Not. However, this does not work!
 * Indeed, in:
 *    (foo/\not xxx) \/ (bar/\not yyy),
 * we should analyze a file if 'foo' *or* 'bar' are in the file. However,
 * the CNF transformation would distribute the \/ with:
 *    (foo\/bar) /\ (foo\/not yyy) /\ (not xxx\/bar) /\ (not xxx\/not yyy)
 * but then in step1 if we remove the not, we get:
 *    (foo\/bar) /\ foo /\ bar
 * and suddently we strongly require 'foo' *and* 'bar' to be in the file.
 * Thus, we must filter the Not before doing the CNF conversion!
 *)

(* less: move the Not to leaves, applying DeMorgan, and then filter them? *)
let rec (remove_not : Rule.formula -> Rule.formula option) =
 fun ({ f; conditions; focus; fix = _; as_ } as formula) ->
  let reconstruct f = R.mk_formula ~conditions ~focus ?as_ f in
  match f with
  | R.And (t, xs) ->
      let ys = List_.filter_map remove_not xs in
      if List_.null ys then (
        Log.debug (fun m -> m "null And after remove_not");
        None)
      else Some (R.And (t, ys) |> reconstruct)
  | R.Or (t, xs) ->
      (* See NOTE "AND vs OR and filter_map". *)
      let* ys = option_map remove_not xs in
      if List_.null ys then (
        Log.debug (fun m -> m "null Or after remove_not");
        None)
      else Some (R.Or (t, ys) |> reconstruct)
  | R.Not (_, formula) -> (
      match formula.f with
      | R.P _ -> None
      (* double negation *)
      | R.Not (_, f) -> remove_not f
      (* todo? apply De Morgan's law? *)
      (* TODO: These logs seem to be pretty noisy. Lower or remove?
         Seems like they're just artifacts from prior stubs, so
         failwith "Not Or" was just translated to below case, etc..
      *)
      | R.Or (_, _xs) ->
          Log.debug (fun m -> m "Not Or");
          None
      | R.And _ ->
          Log.debug (fun m -> m "Not And");
          None
      | R.Inside _ ->
          Log.debug (fun m -> m "Not Inside");
          None
      | R.Anywhere _ ->
          Log.debug (fun m -> m "Not Anywhere");
          None)
  | R.Inside (t, formula) ->
      let* formula = remove_not formula in
      Some (R.Inside (t, formula) |> reconstruct)
  | R.Anywhere (t, formula) ->
      let* formula = remove_not formula in
      Some (R.Anywhere (t, formula) |> reconstruct)
  | R.P _ -> Some formula

let remove_not_final f =
  let final_opt = remove_not f in
  if Option.is_none final_opt then Log.err (fun m -> m "no formula");
  final_opt

type step0 = LPat of Xpattern.t | LCond of Rule.metavar_cond
(*old: does not work: | Not of Rule.leaf | Pos of Rule.leaf *)
[@@deriving show]

type cnf_step0 = step0 cnf [@@deriving show]

(* reference? https://www.cs.jhu.edu/~jason/tutorials/convert-to-CNF.html
 * TODO the current code triggers some Stack_overflow on
 * tests/rules/tainted-filename.yaml. I've replaced some List.map
 * by List_.map, but we still get some Stack_overflow because of the many
 * calls to @.
 *)
let (cnf : Rule.formula -> cnf_step0) =
 fun formula ->
  let rec aux { Rule.f; conditions; _ } =
    let augment_with_conditions (And x) =
      let conditions =
        List_.map (fun (_t, cond) -> Or [ LCond cond ]) conditions
      in
      And (x @ conditions)
    in
    aux' f |> augment_with_conditions
  and aux' kind =
    match kind with
    | R.P pat -> And [ Or [ LPat pat ] ]
    | R.Not (_, _f) ->
        (* should be filtered by remove_not *)
        failwith "call remove_not before cnf"
    (* old:
       * (match f with
       * | R.Leaf x -> And [Or [Not x]]
       * (* double negation *)
       * | R.Not f -> cnf f
       * (* de Morgan's laws *)
       * | R.Or _xs -> failwith "Not Or"
       * | R.And _xs -> failwith "Not And"
       * )
    *)
    | R.Inside (_, formula)
    | R.Anywhere (_, formula) ->
        aux formula
    | R.And (_, xs) ->
        let ys = List_.map aux xs in
        And
          (List.concat_map
             (function
               | And x -> x)
             ys)
    | R.Or (_, xs) ->
        let is_dangerously_large p q =
          List.compare_length_with p 10_000 > 0
          || List.compare_length_with q 10_000 > 0
          ||
          let p_len = List.length p in
          let q_len = List.length q in
          (* Divide rather than multiply to avoid integer overflow *)
          p_len > Int.div 50_000 q_len
        in
        let ys = List_.map aux xs in
        List.fold_left
          (fun (And ps) (And qs) ->
            (* Abort before this starts consuming insane amounts of memory. *)
            if is_dangerously_large ps qs then raise CNF_exploded;
            (* Distributive law *)
            And
              (ps
              |> List.concat_map (fun pi ->
                     let ands =
                       qs
                       |> List_.map (fun qi ->
                              let (Or pi_ors) = pi in
                              let (Or qi_ors) = qi in
                              (* `ps` is the accumulator so we expect it to be larger *)
                              let ors = List.rev_append qi_ors pi_ors in
                              Or ors)
                     in
                     ands)))
          (* If `ys = []`, we have `Or []` which is the same as `false`. Note that
             * the CNF is then `And [Or []]` rather than `And []` (the latter being
             * the same `true`). *)
          (And [ Or [] ]) ys
  in
  aux formula

(*****************************************************************************)
(* Step1: just collect strings, mvars, regexps *)
(*****************************************************************************)
type step1 =
  | StringsAndMvars of string list * MV.mvar list
  | Regexp of Xpattern.regexp_string
  | MvarRegexp of MV.mvar * Xpattern.regexp_string * bool
[@@deriving show]

type cnf_step1 = step1 cnf [@@deriving show]
type is_id_mvar = Metavariable.mvar -> bool

(* Here we overapproximate and just look for _ONE_ occurrence of an $MVAR in an
 * "identifier position" (c.f., 'Analyze_pattern.extract_mvars_in_id_position').
 * But, in a `pattern-either` we could have an $MVAR in an identifier position
 * in one pattern, and the same $MVAR in a non-identifier position in another one.
 * In those cases we may still end up skipping files that we should not skip. *)
let id_mvars_of_formula f =
  
  Visit_rule.visit_xpatterns (fun xp ~inside:_ acc ->
         match xp with
         | { pat = XP.Sem (pat, lang); _ } ->
             
               Analyze_pattern.extract_mvars_in_id_position ~lang pat
               |> MvarSet.union acc 
         | __else__ -> acc) f MvarSet.empty(* simple for now, don't do any conversion *)

(*
let rec (and_step1: Rule.formula -> cnf_step1) = fun f ->
  match f with
  | R.And xs -> And (xs |> List_.filter_map or_step1)
  | _ -> And ([f] |> List_.filter_map or_step1)
and or_step1 f =
  match f with
  | R.Or xs ->
      let ys = (xs |> List_.filter_map leaf_step1) in
      if null ys
      then None
      else (Some (Or ys))
  | _ -> let ys = ([f] |> List_.filter_map leaf_step1) in
      if null ys
      then None
      else (Some (Or ys))

and leaf_step1 f =
  match f with
  | R.And _ | R.Or _ -> failwith "nested And or Or"
  (* we can filter that *)
  | R.Not _ -> None
  | R.Leaf (R.P pat) -> xpat_step1 pat
  | R.Leaf (R.MetavarCond x) ->
      metavarcond_step1 x
*)

let rec (and_step1 : is_id_mvar:is_id_mvar -> cnf_step0 -> cnf_step1) =
 fun ~is_id_mvar cnf ->
  match cnf with
  | And xs -> And (xs |> List_.filter_map (or_step1 ~is_id_mvar))

and or_step1 ~is_id_mvar cnf =
  match cnf with
  | Or xs ->
      (* old: We had `List_.filter_map` here before, but that gives the wrong
       * semantics. See NOTE "AND vs OR and filter_map". *)
      let* ys = option_map (leaf_step1 ~is_id_mvar) xs in
      if List_.null ys then None else Some (Or ys)

and leaf_step1 ~is_id_mvar f =
  match f with
  (* old: we can't filter now; too late, see comment above on step0 *)
  (*  | Not _ -> None *)
  | LPat pat -> xpat_step1 pat
  | LCond x -> metavarcond_step1 ~is_id_mvar x

and xpat_step1 pat =
  match pat.XP.pat with
  | XP.Sem (pat, lang) ->
      let ids, mvars = Analyze_pattern.extract_strings_and_mvars ~lang pat in
      Some (StringsAndMvars (ids, mvars))
  (* less: could also extract ids and mvars, but maybe no need to
   * prefilter for spacegrep; it is probably fast enough already
   *)
  | XP.Regexp re -> Some (Regexp re)
  (* todo? *)
  | XP.Spacegrep _ -> None
  | XP.Aliengrep _ -> None

and metavarcond_step1 ~is_id_mvar x =
  match x with
  | R.CondEval _ -> None
  | R.CondNestedFormula _ -> None
  | R.CondRegexp (mvar, re, const_prop) ->
      if is_id_mvar mvar then Some (MvarRegexp (mvar, re.pattern, const_prop)) else None
  (* TODO? maybe we should extract the strings from the type constraint *)
  | R.CondType _ -> None
  | R.CondName _ -> None
  | R.CondAnalysis _ -> None

(*****************************************************************************)
(* Step2: no more metavariables *)
(*****************************************************************************)
(* TODO: filter patterns without idents but with mvar mentioned
 * in an And in another branch.
 * TODO: replace some Idents [], MVar where mvar mentioned in a
 * MvarRegexp into a Regexp2
 *)

let and_step1bis_filter_general (And xs) =
  let has_empty_idents, rest =
    xs
    |> Either_.partition (function Or xs ->
           if
             xs
             |> List.exists (function
                  | StringsAndMvars ([], _) -> true
                  | __else__ -> false)
           then Left (Or xs)
           else Right (Or xs))
  in
  (* TODO: regression on vertx-sqli.yaml   *)
  let filtered =
    has_empty_idents
    |> List_.filter_map (fun (Or xs) ->
           let xs' =
             xs
             |> List_.exclude (function
                  | StringsAndMvars ([], mvars) ->
                      mvars
                      |> List.exists (fun mvar ->
                             rest
                             |> List.exists (function Or xs ->
                                    xs
                                    |> List.for_all (function
                                         | StringsAndMvars (_, mvars) ->
                                             List.mem mvar mvars
                                         | Regexp _ -> false
                                         | MvarRegexp (mvar2, _, _) ->
                                             mvar2 = mvar)))
                  | __else__ -> false)
           in
           if List_.null xs' then None else Some (Or xs'))
  in
  And (filtered @ rest)
[@@profiling]

type step2 =
  | Idents of string list
  (* a And *)
  | Regexp2_search of Xpattern.compiled_regexp
[@@deriving show]

type cnf_step2 = step2 cnf [@@deriving show]

let or_step2 (Or xs) =
  let step1_to_step2 =
    List_.map (function
      | StringsAndMvars ([], _) -> raise GeneralPattern
      | StringsAndMvars (xs, _) -> Idents xs
      | Regexp re_str -> Regexp2_search (Pcre2_.pcre_compile re_str)
      | MvarRegexp (_mvar, re_str, _const_prop) ->
          (* The original regexp is meant to apply on a substring.
              We rewrite them to remove end-of-string anchors if possible. *)
          let re =
            match
              Pcre2_.remove_end_of_string_assertions
                (Pcre2_.pcre_compile re_str)
            with
            | None -> raise GeneralPattern
            | Some re -> re
          in
          Regexp2_search re)
  in
  (* Remove or cases where any of the possibilities is a general pattern *)
  (* We need to do this because later, in the final regex generation,
     empty cases will be disregarded *)
  try Some (Or (step1_to_step2 xs)) with
  | GeneralPattern -> None

let and_step2 (And xs) =
  let ys = xs |> List_.filter_map or_step2 in
  if List_.null ys then raise GeneralPattern;
  And ys

let prefilter_formula_of_cnf_step2 (And xs) : Semgrep_prefilter_t.formula =
  let xs' =
    xs
    |> List_.map (fun (Or ys) ->
           let ys' =
             ys
             |> List_.map (function
                  | Idents xs -> `Pred (`Idents xs)
                  | Regexp2_search re ->
                      let re_str = Pcre2_.show re in
                      `Pred (`Regexp re_str))
           in
           match ys' with
           | [] -> raise EmptyOr
           | [ x ] -> x
           | xs -> `Or xs)
  in
  match xs' with
  | [] -> raise EmptyAnd
  | [ x ] -> x
  | xs -> `And xs
[@@profiling]

(*****************************************************************************)
(* Final Step: just regexps? *)
(*****************************************************************************)
(*
(* support alt which can be convenient *)
type regexp = Regexp_engine.Re_engine.t
[@@deriving show]

type final_step = Re of regexp
[@@deriving show]

(* just a And *)
type cnf_final = AndFinal of final_step list
[@@deriving show]

let or_final (Or xs) =
  let ys = xs |> List_.map (function
   | Idents [] -> raise Impossible
   (* take the first one *)
   | Idents (x::_) -> Re.matching_exact_string x
   | Regexp2_search (s, _re) ->
        (* PCRE regular expression not supported by Re, grrr *)
        try Re.regexp s
        with _ -> failwith (spf "Could not parse regexp: %s" s)
   ) in
   match ys with
   | [] -> None
   | y::ys ->
    let combined =
      ys |> List.fold_left (fun acc e -> Re.alt acc e) y
    in
    Some (Re combined)

(* TODO: normalize, merge similar Idents *)

(* TODO: detect if all cases are an Idents in which case you can lift
 * up the Idents in an AndFinal
 *)
let and_final (And disjs) =
  AndFinal (disjs |> List_.filter_map or_final)

(* todo: instead of running multiple times for the AndFinal, we could
 * do an or, look at the matched string and detect which parts of the
 * AndFinal it is, increment a counter, and at the end make sure we've
 * found each AndFinal elements.
 *)
let _run_final (AndFinal xs) big_str =
  xs |> List.for_all (fun (Re re) ->
    Re.run re big_str
  )
[@@profiling]

  let final = and_final cnf in
(*  pr2 (show_cnf_final final); *)
  final, cnf_step2

 *)

(*****************************************************************************)
(* Run the regexps *)
(*****************************************************************************)

let eval_and p (And xs) =
  if List_.null xs then raise EmptyAnd;
  xs
  |> List.for_all (function Or xs ->
         if List_.null xs then raise EmptyOr;
         xs |> List.exists (fun x -> p x) |> fun v ->
         (* Dumper.dump fails in js_of_ocaml. It's a printout so let's just
            ignore it *)
         (try
            if not v then
              Log.debug (fun m -> m "this Or failed: %s" (Dumper.dump (Or xs)))
          with
         | e ->
             Log.err (fun m ->
                 m "exception while dumping: %s" (Printexc.to_string e)));
         v)

let run_cnf_step2 cnf big_str =
  cnf
  |> eval_and (function
       | Idents xs ->
           xs
           |> List.for_all (fun id ->
                  Log.debug (fun m -> m "check for the presence of %S" id);
                  (* TODO: matching_exact_word does not work, why??
                     because string literals and metavariables are put under
                     Idents? *)
                  let re = Pcre2_.matching_exact_string id in
                  (* Note that in case of a PCRE error, we want to assume
                     that the rule is relevant, hence ~on_error:true! *)
                  Pcre2_.unanchored_match ~on_error:true re big_str)
       | Regexp2_search re -> Pcre2_.unanchored_match re big_str)
[@@profiling]

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* see mli for more information
 * TODO: use a record.
 *)
type prefilter = Semgrep_prefilter_t.formula * (string -> bool)

(* see mli *)
type prefilter_cache = (Rule_ID.t, prefilter option) Hashtbl.t Domain.DLS.key

let prefilter_formula_of_prefilter (pre : prefilter) :
    Semgrep_prefilter_t.formula =
  let x, _f = pre in
  x

let compute_final_cnf ~(is_id_mvar : is_id_mvar) f =
  let* f = remove_not_final f in
  let cnf = cnf f in
  Log.debug (fun m -> m "cnf0 = %s" (show_cnf_step0 cnf));
  (* let cnf = and_step1 f in *)
  let cnf : cnf_step1 = and_step1 ~is_id_mvar cnf in
  Log.debug (fun m -> m "cnf1 = %s" (show_cnf_step1 cnf));
  (* TODO: regression on vertx-sqli.yaml
     let cnf = and_step1bis_filter_general cnf in
     logger#ldebug (lazy (spf "cnf1bis = %s" (show_cnf_step1 cnf)));
  *)
  let cnf = and_step2 cnf in
  Log.debug (fun m -> m "cnf2 = %s" (show_cnf_step2 cnf));
  Some cnf
[@@profiling]

let regexp_prefilter_of_formula ~xlang f : prefilter option =
  let is_id_mvar =
    match (xlang : Xlang.t) with
    | LRegex
    | LSpacegrep
    | LAliengrep ->
        Fun.const true
    | L _ ->
        let id_mvars = id_mvars_of_formula f in
        fun mvar -> MvarSet.mem mvar id_mvars
  in
  try
    let* final = compute_final_cnf ~is_id_mvar f in
    Some
      ( prefilter_formula_of_cnf_step2 final,
        fun big_str ->
          try
            run_cnf_step2 final big_str
            (* run_cnf_step2 (And [Or [Idents ["jsonwebtoken"]]]) big_str *)
          with
          (* can happen in spacegrep rules as we don't extract anything from t *)
          | EmptyAnd
          | EmptyOr ->
              true )
  with
  | GeneralPattern -> None

let regexp_prefilter_of_taint_rule ~xlang (_rule_id, rule_tok) taint_spec =
  (* We must be able to match some source _and_ some sink. *)
  let sources =
    taint_spec.R.sources |> snd
    |> List_.map (fun (src : R.taint_source) -> src.source_formula)
  in
  let sinks =
    taint_spec.R.sinks |> snd
    |> List_.map (fun (sink : R.taint_sink) -> sink.sink_formula)
  in
  let f =
    (* Note that this formula would likely not yield any meaningful result
     * if executed by search-mode, but it works for the purpose of this
     * analysis! *)
    R.And
      ( rule_tok,
        [ R.f (R.Or (rule_tok, sources)); R.f (R.Or (rule_tok, sinks)) ] )
    |> R.f
  in
  regexp_prefilter_of_formula ~xlang f

let regexp_prefilter_of_rule ~cache (r : R.rule) =
  let rule_id, _t = r.R.id in
  (* rule_id is supposed to be unique so it should work as a key for hmemo.
   * bugfix:
   *    old: let key = PI.file_of_info t ^ "." ^ rule_id
   * but some rules do not have a file (e.g., fake rules coming from -e/-f)
   * which was triggering a FakeInfoStr exn
   *)
  let key = rule_id in
  let regex_prefilter_fun () =
    try
      match r.mode with
      | `Search f
      | `Extract { formula = f; _ } ->
          regexp_prefilter_of_formula ~xlang:r.target_analyzer f
      | `Taint spec ->
          regexp_prefilter_of_taint_rule ~xlang:r.target_analyzer r.R.id spec
      | `Steps _ -> (* TODO *) None
      | `SCA _ -> None
    with
    (* TODO: see tests/rules/tainted-filename.yaml,
                 tests/rules/kotlin_slow_import.yaml *)
    | CNF_exploded ->
        Log.warn (fun m ->
            m "CNF size exploded on rule id %s" (Rule_ID.to_string rule_id));
        None
    | Stack_overflow ->
        Log.err (fun m ->
            m "Stack overflow on rule id %s" (Rule_ID.to_string rule_id));
        None
  in
  match cache with
  | None -> regex_prefilter_fun ()
  (* See [is_relevant_rule_from_xtarget]. Safe as long as we process 1 target per domain
   * at all times, and no other thread in the same domain can access the cache value. *)
  | Some cache ->
    Common.memoized_not_thread_safe (Domain.DLS.get cache) key regex_prefilter_fun
