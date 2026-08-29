(* Heejong Lee
 *
 * Copyright (C) 2024 Semgrep Inc.
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
module SS = Set.Make (String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A differential scan is when we run Semgrep two times:
 *  - once on a "baseline" commit (e.g., develop)
 *  - once on a "current" commit (e.g., HEAD)
 * Semgrep then reports only the new findings, that is findings that occur
 * in the current commit but not in the baseline.
 *
 * This helps a lot to migrate gradually to semgrep and to new rules by
 * not having to deal with all the findings that occur in a baseline.
 *
 * history: similar to my 'cmf --only-new-errors' at Facebook :)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type diff_scan_func =
  Fpath.t list -> Rule.rules -> Core_result.result_or_exn

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* This function removes duplicated matches from the results of the
   head commit scan if they are also present in the results of the
   baseline commit scan. Matches are considered identical if the
   tuples containing the rule ID, file path, and matched code snippet
   are equal. *)
let remove_matches_in_baseline caps (commit : string) (baseline : Core_result.t)
    (head : Core_result.t)
    (renamed : (string (* filename *) * string (* filename *)) list) =
  let extract_sig renamed (m : Core_match.t) =
    let rule_id = m.rule_id in
    let path =
      !!(m.path.internal_path_to_content) |> fun p ->
      Option.bind renamed
        (List_.find_some_opt (fun (before, after) ->
             if String.equal after p then Some before else None))
      |> Option.value ~default:p
    in
    let start_range, end_range = m.range_loc in
    (* TODO: what if we get an exn? *)
    let syntactic_ctx =
      UFile.lines_of_file_exn
        (start_range.pos.line, end_range.pos.line)
        m.path.internal_path_to_content
    in
    (rule_id, path, syntactic_ctx)
  in
  let sigs = Hashtbl.create 10 in
  Git_wrapper.run_with_worktree caps ~commit (fun () ->
      Globals.reset ();
      List.iter
        (fun ({ pm; _ } : Core_result.processed_match) ->
          pm |> extract_sig None |> fun x -> Hashtbl.add sigs x true)
        baseline.processed_matches);
  let removed = ref 0 in
  let processed_matches =
    List_.filter_map
      (fun (pm : Core_result.processed_match) ->
        let s = extract_sig (Some renamed) pm.pm in
        if Hashtbl.mem sigs s then (
          Hashtbl.remove sigs s;
          incr removed;
          None)
        else Some pm)
      (head.processed_matches
       (* Sort the matches in ascending order according to their byte positions.
          This ensures that duplicated matches are not removed arbitrarily;
          rather, priority is given to removing matches positioned closer to the
          beginning of the file. *)
      |> List.sort
           (fun ({ pm = x; _ } : Core_result.processed_match) { pm = y; _ } ->
             let x_start_range, x_end_range = x.range_loc in
             let y_start_range, y_end_range = y.range_loc in
             let start_compare =
               x_start_range.pos.bytepos - y_start_range.pos.bytepos
             in
             if start_compare <> 0 then start_compare
             else x_end_range.pos.bytepos - y_end_range.pos.bytepos))
  in
  Logs.app (fun m ->
      m "Removed %s that were in baseline scan"
        (String_.unit_str !removed "finding"));
  { head with processed_matches }

(* Execute the engine again on the baseline checkout, utilizing only
   the files and rules linked with matches from the head checkout
   scan. Subsequently, eliminate any previously identified matches
   from the results of the head checkout scan. *)
let scan_baseline_and_remove_duplicates (caps : < Cap.chdir ; Cap.tmp >)
    (profiler : Profiler.t)
    (result_or_exn : Core_result.result_or_exn) (rules : Rule.rules)
    (commit : string) (status : Git_wrapper.status)
    (core : diff_scan_func) : Core_result.result_or_exn =
  let/ r = result_or_exn in
  if r.processed_matches <> [] then
    let add_renamed paths =
      List.fold_left (fun x (y, _) -> SS.add y x) paths status.renamed
    in
    let remove_added paths =
      List.fold_left (Fun.flip SS.remove) paths status.added
    in
    let rules_in_match =
      r.processed_matches
      |> List_.map (fun ({ pm; _ } : Core_result.processed_match) ->
             pm.rule_id.id |> Rule_ID.to_string)
      |> SS.of_list
    in
    (* only use the rules that have been identified within the existing
       matches. *)
    let baseline_rules =
      rules
      |> List.filter (fun x ->
             SS.mem (x.Rule.id |> fst |> Rule_ID.to_string) rules_in_match)
    in
    let baseline_result =
      Profiler.record profiler ~name:"baseline_core_time" (fun () ->
          Git_wrapper.run_with_worktree caps ~commit (fun () ->
              Globals.reset ();
              let prepare_targets paths =
                paths |> SS.of_list |> add_renamed |> remove_added |> SS.to_seq
                |> Seq.filter_map (fun x ->
                       if
                         Sys.file_exists x
                         &&
                         match (Unix.lstat x).st_kind with
                         | S_LNK -> false
                         | _ -> true
                       then Some (Fpath.v x)
                       else None)
                |> List.of_seq
              in
              let paths_in_match =
                r.processed_matches
                |> List_.map (fun ({ pm; _ } : Core_result.processed_match) ->
                       !!(pm.path.internal_path_to_content))
                |> prepare_targets
              in
              core paths_in_match baseline_rules))
    in
    match baseline_result with
    | Error _exn -> baseline_result
    | Ok baseline_r ->
        Ok (remove_matches_in_baseline caps commit baseline_r r status.renamed)
  else Ok r

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let scan_baseline (caps : < Cap.chdir ; Cap.tmp >) (profiler : Profiler.t)
    (baseline : Find_targets.baseline_ref) (targets : Fpath.t list)
    (rules : Rule.rules) (diff_scan_func : diff_scan_func) :
    Core_result.result_or_exn =
  Logs.info (fun m ->
      m "running differential scan on baseline %s"
        (Find_targets.show_baseline_ref baseline));
  (* Commit and Rev already are the wanted base ('opengrep ci' hands those
   * out; python: BaselineHandler is_mergebase): recomputing the merge-base
   * can fail on the shallow clones CI providers hand out *)
  let commit =
    match baseline with
    | Find_targets.Merge_base_of rev -> Git_wrapper.merge_base rev
    | Find_targets.Commit sha -> Digestif.SHA1.to_hex sha
    | Find_targets.Rev rev -> rev
  in
  let status = Git_wrapper.status ~cwd:(Fpath.v ".") ~commit () in
  (* git reports plain relative paths; a "./"-prefixed scanning root would
     otherwise never match them *)
  let targets = List_.map Fpath.normalize targets in
  let targets =
    let added_or_modified =
      status.added @ status.modified |> List_.map Fpath.v
    in
    let added_or_modified_set = Fpath.Set.of_list added_or_modified in
    List.filter (fun p -> Fpath.Set.mem p added_or_modified_set) targets
  in
  let (head_scan_result : Core_result.result_or_exn) =
    Profiler.record profiler ~name:"head_core_time" (fun () ->
        diff_scan_func targets rules)
  in
  scan_baseline_and_remove_duplicates caps profiler head_scan_result rules
    commit status diff_scan_func
