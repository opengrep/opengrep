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
  Target_and_root.t list -> Rule.rules -> Core_result.result_or_exn

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* This function removes duplicated matches from the results of the
   head commit scan if they are also present in the results of the
   baseline commit scan. Matches are considered identical if the
   tuples containing the rule ID, file path, and matched code snippet
   are equal. *)
(* The path component is taken relative to the scan root.  Matches normally
   carry the target path as given (relative), but a match on an absolute
   target carries an absolute path — into the baseline worktree for the
   baseline scan, into the real checkout for the head one — so those must be
   relativized against each scan's own root or they could never compare
   equal and such findings could never be deduplicated. *)
let extract_sig ~(root : Fpath.t) renamed (m : Core_match.t) =
  let rule_id = m.rule_id in
  let abs_path = m.path.internal_path_to_content in
  let rel_path =
    match Fpath.relativize ~root abs_path with
    | Some rel -> rel
    | None -> abs_path
  in
  let path =
    !!rel_path |> fun p ->
    Option.bind renamed
      (List_.find_some_opt (fun (before, after) ->
           if String.equal after p then Some before else None))
    |> Option.value ~default:p
  in
  let start_range, end_range = m.range_loc in
  (* TODO: what if we get an exn? *)
  let syntactic_ctx =
    UFile.lines_of_file_exn (start_range.pos.line, end_range.pos.line) abs_path
  in
  (rule_id, path, syntactic_ctx)

(* [baseline_sigs] must have been built inside the worktree that produced the
   baseline matches: reading a match's lines needs its file, and an interfile
   match's path is absolute into that worktree, which is gone by the time we
   get here. *)
let remove_matches_in_baseline sigs (head : Core_result.t)
    (renamed : (string (* filename *) * string (* filename *)) list) =
  let root = Fpath.v (Sys.getcwd ()) in
  let extract_sig renamed m = extract_sig ~root renamed m in
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
    (conf : Scan_CLI.conf) (profiler : Profiler.t)
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
              (* Per-target replay targets carry [project_root = None]: the
                 per-target engine does not consult it.  Interfile replay
                 targets get real roots below instead — see
                 [baseline_targets]. *)
              let wrap_as_targets (fpaths : Fpath.t list)
                  : Target_and_root.t list =
                List_.map
                  (fun (fpath : Fpath.t) : Target_and_root.t ->
                    { target_fpath = fpath; project_root = None })
                  fpaths
              in
              (* A rule can turn interfile on by itself, so the CLI flag alone
                 does not decide this. *)
              let interfile_in_play =
                conf.core_runner_conf.taint_interfile
                || List.exists
                     (fun (rule : Rule.rule) ->
                        match rule.Rule.options with
                        | Some opts -> opts.taint_interfile
                        | None -> false)
                     rules
              in
              let baseline_targets =
                if interfile_in_play then
                  (* An interfile match depends on files that carry no match of
                     their own — the caller supplying the taint — so replaying
                     only [paths_in_match] cannot reproduce it: the baseline
                     comes up empty and every pre-existing cross-file finding
                     gets reported as newly introduced.  The head's scanned set
                     is no help either, since a diff scan already narrowed the
                     head to the changed files, so take the baseline's own full
                     target set. *)
                  (* Rediscover targets AND project roots inside the baseline
                     worktree, exactly as the head scan discovered them in the
                     real checkout.  Rebuilding targets from bare paths with
                     [project_root = None] would make interfile dispatch fall
                     back to [cwd] as the root — and [run_with_worktree] enters
                     the worktree at the subdirectory matching the launch cwd,
                     so a scan launched from a repo subdirectory would build
                     the baseline graph without the companion files outside
                     that subdirectory, resurrecting pre-existing cross-file
                     findings as "new".  Multi-root scans lose their per-target
                     roots the same way. *)
                  let { Find_targets.selected = all_in_baseline; _ } =
                    Find_targets.get_target_fpaths_with_project_roots
                      conf.targeting_conf conf.target_roots
                  in
                  all_in_baseline
                else wrap_as_targets paths_in_match
              in
              let res = core baseline_targets baseline_rules in
              (* Build the signatures HERE, still inside the worktree that
                 produced these matches: an interfile match's path is absolute
                 into this worktree, and it is removed as soon as we return. *)
              let root = Fpath.v (Sys.getcwd ()) in
              let sigs =
                match res with
                | Ok (baseline_r : Core_result.t) ->
                  let tbl =
                    Hashtbl.create
                      (List.length baseline_r.processed_matches)
                  in
                  List.iter
                    (fun ({ pm; _ } : Core_result.processed_match) ->
                       Hashtbl.replace tbl (extract_sig ~root None pm) true)
                    baseline_r.processed_matches;
                  tbl
                | Error _ -> Hashtbl.create 0
              in
              (res, sigs)))
    in
    match baseline_result with
    | res, _sigs when Result.is_error res -> res
    | _res, sigs -> Ok (remove_matches_in_baseline sigs r status.renamed)
  else Ok r

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let scan_baseline (caps : < Cap.chdir ; Cap.tmp >) (conf : Scan_CLI.conf)
    (profiler : Profiler.t) (baseline : Find_targets.baseline_ref)
    (targets : Target_and_root.t list)
    (rules : Rule.rules)
    ~(head_scan_func : diff_scan_func)
    ~(baseline_scan_func : diff_scan_func) :
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
  (* The whole differential scan works on paths relative to the current
     directory: that is the form git lists ('--relative' in
     Git_wrapper.status) and the form the baseline worktree is scanned in.
     The targets of an absolute scanning root are absolute, and those of a
     root above the current directory keep its '../' prefix; both are
     spelled from the current directory here. Without this they match none
     of git's paths and the scan silently reports no finding at all. *)
  let cwd = Rpath.getcwd () |> Rpath.to_fpath |> Fpath.to_dir_path in
  (* the current directory above is free of symbolic links and a root the
     user spelled through one is not ('/tmp' on macOS, a junction on
     Windows); the two forms cannot be relativized against each other.
     Only the directory is resolved, so a target that is itself a symlink
     keeps the name git lists it under. The targets share a few directories,
     each resolved once. *)
  let resolved_dirs : (Fpath.t, Fpath.t) Hashtbl.t = Hashtbl.create 16 in
  let resolve_dir (dir : Fpath.t) : Fpath.t =
    match Hashtbl.find_opt resolved_dirs dir with
    | Some real -> real
    | None ->
        let real =
          match Rpath.of_fpath dir with
          | Ok real -> Rpath.to_fpath real
          | Error (_ : string) -> dir
        in
        Hashtbl.add resolved_dirs dir real;
        real
  in
  let relative_to_cwd (path : Fpath.t) : Fpath.t =
    let absolute =
      Fpath.normalize (if Fpath.is_abs path then path else Fpath.(cwd // path))
    in
    let resolved =
      let dir, last_segment = Fpath.split_base absolute in
      Fpath.(resolve_dir dir // last_segment)
    in
    match Fpath.relativize ~root:cwd resolved with
    | Some rel -> Fpath.normalize rel
    | None -> Fpath.normalize path
  in
  (* git reports plain relative paths; a "./"-prefixed scanning root would
     otherwise never match them *)
  let targets =
    List_.map
      (fun (t : Target_and_root.t) ->
        { t with target_fpath = relative_to_cwd t.target_fpath })
      targets
  in
  let targets =
    let added_or_modified =
      status.added @ status.modified |> List_.map Fpath.v
    in
    let added_or_modified_set = Fpath.Set.of_list added_or_modified in
    List.filter
      (fun ({ Target_and_root.target_fpath; _ }) ->
        Fpath.Set.mem target_fpath added_or_modified_set)
      targets
  in
  let (head_scan_result : Core_result.result_or_exn) =
    Profiler.record profiler ~name:"head_core_time" (fun () ->
        head_scan_func targets rules)
  in
  scan_baseline_and_remove_duplicates caps conf profiler head_scan_result rules
    commit status baseline_scan_func
