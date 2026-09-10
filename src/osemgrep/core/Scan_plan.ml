(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Builds the Skin_model.Plan.t the report states before the scan runs.
 *
 * The counting the text used to do while it printed lives here, so that a
 * skin receives numbers rather than rules and jobs.
 *
 * Partially translated from semgrep_main.py (print_scan_status()) and from
 * core_runner.py (print()).
 *)

module P = Skin_model.Plan

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let origin rule =
  Option.value ~default:"custom"
    (match rule.Rule.metadata with
    | Some (Object _ as meta) -> (
        match Yojson.Basic.Util.member "semgrep.dev" (JSON.to_yojson meta) with
        | `Assoc _ as things -> (
            match Yojson.Basic.Util.member "rule" things with
            | `Assoc _ as things -> (
                match Yojson.Basic.Util.member "origin" things with
                | `String s -> Some s
                | _else -> None)
            | _else -> None)
        | _else -> None)
    | _else -> None)

(* NOTE: Some "languages" are instead general-purpose text analyzers and not
 * true programming languages. These include "regex", "generic" AKA
 * "spacegrep", and "aliengrep". Each of these "languages" have their own
 * pattern syntax and engine for matching patterns against targets, and thus
 * need to be executed separately from each other. However, for simplicity, we
 * merge the stats for these "languages" into a single "<multilang>" row.
 *)
let xlang_label = function
  | Xlang.LSpacegrep
  | Xlang.LAliengrep
  | Xlang.LRegex ->
      "<multilang>"
  | Xlang.L (l, _) -> Lang.to_lowercase_alnum l

(* the languages the jobs scan, under the labels of the table below *)
let languages_of (lang_jobs : Lang_job.t list) : string list =
  lang_jobs
  |> List_.map (fun (job : Lang_job.t) -> xlang_label job.xlang)
  |> List_.deduplicate

(* python: core_targets_plan.py table_by_origin() orders the rows by count,
   descending; the grouping has no order of its own, so equal counts are
   ordered by name *)
let origin_rows (rules : Rule.t list) : P.origin_row list =
  rules |> List_.map origin |> Assoc.group_by Fun.id
  |> List_.map (fun (src, xs) ->
         { P.origin = String.capitalize_ascii src; rules = List.length xs })
  |> List.sort (fun (a : P.origin_row) (b : P.origin_row) ->
         match Int.compare b.rules a.rules with
         | 0 -> String.compare a.origin b.origin
         | cmp -> cmp)

(* one row per language, counting the distinct files a job scans and the
   distinct rules it runs; sorted by files desc, rules desc, language asc *)
let lang_rows (lang_jobs : Lang_job.t list) : P.lang_row list =
  lang_jobs
  |> List_.map (fun Lang_job.{ xlang; targets; rules } ->
         (xlang_label xlang, rules, targets))
  |> Assoc.group_by (fun (xlang, _, _) -> xlang)
  |> List_.map (fun (language, xxs) ->
         let files =
           xxs
           |> List.concat_map (fun (_, _, targets) -> targets)
           |> Assoc.group_by (fun ({ target_fpath; _ } : Target_and_root.t) ->
                  target_fpath)
           |> List.length
         in
         let rules =
           xxs
           |> List.concat_map (fun (_, rules, _) -> rules)
           |> Assoc.group_by Fun.id |> List.length
         in
         { P.language; rules; files })
  |> List.sort (fun (a : P.lang_row) (b : P.lang_row) ->
         match Int.compare b.files a.files with
         | 0 -> (
             match Int.compare b.rules a.rules with
             | 0 -> String.compare a.language b.language
             | cmp -> cmp)
         | cmp -> cmp)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let of_lang_jobs ~(rules : Rule.t list) ~(num_targets : int)
    ~(tracked_by_git : bool) (lang_jobs : Lang_job.t list) : P.t =
  let num_files_with_a_rule =
    lang_jobs
    |> List.concat_map (fun (job : Lang_job.t) -> job.targets)
    |> List_.map (fun ({ target_fpath; _ } : Target_and_root.t) -> target_fpath)
    |> List_.deduplicate |> List.length
  in
  (* python: Plan.rule_count_for_product(), the rules that appear in a task.
     num_rules below counts every loaded rule; this one counts only the rules
     a target was paired with. *)
  let num_rules_with_a_target =
    lang_jobs
    |> List.concat_map (fun (job : Lang_job.t) ->
           match job.targets with
           | [] -> []
           | _ :: _ -> job.rules)
    |> List_.map (fun (rule : Rule.t) -> fst rule.id)
    |> List.sort_uniq Rule_ID.compare
    |> List.length
  in
  {
    P.num_targets;
    tracked_by_git;
    num_rules = List.length rules;
    num_files_with_a_rule;
    num_rules_with_a_target;
    languages = languages_of lang_jobs;
    lang_rows = lang_rows lang_jobs;
    origin_rows = origin_rows rules;
  }
