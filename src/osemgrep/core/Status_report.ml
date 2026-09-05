(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from semgrep_main.py (print_scan_status()) and from
  core_runner.py (print()).
*)

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

(* NOTE: Some "languages" are instead general-purpose text analyzers and not true
 * programming languages. These include "regex", "generic" AKA "spacegrep",
 * and "aliengrep". Each of these "languages" have their own pattern syntax
 * and engine for matching patterns against targets, and thus need to be
 * executed separately from each other. However, for simplicity, we merge the
 * stats for these "languages" into a single "<multilang>" row.
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* The first line counts the files found by targeting and the rules
   loaded; the rest counts what the jobs pair up: a file some rule scans,
   a rule with a file. *)
let pp_status ~(rules : Rule.t list) ~num_targets ~tracked_by_git
    (lang_jobs : Lang_job.t list) ppf =
  let num_rules = List.length rules in
  Fmt_.pp_heading ppf "Scan Status";
  Fmt.pf ppf "  Scanning %s%s with %s"
    (String_.unit_str num_targets "file")
    (if tracked_by_git then " tracked by git" else "")
    (String_.unit_str num_rules "Code rule");

  (* TODO if sca_rules ...
     Fmt.(option ~none:(any "") (any ", " ++ int ++ any "Supply Chain rule" *)
  (* TODO pro_rule
         if get_path(rule.metadata, ("semgrep.dev", "rule", "origin"), default=None)
         == "pro_rules"
     if pro_rule_count:
         summary_line += f", {unit_str(pro_rule_count, 'Pro rule')}"
  *)
  Fmt.pf ppf ":@.";
  let num_files_with_a_rule =
    lang_jobs
    |> List.concat_map (fun (job : Lang_job.t) -> job.targets)
    |> List_.deduplicate |> List.length
  in
  (* python: Plan.rule_count_for_product(), the rules that appear in a
     task. The heading above counts every loaded rule; the lines below
     count only the rules a target was paired with. *)
  let num_rules_with_a_target =
    lang_jobs
    |> List.concat_map (fun (job : Lang_job.t) ->
           match job.targets with
           | [] -> []
           | _ :: _ -> job.rules)
    |> List_.map (fun (rule : Rule.t) -> Rule_ID.to_string (fst rule.id))
    |> List_.deduplicate |> List.length
  in
  if num_rules_with_a_target = 0 || num_files_with_a_rule = 0 then
    Fmt.pf ppf "  Nothing to scan."
  else if num_rules_with_a_target = 1 then
    Fmt.pf ppf "  Scanning %s." (String_.unit_str num_files_with_a_rule "file")
  else
    match languages_of lang_jobs with
    (* python: scan_report.py _print_sast_table(), which prints one line
       instead of the tables when a single language is scanned *)
    | [ language ] ->
        Fmt.pf ppf "  Scanning %s with %s."
          (String_.unit_str num_files_with_a_rule "file")
          (String_.unit_str num_rules_with_a_target (language ^ " rule"))
    | _else_ ->
        let rule_origins =
          rules |> List_.map origin
          |> Assoc.group_by Fun.id
          |> List_.map (fun (src, xs) ->
                 (String.capitalize_ascii src, List.length xs))
          (* python: core_targets_plan.py table_by_origin() orders the rows
             by count, descending; the grouping above has no order of its
             own, so equal counts are ordered by name *)
          |> List.sort (fun ((src : string), (n : int)) (src', n') ->
                 match Int.compare n' n with
                 | 0 -> String.compare src src'
                 | cmp -> cmp)
          |> List_.map (fun ((src : string), (n : int)) -> (src, [ n ]))
        in
        Fmt.pf ppf "@.";
        let compare (lang, rules_targets) (lang', rules_targets') =
          match (rules_targets, rules_targets') with
          | [ rules; targets ], [ rules'; targets' ] -> (
              match -compare targets targets' with
              | 0 -> (
                  match -compare rules rules' with
                  | 0 -> compare lang lang'
                  | cmp -> cmp)
              | cmp -> cmp)
          | _ -> failwith "Unexpected pattern"
        in
        let lang_stats =
          lang_jobs
          (* Unpack each job, transforming xlang into its mapped language key *)
          |> List_.map (fun Lang_job.{ xlang; targets; rules } ->
                 (xlang_label xlang, rules, targets))
          (* Merge jobs by mapped language key *)
          |> Assoc.group_by (fun (xlang, _, _) -> xlang)
          |> List_.map (fun (xlang, xxs) ->
                 let targets =
                   xxs
                   |> List.concat_map (fun (_, _, targets) -> targets)
                   |> Assoc.group_by Fun.id
                   |> List_.map (fun (target, _) -> target)
                   |> List.length
                 in
                 let rules =
                   xxs
                   |> List.concat_map (fun (_, rules, _) -> rules)
                   |> Assoc.group_by Fun.id
                   |> List_.map (fun (rules, _) -> rules)
                   |> List.length
                 in
                 (xlang, rules, targets))
        in
        Fmt_.pp_tables ppf
          ( "Language",
            [ "Rules"; "Files" ],
            lang_stats
            |> List.fold_left
                 (fun acc (lang, rules, targets) ->
                   match List.partition (fun (l, _) -> l = lang) acc with
                   | [], others -> (lang, [ rules; targets ]) :: others
                   | [ (_, [ r1; t1 ]) ], others ->
                       (lang, [ rules + r1; targets + t1 ]) :: others
                   | _ -> assert false)
                 []
            (* Sort by files desc, rules desc, lang asc *)
            |> List.sort compare )
          ("Origin", [ "Rules" ], rule_origins)
