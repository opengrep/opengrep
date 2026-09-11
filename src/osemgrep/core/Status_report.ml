(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  The legacy rendering of the scan plan. The counting it used to do while it
  printed is now in Scan_plan.ml, so that a skin gets numbers, not rules.

  Partially translated from semgrep_main.py (print_scan_status()) and from
  core_runner.py (print()).
*)

module P = Skin_model.Plan

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* The first line counts the files found by targeting and the rules
   loaded; the rest counts what the jobs pair up: a file some rule scans,
   a rule with a file. *)
let pp_status ppf (plan : P.t) : unit =
  Fmt_.pp_heading ppf "Scan Status";
  Fmt.pf ppf "  Scanning %s%s with %s"
    (String_.unit_str plan.num_targets "file")
    (if plan.tracked_by_git then " tracked by git" else "")
    (String_.unit_str plan.num_rules "Code rule");

  (* TODO if sca_rules ...
     Fmt.(option ~none:(any "") (any ", " ++ int ++ any "Supply Chain rule" *)
  (* TODO pro_rule
         if get_path(rule.metadata, ("semgrep.dev", "rule", "origin"), default=None)
         == "pro_rules"
     if pro_rule_count:
         summary_line += f", {unit_str(pro_rule_count, 'Pro rule')}"
  *)
  Fmt.pf ppf ":@.";
  if plan.num_rules_with_a_target = 0 || plan.num_files_with_a_rule = 0 then
    Fmt.pf ppf "  Nothing to scan."
  else if plan.num_rules_with_a_target = 1 then
    Fmt.pf ppf "  Scanning %s." (String_.unit_str plan.num_files_with_a_rule "file")
  else
    match plan.languages with
    (* python: scan_report.py _print_sast_table(), which prints one line
       instead of the tables when a single language is scanned *)
    | [ language ] ->
        Fmt.pf ppf "  Scanning %s with %s."
          (String_.unit_str plan.num_files_with_a_rule "file")
          (String_.unit_str plan.num_rules_with_a_target (language ^ " rule"))
    | _else_ ->
        Fmt.pf ppf "@.";
        Fmt_.pp_tables ppf
          ( "Language",
            [ "Rules"; "Files" ],
            plan.lang_rows
            |> List_.map (fun (r : P.lang_row) ->
                   (r.language, [ r.rules; r.files ])) )
          ( "Origin",
            [ "Rules" ],
            plan.origin_rows
            |> List_.map (fun (r : P.origin_row) -> (r.origin, [ r.rules ])) )
