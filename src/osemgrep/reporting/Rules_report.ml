(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reporting the config and rules used to the user *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pp_rule_sources ppf = function
  | Rules_source.Pattern _ -> Format.pp_print_string ppf "pattern"
  | Configs [ x ] -> Format.fprintf ppf "1 config %s" x
  | Configs xs -> Format.fprintf ppf "%d configs" (List.length xs)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* the rules that run, see Rule_filtering.filter_rules *)
let pp_rules ~too_many_entries ppf (rules_source, filtered_rules) =
  Fmt.pf ppf "running %d rules from %a@."
    (List.length filtered_rules)
    pp_rule_sources rules_source;
  (* TODO should output whether .semgrepignore is found and used
     (as done in semgrep_main.py get_file_ignore()) *)
  Fmt.pf ppf "Rules:@.";
  let rule_id r = fst r.Rule.id in
  if too_many_entries > 0 && List.length filtered_rules > too_many_entries
  then Fmt.pf ppf "%s" Skipped_report.too_much_data
  else
    filtered_rules
    |> List.sort (fun r1 r2 -> Rule_ID.compare (rule_id r1) (rule_id r2))
    |> List.iter (fun rule ->
           Fmt.pf ppf "- %s@." (Rule_ID.to_string (rule_id rule)))

(* One warning for the interfile rules [ids] that run per file because
   --enable-interfile is not given, one rule per line: the first
   [max_listed] of them at the default level, all of them under --verbose or
   --debug; then a hint printed by [pp_hint]. Nothing when [ids] is empty. *)
let warn_rules_run_per_file ~(pp_hint : string Fmt.t) (ids : Rule_ID.t list) :
    unit =
  let max_listed = 5 in
  match List_.map Rule_ID.to_string ids with
  | [] -> ()
  | (ids : string list) ->
      let num_ids = List.length ids in
      let listed =
        if num_ids <= max_listed || not (Logs_.is_terse_level (Logs.level ()))
        then ids
        else
          List_.take max_listed ids
          @ [ Common.spf "and %d more, see --verbose" (num_ids - max_listed) ]
      in
      Logs.warn (fun m ->
          m "interfile mode not enabled: %s per file only:\n%s"
            (if Int.equal num_ids 1 then "1 interfile rule runs"
             else Common.spf "%d interfile rules run" num_ids)
            (String.concat "\n" listed));
      Logs.app (fun m ->
          m "%a" pp_hint "use --enable-interfile for cross-file analysis")
