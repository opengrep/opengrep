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
  then Fmt.pf ppf "%s" Output.too_much_data
  else
    filtered_rules
    |> List.sort (fun r1 r2 -> Rule_ID.compare (rule_id r1) (rule_id r2))
    |> List.iter (fun rule ->
           Fmt.pf ppf "- %s@." (Rule_ID.to_string (rule_id rule)))
