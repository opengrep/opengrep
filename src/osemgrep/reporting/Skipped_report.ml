open Common
open Fpath_.Operators
module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from target_manager.py (yield_verbose_lines())
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* this is used both in this file and in Summary_report.ml *)
type skipped_targets_grouped = {
  (* targets skipped because of file targeting spec *)
  ignored : Semgrep_output_v1_t.skipped_target list;
  size : Semgrep_output_v1_t.skipped_target list;
  include_ : Semgrep_output_v1_t.skipped_target list;
  exclude : Semgrep_output_v1_t.skipped_target list;
  always : Semgrep_output_v1_t.skipped_target list;
  other : Semgrep_output_v1_t.skipped_target list;
  (* targets possibly skipped because there was a parsing/matching/...
   * error while running the engine on it.
   *)
  errors : Semgrep_output_v1_t.skipped_target list;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let errors_to_skipped (errors : OutJ.core_error list) : OutJ.skipped_target list
    =
  errors
  |> List_.filter_map (fun OutJ.{ location; message; rule_id; _ } ->
         let* loc = location in
         Some
           OutJ.
             {
               path = loc.path;
               reason = Analysis_failed_parser_or_internal_error;
               details = Some message;
               rule_id;
             })

let group_skipped (skipped : OutJ.skipped_target list) : skipped_targets_grouped
    =
  let groups =
    Assoc.group_by
      (fun (OutJ.{ reason; _ } : OutJ.skipped_target) ->
        match reason with
        | Gitignore_patterns_match
        | Semgrepignore_patterns_match ->
            `Semgrepignore
        | Too_big
        | Exceeded_size_limit ->
            `Size
        | Cli_include_flags_do_not_match -> `Include
        | Cli_exclude_flags_match -> `Exclude
        | Analysis_failed_parser_or_internal_error -> `Error
        | Always_skipped -> `Always
        | Excluded_by_config
        | Wrong_language
        | Minified
        | Binary
        | Dotfile
        | Nonexistent_file
        | Irrelevant_rule
        | Too_many_matches
        | Insufficient_permissions ->
            `Other)
      skipped
  in
  {
    ignored =
      (try List.assoc `Semgrepignore groups with
      | Not_found -> []);
    include_ =
      (try List.assoc `Include groups with
      | Not_found -> []);
    exclude =
      (try List.assoc `Exclude groups with
      | Not_found -> []);
    size =
      (try List.assoc `Size groups with
      | Not_found -> []);
    always =
      (try List.assoc `Always groups with
      | Not_found -> []);
    other =
      (try List.assoc `Other groups with
      | Not_found -> []);
    errors =
      (try List.assoc `Error groups with
      | Not_found -> []);
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_skipped ~too_many_entries ppf
    (respect_git_ignore, maturity, max_target_bytes, skipped_groups) : unit =
  let {
    ignored = semgrep_ignored;
    include_ = include_ignored;
    exclude = exclude_ignored;
    size = file_size_ignored;
    always = always_ignored;
    other = other_ignored;
    errors;
  } =
    skipped_groups
  in

  (* not sure why but pysemgrep does not use the classic heading for skipped *)
  (* nope: Fmt_helpers.pp_heading ppf "Files skipped"; *)
  Fmt.pf ppf "@.%s@.Files skipped:@.%s@.@." (String.make 40 '=')
    (String.make 40 '=');

  (* python compatibility: the 24m is "underline off" *)
  let esc =
    if Fmt.style_renderer Fmt.stderr =*= `Ansi_tty then "\027[24m" else ""
  in

  let pp_list (xs : OutJ.skipped_target list) =
    match xs with
    | [] -> Fmt.pf ppf "   • <none>@."
    | xs ->
        List.iter
          (fun ({ path; _ } : OutJ.skipped_target) ->
            Fmt.pf ppf "   • %s@." !!path)
          (List.sort
             (fun (a : OutJ.skipped_target) (b : OutJ.skipped_target) ->
               Fpath.compare a.path b.path)
             xs)
  in

  Fmt.pf ppf "  %a@.@."
    Fmt.(styled `Bold string)
    (esc ^ "Always skipped by Opengrep:");
  pp_list always_ignored;
  Fmt.pf ppf "@.";
  Fmt.pf ppf "  %a@." Fmt.(styled `Bold string) (esc ^ "Skipped by .gitignore:");
  if respect_git_ignore then (
    Fmt.pf ppf "  %a@.@."
      Fmt.(styled `Bold string)
      (esc ^ "(Disable by passing --no-git-ignore)");
    Fmt.pf ppf "   • <all files not listed by `git ls-files` were skipped>@.")
  else (
    Fmt.pf ppf "  %a@.@."
      Fmt.(styled `Bold string)
      (esc ^ "(Disabled with --no-git-ignore)");
    Fmt.pf ppf "   • <none>@.");
  Fmt.pf ppf "@.";

  Fmt.pf ppf "  %a@.  %a@.@."
    Fmt.(styled `Bold string)
    (esc ^ "Skipped by .semgrepignore:")
    Fmt.(styled `Bold string)
    (esc
   ^ "(See: \
      https://semgrep.dev/docs/ignoring-files-folders-code/#understand-semgrep-defaults)"
    );
  pp_list semgrep_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf "  %a@.@."
    Fmt.(styled `Bold string)
    (esc ^ "Skipped by --include patterns:");
  pp_list include_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf "  %a@.@."
    Fmt.(styled `Bold string)
    (esc ^ "Skipped by --exclude patterns:");
  if too_many_entries > 0 && List.length exclude_ignored > too_many_entries then
    Fmt.pf ppf "   • %s@." Output.too_much_data
  else pp_list exclude_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf "  %a@.  %a@.@."
    Fmt.(styled `Bold string)
    (esc ^ "Skipped by limiting to files smaller than "
    ^ string_of_int max_target_bytes
    ^ " bytes:")
    Fmt.(styled `Bold string)
    (esc ^ "(Adjust with the --max-target-bytes flag)");
  pp_list file_size_ignored;
  Fmt.pf ppf "@.";

  (match maturity with
  | Maturity.Develop ->
      Fmt.pf ppf "  %a@.@."
        Fmt.(styled `Bold string)
        (esc ^ "Skipped for other reasons:");
      pp_list other_ignored;
      Fmt.pf ppf "@."
  | _else_ -> ());

  Fmt.pf ppf "  %a@.@."
    Fmt.(styled `Bold string)
    (esc ^ "Partially analyzed due to parsing or internal Opengrep errors");
  pp_list errors;
  Fmt.pf ppf "@."
