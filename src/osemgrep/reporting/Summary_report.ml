module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from output.py
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* python: SemgrepError.format_for_terminal in error.py: the level label,
   then the message with its location. The label takes the colour the rest
   of the text output gives that severity. *)
let pp_cli_error ppf (error : OutJ.cli_error) : unit =
  let (label : string), (style : Fmt.style) =
    match error.level with
    | `Error -> ("[ERROR]", `Fg `Red)
    | `Warning -> ("[WARN]", `Fg `Yellow)
    | `Info -> ("[INFO]", `Fg `Green)
  in
  Fmt.pf ppf "%a %s"
    Fmt.(styled style string)
    label
    (Option.value ~default:"" error.message)

(* python: OutputHandler.handle_semgrep_errors, which reports a warning
   only with --verbose and leaves the timeouts and the rules needing a
   missing plugin to their own summary lines *)
let cli_errors_to_report ~(verbose : bool) (errors : OutJ.cli_error list) :
    OutJ.cli_error list =
  let has_own_line (error : OutJ.cli_error) : bool =
    match error.type_ with
    | Timeout
    | MissingPlugin ->
        true
    | _else_ -> false
  in
  let is_reported (error : OutJ.cli_error) : bool =
    match error.level with
    | `Warning -> verbose
    | `Error
    | `Info ->
        true
  in
  errors
  |> List.filter (fun (error : OutJ.cli_error) ->
         (not (has_own_line error)) && is_reported error)

let pp_summary ~respect_gitignore ~is_git_repo ~(maturity : Maturity.t) ~max_target_bytes
    ~skipped_groups ppf () : unit =
  let {
    Skipped_report.ignored = semgrep_ignored;
    include_ = include_ignored;
    exclude = exclude_ignored;
    size = file_size_ignored;
    always = always_ignored;
    permissions = permissions_ignored;
    other = other_ignored;
    errors;
  } =
    skipped_groups
  in

  Fmt_.pp_heading ppf "Scan Summary";
  (* TODO
        if self.target_manager.baseline_handler:
            limited_fragments.append(
                "Scan was limited to files changed since baseline commit."
            )
  *)
  (* Printed on its own: whether git left a file out is not known, so it
     stays apart from the counts below. *)
  if respect_gitignore && is_git_repo then
      (* # Each target could be a git repo, and we respect the git ignore
         # of each target, so to be accurate with this print statement we
         # need to check if any target is a git repo and not just the cwd
         targets_not_in_git = 0
         dir_targets = 0
         for t in self.target_manager.targets:
             if t.path.is_dir():
                 dir_targets += 1
                 try:
                     t.files_from_git_ls()
                 except (subprocess.SubprocessError, FileNotFoundError):
                     targets_not_in_git += 1
                     continue
         if targets_not_in_git != dir_targets: *)
    Fmt.pf ppf "Scan was limited to files tracked by git.@\n";
  let opt_msg msg = function
    | [] -> None
    | xs -> Some (string_of_int (List.length xs) ^ " " ^ msg)
  in
  (* the ignored directories, reported once each, count apart *)
  let semgrepignored =
    let dirs, files =
      List.partition
        (fun (x : OutJ.skipped_target) -> UFile.is_dir ~follow_symlinks:true x.path)
        semgrep_ignored
    in
    match
      List_.filter_map Fun.id [ opt_msg "files" files; opt_msg "directories" dirs ]
    with
    | [] -> None
    | counts ->
        Some (String.concat " and " counts ^ " matching .semgrepignore patterns")
  in
  let out_skipped =
    (* in bytes below one megabyte, so that a small limit reads plainly *)
    let size : string =
      if max_target_bytes < 1_000_000 then
        String_.unit_str max_target_bytes "byte"
      else Printf.sprintf "%g MB" (float_of_int max_target_bytes /. 1e6)
    in
    List_.filter_map Fun.id
      [
        opt_msg "files not matching --include patterns" include_ignored;
        opt_msg "files matching --exclude patterns" exclude_ignored;
        opt_msg "files without read permission" permissions_ignored;
        opt_msg "files never scanned by Opengrep" always_ignored;
        opt_msg ("files larger than " ^ size) file_size_ignored;
        semgrepignored;
        (match maturity with
        | Develop -> opt_msg "other files ignored" other_ignored
        | Default
        | Legacy
        | Experimental ->
            None);
      ]
  in
  let out_partial =
    opt_msg
      "files only partially analyzed due to a parsing or internal Opengrep error"
      (Skipped_report.group_errors_by_file errors)
  in
  match (out_skipped, out_partial) with
  | [], None -> ()
  | xs, parts -> (
      Fmt.pf ppf "Some files were skipped or only partially analyzed.@\n";
      Option.iter (fun txt -> Fmt.pf ppf "  Partially scanned: %s@\n" txt) parts;
      match xs with
      | [] -> ()
      | xs ->
          Fmt.pf ppf "  Scan skipped: %s.@\n" (String.concat ", " xs);
          Fmt.pf ppf
            "  For a full list of skipped files, run opengrep with the \
             --verbose flag.@\n")

(* python: OutputHandler._handle_semgrep_timeout_errors *)
let pp_timeout_warnings ~(timeout_threshold : int) ppf
    (errors : OutJ.cli_error list) : unit =
  let timeouts_by_file : (Fpath.t * Rule_ID.t list) list =
    errors
    |> List_.filter_map (fun (e : OutJ.cli_error) ->
           match (e.type_, e.path, e.rule_id) with
           | OutJ.Timeout, Some path, Some rule_id -> Some (path, rule_id)
           | _ -> None)
    |> Assoc.group_by fst
    |> List_.map (fun (path, xs) ->
           (path, xs |> List_.map snd |> List.sort Rule_ID.compare))
  in
  timeouts_by_file
  |> List.iter (fun ((path : Fpath.t), (rule_ids : Rule_ID.t list)) ->
         let num_errs = List.length rule_ids in
         Fmt.pf ppf
           "%d timeout error(s) in %s when running the following rules: [%s]@\n"
           num_errs (Fpath.to_string path)
           (rule_ids |> List_.map Rule_ID.to_string |> String.concat ", ");
         if Int.equal num_errs timeout_threshold then
           Fmt.pf ppf
             "Opengrep stopped running rules on %s after %d timeout \
              error(s). See `--timeout-threshold` for more info.@\n"
             (Fpath.to_string path) num_errs);
  if
    Int.equal timeout_threshold 0
    && timeouts_by_file
       |> List.exists (fun (_path, rule_ids) -> List.length rule_ids > 5)
  then
    Fmt.pf ppf
      "You can use the `--timeout-threshold` flag to set a number of \
       timeouts after which a file will be skipped.@\n"
