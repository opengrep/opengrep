(*
   Comparison Tool for Opengrep/Semgrep Scan Results
   =================================================

   Compares two JSON output files from opengrep/semgrep scans and classifies
   each finding as: exact match, proximity match, or unique to one file.

   MATCHING STAGES
   ---------------

   1. EXACT MATCHING
      All fields identical: rule_id, path, start/end line+column, message

   2. PROXIMITY MATCHING (same rule + file, close byte offsets)
      Spans are "close" if one contains the other (subrange) or both start
      and end byte offsets are within tolerance.

   Each stage consumes matched findings, so a finding matches at most once.
*)

module Out = Semgrep_output_v1_j
module SOU = Semgrep_output_utils
open Cmdliner

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec compare_aux fns a b =
  match fns with
  | [] -> 0
  | fn :: rest ->
      let c = fn a b in
      if c <> 0 then c else compare_aux rest a b

(*****************************************************************************)
(* Keys for grouping/matching *)
(*****************************************************************************)

(* Full key for exact matching *)
module FindingKey = struct
  type t = {
    check_id : Out.rule_id;
    path : Out.fpath;
    start_line : int;
    end_line : int;
    start_col : int;
    end_col : int;
    msg : string;
  }

  let of_match (m : Out.cli_match) : t =
    {
      check_id = m.Out.check_id;
      path = m.Out.path;
      start_line = m.Out.start.line;
      end_line = m.Out.end_.line;
      start_col = m.Out.start.col;
      end_col = m.Out.end_.col;
      msg = m.Out.extra.message;
    }

  let compare a b =
    compare_aux
      [
        (fun a b -> Rule_ID.compare a.check_id b.check_id);
        (fun a b -> Fpath.compare a.path b.path);
        (fun a b -> Int.compare a.start_line b.start_line);
        (fun a b -> Int.compare a.end_line b.end_line);
        (fun a b -> Int.compare a.start_col b.start_col);
        (fun a b -> Int.compare a.end_col b.end_col);
        (fun a b -> String.compare a.msg b.msg);
      ]
      a b
end

(* Partial key for grouping by rule + file *)
module RuleFileKey = struct
  type t = { check_id : Out.rule_id; path : Out.fpath }

  let of_match (m : Out.cli_match) : t =
    { check_id = m.Out.check_id; path = m.Out.path }

  let compare a b =
    compare_aux
      [
        (fun a b -> Rule_ID.compare a.check_id b.check_id);
        (fun a b -> Fpath.compare a.path b.path);
      ]
      a b
end

module FindingKeySet = Set.Make (FindingKey)
module FindingKeyMap = Map.Make (FindingKey)
module RuleFileKeyMap = Map.Make (RuleFileKey)

(*****************************************************************************)
(* Matching logic *)
(*****************************************************************************)

(* Check if two spans are "close enough" to be the same finding.
   Uses byte offsets for precise comparison across line boundaries.
   Matches if one span contains the other OR both endpoints are within tolerance. *)
let spans_are_close ~(tolerance : int) (m1 : Out.cli_match) (m2 : Out.cli_match) : bool =
  let s1, e1 = m1.Out.start.offset, m1.Out.end_.offset in
  let s2, e2 = m2.Out.start.offset, m2.Out.end_.offset in
  let is_subrange = (s1 <= s2 && e2 <= e1) || (s2 <= s1 && e1 <= e2) in
  let start_close = abs (s1 - s2) <= tolerance in
  let end_close = abs (e1 - e2) <= tolerance in
  is_subrange || (start_close && end_close)

let group_by_rule_file (matches : Out.cli_match list) : Out.cli_match list RuleFileKeyMap.t =
  List.fold_left
    (fun acc m ->
      let key = RuleFileKey.of_match m in
      let existing = RuleFileKeyMap.find_opt key acc |> Option.value ~default:[] in
      RuleFileKeyMap.add key (m :: existing) acc)
    RuleFileKeyMap.empty matches

(* Try to match each item from file1 against file2 using a matcher function.
   Returns (matched_pairs, unmatched_from_file1, unmatched_from_file2) *)
let matching_pass
    ~(candidates : Out.cli_match list RuleFileKeyMap.t)
    ~(try_match : Out.cli_match -> Out.cli_match list -> Out.cli_match option)
    (file1_items : Out.cli_match list)
    (file2_items : Out.cli_match list) :
    (Out.cli_match * Out.cli_match) list * Out.cli_match list * Out.cli_match list =
  let matched, remaining1, used =
    List.fold_left
      (fun (matched_acc, remaining_acc, used_set) m1 ->
        let key = RuleFileKey.of_match m1 in
        let avail =
          RuleFileKeyMap.find_opt key candidates
          |> Option.value ~default:[]
          |> List.filter (fun m -> not (FindingKeySet.mem (FindingKey.of_match m) used_set))
        in
        match try_match m1 avail with
        | Some m2 ->
            let used_set' = FindingKeySet.add (FindingKey.of_match m2) used_set in
            ((m1, m2) :: matched_acc, remaining_acc, used_set')
        | None ->
            (matched_acc, m1 :: remaining_acc, used_set))
      ([], [], FindingKeySet.empty) file1_items
  in
  let remaining2 =
    List.filter (fun m -> not (FindingKeySet.mem (FindingKey.of_match m) used)) file2_items
  in
  (matched, remaining1, remaining2)

(*****************************************************************************)
(* Comparison result *)
(*****************************************************************************)

type proximity_match = {
  file1_match : Out.cli_match;
  file2_match : Out.cli_match;
}

type comparison_result = {
  exact_matches : int;
  proximity_matches : proximity_match list;
  only_in_first : Out.cli_match list;
  only_in_second : Out.cli_match list;
}

let compare_matches ~(tolerance : int) (matches1 : Out.cli_match list)
    (matches2 : Out.cli_match list) : comparison_result =
  (* Build maps for exact matching *)
  let map1 = List.fold_left (fun acc m -> FindingKeyMap.add (FindingKey.of_match m) m acc)
      FindingKeyMap.empty matches1 in
  let map2 = List.fold_left (fun acc m -> FindingKeyMap.add (FindingKey.of_match m) m acc)
      FindingKeyMap.empty matches2 in

  (* Exact matches and non-matches *)
  let exact_matches = FindingKeyMap.cardinal
      (FindingKeyMap.filter (fun k _ -> FindingKeyMap.mem k map2) map1) in
  let only1 = FindingKeyMap.fold
      (fun k v acc -> if FindingKeyMap.mem k map2 then acc else v :: acc) map1 [] in
  let only2 = FindingKeyMap.fold
      (fun k v acc -> if FindingKeyMap.mem k map1 then acc else v :: acc) map2 [] in

  (* Proximity matching *)
  let candidates2 = group_by_rule_file only2 in
  let try_proximity m1 avail =
    List.find_opt (spans_are_close ~tolerance m1) avail
  in
  let proximity_matched, final1, final2 =
    matching_pass ~candidates:candidates2 ~try_match:try_proximity only1 only2
  in

  let proximity_matches =
    List.map (fun (m1, m2) -> { file1_match = m1; file2_match = m2 }) proximity_matched
  in
  { exact_matches; proximity_matches; only_in_first = final1; only_in_second = final2 }

(*****************************************************************************)
(* JSON output *)
(*****************************************************************************)

type match_summary = {
  check_id : string;
  path : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
  start_offset : int;
  end_offset : int;
  message : string;
  fingerprint : string;
}
[@@deriving yojson]

type proximity_match_json = {
  file1_match : match_summary;
  file2_match : match_summary;
}
[@@deriving yojson]

type comparison_result_json = {
  exact_match_count : int;
  proximity_match_count : int;
  only_in_first_count : int;
  only_in_second_count : int;
  only_in_first : match_summary list;
  only_in_second : match_summary list;
  proximity_matches : proximity_match_json list;
}
[@@deriving yojson]

let summarize (m : Out.cli_match) : match_summary =
  {
    check_id = Rule_ID.to_string m.Out.check_id;
    path = Fpath.to_string m.Out.path;
    start_line = m.Out.start.line;
    start_col = m.Out.start.col;
    end_line = m.Out.end_.line;
    end_col = m.Out.end_.col;
    start_offset = m.Out.start.offset;
    end_offset = m.Out.end_.offset;
    message = m.Out.extra.message;
    fingerprint = m.Out.extra.fingerprint;
  }

let to_json (r : comparison_result) : comparison_result_json =
  {
    exact_match_count = r.exact_matches;
    proximity_match_count = List.length r.proximity_matches;
    only_in_first_count = List.length r.only_in_first;
    only_in_second_count = List.length r.only_in_second;
    only_in_first = List.map summarize r.only_in_first;
    only_in_second = List.map summarize r.only_in_second;
    proximity_matches = List.map (fun (pm : proximity_match) ->
      { file1_match = summarize pm.file1_match;
        file2_match = summarize pm.file2_match;
      }) r.proximity_matches;
  }

(*****************************************************************************)
(* Text output *)
(*****************************************************************************)

let print_match_simple (m : Out.cli_match) : unit =
  Printf.printf "- Line %d:%d-%d:%d (bytes %d-%d)\n"
    m.Out.start.line m.Out.start.col m.Out.end_.line m.Out.end_.col
    m.Out.start.offset m.Out.end_.offset

let print_match_with_code (m : Out.cli_match) : unit =
  print_match_simple m;
  let lines = String.split_on_char '\n' m.Out.extra.lines in
  let n = List.length lines in
  let display =
    if n <= 10 then m.Out.extra.lines
    else
      let first = List.filteri (fun i _ -> i < 5) lines in
      let last = List.filteri (fun i _ -> i >= n - 5) lines in
      String.concat "\n" first ^ "\n    ...\n" ^ String.concat "\n" last
  in
  Printf.printf "```\n%s\n```\n" display

let print_grouped ~show_code ~max_items (matches : Out.cli_match list) : unit =
  let to_show = match max_items with Some n -> List.take n matches | None -> matches in
  let grouped = group_by_rule_file to_show in
  RuleFileKeyMap.iter
    (fun key ms ->
      Printf.printf "### `%s`\n" (Rule_ID.to_string key.RuleFileKey.check_id);
      Printf.printf "**in** `%s`\n\n" (Fpath.to_string key.RuleFileKey.path);
      let sorted = List.sort SOU.compare_cli_matches ms in
      List.iter (if show_code then print_match_with_code else print_match_simple) sorted;
      Printf.printf "\n")
    grouped

let print_only_section ~verbose ~show_code ~max_default filename matches : unit =
  let count = List.length matches in
  Printf.printf "## Only in `%s`: %d matches\n\n" filename count;
  if count > 0 then
    if verbose || count <= max_default then
      print_grouped ~show_code ~max_items:None matches
    else begin
      print_grouped ~show_code ~max_items:(Some max_default) matches;
      Printf.printf "*Showing first %d of %d. Use `--verbose` to see all.*\n\n" max_default count
    end

let print_proximity_section ~verbose ~max_default file1 file2 (matches : proximity_match list) : unit =
  if matches <> [] then begin
    Printf.printf "## Proximity matches: %d\n\n" (List.length matches);
    Printf.printf "*Tools derived slightly different spans for the same finding*\n\n";
    let to_show = if verbose then matches else List.take max_default matches in
    List.iter (fun (pm : proximity_match) ->
        let m1, m2 = pm.file1_match, pm.file2_match in
        Printf.printf "### `%s`\n" (Rule_ID.to_string m1.Out.check_id);
        Printf.printf "**in** `%s`\n\n" (Fpath.to_string m1.Out.path);
        Printf.printf "- **%s**: %d:%d - %d:%d (bytes %d-%d)\n" file1
          m1.Out.start.line m1.Out.start.col m1.Out.end_.line m1.Out.end_.col
          m1.Out.start.offset m1.Out.end_.offset;
        Printf.printf "- **%s**: %d:%d - %d:%d (bytes %d-%d)\n" file2
          m2.Out.start.line m2.Out.start.col m2.Out.end_.line m2.Out.end_.col
          m2.Out.start.offset m2.Out.end_.offset;
        Printf.printf "\n")
      to_show;
    if not verbose && List.length matches > max_default then
      Printf.printf "*Showing first %d of %d.*\n\n" max_default (List.length matches)
  end

let output_text ~verbose ~show_code ~tolerance ~file1 ~file2
    ~(stats1 : int * int) ~(stats2 : int * int) (r : comparison_result) : unit =
  let total1, unique1 = stats1 in
  let total2, unique2 = stats2 in
  Printf.printf "# Comparison Results\n\n";
  Printf.printf "**Byte tolerance**: %d\n\n" tolerance;
  Printf.printf "## Summary\n\n";
  Printf.printf "| File | Total | Unique | Dupes |\n";
  Printf.printf "|------|-------|--------|-------|\n";
  Printf.printf "| `%s` | %d | %d | %d |\n" file1 total1 unique1 (total1 - unique1);
  Printf.printf "| `%s` | %d | %d | %d |\n\n" file2 total2 unique2 (total2 - unique2);
  print_only_section ~verbose ~show_code ~max_default:20 file1 r.only_in_first;
  print_only_section ~verbose ~show_code ~max_default:20 file2 r.only_in_second;
  Printf.printf "## Exact matches: %d\n\n" r.exact_matches;
  print_proximity_section ~verbose ~max_default:10 file1 file2 r.proximity_matches

(*****************************************************************************)
(* Main *)
(*****************************************************************************)

let read_and_parse path =
  Out.cli_output_of_string (UFile.read_file (Fpath.v path))

let diff_files verbose show_code tolerance json_output file1 file2 =
  try
    let output1 = read_and_parse file1 in
    let output2 = read_and_parse file2 in
    let matches1 = output1.Out.results in
    let matches2 = output2.Out.results in

    let result = compare_matches ~tolerance matches1 matches2 in

    if json_output then
      Printf.printf "%s\n" (Yojson.Safe.pretty_to_string
        (comparison_result_json_to_yojson (to_json result)))
    else begin
      let unique1 = FindingKeyMap.cardinal
          (List.fold_left (fun acc m -> FindingKeyMap.add (FindingKey.of_match m) m acc)
             FindingKeyMap.empty matches1) in
      let unique2 = FindingKeyMap.cardinal
          (List.fold_left (fun acc m -> FindingKeyMap.add (FindingKey.of_match m) m acc)
             FindingKeyMap.empty matches2) in
      output_text ~verbose ~show_code ~tolerance ~file1 ~file2
        ~stats1:(List.length matches1, unique1)
        ~stats2:(List.length matches2, unique2) result
    end;
    Cmd.Exit.ok
  with
  | Sys_error msg ->
      Printf.eprintf "Error reading file: %s\n" msg;
      Cmd.Exit.some_error
  | Atdgen_runtime.Oj_run.Error msg ->
      Printf.eprintf "Error parsing JSON: %s\n" msg;
      Cmd.Exit.some_error
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      Cmd.Exit.some_error

(*****************************************************************************)
(* CLI *)
(*****************************************************************************)

let verbose_arg =
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc:"Show all matches")

let show_code_arg =
  Arg.(value & flag & info [ "c"; "show-code" ] ~doc:"Display code fragments")

let tolerance_arg =
  Arg.(value & opt int 5 & info [ "t"; "tolerance" ] ~docv:"BYTES"
         ~doc:"Byte tolerance for proximity matching (default: 5)")

let json_arg =
  Arg.(value & flag & info [ "j"; "json" ] ~doc:"Output as JSON")

let file1_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE1"
         ~doc:"First scan output file")

let file2_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE2"
         ~doc:"Second scan output file")

let cmd =
  let doc = "Compare two Opengrep/Semgrep scan output files" in
  let man = [
    `S Manpage.s_description;
    `P "Compares findings using exact matching and proximity matching (by byte offset).";
    `S Manpage.s_examples;
    `Pre "$(cmd) output1.json output2.json";
    `Pre "$(cmd) -t 10 --show-code output1.json output2.json";
  ] in
  Cmd.v (Cmd.info "opengrep-diff" ~version:"1.0.0" ~doc ~man)
  @@ Term.(const diff_files $ verbose_arg $ show_code_arg $ tolerance_arg
           $ json_arg $ file1_arg $ file2_arg)

let () = exit (Cmd.eval' cmd)
