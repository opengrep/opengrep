(* Opengrep authors
 *
 * Copyright (C) 2026 Opengrep authors
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
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The --time summary of the text output: the times of the command and of
 * the engine, the slowest files and rules, the files analysed per language
 * and the files with errors. Same data as the "time" field of the JSON
 * output.
 *
 * python: formatter/text.py print_time_summary()
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* python: print_time_summary(): the number of slowest files and rules
   listed, and the width at which their names are truncated *)
let items_to_show = 5
let col_lim = 50

(* python: util.format_bytes *)
let format_bytes (num : int) : string =
  let rec go (num : float) (units : string list) : string =
    match units with
    | [] -> spf "%.1fYB" num
    | unit :: rest ->
        if Float.abs num < 1024.0 then spf "%3d%sB" (int_of_float num) unit
        else go (num /. 1024.0) rest
  in
  go (float_of_int num) [ ""; "K"; "M"; "G"; "T"; "P"; "E"; "Z" ]

(* python: util.truncate, keeps the end of the name *)
let truncate (name : string) : string =
  let len = String.length name in
  if len > col_lim then
    let prefix = "..." in
    prefix ^ Str.string_after name (len - col_lim + String.length prefix)
  else name

let lang_of_path (path : Fpath.t) : string =
  match Lang.langs_of_filename path with
  | lang :: _ -> Lang.to_lowercase_alnum lang
  | [] -> "generic"

let sum (xs : float list) : float = List.fold_left ( +. ) 0.0 xs

let profiling_time (time : Out.profile) (name : string) : float =
  List.assoc_opt name time.profiling_times |> Option.value ~default:0.0

(* the slowest first, at most items_to_show *)
let slowest (by_time : ('a * float) list) : ('a * float) list =
  by_time
  |> List.stable_sort (fun (_, (a : float)) (_, (b : float)) -> Float.compare b a)
  |> List_.take_safe items_to_show

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_time_summary ppf (time : Out.profile) (errors : Out.cli_error list) :
    unit =
  let targets = time.targets in
  (* match_times only lists the rules that ran on a target *)
  let rule_match_times : (Rule_ID.t * float) list =
    time.rules
    |> List_.map (fun (rule_id : Rule_ID.t) ->
           ( rule_id,
             targets
             |> List.concat_map (fun (target : Out.target_times) ->
                    target.match_times
                    |> List_.filter_map (fun ((id : Rule_ID.t), (t : float)) ->
                           if Rule_ID.equal id rule_id then Some t else None))
             |> sum ))
  in
  let file_parsing_time =
    targets |> List_.map (fun (t : Out.target_times) -> t.parse_time) |> sum
  in
  let total_engine_time =
    (targets |> List_.map (fun (t : Out.target_times) -> t.run_time) |> sum)
    +. time.rules_parse_time
  in
  let total_matching_time = rule_match_times |> List_.map snd |> sum in
  Fmt.pf ppf "@.============================[ summary ]============================@.";
  Fmt.pf ppf "Total time: %.4fs Config time: %.4fs Core time: %.4fs@."
    (profiling_time time "total_time")
    (profiling_time time "config_time")
    (profiling_time time "core_time");
  Fmt.pf ppf "@.Engine time:@.";
  Fmt.pf ppf
    "Total CPU time: %.4fs  File parse time: %.4fs  Rule parse time: %.4fs  \
     Match time: %.4fs@."
    total_engine_time file_parsing_time time.rules_parse_time
    total_matching_time;
  Fmt.pf ppf "Slowest %d/%d files@." items_to_show (List.length targets);
  targets
  |> List_.map (fun (t : Out.target_times) -> (t, t.run_time))
  |> slowest
  |> List.iter (fun ((t : Out.target_times), (run_time : float)) ->
         Fmt.pf ppf "%a %-8s %.3fs (%.3fs to parse)@."
           Fmt.(styled (`Fg `Green) string)
           (spf "%-50s" (truncate (Fpath.to_string t.path)))
           (spf "(%s):" (format_bytes t.num_bytes))
           run_time t.parse_time);
  Fmt.pf ppf "Slowest %d rules to match@." items_to_show;
  rule_match_times |> slowest
  |> List.iter (fun ((rule_id : Rule_ID.t), (match_time : float)) ->
         Fmt.pf ppf "%a %.3fs@."
           Fmt.(styled (`Fg `Yellow) string)
           (spf "%-59s" (truncate (Rule_ID.to_string rule_id) ^ ":"))
           match_time);
  (* the files analysed, per language *)
  let by_lang : (string * Out.target_times list) list =
    targets
    |> Assoc.group_by (fun (t : Out.target_times) -> lang_of_path t.path)
    |> List.sort (fun ((a : string), _) ((b : string), _) -> String.compare a b)
  in
  let heading_width = String.length "Analyzed: " in
  let pp_headed (heading : string) (lines : string list) : unit =
    lines
    |> List.iteri (fun (i : int) (line : string) ->
           Fmt.pf ppf "%-*s%s@." heading_width
             (if Int.equal i 0 then heading else "")
             line)
  in
  Fmt.pf ppf "@.";
  pp_headed "Analyzed:"
    (by_lang
    |> List_.map (fun ((lang : string), (ts : Out.target_times list)) ->
           spf "%d %s files (%s in %.3f seconds)" (List.length ts) lang
             (format_bytes
                (ts |> List_.map (fun (t : Out.target_times) -> t.num_bytes)
               |> List.fold_left ( + ) 0))
             (ts |> List_.map (fun (t : Out.target_times) -> t.run_time) |> sum)));
  (* the files with errors, per error type; an error with no path is not
     an error of a file *)
  let file_errors : (Fpath.t * string) list =
    errors
    |> List_.filter_map (fun (e : Out.cli_error) ->
           Option.map
             (fun (path : Fpath.t) -> (path, Error.string_of_error_type e.type_))
             e.path)
    |> List.sort_uniq compare
  in
  let num_errors = List.length file_errors in
  let see_more =
    if Int.equal num_errors 0 then ""
    else ", see output before the results for details or run with --strict"
  in
  pp_headed "Errors:"
    (spf "%d files with errors%s" num_errors see_more
    :: (file_errors
       |> Assoc.group_by snd
       |> List.sort (fun ((a : string), _) ((b : string), _) ->
              String.compare a b)
       |> List_.map (fun ((error_type : string), (files : (Fpath.t * string) list)) ->
              spf "%s (%d files)" error_type (List.length files))));
  Fmt.pf ppf "@."
