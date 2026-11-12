(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 * Copyright (C) 2020-2024 Semgrep Inc.
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
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file is mostly deprecated. You should use osemgrep text output instead
 * and stop using directly semgrep-core.
 *
 * it is unfortunately still useful to use the text output of semgrep-core,
 * even when combined with the -rules and -targets passed from pysemgrep,
 * to better see what is going on when debugging.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* When we print in the OneLine format we want to normalize the matched
 * expression or code and so only print the tokens in the AST (and not
 * the extra whitespace, newlines or comments). It's not enough though
 * to just List.map str_of_info because some PHP expressions such as
 * '$x = print FOO' would then be transformed into $x=printFOO, hence
 * this function
 *)
let rec join_with_space_if_needed xs =
  match xs with
  | [] -> ""
  | [ x ] -> x
  | x :: y :: xs ->
      if x =~ ".*[a-zA-Z0-9_]$" && y =~ "^[a-zA-Z0-9_]" then
        x ^ " " ^ join_with_space_if_needed (y :: xs)
      else x ^ join_with_space_if_needed (y :: xs)

(*****************************************************************************)
(* Print_aux *)
(*****************************************************************************)

let print_match_lines ?(str = "") ?(spaces = 0) (caps : < Cap.stdout >)
    (file : Fpath.t) (start_line : int) (end_line : int) : unit =
  let print = CapConsole.print caps#stdout in
  let prefix = spf "%s:%d" !!file start_line in
  let lines_str = UFile.lines_of_file_exn (start_line, end_line) file in
  let prefix = if str = "" then prefix else prefix ^ " " ^ str in
  let spaces_string = String.init spaces (fun _ -> ' ') in
  print (spaces_string ^ prefix);
  (* todo? some context too ? *)
  lines_str |> List.iter (fun s -> print (spaces_string ^ " " ^ s))

let print_intermediate_vars ~spaces (caps : < Cap.stdout >)
    (steps : Out.match_dataflow_trace) : unit =
  let spaces_string = String.init spaces (fun _ -> ' ') in
  let print str = CapConsole.print caps#stdout (spaces_string ^ str) in
  let rec loop_print curr_file = function
    | [] -> ()
    | (step : Out.match_dataflow_step) :: steps ->
        if not (Option.equal Fpath.equal (Some step.location.path) curr_file)
        then print !!(step.location.path);
        print (spf "- %s @l.%d" step.content step.location.start.line);
        loop_print (Some step.location.path) steps
  in
  loop_print None (Semgrep_output_utils.flat_dataflow_steps steps)

(* [leads_to_end] holds for the step under a call that leads to the source
   or to the sink *)
let rec print_taint_call_trace (caps : < Cap.stdout >) ~spaces
    ~(leads_to_end : Out.match_dataflow_step -> bool)
    (step : Out.match_dataflow_step) =
  let loc = step.location in
  match step.kind with
  | Out.SourceInCall
  | Out.SinkInCall ->
      let call_trace, intermediate_vars =
        List.partition leads_to_end step.children
      in
      let print = CapConsole.print caps#stdout in
      let spaces_string = String.init spaces (fun _ -> ' ') in
      print (spaces_string ^ "call to");
      print_match_lines caps ~spaces loc.path loc.start.line loc.end_.line;
      if not (List_.null intermediate_vars) then (
        print (spf "%sthese intermediate values are tainted:" spaces_string);
        print_intermediate_vars caps ~spaces:(spaces + 2) intermediate_vars);
      print (spaces_string ^ "then");
      call_trace
      |> List.iter
           (print_taint_call_trace caps ~spaces:(spaces + 2) ~leads_to_end)
  | Out.Source
  | Out.Through
  | Out.ThroughCall
  | Out.Parameter
  | Out.Return
  | Out.Sink ->
      print_match_lines caps ~spaces loc.path loc.start.line loc.end_.line

let print_taint_trace (caps : < Cap.stdout >)
    (taint_trace : Out.match_dataflow_trace) =
  let print = CapConsole.print caps#stdout in
  let to_source, rest =
    List.partition Semgrep_output_utils.leads_to_source taint_trace
  in
  let to_sink, intermediate_vars =
    List.partition Semgrep_output_utils.leads_to_sink rest
  in
  print "  * Taint may come from this source:";
  to_source
  |> List.iter
       (print_taint_call_trace caps ~spaces:4
          ~leads_to_end:Semgrep_output_utils.leads_to_source);
  if not (List_.null intermediate_vars) then (
    print "  * These intermediate values are tainted:";
    print_intermediate_vars caps ~spaces:4 intermediate_vars);
  print "  * This is how taint reaches the sink:";
  to_sink
  |> List.iter
       (print_taint_call_trace caps ~spaces:4
          ~leads_to_end:Semgrep_output_utils.leads_to_sink)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let print_match (caps : < Cap.stdout >) (match_ : Out.core_match) : unit =
  let str = spf "with rule %s" (Rule_ID.to_string match_.check_id) in
  let file = match_.path in
  let start_line = match_.start.line in
  let end_line = match_.end_.line in
  print_match_lines caps ~str file start_line end_line;
  match_.extra.dataflow_trace |> Option.iter (print_taint_trace caps)
