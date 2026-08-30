(* Yoann Padioleau
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pad's extensions to Arg for actions. See pfff's Main.ml for
 * an example of use.
 *
 * DEPRECATED: this module is deprecated, you should use the
 * Cmdliner library instead.
 *
 * todo? or just use Cmdliner ...
 *  - isn't unison or scott-mcpeak-lib-in-cil handles that kind of
 *    stuff better ? That is the need to localize command line argument
 *    while still being able to gathering them. Same for logging.
 *    Similiar to the type prof = PALL | PNONE | PSOME of string list.
 *    Same spirit of fine grain config in log4j ?
 *  - how mercurial/cvs/git manage command line options ? because they
 *    all have a kind of DSL around arguments with some common options,
 *    specific options, conventions, etc.
 *  - generate the corresponding noxxx options ?
 *  - generate list of options and show their value ?
 *  - make it possible to set this value via a config file ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list

(* the format is a list of triples:
 *  (title of section * (optional) explanation of sections * options)
 *)
type options_with_title = string * string * arg_spec_full list
type cmdline_sections = options_with_title list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* now I use argv as I like at the call sites to show that
 * this function internally use argv.
 *)
let parse_options options usage_msg argv =
  let args = ref [] in
  try
    Arg.parse_argv argv options (fun file -> args := file :: !args) usage_msg;
    args := List.rev !args;
    !args
  with
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg;
      raise (UnixExit 2)
  | Arg.Help msg ->
      UPrintf.printf "%s" msg;
      raise (UnixExit 0)

let usage usage_msg options = Arg.usage (Arg.align options) usage_msg

(* ---------------------------------------------------------------------- *)

type flag_spec = Arg.key * Arg.spec * Arg.doc

type action_spec = Arg.key * Arg.doc * action_func
and action_func = string list -> unit

type cmdline_actions = action_spec list

exception WrongNumberOfArguments

let options_of_actions action_ref actions =
  actions
  |> List_.map (fun (key, doc, _func) ->
         (key, Arg.Unit (fun () -> action_ref := key), doc))

let (action_list : cmdline_actions -> Arg.key list) =
 fun xs -> List_.map (fun (a, _b, _c) -> a) xs

let (do_action : Arg.key -> string list (* args *) -> cmdline_actions -> unit) =
 fun key args xs ->
  let assoc = xs |> List_.map (fun (a, _b, c) -> (a, c)) in
  let action_func = List.assoc key assoc in
  action_func args

(* todo? if have a function with default argument ? would like a
 *  mk_action_0_or_1_arg ?
 *)

let mk_action_0_arg f = function
  | [] -> f ()
  | _ -> raise WrongNumberOfArguments

let mk_action_1_arg f = function
  | [ file ] -> f file
  | _ -> raise WrongNumberOfArguments

let mk_action_2_arg f = function
  | [ file1; file2 ] -> f file1 file2
  | _ -> raise WrongNumberOfArguments

let mk_action_n_arg f = f
let mk_action_1_conv conv f = mk_action_1_arg (fun str -> f (conv str))
let mk_action_n_conv conv f = mk_action_n_arg (fun xs -> f (List_.map conv xs))
