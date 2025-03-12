(*
   External parsers to be registered here by proprietary extensions of semgrep.
*)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

type pattern_parser =
  string (* pattern content *) ->
  (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t

type target_file_parser =
  Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t

module type T = sig
  val is_optional : bool

  val register_parsers :
    parse_pattern:pattern_parser -> parse_target:target_file_parser -> unit

  val is_available : unit -> bool
  val parse_pattern : pattern_parser
  val parse_target : target_file_parser
end

exception Missing_plugin of string

(* Table of missing plugins that are not optional *)
let missing_plugins : (Lang.t, unit) Hashtbl.t = Hashtbl.create 10

type pattern_parser = string -> AST_generic.any Tree_sitter_run.Parsing_result.t

type target_file_parser =
  string (* filename *) -> AST_generic.program Tree_sitter_run.Parsing_result.t

let missing_plugin_msg lang =
  spf
    "Missing Semgrep extension needed for parsing %s target. Try adding \
     `--pro-languages` to your command."
    (Lang.to_string lang)

let check_if_missing lang =
  if Hashtbl.mem missing_plugins lang then Error (missing_plugin_msg lang)
  else Ok ()

let check_if_missing_analyzer (analyzer : Xlang.t) =
  match analyzer with
  | LRegex
  | LSpacegrep
  | LAliengrep ->
      Ok ()
  | L (lang, other_langs) -> (
      match check_if_missing lang with
      | Ok () -> (
          other_langs
          |> List.find_map (fun lang ->
                 match check_if_missing lang with
                 | Ok () -> None
                 | Error msg -> Some msg)
          |> function
          | None -> Ok ()
          | Some msg -> Error msg)
      | Error _ as res -> res)

let all_possible_plugins = ref []

(* Create and manage the reference holding a plugin. *)
let make ?(optional = false) lang =
  all_possible_plugins := lang :: !all_possible_plugins;
  let parsers = ref None in
  if not optional then Hashtbl.add missing_plugins lang ();
  let register ~parse_pattern ~parse_target =
    match !parsers with
    | None ->
        parsers := Some (parse_pattern, parse_target);
        Hashtbl.remove missing_plugins lang
    | Some _existing_parsers ->
        (* This is a bug
         * update: this is slightly annoying though because in tests
         * we can call multiple time the same register function. See
         * the note in Proprietary_parser.ml about 'already_done'
         *)
        let msg =
          spf
            "Plugin initialization error: a %s parser is being registered \
             twice."
            (Lang.to_string lang)
        in
        failwith msg
  in
  let is_available () = !parsers <> None in
  let parse_pattern file =
    match !parsers with
    | None -> raise (Missing_plugin (missing_plugin_msg lang))
    | Some (parse_pattern, _) -> parse_pattern file
  in
  let parse_target file =
    match !parsers with
    | None ->
        let msg =
          spf
            "Missing Semgrep extension needed for parsing %s pattern. Try \
             adding `--pro` to your command."
            (Lang.to_string lang)
        in
        raise (Missing_plugin msg)
    | Some (_, parse_target) -> parse_target file
  in
  (optional, register, is_available, parse_pattern, parse_target)

module type T = sig
  val register_parsers :
    parse_pattern:pattern_parser -> parse_target:target_file_parser -> unit

  val is_available : unit -> bool
  val parse_pattern : pattern_parser
  val parse_target : target_file_parser
end

module Apex = struct
  let is_optional, register_parsers, is_available, parse_pattern, parse_target =
    make Lang.Apex
end

module Elixir = struct
  let is_optional, register_parsers, is_available, parse_pattern, parse_target =
    make Lang.Elixir
end

let all_possible_plugins = List.rev !all_possible_plugins
