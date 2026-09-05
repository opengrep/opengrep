module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep test' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* The result of parsing a 'semgrep test' command. This is also used in
 * Scan_CLI.ml to transform legacy commands such as 'semgrep scan --tests <dir>'
 * into the new 'semgrep test <dir>'
 *)
type conf = {
  target : target_kind;
  (* ??? *)
  ignore_todo : bool;
  (* TODO? do we need those options? people use the JSON output?
   * the playground? and the optimizations and strict?
   *)
  json : bool;
  (* --force-color, which wins over $NO_COLOR like it does for a scan *)
  force_color : bool;
  (* take the whole core_runner_conf? like for validate? *)
  optimizations : bool;
  strict : bool;
  matching_diagnosis : bool;
  taint_intrafile : bool;
  (* None when the flag was not given, in which case the engine defaults
   * apply, and those set no limit
   *)
  timeout : float option;
  timeout_threshold : int option;
  max_memory_mb : int option;
  common : CLI_common.conf;
}

(* alt: we could accept multiple dirs, and multiple files
 * TODO? should we restrict the config_str to File or Dir?
 *)
and target_kind =
  | Dir of Fpath.t * Rules_config.config_string option (* optional --config *)
  | Files of Fpath.t list * Rules_config.config_string (* mandatory --config *)
[@@deriving show]

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* Flags *)
(* ------------------------------------------------------------------ *)
let o_test_ignore_todo : bool Term.t =
  H.negatable_flag [ "test-ignore-todo" ] ~neg_options:[ "no-test-ignore-todo" ]
    ~default:false
    ~doc:
      {|If --test-ignore-todo, ignores rules marked as '#todoruleid:' in
test files.
|}

let o_json : bool Term.t =
  let info = Arg.info [ "json" ] ~doc:{|Output results in JSON format.|} in
  Arg.value (Arg.flag info)

(* coupling: similar to Scan_CLI.o_strict? *)
(* TODO: be stricter when parsing target files; reject files that partially
 * parse.
 *)
let o_strict : bool Term.t =
  let info = Arg.info [ "strict" ] ~doc:{|???.|} in
  Arg.value (Arg.flag info)

(* coupling: Scan_CLI.o_config *)
let o_config : string list Term.t =
  H.string_list_with_env [ "c"; "f"; "config" ] ~env:"SEMGREP_RULES"
    ~doc:
      {|YAML configuration file, directory of YAML files ending in
.yml|.yaml, URL of a configuration file, or Semgrep registry entry name.
May also be set with SEMGREP_RULES, a whitespace-separated list of rule
sources.
|}

(* osemgrep-only: brandon's experiment *)
let o_matching_diagnosis : bool Term.t =
  let info =
    Arg.info [ "matching-diagnosis" ]
      ~doc:
        {|Whether to emit "matching diagnosis", which analyzes failing
test annotation cases and matching explanations to determine
why a rule did or did not match.|}
  in
  Arg.value (Arg.flag info)

(* The engine limits below are options: without the flag the test run keeps
 * the limits of a scan (see Test_subcommand.core_scan_config), which the
 * help text below spells out.
 *)

(* coupling: Scan_CLI.o_timeout *)
let o_timeout : float option Term.t =
  let info =
    Arg.info [ "timeout" ]
      ~doc:
        (Common.spf
           {|Maximum time to spend running a rule on a single file in
seconds. If set to 0 will not have time limit. Defaults to %.1f s.
|}
           Core_runner.default_conf.timeout)
  in
  Arg.value (Arg.opt (Arg.some Arg.float) None info)

(* coupling: Scan_CLI.o_timeout_threshold *)
let o_timeout_threshold : int option Term.t =
  let info =
    Arg.info [ "timeout-threshold" ]
      ~doc:
        (Common.spf
           {|Maximum number of rules that can time out on a file before
the file is skipped. If set to 0 will not have limit. Defaults to %d.
|}
           Core_runner.default_conf.timeout_threshold)
  in
  Arg.value (Arg.opt (Arg.some Arg.int) None info)

(* coupling: Scan_CLI.o_max_memory_mb *)
let o_max_memory_mb : int option Term.t =
  let info =
    Arg.info [ "max-memory" ]
      ~doc:
        (Common.spf
           {|Maximum system memory in MiB to use during the interfile pre-processing
phase, or when running a rule on a single file. If set to 0, will
not have memory limit. Defaults to %d.
|}
           Core_runner.default_conf.max_memory_mb)
  in
  Arg.value (Arg.opt (Arg.some Arg.int) None info)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

(* TODO: we accept just one elt here, so why not use just Arg.pos? *)
let o_args : string list Term.t =
  let info =
    Arg.info [] ~docv:"STRINGS" ~doc:{|Directory or file containing tests.|}
  in
  Arg.value (Arg.pos_all Arg.string [] info)

(* ------------------------------------------------------------------ *)
(* Intrafile tainting *)
(* ------------------------------------------------------------------ *)
let o_taint_intrafile : bool Term.t =
  let info =
    Arg.info [ "taint-intrafile" ]
      ~doc:
        ("Enable intra-file inter-procedural taint analysis. \
          Supported languages: Apex, C, Clojure, C#, C++, Dart, Elixir, Go, Java, JavaScript, Julia, Kotlin, Lua, Python, Ruby, Rust, Scala, Swift, TypeScript, Visual Basic. \
          Other languages will fall back to intraprocedural analysis only.")
  in
  Arg.value (Arg.flag info)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let target_kind_of_roots_and_config target_roots config =
  (* a target that does not exist is an error, as for 'scan' *)
  (match
     target_roots
     |> List.filter (fun (root : Fpath.t) -> not (Sys.file_exists !!root))
   with
  | [] -> ()
  | missing_roots ->
      Error.abort
        (missing_roots
        |> List_.map (fun (root : Fpath.t) ->
               Printf.sprintf "File not found: %s" !!root)
        |> String.concat "\n"));
  match (target_roots, config) with
  | [ file ], [ config ] ->
      if Sys.is_directory !!file then Dir (file, Some config)
      else Files ([file], config)
  | [ file ], [] ->
      if Sys.is_directory !!file then Dir (file, None)
      else
        (* was raise Exception but cleaner abort I think *)
        Error.abort "--config is required when running a test on single file"
  | [], _ -> Error.abort "at least one target required for tests"
  | files, [ config ] ->
      Files (files, config)
  | _, _ :: _ :: _ ->
      (* stricter: removed 'config directory' *)
      Error.abort "only one config allowed for tests"
  | _ :: _, [] ->
      Error.abort "--config required when running a test on multiple files"

let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine args common config force_color json matching_diagnosis
      max_memory_mb strict taint_intrafile test_ignore_todo timeout
      timeout_threshold =
    let target =
      target_kind_of_roots_and_config (Fpath_.of_strings args) config
    in
    {
      target;
      strict;
      json;
      force_color;
      ignore_todo = test_ignore_todo;
      common;
      optimizations = true;
      matching_diagnosis;
      taint_intrafile;
      timeout;
      timeout_threshold;
      max_memory_mb;
    }
  in
  Term.(
    const combine $ o_args $ CLI_common.o_common $ o_config
    $ CLI_common.o_force_color ~default:Output.default.force_color
    $ o_json $ o_matching_diagnosis
    $ o_max_memory_mb $ o_strict $ o_taint_intrafile $ o_test_ignore_todo
    $ o_timeout $ o_timeout_threshold)

let doc = "testing the rules"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "See https://semgrep.dev/docs/writing-rules/testing-rules/";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "opengrep test" ~doc ~man ~exits:CLI_common.exits_test

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
