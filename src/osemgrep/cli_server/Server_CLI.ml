module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'opengrep server' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = {
  common : CLI_common.conf;
  config : string;
  host : string;
  port : int;
  roots : string list;
}
[@@deriving show]

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

let o_config : string Term.t =
  let info =
    Arg.info [ "c"; "config" ]
      ~doc:
        "Rules to serve, as in 'opengrep scan --config': a YAML file, a \
         directory of rules, or a registry reference. Rules are parsed once \
         at startup and reused for every scan."
  in
  Arg.required (Arg.opt Arg.(some string) None info)

let o_host : string Term.t =
  let info = Arg.info [ "host" ] ~doc:"Address to listen on." in
  Arg.value (Arg.opt Arg.string "127.0.0.1" info)

let o_port : int Term.t =
  let info = Arg.info [ "port" ] ~doc:"Port to listen on." in
  Arg.value (Arg.opt Arg.int 8477 info)

let o_roots : string list Term.t =
  let info =
    Arg.info [] ~docv:"DIRS"
      ~doc:"Directories to register and scan (default: current directory)."
  in
  Arg.value (Arg.pos_all Arg.string [ "." ] info)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let cmdline_term : conf Term.t =
  let combine common config host port roots =
    { common; config; host; port; roots }
  in
  Term.(
    const combine $ CLI_common.o_common $ o_config $ o_host $ o_port $ o_roots)

let doc = "Start a long-running opengrep HTTP server (experimental)"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "Parses the rules once, registers the given directories as repos, runs \
       an initial scan of each, then serves results and per-file call graphs \
       over HTTP until killed.";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "opengrep server" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
