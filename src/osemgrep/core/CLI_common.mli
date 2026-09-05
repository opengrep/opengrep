(*
   Shared flags across the different Semgrep commands and utilities to help
   with command-line parsing and handling (relies on the cmdliner library)

   The o_ below stands for option (as in command-line argument option).
*)

type conf = {
  (* mix of --debug, --quiet, --verbose *)
  logging_level : Logs.level option;
  profile : bool;
  (* mix of --experimental, --develop *)
  maturity : Maturity.t;
}
[@@deriving show]

(* handles logging arguments (--quiet/--verbose/--debug) *)
val o_logging : Logs.level option Cmdliner.Term.t

(* for --force-color/--no-force-color and $SEMGREP_FORCE_COLOR;
 * [default] is the force_color of the caller's default output
 * configuration *)
val o_force_color : default:bool -> bool Cmdliner.Term.t

(* small wrapper around Logs_helper.setup_logging and Logging_helpers.setup *)
val setup_logging : force_color:bool -> level:Logs.level option -> unit

(* for --profile *)
val o_profile : bool Cmdliner.Term.t

(* for --opengrep-ignore-pattern, shared by 'scan', 'ci' and 'test' *)
val o_opengrep_ignore_pattern : string option Cmdliner.Term.t

(* gather all the common flags under one term *)
val o_common : conf Cmdliner.Term.t
(* the exit codes a subcommand can return, to document in its man page;
   pass them to Cmdliner.Cmd.info ~exits, otherwise cmdliner documents its
   own defaults *)
val exits_scan : Cmdliner.Cmd.Exit.info list
val exits_ci : Cmdliner.Cmd.Exit.info list
val exits_test : Cmdliner.Cmd.Exit.info list
val exits_validate : Cmdliner.Cmd.Exit.info list
val exits_show : Cmdliner.Cmd.Exit.info list
val exits_lsp : Cmdliner.Cmd.Exit.info list
val exits_install_ci : Cmdliner.Cmd.Exit.info list
val help_page_bottom : Cmdliner.Manpage.block list

(* small wrapper around Cmdliner.Cmd.eval_value *)
val eval_value : argv:string array -> 'a Cmdliner.Cmd.t -> 'a
