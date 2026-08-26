(* Define a flag that can be negated e.g. --foo and --no-foo.
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
*)
val negatable_flag :
  ?default:bool ->
  neg_options:string list ->
  doc:string ->
  string list ->
  bool Cmdliner.Term.t

(* Define a flag that can be negated e.g. --foo and --no-foo, and being able to
   specify an environment variable. env is the variable's name: the helper
   also reads the variable itself, so that an explicit false value and the
   OPENGREP_/SEMGREP_ alias are honoured (see the .ml).
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
*)
val negatable_flag_with_env :
  ?default:bool ->
  ?env:string ->
  neg_options:string list ->
  doc:string ->
  string list ->
  bool Cmdliner.Term.t

(* A repeatable string option whose environment variable holds a
   whitespace-separated list. env is the variable's name: the helper reads
   the variable itself, so that the value is split like the Python CLI
   splits it and the OPENGREP_/SEMGREP_ alias is honoured (see the .ml).
   Occurrences on the command line win over the environment. *)
val string_list_with_env :
  ?default:string list ->
  env:string ->
  doc:string ->
  string list ->
  string list Cmdliner.Term.t

(* A single-valued option whose value can also come from one of several
   environment variables (cmdliner supports only one per option). The
   first set variable wins; the command line wins over the environment. *)
val string_opt_with_envs :
  envs:string list ->
  doc:string ->
  string list ->
  string option Cmdliner.Term.t

(* Parse command-line arguments representing a number of bytes, such as
 * '5 mb' or '3.2GiB'
 *)
val number_of_bytes_converter : int Cmdliner.Arg.conv

val uri : Uri.t Cmdliner.Arg.conv
(** A simple [Uri.t] cmdliner's converter. *)

val sha1 : Digestif.SHA1.t Cmdliner.Arg.conv
(** A simple [Digestif.SHA1.t] cmdliner's converter. *)
