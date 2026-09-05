open Common
open Cmdliner

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Cmdliner helpers not found in the default API.

   TODO: parser+printer for file path so we can write things like:

        Arg.value (Arg.opt (Arg.some fpath) None info)

      instead of

        Arg.value (Arg.opt (Arg.some Arg.string) None info)
        (* + having to convert the string to an fpath by hand *)

      The main benefit would be to clarify error messages by having Fpath.t
      instead of string.

   val fpath : Fpath.t Cmdliner.conv????
*)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

let uri =
  let parser str = Ok (Uri.of_string str) in
  let pp = Fmt.(using Uri.to_string string) in
  Arg.conv ~docv:"<URL>" (parser, pp)

let sha1 =
  let parser str =
    match Digestif.SHA1.consistent_of_hex_opt str with
    | Some sha1 -> Ok sha1
    | None -> Error (`Msg (Fmt.str "Invalid SHA1 value: %S" str))
  in
  let pp = Digestif.SHA1.pp in
  Arg.conv ~docv:"<SHA1>" (parser, pp)

(* Turn "a" into "-a" and "abc" into "--abc" *)
let add_option_dashes option_names =
  List_.map
    (fun s ->
      assert (s <> "");
      if String.length s =|= 1 then "-" ^ s else "--" ^ s)
    option_names

(* Define a flag that can be negated e.g. --foo and --no-foo.
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
   See https://github.com/dbuenzli/cmdliner/issues/164
*)
let negatable_flag ?(default = false) ~neg_options ~doc options =
  let neg_doc =
    let options_str = add_option_dashes options |> String.concat "/" in
    Printf.sprintf "negates %s" options_str
  in
  let enable = (true, Arg.info options ~doc) in
  let disable = (false, Arg.info neg_options ~doc:neg_doc) in
  Arg.value (Arg.vflag default [ enable; disable ])

(* the error of a variable whose value is not what the option expects *)
let env_value_error ~(var : string) ~(value : string) (expected : string) :
    [> `Msg of string ] =
  `Msg
    (Printf.sprintf "environment variable %s: invalid value %S, expected %s"
       var value expected)

(* same vocabulary as cmdliner's env_bool_parse, which is not exported *)
let parse_env_bool (var : string) (value : string) :
    (bool, [ `Msg of string ]) result =
  match String.lowercase_ascii value with
  | "false"
  | "no"
  | "n"
  | "0" ->
      Ok false
  | "true"
  | "yes"
  | "y"
  | "1" ->
      Ok true
  | _else_ -> Error (env_value_error ~var ~value "a boolean")

(* Cmdliner.Arg.vflag_all ignores environment variables (the one on the
   positive flag is attached only for the man page), so the variable is read
   here instead, through Opengrep_env so the OPENGREP_/SEMGREP_ alias also
   counts, and an explicit false is distinguished from an unset variable.
   vflag_all keeps the command-line order: with the flag and its negation
   both given, the last one wins, and an explicit flag wins over the
   environment. *)
(* An option given on the command line wins over its environment variable;
   the user is told which variable was ignored. [vars] are the names the
   option reads, the first one set is the one named. *)
let warn_env_ignored ~(vars : string list) (options : string list) : unit =
  match List.find_map Opengrep_env.getenv_with_name_opt vars with
  | None -> ()
  | Some ((name : string), (_ : string)) ->
      let option =
        add_option_dashes options |> List.hd
      in
      Logs.warn (fun m -> m "%s is given; ignoring $%s" option name)

let negatable_flag_with_env ?(default = false) ?env ~neg_options ~doc options =
  let neg_doc =
    let options_str = add_option_dashes options |> String.concat "/" in
    Printf.sprintf "negates %s" options_str
  in
  let env_info = Option.map Cmd.Env.info env in
  let enable = (true, Arg.info options ~doc ?env:env_info) in
  let disable = (false, Arg.info neg_options ~doc:neg_doc) in
  let flags = Arg.(value (vflag_all [] [ enable; disable ])) in
  (* a bad value is reported by cmdliner like a bad option value, not as
     an exception *)
  let combine (values : bool list) : (bool, [ `Msg of string ]) result =
    match List.rev values with
    | last :: _ ->
        warn_env_ignored ~vars:(Option.to_list env) options;
        Ok last
    | [] -> (
        match env with
        | None -> Ok default
        | Some var -> (
            match Opengrep_env.getenv_opt var with
            | Some value -> parse_env_bool var value
            | None -> Ok default))
  in
  Term.cli_parse_result Term.(const combine $ flags)

(* A repeatable string option whose environment variable holds a
   whitespace-separated list (e.g. SEMGREP_RULES="p/default extra.yml").
   Cmdliner's own env handling would turn the variable into a single
   list element, so the variable is read here instead, through
   Opengrep_env so the OPENGREP_/SEMGREP_ alias also counts and an empty
   value means unset. Occurrences on the command line win over the
   environment and are never split. *)
let string_list_with_env ?(default = []) ~env ~doc options =
  let values = Arg.(value (opt_all string [] (Arg.info options ~doc))) in
  let combine (values : string list) =
    match values with
    | _ :: _ ->
        warn_env_ignored ~vars:[ env ] options;
        values
    | [] -> (
        match Opengrep_env.getenv_opt env with
        | Some value -> String_.split ~sep:"[ \t\r\n]+" value
        | None -> default)
  in
  Term.(const combine $ values)

(* A single-valued float option that can also be set by an environment
   variable, read through Opengrep_env so that the OPENGREP_* alias counts.
   The command line wins over the environment, and a variable that does not
   hold a number is reported the way a bad option value is. *)
let float_opt_with_env ~(env : string) ~(doc : string) (options : string list)
    =
  let value = Arg.(value (opt (some float) None (Arg.info options ~doc))) in
  let combine (value : float option) :
      (float option, [ `Msg of string ]) result =
    match value with
    | Some _ ->
        warn_env_ignored ~vars:[ env ] options;
        Ok value
    | None -> (
        match Opengrep_env.getenv_with_name_opt env with
        | None -> Ok None
        | Some ((name : string), (str : string)) -> (
            match float_of_string_opt str with
            | Some (f : float) -> Ok (Some f)
            | None -> Error (env_value_error ~var:name ~value:str "a number")))
  in
  Term.cli_parse_result Term.(const combine $ value)

(* A single-valued option whose value can also come from one of several
   environment variables (cmdliner supports only one per option). The
   first set variable wins; the command line wins over the environment. *)
let string_opt_with_envs ~envs ~doc options =
  let value = Arg.(value (opt (some string) None (Arg.info options ~doc))) in
  let combine (value : string option) =
    match value with
    | Some _ as v ->
        warn_env_ignored ~vars:envs options;
        v
    | None -> List.find_map Opengrep_env.getenv_opt envs
  in
  Term.(const combine $ value)

(* Parse command-line arguments representing a number of bytes, such as
 * '5 mb' or '3.2GiB'
 *
 * ported from bytesize.py
 *)

let units_conversion =
  [
    ("B", 1.);
    ("KIB", 2. ** 10.);
    ("MIB", 2. ** 20.);
    ("GIB", 2. ** 30.);
    ("TIB", 2. ** 40.);
    ("KB", 10. ** 3.);
    ("MB", 10. ** 6.);
    ("GB", 10. ** 9.);
    ("TB", 10. ** 12.);
  ]

let number_of_bytes_converter : int Cmdliner.Arg.conv =
  let parser s =
    let fail =
      Error (`Msg (spf "Invalid representation for a number of bytes: %s" s))
    in
    let s = String.uppercase_ascii s in
    if s =~ "^\\([^ BKMGT]*\\)[ ]*\\([BKMGT][A-Z]*\\)$" then
      let number, unit = Common.matched2 s in
      match
        (float_of_string_opt number, List.assoc_opt unit units_conversion)
      with
      | Some n, Some unit -> Ok (int_of_float (n *. unit))
      | _else_ -> fail
    else
      match int_of_string_opt s with
      | Some i -> Ok i
      | None -> fail
  in
  let printer ppf x = Format.pp_print_int ppf x in
  Arg.conv (parser, printer)
