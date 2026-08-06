(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse an 'opengrep server' command, load the rules once, then serve
   scan results and call graphs over HTTP until killed.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type caps = Session.caps

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let absolute_dir (path : string) : Fpath.t =
  let p = Fpath.v path in
  let p =
    if Fpath.is_abs p then p else Fpath.(v (Sys.getcwd ()) // p)
  in
  Fpath.normalize p |> Fpath.rem_empty_seg

(* Same pipeline as Session.fetch_rules, but synchronous and from a single
   --config string: fetch, partition, dedupe, drop pro-only products. *)
let load_rules (caps : < Cap.network ; Cap.tmp ; .. >) (config_str : string) :
    Rule.t list =
  let in_docker = !Semgrep_envvars.v.in_docker in
  let config = Rules_config.parse_config_string ~in_docker config_str in
  let rules_and_origins, fatal_errors =
    Rule_fetching.rules_from_dashdash_config ~rewrite_rule_ids:true
      (caps :> < Cap.network ; Cap.tmp >)
      config
  in
  if fatal_errors <> [] then
    Error.abort
      (Common.spf "invalid configuration string found: %s" config_str);
  let rules, invalid_rules =
    Rule_fetching.partition_rules_and_invalid rules_and_origins
  in
  if invalid_rules <> [] then
    Logs.warn (fun m ->
        m "skipped %d invalid rule(s)" (List.length invalid_rules));
  let rules =
    List_.deduplicate_gen
      ~get_key:(fun r -> Rule_ID.to_string (fst r.Rule.id))
      rules
  in
  let rule_filtering_conf =
    Rule_filtering.
      {
        exclude_rule_ids = [];
        severity = [];
        exclude_products = [ `SCA; `Secrets ];
      }
  in
  Rule_filtering.filter_rules rule_filtering_conf rules

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : < caps ; .. >) (conf : Server_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "Starting opengrep server");
  let rules = load_rules caps conf.config in
  Logs.app (fun m -> m "Loaded %d rules from %s" (List.length rules) conf.config);
  let roots = conf.roots |> List_.map absolute_dir in
  match
    Server_core.create ~caps:(caps :> Session.caps) ~rules ~roots
  with
  | Error e -> Error.abort e.Server_types.message
  | Ok core ->
      let graphs = Server_graphs.create () in
      Lwt_platform.set_engine ();
      Lwt_platform.run
        (let scan_ids = Server_core.start_scan_all core in
         Logs.app (fun m ->
             m "Started initial scan(s): %s" (String.concat ", " scan_ids));
         Http_server.serve ~host:conf.host ~port:conf.port ~core ~graphs ());
      Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Server_CLI.parse_argv argv in
  run_conf caps conf
