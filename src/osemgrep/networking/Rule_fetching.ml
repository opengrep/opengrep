open Common
open Fpath_.Operators
module E = Error
module Env = Semgrep_envvars
module FT = File_type
module C = Rules_config
module R = Rule
module XP = Xpattern

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Fetching rules from the local filesystem or from the network (registry).

   Note that you should not hadd some [@@profiling] on functions suffixed
   with _async because the profiling info will be useless; the body of
   the function is actually called from elsewhere.

   TODO:
    - lots of stuff ...

   osemgrep-only:
    - can pass -e without -l (try all possible languages)
   osemgrep-pro-only:
    - TODO use a registry cache to speedup things

   Partially translated from config_resolver.py
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* python: was called ConfigFile, and called a 'config' in text output.
 * TODO? maybe we don't need this intermediate type anymore; just return
 * a pair, which would remove the need for partition_rules_and_invalid.
 *)
type rules_and_origin = {
  rules : Rule.rule list;
  invalid_rules : Rule_error.invalid_rule list;
  origin : origin; (* used by Validate_subcommand *)
  (* the time spent parsing the rules (not fetching them), for --time *)
  parse_time : float;
}

(* TODO? more complex origin? Remote of Uri.t | Embedded of Fpath.t ?
 * or just put the Semgrep_dashdash_config.config_kind it comes from?
 * This type is used only for rewrite_rule_ids.
 *)
and origin =
  | CLI_argument
  | Local_file of Fpath.t
  | Registry
  | App
  | Untrusted_remote of Uri.t
  (* For rules cloned from a remote git repository passed as 'git+<url>'.
   * Like Untrusted_remote, these are third-party rules and do not get the
   * trust granted to registry/app rules. *)
  | Git_repo of Uri.t
[@@deriving show]

(*****************************************************************************)
(* Rewrite rule ids *)
(*****************************************************************************)

let prefix_for_fpath_opt (fpath : Fpath.t) : string option =
  assert (Fpath.is_file_path fpath);
  let* rel_path =
    if Fpath.is_rel fpath then Some fpath
      (* python: paths had no common prefix; not possible to relativize *)
    else Fpath.rem_prefix (Fpath.v (Sys.getcwd ())) fpath
  in
  (* LATER: we should use Fpath.normalize first, but pysemgrep
   * doesn't as shown by
     tests/default/e2e/test_check.py::test_basic_rule__relative
   * so we reproduce the same behavior, leading sometimes to some
   * weird rule id like "rules....rules.test" when passing
   * rules/../rules/test.yaml to --config.
   * TODO? pass legacy flag and improve the behavior when not legacy?
   *)
  match List.rev (Fpath.segs rel_path) with
  | [] -> raise Impossible
  | [ _file ] -> None
  | _file :: dirs ->
      let prefix =
        dirs |> List.rev |> List_.map (fun s -> s ^ ".") |> String.concat ""
      in
      Some prefix

let mk_rewrite_rule_ids (origin : origin) : Rule_ID.t -> Rule_ID.t =
 fun (rule_id : Rule_ID.t) ->
  (*
   Check the validity of the rule ID and prepend the path to rule file if
   the rewrite_rule_ids option is set.
*)
  let opt_prefix =
    match origin with
    | Local_file fpath -> prefix_for_fpath_opt fpath
    | _ -> None
  in
  match opt_prefix with
  | None -> rule_id
  | Some prefix ->
      Rule_ID.sanitize_string prefix ^ Rule_ID.to_string rule_id
      |> Rule_ID.of_string_exn

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let partition_rules_and_invalid (xs : rules_and_origin list) :
    Rule_error.rules_and_invalid =
  let (rules : Rule.rules) = xs |> List.concat_map (fun x -> x.rules) in
  let (invalid_rules : Rule_error.invalid_rule list) =
    xs |> List.concat_map (fun x -> x.invalid_rules)
  in
  (rules, invalid_rules)

(* When [skip_invalid_configs] is set (via --skip-invalid-configs), files that
 * fail to parse as a rule config (such as unrelated YAML files found in a
 * directory or a cloned git repo, e.g. GitHub workflows) are dropped with a
 * warning instead of aborting the whole scan. Only used for the discovery-
 * based sources (Dir, Git); explicitly-named configs still fail loudly. *)
let partition_or_skip ~skip_invalid_configs
    (results : (rules_and_origin, Rule_error.t) result list) :
    rules_and_origin list * Rule_error.t list =
  let oks, errs = Result_.partition Fun.id results in
  if skip_invalid_configs then (
    errs
    |> List.iter (fun (e : Rule_error.t) ->
           Logs.warn (fun m ->
               m "skipping invalid rule config %s: %s" !!(e.file)
                 (Rule_error.string_of_error e)));
    (oks, []))
  else (oks, errs)

let fetch_content_from_url_async caps (url : Uri.t) : string Lwt.t =
  (* TOPORT? _nice_semgrep_url() *)
  Logs.info (fun m -> m "trying to download from %s" (Uri.to_string url));
  let content =
    let headers = None
    in
    match%lwt Http_helpers.get ?headers caps#network url with
    | Ok { body = Ok body; _ } -> Lwt.return body
    | Ok { body = Error error; code; _ } ->
        Error.abort
          (spf "Failed to download config from %s, returned code %u: %s"
             (Uri.to_string url) code error)
    | Error e ->
        (* was raise Semgrep_error, but equivalent to abort now *)
        Error.abort
          (spf "Failed to download config from %s: %s" (Uri.to_string url) e)
  in
  Logs.info (fun m -> m "finished downloading from %s" (Uri.to_string url));
  content

let fetch_content_from_registry_url_async caps url =
  fetch_content_from_url_async caps url

let fetch_content_from_registry_url caps url =
  Lwt_platform.run (fetch_content_from_registry_url_async caps url)
[@@profiling]

(*****************************************************************************)
(* Registry and yaml aware jsonnet *)
(*****************************************************************************)

let parse_yaml_for_jsonnet (file : Fpath.t) : AST_jsonnet.program =
  Logs.info (fun m -> m "loading yaml file %s, converting to jsonnet" !!file);
  (* TODO? or use Yaml_to_generic.parse_yaml_file which seems
   * to be used to parse semgrep rules?
   *)
  let gen = Yaml_to_generic.program file in
  (* python: we were simply using a yaml parser and then
   * dumping it back as JSON and then parsing the JSON (which is
   * valid jsonnet). What we do here is a bit more complicated but
   * the advantage is that we get proper error location then!
   *)
  AST_generic_to_jsonnet.program gen

let mk_import_callback (caps : < Cap.network ; Cap.tmp ; .. >) base str =
  match str with
  | s when s =~ ".*\\.y[a]?ml$" ->
      (* On the fly conversion from yaml to jsonnet. We can do
       * 'local x = import "foo.yml";'!
       *)
      let final_path = Fpath.v base / str in
      Some (parse_yaml_for_jsonnet final_path)
  | s ->
      let url_opt =
        try
          let in_docker = !Semgrep_envvars.v.in_docker in
          let kind = Rules_config.parse_config_string ~in_docker s in
          match kind with
          | C.A _ -> failwith "TODO: app_config in jsonnet not handled"
          | C.R rkind ->
              let url = Semgrep_Registry.url_of_registry_config_kind rkind in
              Some url
          | C.URL url -> Some url
          (* TODO? allow to import any config_str? even a directory?
           * factorize with rules_from_dashdash_config?
           *)
          | C.Dir _
          | C.File _
          | C.Git _ ->
              None
        with
        | E.Semgrep_error _ -> None
      in
      url_opt
      |> Option.map (fun url ->
             (* similar to load_rules_from_url() but here we
              * must return some AST_jsonnet, not rules (yet).
              *
              * TODO: ask for JSON in headers which improves performance
              * because Yaml rule parsing is slower than Json rule parsing.
              *
              * TODO: fix token_opt parameter. Currently we don't pass it.
              * import_callback either needs an additional parameter, or
              * parse_rule should take an import_callback as a parameter.
              *)
             let contents =
               fetch_content_from_registry_url caps url
             in
             (* TODO: this assumes every URLs are for yaml, but maybe we could
              * also import URLs to jsonnet files or gist! or look at the
              * header mimetype when downloading the URL to decide how to
              * convert it further?
              *)
             CapTmp.with_temp_file caps#tmp ~contents ~suffix:".yaml"
               (fun file ->
                 (* LATER: adjust locations so refer to registry URL *)
                 parse_yaml_for_jsonnet file))
[@@profiling]

(* Performs modification to metadata for non-registry/app originating rules.
 * This is so that we can have trusted data in metadata subsequent to this
 * point and rely on the values in certain fields later.
 *
 * Currently this is important for:
 *  - Secrets
 *    * 'semgrep.dev'.rule.origin checked for validators
 *)
let modify_registry_provided_metadata (origin : origin) (rule : Rule.t) =
  match origin with
  | Registry
  | App ->
      rule
  | CLI_argument
  | Local_file _
  | Untrusted_remote _
  | Git_repo _ ->
      let replace obj key v =
        match (obj : JSON.t) with
        | Object members ->
            JSON.Object
              (List_.map
                 (function
                   | key', _ when key = key' -> (key, v)
                   | x -> x)
                 members)
        | x -> x
      in
      (* SECURITY: Set metadata from non-registry secrets rules so that
       * validators are not run. The default requirement is that the rule be
       * served from the pro origin. Without this, local rules could use
       * validators which may exfiltrate data from source code.
       *)
      let updated_metdata =
        let* metadata = rule.metadata in
        let* registry_data = JSON.member "semgrep.dev" metadata in
        let* rule_data = JSON.member "rule" registry_data in
        match rule.product with
        | `Secrets ->
            replace rule_data "origin" (JSON.String "local")
            |> replace registry_data "rule"
            |> replace metadata "semgrep.dev"
            |> fun x -> Some (Some x)
        | _ -> None
      in
      { rule with metadata = updated_metdata ||| rule.metadata }

(* similar to Parse_rule.parse_file but with special import callbacks
 * for a registry-aware jsonnet.
 *)
let parse_rule ~rewrite_rule_ids ~origin caps (file : Fpath.t) :
    (Rule_error.rules_and_invalid, Rule_error.t) Result.t =
  let rule_id_rewriter =
    if rewrite_rule_ids then Some (mk_rewrite_rule_ids origin) else None
  in
  let rules_and_invalid =
    match FT.file_type_of_file file with
    | FT.Config FT.Jsonnet ->
        Logs.warn (fun m ->
            m
              "Support for Jsonnet rules is experimental and currently meant \
               for internal use only. The syntax may change or be removed at \
               any point.");
        let ast = Parse_jsonnet.parse_program file in
        let core =
          Desugar_jsonnet.desugar_program
            ~import_callback:(mk_import_callback caps) file ast
        in
        let value_ = Eval_jsonnet.eval_program core in
        let gen = Manifest_jsonnet_to_AST_generic.manifest_value value_ in
        (* TODO: put to true at some point *)
        Parse_rule.parse_generic_ast ?rewrite_rule_ids:rule_id_rewriter
          ~error_recovery:false file gen
    | _ ->
        Parse_rule.parse_and_filter_invalid_rules
          ?rewrite_rule_ids:rule_id_rewriter file
  in
  match rules_and_invalid with
  | Ok (rules, invalid) ->
      Ok (List_.map (modify_registry_provided_metadata origin) rules, invalid)
  | Error err -> Error err

(*****************************************************************************)
(* Loading rules *)
(*****************************************************************************)

(* Note that we don't sanity check Parse_rule.is_valid_rule_filename(),
 * so if you explicitely pass a file that does not have the right
 * extension, we will still process it
 * (could be useful for .jsonnet, which is not recognized yet as a
 *  Parse_rule.is_valid_rule_filename, but we still need ojsonnet to
 *  be done).
 *)
let load_rules_from_file ~rewrite_rule_ids ~origin caps (file : Fpath.t) :
    (rules_and_origin, Rule_error.t) result =
  Logs.info (fun m -> m "loading local config from %s" !!file);
  if Sys.file_exists !!file then
    let rules_and_invalid, parse_time =
      Common.with_time (fun () -> parse_rule ~rewrite_rule_ids ~origin caps file)
    in
    match rules_and_invalid with
    | Ok (rules, invalid_rules) ->
        Logs.info (fun m -> m "Done loading local config from %s" !!file);
        Ok { rules; invalid_rules; origin = Local_file file; parse_time }
    | Error err -> Error err
  else
    (* This should never happen because Semgrep_dashdash_config only builds
     * a File case if the file actually exists.
     *)
    Error.abort (spf "file %s does not exist anymore" !!file)

let load_rules_from_url_async ~origin ?(ext = "yaml") caps url :
    (rules_and_origin, Rule_error.t) result Lwt.t =
  let%lwt contents = fetch_content_from_url_async caps url in
  let ext, contents =
    if ext = "policy" then
      (* project rule_config, from config_resolver.py in _make_config_request *)
      try
        match Yojson.Basic.from_string contents with
        | `Assoc e -> (
            match List.assoc "rule_config" e with
            | `String e -> ("json", e)
            | _else -> (ext, contents))
        | _else -> (ext, contents)
      with
      | _failure -> (ext, contents)
    else (ext, contents)
  in
  CapTmp.with_temp_file caps#tmp ~contents ~suffix:("." ^ ext) (fun file ->
      load_rules_from_file ~rewrite_rule_ids:false ~origin caps file)
  |> Lwt.return

let load_rules_from_url ~origin ?(ext = "yaml") caps url :
    (rules_and_origin, Rule_error.t) result =
  Lwt_platform.run (load_rules_from_url_async ~origin ~ext caps url)
[@@profiling]

(* TODO: merge caps and token_opt and caps_opt? *)
let rules_from_dashdash_config_async ?(skip_invalid_configs = false)
    ~rewrite_rule_ids caps kind :
    (* alt: (rules_and_origin list, Rule.Error.t list) result
       here and below:
       we could do this, but it lacks flexibility compared with this output type
       for instance, Validate_subcommand and Publish_subcommand pool together the
       invalid rule errors (located inside each `rules_and_origin`) and the fatal
       rule errors (the `Rule.Error.t`s), and decides to fail if either are present

       if we split each into a sum, we never have either at the same time. this
       breaks the code's behavior *
    *)
    (rules_and_origin list * Rule_error.t list) Lwt.t =
  match kind with
  | C.File path ->
      Lwt.return
        (Result_.partition
           (load_rules_from_file ~rewrite_rule_ids ~origin:(Local_file path)
              caps)
           [ path ])
  | C.Dir dir ->
      (* We used to skip dot files under [dir], but keeping rules/.semgrep.yml,
       * but not path/.github/foo.yml, but keeping src/.semgrep/bad_pattern.yml
       * but not ./.pre-commit-config.yaml, ... This was mainly because
       * we used to fetch rules from ~/.semgrep/ implicitely when --config
       * was not given, but this feature was removed, so now we can KISS.
       *)
      List_files.list dir
      |> List.filter Rule_file.is_valid_rule_filename
      |> List_.map (fun file ->
             load_rules_from_file ~rewrite_rule_ids ~origin:(Local_file file)
               caps file)
      |> partition_or_skip ~skip_invalid_configs |> Lwt.return
  | C.URL url ->
      (* TODO: Re-enable passing in our token to trusted remote urls.
         * This is currently disabled because we don't want to pass our token
         * to untrusted endpoints. There should be a relatively painless way
         * to do this, but this can be addressed in a follow-up PR.
      *)
      let%lwt rules =
        load_rules_from_url_async ~origin:(Untrusted_remote url) caps url
      in
      [ rules ] |> Result_.partition Fun.id |> Lwt.return
  | C.Git { url; ref_ } ->
      (* Clone the repo into a fresh temp dir, then load every rule file in it
       * like a Dir. The temp dir (including the .git it contains) is removed
       * as soon as the rules are parsed into memory, thanks to with_temp_dir.
       * Auth is entirely git's business (see Git_wrapper); we run
       * non-interactively and fail fast rather than prompt. *)
      CapTmp.with_temp_dir ~prefix:"opengrep-git-" caps#tmp (fun checkout_dir ->
          (match Git_wrapper.shallow_clone ?ref_ url checkout_dir with
          | Ok () -> ()
          | Error msg -> Error.abort msg);
          (* We stamp the origin as [Git_repo url] rather than [Local_file
           * <tmp>] so that rule-ids are not prefixed with the temp path and
           * the true (untrusted) origin is tracked. *)
          List_files.list checkout_dir
          |> List.filter Rule_file.is_valid_rule_filename
          |> List_.map (fun file ->
                 load_rules_from_file ~rewrite_rule_ids ~origin:(Git_repo url)
                   caps file
                 |> Result.map (fun r -> { r with origin = Git_repo url }))
          |> partition_or_skip ~skip_invalid_configs)
      |> Lwt.return
  | C.R rkind ->
      let url = Semgrep_Registry.url_of_registry_config_kind rkind in
      let%lwt contents =
        fetch_content_from_registry_url_async caps url
      in
      CapTmp.with_temp_file caps#tmp ~contents ~suffix:".yaml" (fun file ->
          [ load_rules_from_file ~rewrite_rule_ids ~origin:Registry caps file ])
      |> Result_.partition Fun.id |> Lwt.return
  | C.A Policy ->
    Error.abort
      (spf
         "Cannot to download rules from policy without authorization \
          token")
  | C.A SupplyChain ->
      failwith "TODO: SupplyChain not handled yet"

let rules_from_dashdash_config ?(skip_invalid_configs = false) ~rewrite_rule_ids
    caps kind : rules_and_origin list * Rule_error.t list =
  Lwt_platform.run
    (rules_from_dashdash_config_async ~skip_invalid_configs ~rewrite_rule_ids
       caps kind)
[@@profiling]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* The languages the pattern parses in; with -l, the pattern parse error
   is returned to be reported like the parse error of a rule file. *)
let langs_of_pattern (pat, xlang_opt) : (Xlang.t list, Rule_error.t) result =
  let xlang_compatible_with_pat xlang =
    let/ _xpat = Parse_rule.parse_fake_xpattern xlang pat in
    Ok xlang
  in
  match xlang_opt with
  | Some xlang ->
      let/ xlang = xlang_compatible_with_pat xlang in
      Ok [ xlang ]
  (* osemgrep-only: better: can use -e without -l! we try all languages *)
  | None ->
      (* We need uniq_by because Lang.assoc contain multiple times the
         * same value, for instance we have ("cpp", Cpp); ("c++", Cpp) in
         * Lang.assoc
         * TODO? use Xlang.assoc instead?
      *)
      let all_langs =
        Lang.assoc
        |> List_.map (fun (_k, l) -> l)
        |> List_.deduplicate
        (* TODO: we currently get a segfault with the Dart parser
           * (for example on a pattern like ': string (* filename *)'), so we
           * skip Dart for now (which anyway is not really supported).
        *)
        |> List_.exclude (fun x -> x =*= Lang.Dart)
      in
      Ok
        (all_langs
        |> List_.filter_map (fun l ->
               match
                 let xlang = Xlang.of_lang l |> xlang_compatible_with_pat in
                 Logs.debug (fun m ->
                     m "language %s valid for the pattern" (Lang.show l));
                 xlang
               with
               | Ok xlang -> Some xlang
               | Error _
               | (exception Failure _) ->
                   None))

let rules_and_origin_of_rule ~(parse_time : float) rule =
  { rules = [ rule ]; invalid_rules = []; origin = CLI_argument; parse_time }

(* python: mix of resolver_config.get_config() and get_rules() *)
let rules_from_rules_source_async ?(skip_invalid_configs = false)
    ~rewrite_rule_ids ~strict:_ caps (src : Rules_source.t) :
    (rules_and_origin list * Rule_error.t list) Lwt.t =
  let%lwt rules_and_origins, errors =
    match src with
    | Configs xs ->
        let%lwt pairs_list =
          xs
          |> Lwt_list.map_p (fun str ->
                 let in_docker = !Semgrep_envvars.v.in_docker in
                 let config = Rules_config.parse_config_string ~in_docker str in
                 rules_from_dashdash_config_async ~skip_invalid_configs
                   ~rewrite_rule_ids caps config)
        in
        let rules_and_origins_nested, errors_nested =
          Common2.unzip pairs_list
        in
        let rules_and_origins, errors =
          (List_.flatten rules_and_origins_nested, List_.flatten errors_nested)
        in

        (* NOTE: We should default to config auto if no config was passed in an earlier step,
            but if we reach this step without a config, we emit the error below.
        *)
        (* we would prefer to emit output based on the fatal errors here over complaining
           about not obtaining any configs, so we only emit this error if we didn't get
           any fatal errors (which will separately be processed)
        *)
        if rules_and_origins =*= [] && errors =*= [] then
          raise
            (Error.Semgrep_error
               ( "No config given. Run with `--config auto` or see \
                  https://semgrep.dev/docs/running-rules/ for instructions on \
                  running with a specific config",
                 Some (Exit_code.missing_config ~__LOC__) ));

        Lwt.return (rules_and_origins, errors)
    (* better: '-e foo -l regex' was not handled in pysemgrep
       *  (got a weird 'invalid pattern clause' error)
       * better: '-e foo -l generic' was not handled in semgrep-core
    *)
    | Pattern (pat, xlang_opt, fix) ->
        let valid_langs, invalid_rules_and_origins, errors =
          match langs_of_pattern (pat, xlang_opt) with
          | Ok valid_langs -> (valid_langs, [], [])
          (* reported like an invalid rule of a rule file *)
          | Error { kind = InvalidRule invalid_rule; _ } ->
              ( [],
                [
                  {
                    rules = [];
                    invalid_rules = [ invalid_rule ];
                    origin = CLI_argument;
                    parse_time = 0.;
                  };
                ],
                [] )
          | Error e -> ([], [], [ e ])
        in
        let rules_and_origins =
          invalid_rules_and_origins
          @ (valid_langs
          |> List_.map (fun xlang ->
                 let rule, parse_time =
                   Common.with_time (fun () ->
                       let xpat =
                         match Parse_rule.parse_fake_xpattern xlang pat with
                         | Ok xpat -> xpat
                         (* TODO: this shouldn't be any worse than the status
                            quo but this should be more robust *)
                         | Error e -> failwith (Rule_error.string_of_error e)
                       in
                       Rule.rule_of_xpattern ?fix xlang xpat)
                 in
                 rules_and_origin_of_rule ~parse_time rule))
        in
        (* In run_scan.py, in the pattern case, we would do this:
           if real_config_errors and strict:
               raise SemgrepError(
                   f"Ran with --strict and got {unit_str(len(config_errors), 'error')} while loading configs",
                   code=MISSING_CONFIG_EXIT_CODE,
               )
           except we now don't have errors.

           THINK: this may be impossible now in `osemgrep`?
        *)
        Lwt.return (rules_and_origins, errors)
  in

  Lwt.return (rules_and_origins, errors)

(* You should probably avoid using directly this function and prefer
 * to use the _async variant above mixed with a spinner as in
 * Scan_subcommand.rules_from_rules_source()
 *)
let rules_from_rules_source ?(skip_invalid_configs = false) ~rewrite_rule_ids
    ~strict caps (src : Rules_source.t) :
    rules_and_origin list * Rule_error.t list =
  Lwt_platform.run
    (rules_from_rules_source_async ~skip_invalid_configs ~rewrite_rule_ids
       ~strict caps src)
[@@profiling]
