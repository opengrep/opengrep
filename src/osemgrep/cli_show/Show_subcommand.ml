open Common
module J = JSON
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There was no 'pysemgrep show' subcommand. Dumps were run via
 * 'semgrep scan --dump-ast ...' but it is better to have a separate
 * subcommand. Note that the legacy 'semgrep scan --dump-xxx' are
 * redirected to this file after having built a compatible Show_CLI.conf
 *
 * LATER: get rid of Core_CLI.dump_pattern and Core_CLI.dump_ast functions
 *
 * Note that we're using CapConsole.out() here, to print on stdout (Logs.app()
 * is printing on stderr, but for a show command it's probably better to
 * print on stdout).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* we need the network for the 'semgrep show identity/deployment' *)
type caps = < Cap.stdout ; Cap.network ; Cap.tmp >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* copy paste of Core_CLI.json_of_v *)
let json_of_v (v : OCaml.v) =
  let rec aux v =
    match v with
    | OCaml.VUnit -> J.String "()"
    | OCaml.VBool v1 -> if v1 then J.String "true" else J.String "false"
    | OCaml.VFloat v1 -> J.Float v1 (* ppf "%f" v1 *)
    | OCaml.VChar v1 -> J.String (spf "'%c'" v1)
    | OCaml.VString v1 -> J.String v1
    | OCaml.VInt i -> J.Int (Int64.to_int i)
    | OCaml.VTuple xs -> J.Array (List_.map aux xs)
    | OCaml.VDict xs -> J.Object (List_.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ :: _ :: _ -> J.Object [ (s, J.Array (List_.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%Ld" s i64)
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (List_.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

(* mostly a copy paste of Core_CLI.dump_v_to_format *)
let dump_any_to_format ~json ~html (any : AST_generic.any) =
  let (v : OCaml.v) = Meta_AST.vof_any any in
  match json, html with
  | true, false ->
      J.string_of_json (json_of_v v)
  | false, false ->
      OCaml.string_of_v v
  | _, true ->
      Show_html.generate_html v

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run_conf (caps : < caps ; .. >) (conf : Show_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "conf = %s" (Show_CLI.show_conf conf));
  let print = CapConsole.print caps#stdout in
  match conf.show_kind with
  | Version ->
      print Version.version;
      (* TODO? opportunity to perform version-check? *)
      Exit_code.ok ~__LOC__
  | SupportedLanguages ->
      print (spf "supported languages are: %s" Xlang.supported_xlangs);
      Exit_code.ok ~__LOC__ (* dumpers *)
  (* TODO? error management? improve error message for parse errors?
   * or let CLI.safe_run do the right thing?
   *)
  | DumpPattern (str, lang) -> (
      (* mostly a copy paste of Core_CLI.dump_pattern *)
      (* TODO: maybe enable the "semgrep.parsing" src here *)
      match Parse_pattern.parse_pattern lang str with
      | Ok any ->
          let s = dump_any_to_format ~json:conf.json ~html:conf.html any in
          print s;
          Exit_code.ok ~__LOC__
      | Error s ->
          Logs.app (fun m -> m "Parse error: %s" s);
          Exit_code.invalid_pattern ~__LOC__)
  | DumpCST (file, lang) ->
      Test_parsing.dump_tree_sitter_cst lang file;
      Exit_code.ok ~__LOC__
  | DumpAST (file, lang) -> (
      (* mostly a copy paste of Core_CLI.dump_ast *)
      let Parsing_result2.
            {
              ast;
              errors;
              tolerated_errors;
              skipped_tokens;
              inserted_tokens;
              stat = _;
            } =
        (* alt: call Parse_target.just_parse_with_lang()
         * but usually we also want the naming/typing info.
         * we could add a flag --naming, but simpler to just call
         * parse_and_resolve_name by default
         * LATER? could also have a --pro where we use the advanced
         * naming/typing of Deep_scan by analyzing the files around too?
         *)
        Parse_target.parse_and_resolve_name lang file
      in
      (* 80 columns is too little *)
      UFormat.set_margin 120;
      let s = dump_any_to_format ~json:conf.json ~html:conf.html (AST_generic.Pr ast) in
      print s;
      match (errors @ tolerated_errors, skipped_tokens @ inserted_tokens) with
      | [], [] -> Exit_code.ok ~__LOC__
      | _, _ ->
          Logs.err (fun m ->
              m "errors=%s\ntolerated errors=%s\nskipped=%s\ninserted=%s"
                (Parsing_result2.format_errors errors)
                (Parsing_result2.format_errors tolerated_errors)
                (skipped_tokens
                |> List_.map Tok.show_location
                |> String.concat ", ")
                (inserted_tokens
                |> List_.map Tok.show_location
                |> String.concat ", "));
          Exit_code.invalid_code ~__LOC__)
  | DumpIL (file, lang) ->
      let parse = Parse_target.parse_and_resolve_name lang file in
      let ast = parse.Parsing_result2.ast in
      let xs = AST_to_IL.stmt lang (AST_generic.stmt1 ast) in
      print "=== Toplevel ===";
      (match xs with
      | [] -> print "(none)"
      | _ -> xs |> List.iter (fun stmt -> print (IL.show_stmt stmt)));
        let report_func_def_with_name ent_opt fdef =
            let name =
            match ent_opt with
            | None -> "<lambda>"
            | Some { G.name = EN n; _ } -> G.show_name n
            | Some _ -> "<entity>"
            in
            print (spf "\n=== Function ===\nName: %s" name);
            let s =
            AST_generic.show_any
                (G.S (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody))
            in
            print s;
            print "==>";

            (* Creating a CFG and throwing it away here so the implicit return
            * analysis pass may be run in order to mark implicit return nodes.
            *)
            let _ = CFG_build.cfg_of_gfdef lang fdef in

            (* This round, the IL stmts will show return nodes when
            * they were implicit before.
            *)
            let IL.{ fbody = xs; _ } = AST_to_IL.function_definition lang fdef in
            let s = IL.show_any (IL.Ss xs) in
            print s
        in
        Visit_function_defs.visit report_func_def_with_name ast;
      Exit_code.ok ~__LOC__
  | DumpConfig config_str ->
      let in_docker = !Semgrep_envvars.v.in_docker in
      let config = Rules_config.parse_config_string ~in_docker config_str in
      let rules_and_errors, errors =
        Rule_fetching.rules_from_dashdash_config
          ~rewrite_rule_ids:true (* command-line default *)
          (caps :> < Cap.network ; Cap.tmp >)
          config
      in

      if errors <> [] then
        raise
          (Error.Semgrep_error
             ( Common.spf "invalid configuration string found: %s" config_str,
               Some (Exit_code.missing_config ~__LOC__) ));

      rules_and_errors
      |> List.iter (fun x -> print (Rule_fetching.show_rules_and_origin x));
      Exit_code.ok ~__LOC__
  | DumpRule file ->
      Core_actions.dump_rule file;
      Exit_code.ok ~__LOC__
  | DumpRuleV2 file ->
      (* TODO: use validation ocaml code to enforce the
       * CHECK: in rule_schema_v2.atd.
       * For example, check that at least one and only one field is set in formula.
       * Reclaim some of the jsonschema power. Maybe define combinators to express
       * that in rule_schema_v2_adapter.ml?
       *)
      let rules = Parse_rules_with_atd.parse_rules_v2 file in
      print (Rule_schema_v2_t.show_rules rules);
      Exit_code.ok ~__LOC__
  | DumpPatternsOfRule file ->
      Core_CLI.dump_patterns_of_rule file;
      Exit_code.ok ~__LOC__
  | DumpEnginePath _pro -> failwith "TODO: dump-engine-path not implemented yet"
  | DumpCommandForCore ->
      failwith "TODO: dump-command-for-core not implemented yet"
  | DumpIntrafileGraph (file, lang) ->
      let ast = Parse_target.parse_and_resolve_name_warn_if_partial lang file in
      let graph = Graph_from_AST.build_call_graph ~lang ast in
      Call_graph.Dot.output_graph stdout graph;
      Exit_code.ok ~__LOC__
  | DumpTaintSignatures (rule_file, target_file) ->
      let lang = Lang.lang_of_filename_exn target_file in
      let rules =
        match Parse_rule.parse rule_file with
        | Ok rules -> rules
        | Error e ->
            Error.abort
              (Common.spf "Failed to parse rule file %s: %s"
                 (Fpath.to_string rule_file)
                 (Rule_error.string_of_error e))
      in
      let has_disabled_intrafile (r: Rule.t) =
        match r.Rule.options with
        | Some {taint_intrafile = false; _} -> true
        | _ -> false
      in
      let applicable_rules, non_applicable_rules =
        rules
        |> List.partition (fun r ->
               match r.Rule.target_analyzer with
               | Xlang.L (x, xs) when not (has_disabled_intrafile r) ->
                 List.mem lang (x :: xs)
               | _ -> false)
      in
      let _search_rules, taint_rules, _extract_rules, _join_rules =
        Rule.partition_rules applicable_rules
      in
      let num_non_applicable = List.length non_applicable_rules in
      if num_non_applicable > 0 then
        print
          (spf
             "%d rule(s) were not applicable (check target_analyzer field \
              and taint_intrafile rule option)"
             num_non_applicable);
      (match taint_rules with
      | [] ->
          print "No applicable taint rules found";
          Exit_code.ok ~__LOC__
      | _ ->
        List.iter
          (fun rule ->
            let xconf = Match_env.default_xconfig in
            let xconf = { xconf with config = { xconf.config with taint_intrafile = true } } in
            let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.Rule.options in
            let tbl = Formula_cache.mk_specialized_formula_cache [] in
            let xlang = Xlang.L (lang, []) in
            let parser xlang file =
              let { Parsing_result2.ast; skipped_tokens; _ } =
                Parse_target.parse_and_resolve_name xlang file
              in
              (ast, skipped_tokens)
            in
            let xtarget =
              Xtarget.resolve parser (Target.mk_regular xlang Product.all (File target_file))
            in
            let _report, signature_db_opt =
              Match_tainting_mode.check_rule tbl rule Fun.id xconf xtarget
            in
            begin match signature_db_opt with
            | None ->
                print (spf "Could not obtain taint signatures for rule %s:\n"
                         (Rule_ID.to_string (fst rule.Rule.id)));
            | Some signature_db ->
                print (spf "Taint signatures for rule %s:\n"
                         (Rule_ID.to_string (fst rule.Rule.id)));
                print (Shape_and_sig.show_signature_database signature_db)
            end)
          taint_rules;
        Exit_code.ok ~__LOC__)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Show_CLI.parse_argv argv in
  run_conf caps conf
