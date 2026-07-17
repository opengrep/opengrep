(* Each case is a dir tests/interfile/<lang>/<case>/ with rule.yaml + annotated sources. *)

open Common
open Fpath_.Operators
module TCM = Test_compare_matches
module RP = Core_result

let interfile_tests_path = "tests/interfile"

let discover_test_cases (root : Fpath.t)
    : (string * string * Fpath.t) list =
  let lang_dirs =
    Sys.readdir !!root |> Array.to_list
    |> List.filter (fun (name : string) ->
           Sys.is_directory (Filename.concat !!root name)
           && not (String.equal name "." || String.equal name ".."))
    |> List_.sort
  in
  List.concat_map
    (fun (lang_dir : string) ->
      let lang_path = Filename.concat !!root lang_dir in
      Sys.readdir lang_path |> Array.to_list
      |> List.filter (fun (name : string) ->
             let case_path = Filename.concat lang_path name in
             Sys.is_directory case_path
             && Sys.file_exists (Filename.concat case_path "rule.yaml"))
      |> List_.sort
      |> List_.map (fun (case_dir : string) ->
             (lang_dir, case_dir, Fpath.v (Filename.concat lang_path case_dir))))
    lang_dirs

let run_test ?(taint_interfile = true) ?(taint_interfile_depth = 3)
    (caps : Core_scan.caps) (test_dir : Fpath.t) () : unit =
  let files = Testutil_files.read test_dir in
  Testutil_git.with_git_repo files (fun (raw_cwd : Fpath.t) ->
      (* realpath so graph- and Find_targets-resolved paths agree (macOS /var → /private/var otherwise breaks the graph lookup). *)
      let cwd = Fpath.v (Unix.realpath !!raw_cwd) in
      let rule_file = Fpath.(cwd / "rule.yaml") in

      let rules =
        match Parse_rule.parse_and_filter_invalid_rules rule_file with
        | Ok (rules, _invalid) -> rules
        | Error e ->
            failwith (spf "failed to parse %s: %s" !!rule_file
                        (Rule_error.show e))
      in
      let xlang = Test_engine.first_xlang_of_rules rules in

      let { Find_targets.selected = all_fpaths; _ } =
        Find_targets.get_target_fpaths Find_targets.default_conf
          [ Scanning_root.of_fpath cwd ]
      in
      (* Drop files misclassified into the rule's xlang (e.g. go.mod -> PL Go) that would crash the parser as bogus source. *)
      let lang_matches (fpath : Fpath.t) : bool =
        let lang_set = Xlang.to_langs xlang in
        let file_langs =
          try Lang.langs_of_filename fpath with _ -> []
        in
        List.exists (fun l -> List.exists (Lang.equal l) lang_set) file_langs
      in
      let targets =
        all_fpaths
        |> List.filter lang_matches
        |> List_.map (fun (fpath : Fpath.t) ->
          Target.mk_target ~project_root:cwd xlang fpath)
      in

      (* PL source files only, to avoid regex-scanning binary fixtures. *)
      let pl_files =
        List.filter
          (fun (f : Fpath.t) ->
            match File_type.file_type_of_file f with
            | File_type.PL _ -> true
            | _ -> false)
          all_fpaths
      in
      (* Every case must state its intent: without at least one
         annotation, a case that produces no findings passes even when
         the engine is broken. *)
      let annotation_regexp =
        Str.regexp ".*\\b\\(ruleid\\|ok\\|todook\\|todoruleid\\):"
      in
      let has_annotation (file : Fpath.t) =
        UFile.cat file
        |> List.exists (fun line ->
               Str.string_match annotation_regexp line 0)
      in
      if not (List.exists has_annotation pl_files) then
        failwith
          (spf
             "interfile case %s has no ruleid:/ok: annotations; add a \
              positive control or an explicit negative marker"
             !!test_dir);
      let regexp = ".*\\b\\(ruleid\\|todook\\):.*" in
      let expected =
        TCM.expected_error_lines_of_files ~regexp pl_files
      in

      let config =
        Core_scan_config.{
          default with
          rule_source = Rule_file rule_file;
          target_source = Targets targets;
          output_format = Text (* NoOutput *);
          taint_intrafile = true;
          taint_interfile;
          taint_interfile_depth;
          engine_config = Engine_config.default;
        }
      in

      let result =
        match Core_scan.scan caps config with
        | Ok r -> r
        | Error e -> Exception.reraise e
      in

      let actual =
        result.RP.processed_matches
        |> List_.map (fun (pm : RP.processed_match) ->
               let (file, line) = TCM.location_of_pm pm.RP.pm in
               let rel_file =
                 match Fpath.rem_prefix cwd file with
                 | Some rel -> rel
                 | None -> file
               in
               (rel_file, line))
      in
      (* Reset globals (even on failure) so cases stay isolated. *)
      Fun.protect
        ~finally:(fun () -> Globals.reset ())
        (fun () ->
          TCM.compare_actual_to_expected_for_alcotest
            ~to_location:Fun.id actual expected))

(* Cases needing non-default interfile config (fixtures run interfile on, depth 3). *)
let regression_tests (caps : Core_scan.caps) : Testo.t list =
  let root = Fpath.v interfile_tests_path in
  [
    (* Negative depth = unbounded; must still pull dispatch impls in. *)
    Testo.create "regression: negative depth still pulls dispatch impls"
      (run_test ~taint_interfile_depth:(-1) caps
         Fpath.(root / "go" / "dispatch_closure_pulls_impls"));
    (* Interfile OFF but project root set: intrafile cross-function must still resolve (tokens absolutified by project_root). *)
    Testo.create "regression: intrafile cross-function with project root"
      (run_test ~taint_interfile:false caps
         Fpath.(root / "python" / "intrafile_cross_function"));
  ]

let tests (caps : Core_scan.caps) : Testo.t list =
  let root = Fpath.v interfile_tests_path in
  let cases = discover_test_cases root in
  let by_lang : (string * (string * Fpath.t) list) list =
    cases
    |> List_.map (fun ((lang : string), (case_name : string), (path : Fpath.t)) ->
           (lang, (case_name, path)))
    |> Assoc.group_assoc_bykey_eff
  in
  Testo.categorize_suites "interfile taint"
    (Testo.categorize "regression" (regression_tests caps)
     :: List_.map
       (fun ((lang : string), (cases : (string * Fpath.t) list)) ->
         Testo.categorize lang
           (List_.map
              (fun ((case_name : string), (case_path : Fpath.t)) ->
                Testo.create case_name (run_test caps case_path))
              cases))
       by_lang)
