(* Nat Mote
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

module Out = Semgrep_output_v1_j

let ( let* ) = Option.bind
let key = Alcotest.testable Core_json_output.pp_key ( = )
let key_not_equal = Alcotest.testable Core_json_output.pp_key ( <> )

(* this makes an artificial Core_result.processed_match that we can feed into the Core_json_output.match_to_match 
   to see that the metadata is no longer 'json $VAR' but 'json php' *)
let make_core_result () : Core_result.processed_match =
  let file =
    Fpath.v
      "tests/rules/null-coallesce.php"
  in
  let startloc = Tok.{ str = ""; pos = Pos.make ~line:1 ~column:1 file 0 } in
  let endloc = Tok.{ str = ""; pos = Pos.make ~line:2 ~column:3 file 5 } in
  let pos = { Pos.bytepos = 2; line = 1; column = 2; file } in
  let pm =
    Core_match.
      {
        rule_id =
          {
            id = Rule_ID.of_string_exn "aa";
            message = "test $VAR";
            metadata = Some (JSON.String "json $VAR");
            fix = None;
            fix_regexp = None;
            langs = [];
            pattern_string = "";
            options = None;
          };
        engine_of_match = `OSS;
        env =
          [
            ( "$VAR",
              Metavariable.(
                Id (("test", Tok.OriginTok Tok.{ str = "php"; pos }), None)) );
          ];
        path = { origin = Origin.File file; internal_path_to_content = file };
        range_loc = (startloc, endloc);
        enclosure = None;
        ast_node = None;
        tokens = lazy [];
        taint_trace = None;
        sca_match = None;
        validation_state = `Confirmed_valid;
        severity_override = None;
        metadata_override = None;
        fix_text = None;
        facts = [];
      }
  in
  Core_result.{ pm; is_ignored = false; autofix_edit = None }

let make_core_match ?(check_id = "fake-rule-id") ?annotated_rule_id
    ?(src = "unchanged") () : Out.core_match =
  let annotated_rule_id = Option.value annotated_rule_id ~default:check_id in
  let metadata : JSON.t =
    JSON.(
      Object
        [
          ( "semgrep.dev",
            Object
              [
                ("src", String src);
                ("rule", Object [ ("rule_name", String annotated_rule_id) ]);
              ] );
        ])
  in
  let extra : Out.core_match_extra =
    Out.
      {
        message = None;
        metadata = Some (JSON.to_yojson metadata);
        severity = None;
        metavars = [];
        fix = None;
        engine_kind = `OSS;
        dataflow_trace = None;
        is_ignored = false;
        sca_match = None;
        validation_state = None;
        historical_info = None;
        extra_extra = None;
        enclosing_context = None;
      }
  in
  Out.
    {
      check_id = Rule_ID.of_string_exn check_id;
      path = Fpath.v "/fake/path/to/target";
      start = { line = 1; col = 1; offset = 1 };
      end_ = { line = 1; col = 2; offset = 2 };
      extra;
    }

let make_json_test () =
  Testo.create "json_test" (fun () ->
      let result = make_core_result () in
      let modified_result = Core_json_output.match_to_match ~inline:true result in
      let json =
        match modified_result with
        | Ok z -> z.extra.metadata
        | Error _ -> failwith "failed test"
      in
      let string_json =
        match json with
        | Some json -> Out.string_of_raw_json json
        | None -> ""
      in
      Alcotest.(
        check string "failed to modify the metadata" string_json "\"json php\""))

let make_test_case test_name key_testable msg match1 match2 =
  Testo.create test_name (fun () ->
      let key1 = Core_json_output.test_core_unique_key match1 in
      let key2 = Core_json_output.test_core_unique_key match2 in
      Alcotest.(check key_testable msg key1 key2))

let test_core_unique_key =
  Testo.categorize "test_core_unique_key"
  @@ [make_json_test ();
       (let match1 = make_core_match ~check_id:"rule1" () in
        let match2 = make_core_match ~check_id:"rule1" () in
        make_test_case "same-rule-matches" key "keys should match" match1 match2);
       (let match1 = make_core_match ~check_id:"rule1" () in
        let match2 = make_core_match ~check_id:"rule2" () in
        make_test_case "different-rule-matches" key_not_equal
          "keys should not match" match1 match2);
       (let match1 =
          make_core_match ~check_id:"orig-rule-name" ~src:"new-rule" ()
        in
        let match2 =
          make_core_match ~check_id:"mangled-rule-name" ~src:"previous-scan"
            ~annotated_rule_id:"orig-rule-name" ()
        in
        make_test_case "previous-scan match deduplication" key_not_equal
          "keys should not match" match1 match2);
     ]

let tests = Testo.categorize "Core_json_output" test_core_unique_key
