let init () =
  (* Those refs are set because the engine is now mostly
   * language independent (so that we can generate small
   * JS files for the playground)
   *)
  (* TODO: Confirm that these references are not an issue with OCaml 5 and Domains,
   * since they are only set once and this is done before we do parallel work. *)
  Parse_target.just_parse_with_lang_ref := Parse_target2.just_parse_with_lang;
  Parse_pattern.parse_pattern_ref := Parse_pattern2.parse_pattern;
  Parse_jsonnet.jsonnet_parser_ref := Parse_jsonnet_tree_sitter.parse;
  ()
