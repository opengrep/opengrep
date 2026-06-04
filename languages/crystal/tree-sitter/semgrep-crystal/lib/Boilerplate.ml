(**
   Boilerplate to be used as a template when mapping the crystal CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_binary_double_star (env : env) (tok : CST.binary_double_star) =
  (* binary_double_star *) token env tok

let map_regular_if_keyword (env : env) (tok : CST.regular_if_keyword) =
  (* regular_if_keyword *) token env tok

let map_instance_var (env : env) (tok : CST.instance_var) =
  (* instance_var *) token env tok

let map_operator_token (env : env) (x : CST.operator_token) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `SLASH tok -> R.Case ("SLASH",
      (* "/" *) token env tok
    )
  | `SLASHSLASH tok -> R.Case ("SLASHSLASH",
      (* "//" *) token env tok
    )
  | `PERC tok -> R.Case ("PERC",
      (* "%" *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `STARSTAR tok -> R.Case ("STARSTAR",
      (* "**" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  | `LTEQGT tok -> R.Case ("LTEQGT",
      (* "<=>" *) token env tok
    )
  | `EQEQEQ tok -> R.Case ("EQEQEQ",
      (* "===" *) token env tok
    )
  | `LBRACKRBRACK tok -> R.Case ("LBRACKRBRACK",
      (* "[]" *) token env tok
    )
  | `LBRACKRBRACKQMARK tok -> R.Case ("LBRACKRBRACKQMARK",
      (* "[]?" *) token env tok
    )
  | `LBRACKRBRACKEQ tok -> R.Case ("LBRACKRBRACKEQ",
      (* "[]=" *) token env tok
    )
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `BANGTILDE tok -> R.Case ("BANGTILDE",
      (* "!~" *) token env tok
    )
  | `EQTILDE tok -> R.Case ("EQTILDE",
      (* "=~" *) token env tok
    )
  | `AMPPLUS tok -> R.Case ("AMPPLUS",
      (* "&+" *) token env tok
    )
  | `AMPDASH tok -> R.Case ("AMPDASH",
      (* "&-" *) token env tok
    )
  | `AMPSTAR tok -> R.Case ("AMPSTAR",
      (* "&*" *) token env tok
    )
  | `AMPSTARSTAR tok -> R.Case ("AMPSTARSTAR",
      (* "&**" *) token env tok
    )
  )

let map_unary_wrapping_plus (env : env) (tok : CST.unary_wrapping_plus) =
  (* unary_wrapping_plus *) token env tok

let map_modifier_unless_keyword (env : env) (tok : CST.modifier_unless_keyword) =
  (* modifier_unless_keyword *) token env tok

let map_regular_rescue_keyword (env : env) (tok : CST.regular_rescue_keyword) =
  (* regular_rescue_keyword *) token env tok

let map_block_ampersand (env : env) (tok : CST.block_ampersand) =
  (* block_ampersand *) token env tok

let map_char_escape_sequence (env : env) (tok : CST.char_escape_sequence) =
  (* char_escape_sequence *) token env tok

let map_imm_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 (env : env) (tok : CST.imm_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8) =
  (* imm_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 *) token env tok

let map_modifier_rescue_keyword (env : env) (tok : CST.modifier_rescue_keyword) =
  (* modifier_rescue_keyword *) token env tok

let map_modifier_if_keyword (env : env) (tok : CST.modifier_if_keyword) =
  (* modifier_if_keyword *) token env tok

let map_anon_opt_COMMA_opt_DOTDOTDOT_0392b4d (env : env) (opt : CST.anon_opt_COMMA_opt_DOTDOTDOT_0392b4d) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 = (* "," *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "..." *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

let map_pat_a84aa85 (env : env) (tok : CST.pat_a84aa85) =
  (* pattern \s* *) token env tok

let map_keyword_named_argument_name (env : env) (tok : CST.keyword_named_argument_name) =
  (* keyword_named_argument_name *) token env tok

let map_semgrep_ellipsis (env : env) (tok : CST.semgrep_ellipsis) =
  (* semgrep_ellipsis *) token env tok

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let map_implicit_object_method_operator (env : env) (tok : CST.implicit_object_method_operator) =
  (* implicit_object_method_operator *) token env tok

let map_unary_star (env : env) (tok : CST.unary_star) =
  (* unary_star *) token env tok

let map_class_var (env : env) (tok : CST.class_var) =
  (* class_var *) token env tok

let map_pseudo_constant (env : env) (x : CST.pseudo_constant) =
  (match x with
  | `X___LINE__ tok -> R.Case ("X___LINE__",
      (* "__LINE__" *) token env tok
    )
  | `X___END_LINE__ tok -> R.Case ("X___END_LINE__",
      (* "__END_LINE__" *) token env tok
    )
  | `X___FILE__ tok -> R.Case ("X___FILE__",
      (* "__FILE__" *) token env tok
    )
  | `X___DIR__ tok -> R.Case ("X___DIR__",
      (* "__DIR__" *) token env tok
    )
  )

let map_start_of_named_tuple_type (env : env) (tok : CST.start_of_named_tuple_type) =
  (* start_of_named_tuple_type *) token env tok

let map_modifier_ensure_keyword (env : env) (tok : CST.modifier_ensure_keyword) =
  (* modifier_ensure_keyword *) token env tok

let map_imm_tok_pat_245b2d0 (env : env) (tok : CST.imm_tok_pat_245b2d0) =
  (* pattern \\. *) token env tok

let map_symbol_array_percent_literal_start (env : env) (tok : CST.symbol_array_percent_literal_start) =
  (* symbol_array_percent_literal_start *) token env tok

let map_macro_delimiter_end (env : env) (tok : CST.macro_delimiter_end) =
  (* macro_delimiter_end *) token env tok

let map_end_of_with_expression (env : env) (tok : CST.end_of_with_expression) =
  (* end_of_with_expression *) token env tok

let map_heredoc_body_start (env : env) (tok : CST.heredoc_body_start) =
  (* heredoc_body_start *) token env tok

let map_implicit_object_method_identifier (env : env) (tok : CST.implicit_object_method_identifier) =
  (* implicit_object_method_identifier *) token env tok

let map_start_of_named_tuple (env : env) (tok : CST.start_of_named_tuple) =
  (* start_of_named_tuple *) token env tok

let map_anon_choice_DOTDOT_ed078ec (env : env) (x : CST.anon_choice_DOTDOT_ed078ec) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

let map_regular_ensure_keyword (env : env) (tok : CST.regular_ensure_keyword) =
  (* regular_ensure_keyword *) token env tok

let map_binary_wrapping_minus (env : env) (tok : CST.binary_wrapping_minus) =
  (* binary_wrapping_minus *) token env tok

let map_implicit_object_ivar (env : env) (tok : CST.implicit_object_ivar) =
  (* implicit_object_ivar *) token env tok

let map_imm_tok_pat_dcdac4f (env : env) (tok : CST.imm_tok_pat_dcdac4f) =
  (* pattern \s *) token env tok

let map_type_field_colon (env : env) (tok : CST.type_field_colon) =
  (* type_field_colon *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_binary_ampersand (env : env) (tok : CST.binary_ampersand) =
  (* binary_ampersand *) token env tok

let map_binary_wrapping_plus (env : env) (tok : CST.binary_wrapping_plus) =
  (* binary_wrapping_plus *) token env tok

let map_binary_slash (env : env) (tok : CST.binary_slash) =
  (* binary_slash *) token env tok

let map_macro_delimiter_else (env : env) (tok : CST.macro_delimiter_else) =
  (* macro_delimiter_else *) token env tok

let map_pat_5eb9c21 (env : env) (tok : CST.pat_5eb9c21) =
  (* pattern :\s *) token env tok

let map_constant_segment (env : env) (tok : CST.constant_segment) =
  (* constant_segment *) token env tok

let map_imm_tok_squot (env : env) (tok : CST.imm_tok_squot) =
  (* "'" *) token env tok

let map_regular_unless_keyword (env : env) (tok : CST.regular_unless_keyword) =
  (* regular_unless_keyword *) token env tok

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  (* heredoc_start *) token env tok

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_tok_perc_pat_fdcd585_rep_pat_fdcd585 (env : env) (tok : CST.tok_perc_pat_fdcd585_rep_pat_fdcd585) =
  (* tok_perc_pat_fdcd585_rep_pat_fdcd585 *) token env tok

let map_line_continuation_explicit (env : env) (() : CST.line_continuation_explicit) =
  R.Tuple []

let map_macro_verbatim_keyword (env : env) ((v1, v2, v3, v4) : CST.macro_verbatim_keyword) =
  let v1 = (* "{%" *) token env v1 in
  let v2 = (* "verbatim" *) token env v2 in
  let v3 = (* "do" *) token env v3 in
  let v4 = (* "%}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_modulo_operator (env : env) (tok : CST.modulo_operator) =
  (* modulo_operator *) token env tok

let map_anon_choice_prop_94a3794 (env : env) (x : CST.anon_choice_prop_94a3794) =
  (match x with
  | `Prop_1a8db4c tok -> R.Case ("Prop_1a8db4c",
      (* "property" *) token env tok
    )
  | `Prop_b128b0f tok -> R.Case ("Prop_b128b0f",
      (* "property?" *) token env tok
    )
  )

let map_special_variable (env : env) (tok : CST.special_variable) =
  (* special_variable *) token env tok

let map_imm_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 (env : env) (tok : CST.imm_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32) =
  (* imm_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 *) token env tok

let map_global_match_data_index (env : env) (tok : CST.global_match_data_index) =
  (* global_match_data_index *) token env tok

let map_string_array_percent_literal_start (env : env) (tok : CST.string_array_percent_literal_start) =
  (* string_array_percent_literal_start *) token env tok

let map_regex_modifier (env : env) (tok : CST.regex_modifier) =
  (* regex_modifier *) token env tok

let map_start_of_symbol (env : env) (tok : CST.start_of_symbol) =
  (* start_of_symbol *) token env tok

let map_binary_minus (env : env) (tok : CST.binary_minus) =
  (* binary_minus *) token env tok

let map_command_literal_end (env : env) (tok : CST.command_literal_end) =
  (* command_literal_end *) token env tok

let map_binary_double_slash (env : env) (tok : CST.binary_double_slash) =
  (* binary_double_slash *) token env tok

let map_imm_tok_lcurl (env : env) (tok : CST.imm_tok_lcurl) =
  (* "{" *) token env tok

let map_heredoc_content (env : env) (tok : CST.heredoc_content) =
  (* heredoc_content *) token env tok

let map_pat_6ebc02d (env : env) (tok : CST.pat_6ebc02d) =
  (* pattern abstract\sstruct *) token env tok

let map_identifier_method_call (env : env) (tok : CST.identifier_method_call) =
  (* identifier_method_call *) token env tok

let map_string_percent_literal_start (env : env) (tok : CST.string_percent_literal_start) =
  (* string_percent_literal_start *) token env tok

let map_identifier_assign (env : env) (tok : CST.identifier_assign) =
  (* identifier_assign *) token env tok

let map_unary_wrapping_minus (env : env) (tok : CST.unary_wrapping_minus) =
  (* unary_wrapping_minus *) token env tok

let map_unary_double_star (env : env) (tok : CST.unary_double_star) =
  (* unary_double_star *) token env tok

let map_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 (env : env) (tok : CST.tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32) =
  (* tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 *) token env tok

let map_start_of_parenless_args (env : env) (tok : CST.start_of_parenless_args) =
  (* start_of_parenless_args *) token env tok

let map_start_of_tuple_type (env : env) (tok : CST.start_of_tuple_type) =
  (* start_of_tuple_type *) token env tok

let map_pointer_star (env : env) (tok : CST.pointer_star) =
  (* pointer_star *) token env tok

let map_start_of_hash_or_tuple (env : env) (tok : CST.start_of_hash_or_tuple) =
  (* start_of_hash_or_tuple *) token env tok

let map_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 (env : env) (tok : CST.tok_choice_0b_rep_pat_78bc49d_choice_choice_u8) =
  (* tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 *) token env tok

let map_tok_prec_p1_hashlcurl (env : env) (tok : CST.tok_prec_p1_hashlcurl) =
  (* tok_prec_p1_hashlcurl *) token env tok

let map_delimited_array_element_start (env : env) (tok : CST.delimited_array_element_start) =
  (* delimited_array_element_start *) token env tok

let map_ignored_backslash (env : env) (tok : CST.ignored_backslash) =
  (* ignored_backslash *) token env tok

let map_line_continuation_explicit_ (env : env) (() : CST.line_continuation_explicit_) =
  R.Tuple []

let map_percent_array_escape_sequence (env : env) (tok : CST.percent_array_escape_sequence) =
  (* pattern \\[\s})\]>|] *) token env tok

let map_start_of_index_operator (env : env) (tok : CST.start_of_index_operator) =
  (* start_of_index_operator *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_line_continuation (env : env) (tok : CST.line_continuation) =
  (* line_continuation *) token env tok

let map_command_percent_literal_start (env : env) (tok : CST.command_percent_literal_start) =
  (* command_percent_literal_start *) token env tok

let map_imm_tok_qmark (env : env) (tok : CST.imm_tok_qmark) =
  (* "?" *) token env tok

let map_pat_b22a502 (env : env) (tok : CST.pat_b22a502) =
  (* pattern abstract\sclass *) token env tok

let map_imm_tok_slash (env : env) (tok : CST.imm_tok_slash) =
  (* "/" *) token env tok

let map_unquoted_symbol_content (env : env) (tok : CST.unquoted_symbol_content) =
  (* unquoted_symbol_content *) token env tok

let map_regex_start (env : env) (tok : CST.regex_start) =
  (* regex_start *) token env tok

let map_start_of_macro_var_exps (env : env) (tok : CST.start_of_macro_var_exps) =
  (* start_of_macro_var_exps *) token env tok

let map_string_escape_sequence (env : env) (tok : CST.string_escape_sequence) =
  (* string_escape_sequence *) token env tok

let map_imm_tok_prec_p1_pat_ab3dea2 (env : env) (tok : CST.imm_tok_prec_p1_pat_ab3dea2) =
  (* pattern [^\\/] *) token env tok

let map_binary_star (env : env) (tok : CST.binary_star) =
  (* binary_star *) token env tok

let map_string_literal_end (env : env) (tok : CST.string_literal_end) =
  (* string_literal_end *) token env tok

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_imm_tok_prec_p1_pat_f2207b3 (env : env) (tok : CST.imm_tok_prec_p1_pat_f2207b3) =
  (* pattern "[^\\\\\"]+" *) token env tok

let map_end_of_range (env : env) (tok : CST.end_of_range) =
  (* end_of_range *) token env tok

let map_macro_begin_keyword (env : env) ((v1, v2, v3) : CST.macro_begin_keyword) =
  let v1 = (* "{%" *) token env v1 in
  let v2 = (* "begin" *) token env v2 in
  let v3 = (* "%}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_unary_minus (env : env) (tok : CST.unary_minus) =
  (* unary_minus *) token env tok

let map_unary_plus (env : env) (tok : CST.unary_plus) =
  (* unary_plus *) token env tok

let map_imm_tok_prec_p1_pat_e77da7f (env : env) (tok : CST.imm_tok_prec_p1_pat_e77da7f) =
  (* pattern [^\\] *) token env tok

let map_macro_start (env : env) (tok : CST.macro_start) =
  (* macro_start *) token env tok

let map_command_literal_start (env : env) (tok : CST.command_literal_start) =
  (* command_literal_start *) token env tok

let map_delimited_array_element_end (env : env) (tok : CST.delimited_array_element_end) =
  (* delimited_array_element_end *) token env tok

let map_imm_tok_choice_plus (env : env) (tok : CST.imm_tok_choice_plus) =
  (* imm_tok_choice_plus *) token env tok

let map_macro_content_nesting (env : env) (tok : CST.macro_content_nesting) =
  (* macro_content_nesting *) token env tok

let map_macro_delimiter_elsif (env : env) (tok : CST.macro_delimiter_elsif) =
  (* macro_delimiter_elsif *) token env tok

let map_string_literal_start (env : env) (tok : CST.string_literal_start) =
  (* string_literal_start *) token env tok

let map_macro_content (env : env) (tok : CST.macro_content) =
  (* macro_content *) token env tok

let map_percent_literal_end (env : env) (tok : CST.percent_literal_end) =
  (* percent_literal_end *) token env tok

let map_start_of_brace_block (env : env) (tok : CST.start_of_brace_block) =
  (* start_of_brace_block *) token env tok

let map_delimited_string_contents (env : env) (tok : CST.delimited_string_contents) =
  (* delimited_string_contents *) token env tok

let map_imm_tok_dquot (env : env) (tok : CST.imm_tok_dquot) =
  (* "\"" *) token env tok

let map_line_break (env : env) (tok : CST.line_break) =
  (* line_break *) token env tok

let map_regex_percent_literal_start (env : env) (tok : CST.regex_percent_literal_start) =
  (* regex_percent_literal_start *) token env tok

let map_empty_parens (env : env) ((v1, v2) : CST.empty_parens) =
  let v1 = (* "(" *) token env v1 in
  let v2 = (* ")" *) token env v2 in
  R.Tuple [v1; v2]

let map_line_continuation_explicit2 (env : env) (() : CST.line_continuation_explicit2) =
  R.Tuple []

let map_anon_choice_RBRACK_57c1d11 (env : env) (x : CST.anon_choice_RBRACK_57c1d11) =
  (match x with
  | `RBRACK tok -> R.Case ("RBRACK",
      (* "]" *) token env tok
    )
  | `RBRACKQMARK tok -> R.Case ("RBRACKQMARK",
      (* "]?" *) token env tok
    )
  )

let map_binary_plus (env : env) (tok : CST.binary_plus) =
  (* binary_plus *) token env tok

let map_macro_end_keyword (env : env) ((v1, v2, v3) : CST.macro_end_keyword) =
  let v1 = (* macro_delimiter_end *) token env v1 in
  let v2 = (* "end" *) token env v2 in
  let v3 = (* "%}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_impl_obj_meth_id_3a9d445 (env : env) (x : CST.anon_choice_impl_obj_meth_id_3a9d445) =
  (match x with
  | `Impl_obj_meth_id tok -> R.Case ("Impl_obj_meth_id",
      (* implicit_object_method_identifier *) token env tok
    )
  | `Impl_obj_meth_op tok -> R.Case ("Impl_obj_meth_op",
      (* implicit_object_method_operator *) token env tok
    )
  )

let map_macro_else_keyword (env : env) ((v1, v2, v3) : CST.macro_else_keyword) =
  let v1 = (* macro_delimiter_else *) token env v1 in
  let v2 = (* "else" *) token env v2 in
  let v3 = (* "%}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_forall (env : env) ((v1, v2, v3) : CST.forall) =
  let v1 = (* "forall" *) token env v1 in
  let v2 = (* constant_segment *) token env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* constant_segment *) token env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_block_body_param (env : env) (x : CST.block_body_param) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Unde tok -> R.Case ("Unde",
      (* "_" *) token env tok
    )
  )

let map_anon_choice_unde_76e38d1 (env : env) (x : CST.anon_choice_unde_76e38d1) =
  (match x with
  | `Unde tok -> R.Case ("Unde",
      (* "_" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_anon_choice_id_684e964 (env : env) (x : CST.anon_choice_id_684e964) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_block_body_splat_param (env : env) ((v1, v2) : CST.block_body_splat_param) =
  let v1 = (* "*" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* semgrep_metavariable *) token env tok
      )
    | `Unde tok -> R.Case ("Unde",
        (* "_" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_anon_choice_id_0dd119e (env : env) (x : CST.anon_choice_id_0dd119e) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Id_meth_call tok -> R.Case ("Id_meth_call",
      (* identifier_method_call *) token env tok
    )
  | `Else tok -> R.Case ("Else",
      (* "else" *) token env tok
    )
  | `Then tok -> R.Case ("Then",
      (* "then" *) token env tok
    )
  | `Ensure tok -> R.Case ("Ensure",
      (* "ensure" *) token env tok
    )
  | `Rescue tok -> R.Case ("Rescue",
      (* "rescue" *) token env tok
    )
  )

let map_anon_choice_id_910ec16 (env : env) (x : CST.anon_choice_id_910ec16) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Id_meth_call tok -> R.Case ("Id_meth_call",
      (* identifier_method_call *) token env tok
    )
  | `Id_assign tok -> R.Case ("Id_assign",
      (* identifier_assign *) token env tok
    )
  )

let map_unquoted_symbol (env : env) ((v1, v2) : CST.unquoted_symbol) =
  let v1 = (* start_of_symbol *) token env v1 in
  let v2 = (* unquoted_symbol_content *) token env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_un_minus_07c71f0 (env : env) (x : CST.anon_choice_un_minus_07c71f0) =
  (match x with
  | `Un_minus tok -> R.Case ("Un_minus",
      (* unary_minus *) token env tok
    )
  | `Un_plus tok -> R.Case ("Un_plus",
      (* unary_plus *) token env tok
    )
  )

let map_char (env : env) ((v1, v2, v3) : CST.char) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    (match v2 with
    | `Imm_tok_prec_p1_pat_e77da7f x -> R.Case ("Imm_tok_prec_p1_pat_e77da7f",
        map_imm_tok_prec_p1_pat_e77da7f env x
      )
    | `Char_esc_seq tok -> R.Case ("Char_esc_seq",
        (* char_escape_sequence *) token env tok
      )
    )
  in
  let v3 = map_imm_tok_squot env v3 in
  R.Tuple [v1; v2; v3]

let map_operator_symbol (env : env) ((v1, v2) : CST.operator_symbol) =
  let v1 = (* start_of_symbol *) token env v1 in
  let v2 = map_imm_tok_choice_plus env v2 in
  R.Tuple [v1; v2]

let map_percent_literal_array_word (env : env) ((v1, v2, v3) : CST.percent_literal_array_word) =
  let v1 = (* delimited_array_element_start *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Deli_str_content tok -> R.Case ("Deli_str_content",
          (* delimited_string_contents *) token env tok
        )
      | `Perc_array_esc_seq tok -> R.Case ("Perc_array_esc_seq",
          (* pattern \\[\s})\]>|] *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* delimited_array_element_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quoted_symbol (env : env) ((v1, v2, v3) : CST.quoted_symbol) =
  let v1 = (* ":\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Imm_tok_prec_p1_pat_f2207b3 x -> R.Case ("Imm_tok_prec_p1_pat_f2207b3",
          map_imm_tok_prec_p1_pat_f2207b3 env x
        )
      | `Str_esc_seq tok -> R.Case ("Str_esc_seq",
          (* string_escape_sequence *) token env tok
        )
      | `Igno_back tok -> R.Case ("Igno_back",
          (* ignored_backslash *) token env tok
        )
      )
    ) v2)
  in
  let v3 = map_imm_tok_dquot env v3 in
  R.Tuple [v1; v2; v3]

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `Line_brk tok -> R.Case ("Line_brk",
      (* line_break *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_regex_literal_content_ (env : env) (xs : CST.regex_literal_content_) =
  R.List (List.map (fun x ->
    (match x with
    | `Imm_tok_prec_p1_pat_ab3dea2 x -> R.Case ("Imm_tok_prec_p1_pat_ab3dea2",
        map_imm_tok_prec_p1_pat_ab3dea2 env x
      )
    | `Imm_tok_pat_245b2d0 x -> R.Case ("Imm_tok_pat_245b2d0",
        map_imm_tok_pat_245b2d0 env x
      )
    | `Line_cont_expl2 x -> R.Case ("Line_cont_expl2",
        map_line_continuation_explicit2 env x
      )
    )
  ) xs)

let rec map_anon_choice_blk_body_param_95e3ea5 (env : env) (x : CST.anon_choice_blk_body_param_95e3ea5) =
  (match x with
  | `Blk_body_param x -> R.Case ("Blk_body_param",
      map_block_body_param env x
    )
  | `Blk_body_splat_param x -> R.Case ("Blk_body_splat_param",
      map_block_body_splat_param env x
    )
  | `Blk_body_nested_param x -> R.Case ("Blk_body_nested_param",
      map_block_body_nested_param env x
    )
  )

and map_block_body_nested_param (env : env) ((v1, v2, v3, v4, v5) : CST.block_body_nested_param) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_anon_choice_blk_body_param_95e3ea5 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_blk_body_param_95e3ea5 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_global_method (env : env) ((v1, v2) : CST.global_method) =
  let v1 = (* "::" *) token env v1 in
  let v2 = map_anon_choice_id_910ec16 env v2 in
  R.Tuple [v1; v2]

let map_float_ (env : env) (x : CST.float_) =
  (match x with
  | `Choice_un_minus_imm_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 (v1, v2) -> R.Case ("Choice_un_minus_imm_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32",
      let v1 = map_anon_choice_un_minus_07c71f0 env v1 in
      let v2 =
        map_imm_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 env v2
      in
      R.Tuple [v1; v2]
    )
  | `Tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 x -> R.Case ("Tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32",
      map_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32 env x
    )
  )

let map_integer (env : env) (x : CST.integer) =
  (match x with
  | `Choice_un_minus_imm_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 (v1, v2) -> R.Case ("Choice_un_minus_imm_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8",
      let v1 = map_anon_choice_un_minus_07c71f0 env v1 in
      let v2 =
        map_imm_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 env v2
      in
      R.Tuple [v1; v2]
    )
  | `Tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 x -> R.Case ("Tok_choice_0b_rep_pat_78bc49d_choice_choice_u8",
      map_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 env x
    )
  )

let map_string_array_percent_literal (env : env) ((v1, v2, v3) : CST.string_array_percent_literal) =
  let v1 =
    (* string_array_percent_literal_start *) token env v1
  in
  let v2 =
    R.List (List.map (map_percent_literal_array_word env) v2)
  in
  let v3 = (* percent_literal_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_symbol_array_percent_literal (env : env) ((v1, v2, v3) : CST.symbol_array_percent_literal) =
  let v1 =
    (* symbol_array_percent_literal_start *) token env v1
  in
  let v2 =
    R.List (List.map (map_percent_literal_array_word env) v2)
  in
  let v3 = (* percent_literal_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_quoted_symb_22fc626 (env : env) (x : CST.anon_choice_quoted_symb_22fc626) =
  (match x with
  | `Quoted_symb x -> R.Case ("Quoted_symb",
      map_quoted_symbol env x
    )
  | `Unqu_symb x -> R.Case ("Unqu_symb",
      map_unquoted_symbol env x
    )
  | `Op_symb x -> R.Case ("Op_symb",
      map_operator_symbol env x
    )
  )

let map_anon_choice_then_eac33c8 (env : env) (x : CST.anon_choice_then_eac33c8) =
  (match x with
  | `Then tok -> R.Case ("Then",
      (* "then" *) token env tok
    )
  | `Term x -> R.Case ("Term",
      map_terminator env x
    )
  )

let map_anon_choice_blk_body_param_c57f2a9 (env : env) (x : CST.anon_choice_blk_body_param_c57f2a9) =
  (match x with
  | `Blk_body_param x -> R.Case ("Blk_body_param",
      map_block_body_param env x
    )
  | `Blk_body_splat_param x -> R.Case ("Blk_body_splat_param",
      map_block_body_splat_param env x
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  | `Blk_body_nested_param x -> R.Case ("Blk_body_nested_param",
      map_block_body_nested_param env x
    )
  )

let map_pseudo_responds_to_argument_list (env : env) (x : CST.pseudo_responds_to_argument_list) =
  (match x with
  | `LPAR_choice_quoted_symb_RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_quoted_symb_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_anon_choice_quoted_symb_22fc626 env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_quoted_symb x -> R.Case ("Choice_quoted_symb",
      map_anon_choice_quoted_symb_22fc626 env x
    )
  )

let map_block_param_list (env : env) ((v1, v2, v3) : CST.block_param_list) =
  let v1 = map_anon_choice_blk_body_param_c57f2a9 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_blk_body_param_c57f2a9 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let rec map_abstract_method_def (env : env) ((v1, v2, v3) : CST.abstract_method_def) =
  let v1 = (* "abstract" *) token env v1 in
  let v2 = map_base_method_def env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_additive_operator (env : env) ((v1, v2, v3) : CST.additive_operator) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `Bin_plus tok -> R.Case ("Bin_plus",
        (* binary_plus *) token env tok
      )
    | `Bin_minus tok -> R.Case ("Bin_minus",
        (* binary_minus *) token env tok
      )
    | `Bin_wrap_plus tok -> R.Case ("Bin_wrap_plus",
        (* binary_wrapping_plus *) token env tok
      )
    | `Bin_wrap_minus tok -> R.Case ("Bin_wrap_minus",
        (* binary_wrapping_minus *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_alias (env : env) ((v1, v2, v3, v4) : CST.alias) =
  let v1 = (* "alias" *) token env v1 in
  let v2 = map_constant env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_bare_type env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_alignof (env : env) ((v1, v2, v3, v4) : CST.alignof) =
  let v1 = (* "alignof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_bare_type env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_and_ (env : env) ((v1, v2, v3) : CST.and_) =
  let v1 = map_expression env v1 in
  let v2 = (* "&&" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_annotation (env : env) ((v1, v2, v3, v4) : CST.annotation) =
  let v1 = (* "@[" *) token env v1 in
  let v2 = map_constant env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_annotation_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_annotation_argument_list (env : env) ((v1, v2, v3) : CST.annotation_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_bracket_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_anon_choice_arg_list_with_parens_and_blk_422157f (env : env) (x : CST.anon_choice_arg_list_with_parens_and_blk_422157f) =
  (match x with
  | `Arg_list_with_parens_and_blk x -> R.Case ("Arg_list_with_parens_and_blk",
      map_argument_list_with_parens_and_block env x
    )
  | `Arg_list_no_parens_with_blk x -> R.Case ("Arg_list_no_parens_with_blk",
      map_argument_list_no_parens_with_block env x
    )
  )

and map_anon_choice_bare_type_7b27a0b (env : env) (x : CST.anon_choice_bare_type_7b27a0b) =
  (match x with
  | `Bare_type x -> R.Case ("Bare_type",
      map_bare_type env x
    )
  | `Nume_type x -> R.Case ("Nume_type",
      map_numeric_type env x
    )
  )

and map_anon_choice_choice_unde_01ec478 (env : env) (x : CST.anon_choice_choice_unde_01ec478) =
  (match x with
  | `Choice_unde x -> R.Case ("Choice_unde",
      map_anon_choice_unde_ca3943d env x
    )
  | `Lhs_splat x -> R.Case ("Lhs_splat",
      map_lhs_splat env x
    )
  )

and map_anon_choice_cst_092cda1 (env : env) (x : CST.anon_choice_cst_092cda1) =
  (match x with
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Gene_inst_type x -> R.Case ("Gene_inst_type",
      map_generic_instance_type env x
    )
  )

and map_anon_choice_cst_10f7578 (env : env) (x : CST.anon_choice_cst_10f7578) =
  (match x with
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Nume_type x -> R.Case ("Nume_type",
      map_numeric_type env x
    )
  | `Type_param_splat (v1, v2) -> R.Case ("Type_param_splat",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_constant env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_cst_6df303a (env : env) (x : CST.anon_choice_cst_6df303a) =
  (match x with
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Gene_inst_type x -> R.Case ("Gene_inst_type",
      map_generic_instance_type env x
    )
  )

and map_anon_choice_cst_segm_3c2ec99 (env : env) (x : CST.anon_choice_cst_segm_3c2ec99) =
  (match x with
  | `Cst_segm tok -> R.Case ("Cst_segm",
      (* constant_segment *) token env tok
    )
  | `Macro_inte_cst_segm (v1, v2) -> R.Case ("Macro_inte_cst_segm",
      let v1 = (* constant_segment *) token env v1 in
      let v2 = map_macro_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_dot_call_efe6367 (env : env) (x : CST.anon_choice_dot_call_efe6367) =
  (match x with
  | `Dot_call (v1, v2, v3) -> R.Case ("Dot_call",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (match v3 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Semg_meta tok -> R.Case ("Semg_meta",
            (* semgrep_metavariable *) token env tok
          )
        | `Cst x -> R.Case ("Cst",
            map_constant env x
          )
        | `Id_meth_call tok -> R.Case ("Id_meth_call",
            (* identifier_method_call *) token env tok
          )
        | `Op_tok x -> R.Case ("Op_tok",
            map_operator_token env x
          )
        | `Inst_var tok -> R.Case ("Inst_var",
            (* instance_var *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Id_meth_call tok -> R.Case ("Id_meth_call",
      (* identifier_method_call *) token env tok
    )
  | `Global_meth x -> R.Case ("Global_meth",
      map_global_method env x
    )
  )

and map_anon_choice_elsif_b9e1083 (env : env) (x : CST.anon_choice_elsif_b9e1083) =
  (match x with
  | `Elsif (v1, v2, v3, v4, v5) -> R.Case ("Elsif",
      let v1 = (* "elsif" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_terminator env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_then_ env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_elsif_b9e1083 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Else x -> R.Case ("Else",
      map_else_ env x
    )
  )

and map_anon_choice_exp_1124375 (env : env) (x : CST.anon_choice_exp_1124375) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Splat x -> R.Case ("Splat",
      map_splat env x
    )
  | `Double_splat x -> R.Case ("Double_splat",
      map_double_splat env x
    )
  | `Named_expr x -> R.Case ("Named_expr",
      map_named_expr env x
    )
  | `Out x -> R.Case ("Out",
      map_out env x
    )
  )

and map_anon_choice_exp_373cb08 (env : env) (x : CST.anon_choice_exp_373cb08) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Out x -> R.Case ("Out",
      map_out env x
    )
  )

and map_anon_choice_exp_5f47da4 (env : env) (x : CST.anon_choice_exp_5f47da4) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Impl_obj_call x -> R.Case ("Impl_obj_call",
      map_implicit_object_call env x
    )
  | `Impl_obj_tuple (v1, v2, v3, v4, v5, v6) -> R.Case ("Impl_obj_tuple",
      let v1 = (* start_of_hash_or_tuple *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_expression env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 = (* "," *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Impl_obj_call x -> R.Case ("Impl_obj_call",
            map_implicit_object_call env x
          )
        | `Unde tok -> R.Case ("Unde",
            (* "_" *) token env tok
          )
        )
      in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (match v2 with
            | `Exp x -> R.Case ("Exp",
                map_expression env x
              )
            | `Impl_obj_call x -> R.Case ("Impl_obj_call",
                map_implicit_object_call env x
              )
            | `Unde tok -> R.Case ("Unde",
                (* "_" *) token env tok
              )
            )
          in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* "}" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_anon_choice_exp_e0308bd (env : env) (x : CST.anon_choice_exp_e0308bd) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Prop_id tok -> R.Case ("Prop_id",
      (* "property" *) token env tok
    )
  | `Splat x -> R.Case ("Splat",
      map_splat env x
    )
  | `Double_splat x -> R.Case ("Double_splat",
      map_double_splat env x
    )
  | `Named_expr x -> R.Case ("Named_expr",
      map_named_expr env x
    )
  | `Out x -> R.Case ("Out",
      map_out env x
    )
  )

and map_anon_choice_fun_param_49d118d (env : env) (x : CST.anon_choice_fun_param_49d118d) =
  (match x with
  | `Fun_param x -> R.Case ("Fun_param",
      map_fun_param env x
    )
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  )

and map_anon_choice_id_2655c0e (env : env) (x : CST.anon_choice_id_2655c0e) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Id_meth_call tok -> R.Case ("Id_meth_call",
      (* identifier_method_call *) token env tok
    )
  | `Inst_var tok -> R.Case ("Inst_var",
      (* instance_var *) token env tok
    )
  | `Class_var tok -> R.Case ("Class_var",
      (* class_var *) token env tok
    )
  | `Macro_var x -> R.Case ("Macro_var",
      map_macro_var env x
    )
  )

and map_anon_choice_id_5969394 (env : env) (x : CST.anon_choice_id_5969394) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  )

and map_anon_choice_id_c52dafb (env : env) (x : CST.anon_choice_id_c52dafb) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Inst_var tok -> R.Case ("Inst_var",
      (* instance_var *) token env tok
    )
  | `Class_var tok -> R.Case ("Class_var",
      (* class_var *) token env tok
    )
  | `Macro_var x -> R.Case ("Macro_var",
      map_macro_var env x
    )
  )

and map_anon_choice_id_df17f27 (env : env) (x : CST.anon_choice_id_df17f27) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Id_meth_call tok -> R.Case ("Id_meth_call",
      (* identifier_method_call *) token env tok
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  )

and map_anon_choice_macro_elsif_8f09864 (env : env) (x : CST.anon_choice_macro_elsif_8f09864) =
  (match x with
  | `Macro_elsif (v1, v2, v3) -> R.Case ("Macro_elsif",
      let v1 = map_macro_elsif_cond env v1 in
      let v2 = R.List (List.map (map_macro_content_ env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_macro_elsif_8f09864 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Macro_else x -> R.Case ("Macro_else",
      map_macro_else env x
    )
  )

and map_anon_choice_param_1e36750 (env : env) (x : CST.anon_choice_param_1e36750) =
  (match x with
  | `Param x -> R.Case ("Param",
      map_param env x
    )
  | `Splat_param (v1, v2, v3, v4) -> R.Case ("Splat_param",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = (* "*" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_id_c52dafb env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* type_field_colon *) token env v1 in
            let v2 = map_bare_type env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Double_splat_param (v1, v2, v3, v4) -> R.Case ("Double_splat_param",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_anon_choice_id_c52dafb env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* type_field_colon *) token env v1 in
            let v2 = map_bare_type env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  )

and map_anon_choice_semg_meta_de6d985 (env : env) (x : CST.anon_choice_semg_meta_de6d985) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Gene_inst_type x -> R.Case ("Gene_inst_type",
      map_generic_instance_type env x
    )
  )

and map_anon_choice_semg_meta_ed0b729 (env : env) (x : CST.anon_choice_semg_meta_ed0b729) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Gene_type x -> R.Case ("Gene_type",
      map_generic_type env x
    )
  )

and map_anon_choice_unde_ca3943d (env : env) (x : CST.anon_choice_unde_ca3943d) =
  (match x with
  | `Unde tok -> R.Case ("Unde",
      (* "_" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Inst_var tok -> R.Case ("Inst_var",
      (* instance_var *) token env tok
    )
  | `Class_var tok -> R.Case ("Class_var",
      (* class_var *) token env tok
    )
  | `Macro_var x -> R.Case ("Macro_var",
      map_macro_var env x
    )
  | `Assign_call x -> R.Case ("Assign_call",
      map_assign_call env x
    )
  | `Index_call x -> R.Case ("Index_call",
      map_index_call env x
    )
  | `Index_op x -> R.Case ("Index_op",
      map_index_operator env x
    )
  )

and map_argument_list_no_parens (env : env) ((v1, v2, v3) : CST.argument_list_no_parens) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* start_of_parenless_args *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_anon_choice_exp_1124375 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_1124375 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_argument_list_no_parens_with_block (env : env) ((v1, v2, v3) : CST.argument_list_no_parens_with_block) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* start_of_parenless_args *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_exp_1124375 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_exp_1124375 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 = (* "," *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = map_block_argument env v3 in
  R.Tuple [v1; v2; v3]

and map_argument_list_with_parens (env : env) ((v1, v2, v3) : CST.argument_list_with_parens) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_exp_e0308bd env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_exp_e0308bd env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_argument_list_with_parens_and_block (env : env) ((v1, v2, v3, v4) : CST.argument_list_with_parens_and_block) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_exp_e0308bd env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_exp_e0308bd env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 = (* "," *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = map_block_argument env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_array_ (env : env) (x : CST.array_) =
  (match x with
  | `LBRACK_exp_rep_COMMA_exp_opt_COMMA_RBRACK_opt_of_bare_type (v1, v2, v3, v4, v5, v6) -> R.Case ("LBRACK_exp_rep_COMMA_exp_opt_COMMA_RBRACK_opt_of_bare_type",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* "]" *) token env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "of" *) token env v1 in
            let v2 = map_bare_type env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `LBRACK_RBRACK_of_bare_type (v1, v2, v3, v4) -> R.Case ("LBRACK_RBRACK_of_bare_type",
      let v1 = (* "[" *) token env v1 in
      let v2 = (* "]" *) token env v2 in
      let v3 = (* "of" *) token env v3 in
      let v4 = map_bare_type env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_array_like (env : env) ((v1, v2) : CST.array_like) =
  let v1 = map_anon_choice_cst_6df303a env v1 in
  let v2 = map_tuple env v2 in
  R.Tuple [v1; v2]

and map_asm (env : env) ((v1, v2, v3, v4, v5) : CST.asm) =
  let v1 = (* "asm" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_string_ env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_asm_outputs env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_asm_clobbers (env : env) ((v1, v2, v3) : CST.asm_clobbers) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_asm_clobbers_ env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_asm_options env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_asm_clobbers_ (env : env) ((v1, v2) : CST.asm_clobbers_) =
  let v1 = map_string_ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_string_ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_asm_inputs (env : env) ((v1, v2, v3) : CST.asm_inputs) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_asm_operands env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_asm_clobbers env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_asm_operand (env : env) ((v1, v2, v3, v4) : CST.asm_operand) =
  let v1 = map_string_ env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_asm_operands (env : env) ((v1, v2) : CST.asm_operands) =
  let v1 = map_asm_operand env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_asm_operand env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_asm_options (env : env) ((v1, v2) : CST.asm_options) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_asm_options_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_asm_options_ (env : env) ((v1, v2) : CST.asm_options_) =
  let v1 = map_string_ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_string_ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_asm_outputs (env : env) ((v1, v2, v3) : CST.asm_outputs) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_asm_operands env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_asm_inputs env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_assign (env : env) ((v1, v2, v3) : CST.assign) =
  let v1 =
    (match v1 with
    | `Unde tok -> R.Case ("Unde",
        (* "_" *) token env tok
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Prop tok -> R.Case ("Prop",
        (* "property" *) token env tok
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* semgrep_metavariable *) token env tok
      )
    | `Inst_var tok -> R.Case ("Inst_var",
        (* instance_var *) token env tok
      )
    | `Class_var tok -> R.Case ("Class_var",
        (* class_var *) token env tok
      )
    | `Macro_var x -> R.Case ("Macro_var",
        map_macro_var env x
      )
    | `Assign_call x -> R.Case ("Assign_call",
        map_assign_call env x
      )
    | `Index_call x -> R.Case ("Index_call",
        map_index_call env x
      )
    | `Index_op x -> R.Case ("Index_op",
        map_index_operator env x
      )
    | `Spec_var tok -> R.Case ("Spec_var",
        (* special_variable *) token env tok
      )
    )
  in
  let v2 = (* "=" *) token env v2 in
  let v3 =
    (match v3 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Prop_id tok -> R.Case ("Prop_id",
        (* "property" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_assign_call (env : env) ((v1, v2, v3) : CST.assign_call) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_bare_type (env : env) (x : CST.bare_type) =
  (match x with
  | `Proc_type (v1, v2, v3, v4) -> R.Case ("Proc_type",
      let v1 = map_splattable_type env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_splattable_type env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* "->" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Spla_type x -> R.Case ("Spla_type",
      map_splattable_type env x
    )
  )

and map_base_fun_def (env : env) ((v1, v2, v3, v4, v5) : CST.base_fun_def) =
  let v1 = (* "fun" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Id_meth_call tok -> R.Case ("Id_meth_call",
        (* identifier_method_call *) token env tok
      )
    )
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_anon_choice_id_5969394 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_fun_param_list env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* type_field_colon *) token env v1 in
        let v2 = map_bare_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_base_method_def (env : env) ((v1, v2, v3, v4, v5, v6) : CST.base_method_def) =
  let v1 = (* "def" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | `Semg_meta tok -> R.Case ("Semg_meta",
              (* semgrep_metavariable *) token env tok
            )
          | `Cst x -> R.Case ("Cst",
              map_constant env x
            )
          | `Self tok -> R.Case ("Self",
              (* "self" *) token env tok
            )
          )
        in
        let v2 = (* "." *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* semgrep_metavariable *) token env tok
      )
    | `Id_meth_call tok -> R.Case ("Id_meth_call",
        (* identifier_method_call *) token env tok
      )
    | `Id_assign tok -> R.Case ("Id_assign",
        (* identifier_assign *) token env tok
      )
    | `Op_tok x -> R.Case ("Op_tok",
        map_operator_token env x
      )
    | `BQUOT tok -> R.Case ("BQUOT",
        (* "`" *) token env tok
      )
    )
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_param_list env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* type_field_colon *) token env v1 in
        let v2 = map_bare_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_forall env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_begin_ (env : env) ((v1, v2, v3, v4, v5) : CST.begin_) =
  let v1 = (* "begin" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_rescue_else_ensure env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_beginless_range (env : env) ((v1, v2, v3) : CST.beginless_range) =
  let v1 = map_anon_choice_DOTDOT_ed078ec env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* end_of_range *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_binary_and_operator (env : env) ((v1, v2, v3) : CST.binary_and_operator) =
  let v1 = map_expression env v1 in
  let v2 = (* binary_ampersand *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_binary_or_operator (env : env) ((v1, v2, v3) : CST.binary_or_operator) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `BAR tok -> R.Case ("BAR",
        (* "|" *) token env tok
      )
    | `HAT tok -> R.Case ("HAT",
        (* "^" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_block_argument (env : env) ((v1, v2) : CST.block_argument) =
  let v1 = (* block_ampersand *) token env v1 in
  let v2 =
    (match v2 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Impl_obj_call x -> R.Case ("Impl_obj_call",
        map_implicit_object_call env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_block_param (env : env) ((v1, v2, v3, v4) : CST.block_param) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 = (* "&" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_id_c52dafb env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_pat_5eb9c21 env v1 in
        let v2 = map_bare_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_brace_block (env : env) ((v1, v2, v3, v4) : CST.brace_block) =
  let v1 = (* start_of_brace_block *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "|" *) token env v1 in
        let v2 = map_block_param_list env v2 in
        let v3 = (* "|" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_bracket_argument_list (env : env) ((v1, v2, v3) : CST.bracket_argument_list) =
  let v1 = map_anon_choice_exp_1124375 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_1124375 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_c_struct_expression (env : env) (x : CST.c_struct_expression) =
  (match x with
  | `Macro_node x -> R.Case ("Macro_node",
      map_macro_node env x
    )
  | `Incl x -> R.Case ("Incl",
      map_include_ env x
    )
  | `C_struct_fields (v1, v2, v3, v4) -> R.Case ("C_struct_fields",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = (* identifier *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* type_field_colon *) token env v3 in
      let v4 = map_bare_type env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_c_struct_expressions (env : env) (x : CST.c_struct_expressions) =
  (match x with
  | `Rep1_choice_c_struct_exp_term_opt_c_struct_exp (v1, v2) -> R.Case ("Rep1_choice_c_struct_exp_term_opt_c_struct_exp",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `C_struct_exp_term (v1, v2) -> R.Case ("C_struct_exp_term",
              let v1 = map_c_struct_expression env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            )
          | `SEMI tok -> R.Case ("SEMI",
              (* ";" *) token env tok
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_c_struct_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `C_struct_exp x -> R.Case ("C_struct_exp",
      map_c_struct_expression env x
    )
  )

and map_call (env : env) (x : CST.call) =
  (match x with
  | `Choice_dot_call_opt_choice_arg_list_with_parens (v1, v2) -> R.Case ("Choice_dot_call_opt_choice_arg_list_with_parens",
      let v1 = map_anon_choice_dot_call_efe6367 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_id_choice_arg_list_with_parens (v1, v2) -> R.Case ("Choice_id_choice_arg_list_with_parens",
      let v1 = map_anon_choice_id_684e964 env v1 in
      let v2 = map_control_expressions env v2 in
      R.Tuple [v1; v2]
    )
  | `Macro_exp_arg_list_with_parens (v1, v2) -> R.Case ("Macro_exp_arg_list_with_parens",
      let v1 = map_macro_expression env v1 in
      let v2 = map_argument_list_with_parens env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_prop_prop_arg_list (v1, v2) -> R.Case ("Choice_prop_prop_arg_list",
      let v1 = map_anon_choice_prop_94a3794 env v1 in
      let v2 = map_property_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_prop_prop_arg_list_brace_blk (v1, v2, v3) -> R.Case ("Choice_prop_prop_arg_list_brace_blk",
      let v1 = map_anon_choice_prop_94a3794 env v1 in
      let v2 = map_property_argument_list env v2 in
      let v3 = map_brace_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_prop_arg_list_with_parens (v1, v2) -> R.Case ("Choice_prop_arg_list_with_parens",
      let v1 = map_anon_choice_prop_94a3794 env v1 in
      let v2 = map_argument_list_with_parens env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_prop_arg_list_with_parens_brace_blk (v1, v2, v3) -> R.Case ("Choice_prop_arg_list_with_parens_brace_blk",
      let v1 = map_anon_choice_prop_94a3794 env v1 in
      let v2 = map_argument_list_with_parens env v2 in
      let v3 = map_brace_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_dot_call_opt_choice_arg_list_with_parens_brace_blk (v1, v2, v3) -> R.Case ("Choice_dot_call_opt_choice_arg_list_with_parens_brace_blk",
      let v1 = map_anon_choice_dot_call_efe6367 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_brace_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_id_opt_choice_arg_list_with_parens_brace_blk (v1, v2, v3) -> R.Case ("Choice_id_opt_choice_arg_list_with_parens_brace_blk",
      let v1 = map_anon_choice_id_684e964 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_brace_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_dot_call_opt_choice_arg_list_with_parens_do_end_blk (v1, v2, v3) -> R.Case ("Choice_dot_call_opt_choice_arg_list_with_parens_do_end_blk",
      let v1 = map_anon_choice_dot_call_efe6367 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_do_end_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_id_opt_choice_arg_list_with_parens_do_end_blk (v1, v2, v3) -> R.Case ("Choice_id_opt_choice_arg_list_with_parens_do_end_blk",
      let v1 = map_anon_choice_id_684e964 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_do_end_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_dot_call_choice_arg_list_with_parens_and_blk (v1, v2) -> R.Case ("Choice_dot_call_choice_arg_list_with_parens_and_blk",
      let v1 = map_anon_choice_dot_call_efe6367 env v1 in
      let v2 =
        map_anon_choice_arg_list_with_parens_and_blk_422157f env v2
      in
      R.Tuple [v1; v2]
    )
  | `Choice_id_choice_arg_list_with_parens_and_blk (v1, v2) -> R.Case ("Choice_id_choice_arg_list_with_parens_and_blk",
      let v1 = map_anon_choice_id_684e964 env v1 in
      let v2 =
        map_anon_choice_arg_list_with_parens_and_blk_422157f env v2
      in
      R.Tuple [v1; v2]
    )
  )

and map_case (env : env) ((v1, v2, v3, v4, v5) : CST.case) =
  let v1 = (* "case" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_when_ env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_else_ env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_chained_string (env : env) ((v1, v2) : CST.chained_string) =
  let v1 = map_string_ env v1 in
  let v2 = R.List (List.map (map_string_ env) v2) in
  R.Tuple [v1; v2]

and map_class_def (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_def) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "abstract" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "class" *) token env v2 in
  let v3 = map_anon_choice_semg_meta_ed0b729 env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "<" *) token env v1 in
        let v2 = map_anon_choice_semg_meta_de6d985 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_type (env : env) ((v1, v2, v3) : CST.class_type) =
  let v1 = map_type_ env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "class" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_command (env : env) ((v1, v2, v3) : CST.command) =
  let v1 = (* command_literal_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_string_literal_content env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* command_literal_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_command_percent_literal (env : env) ((v1, v2, v3) : CST.command_percent_literal) =
  let v1 = (* command_percent_literal_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_string_percent_literal_content env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* percent_literal_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_comparison_operator (env : env) ((v1, v2, v3) : CST.comparison_operator) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `LT tok -> R.Case ("LT",
        (* "<" *) token env tok
      )
    | `LTEQ tok -> R.Case ("LTEQ",
        (* "<=" *) token env tok
      )
    | `GT tok -> R.Case ("GT",
        (* ">" *) token env tok
      )
    | `GTEQ tok -> R.Case ("GTEQ",
        (* ">=" *) token env tok
      )
    | `LTEQGT tok -> R.Case ("LTEQGT",
        (* "<=>" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_complement_operator (env : env) ((v1, v2) : CST.complement_operator) =
  let v1 = (* "~" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_conditional (env : env) ((v1, v2, v3, v4, v5) : CST.conditional) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_const_assign (env : env) ((v1, v2, v3) : CST.const_assign) =
  let v1 = map_constant env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_constant (env : env) ((v1, v2, v3) : CST.constant) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "::" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_anon_choice_cst_segm_3c2ec99 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "::" *) token env v1 in
      let v2 = map_anon_choice_cst_segm_3c2ec99 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_control_expressions (env : env) (x : CST.control_expressions) =
  (match x with
  | `Arg_list_with_parens x -> R.Case ("Arg_list_with_parens",
      map_argument_list_with_parens env x
    )
  | `Arg_list_no_parens x -> R.Case ("Arg_list_no_parens",
      map_argument_list_no_parens env x
    )
  )

and map_do_end_block (env : env) ((v1, v2, v3, v4, v5) : CST.do_end_block) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "|" *) token env v1 in
        let v2 = map_block_param_list env v2 in
        let v3 = (* "|" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_rescue_else_ensure env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_double_splat (env : env) ((v1, v2) : CST.double_splat) =
  let v1 = (* unary_double_star *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_else_ (env : env) ((v1, v2) : CST.else_) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_ensure (env : env) ((v1, v2) : CST.ensure) =
  let v1 = (* regular_ensure_keyword *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_enum_def (env : env) ((v1, v2, v3, v4, v5) : CST.enum_def) =
  let v1 = (* "enum" *) token env v1 in
  let v2 = map_constant env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_pat_5eb9c21 env v1 in
        let v2 = map_bare_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_enum_statements env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_enum_statement (env : env) (x : CST.enum_statement) =
  (match x with
  | `Macro_node x -> R.Case ("Macro_node",
      map_macro_node env x
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Const_assign x -> R.Case ("Const_assign",
      map_const_assign env x
    )
  | `Meth_def x -> R.Case ("Meth_def",
      map_method_def env x
    )
  | `Macro_def x -> R.Case ("Macro_def",
      map_macro_def env x
    )
  | `Class_var tok -> R.Case ("Class_var",
      (* class_var *) token env tok
    )
  | `Class_var_assign (v1, v2, v3) -> R.Case ("Class_var_assign",
      let v1 = (* class_var *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  | `Visi_modi x -> R.Case ("Visi_modi",
      map_visibility_modifier env x
    )
  )

and map_enum_statements (env : env) (x : CST.enum_statements) =
  (match x with
  | `Rep1_choice_enum_stmt_term_opt_enum_stmt (v1, v2) -> R.Case ("Rep1_choice_enum_stmt_term_opt_enum_stmt",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Enum_stmt_term (v1, v2) -> R.Case ("Enum_stmt_term",
              let v1 = map_enum_statement env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            )
          | `SEMI tok -> R.Case ("SEMI",
              (* ";" *) token env tok
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_enum_statement env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Enum_stmt x -> R.Case ("Enum_stmt",
      map_enum_statement env x
    )
  )

and map_equality_operator (env : env) ((v1, v2, v3) : CST.equality_operator) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `EQEQ tok -> R.Case ("EQEQ",
        (* "==" *) token env tok
      )
    | `BANGEQ tok -> R.Case ("BANGEQ",
        (* "!=" *) token env tok
      )
    | `EQTILDE tok -> R.Case ("EQTILDE",
        (* "=~" *) token env tok
      )
    | `BANGTILDE tok -> R.Case ("BANGTILDE",
        (* "!~" *) token env tok
      )
    | `EQEQEQ tok -> R.Case ("EQEQEQ",
        (* "===" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_exhaustive_case (env : env) ((v1, v2, v3, v4) : CST.exhaustive_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = R.List (List.map (map_in_ env) v3) in
  let v4 = (* "end" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_exponential_operator (env : env) ((v1, v2, v3) : CST.exponential_operator) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `Bin_double_star tok -> R.Case ("Bin_double_star",
        (* binary_double_star *) token env tok
      )
    | `AMPSTARSTAR tok -> R.Case ("AMPSTARSTAR",
        (* "&**" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_macro_node x -> R.Case ("Choice_macro_node",
      (match x with
      | `Macro_node x -> R.Case ("Macro_node",
          map_macro_node env x
        )
      | `Nil tok -> R.Case ("Nil",
          (* "nil" *) token env tok
        )
      | `True tok -> R.Case ("True",
          (* "true" *) token env tok
        )
      | `False tok -> R.Case ("False",
          (* "false" *) token env tok
        )
      | `Int x -> R.Case ("Int",
          map_integer env x
        )
      | `Float x -> R.Case ("Float",
          map_float_ env x
        )
      | `Char x -> R.Case ("Char",
          map_char env x
        )
      | `Array x -> R.Case ("Array",
          map_array_ env x
        )
      | `Hash x -> R.Case ("Hash",
          map_hash env x
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `Chai_str x -> R.Case ("Chai_str",
          map_chained_string env x
        )
      | `Str_perc_lit x -> R.Case ("Str_perc_lit",
          map_string_percent_literal env x
        )
      | `Str_array_perc_lit x -> R.Case ("Str_array_perc_lit",
          map_string_array_percent_literal env x
        )
      | `Op_symb x -> R.Case ("Op_symb",
          map_operator_symbol env x
        )
      | `Unqu_symb x -> R.Case ("Unqu_symb",
          map_unquoted_symbol env x
        )
      | `Quoted_symb x -> R.Case ("Quoted_symb",
          map_quoted_symbol env x
        )
      | `Symb_array_perc_lit x -> R.Case ("Symb_array_perc_lit",
          map_symbol_array_percent_literal env x
        )
      | `Here_start tok -> R.Case ("Here_start",
          (* heredoc_start *) token env tok
        )
      | `Range x -> R.Case ("Range",
          map_range env x
        )
      | `Begi_range x -> R.Case ("Begi_range",
          map_beginless_range env x
        )
      | `Tuple x -> R.Case ("Tuple",
          map_tuple env x
        )
      | `Named_tuple x -> R.Case ("Named_tuple",
          map_named_tuple env x
        )
      | `Proc x -> R.Case ("Proc",
          map_proc env x
        )
      | `Meth_proc x -> R.Case ("Meth_proc",
          map_method_proc env x
        )
      | `Cmd x -> R.Case ("Cmd",
          map_command env x
        )
      | `Cmd_perc_lit x -> R.Case ("Cmd_perc_lit",
          map_command_percent_literal env x
        )
      | `Regex x -> R.Case ("Regex",
          map_regex env x
        )
      | `Regex_perc_lit x -> R.Case ("Regex_perc_lit",
          map_regex_percent_literal env x
        )
      | `Empty_parens x -> R.Case ("Empty_parens",
          map_empty_parens env x
        )
      | `Paren_expres x -> R.Case ("Paren_expres",
          map_parenthesized_expressions env x
        )
      | `Begin x -> R.Case ("Begin",
          map_begin_ env x
        )
      | `Self tok -> R.Case ("Self",
          (* "self" *) token env tok
        )
      | `Cst x -> R.Case ("Cst",
          map_constant env x
        )
      | `Gene_inst_type x -> R.Case ("Gene_inst_type",
          map_generic_instance_type env x
        )
      | `Nila_cst x -> R.Case ("Nila_cst",
          map_nilable_constant env x
        )
      | `Pseudo_cst x -> R.Case ("Pseudo_cst",
          map_pseudo_constant env x
        )
      | `Spec_var tok -> R.Case ("Spec_var",
          (* special_variable *) token env tok
        )
      | `Global_match_data_index tok -> R.Case ("Global_match_data_index",
          (* global_match_data_index *) token env tok
        )
      | `Id tok -> R.Case ("Id",
          (* identifier *) token env tok
        )
      | `Inst_var tok -> R.Case ("Inst_var",
          (* instance_var *) token env tok
        )
      | `Class_var tok -> R.Case ("Class_var",
          (* class_var *) token env tok
        )
      | `Macro_var x -> R.Case ("Macro_var",
          map_macro_var env x
        )
      | `Type_decl x -> R.Case ("Type_decl",
          map_type_declaration env x
        )
      | `While x -> R.Case ("While",
          map_while_ env x
        )
      | `Until x -> R.Case ("Until",
          map_until env x
        )
      | `If x -> R.Case ("If",
          map_if_ env x
        )
      | `Unless x -> R.Case ("Unless",
          map_unless env x
        )
      | `Cond x -> R.Case ("Cond",
          map_conditional env x
        )
      | `Case x -> R.Case ("Case",
          map_case env x
        )
      | `Exha_case x -> R.Case ("Exha_case",
          map_exhaustive_case env x
        )
      | `Select x -> R.Case ("Select",
          map_select env x
        )
      | `Call x -> R.Case ("Call",
          map_call env x
        )
      | `Addi_op x -> R.Case ("Addi_op",
          map_additive_operator env x
        )
      | `Un_addi_op x -> R.Case ("Un_addi_op",
          map_unary_additive_operator env x
        )
      | `Mult_op x -> R.Case ("Mult_op",
          map_multiplicative_operator env x
        )
      | `Expo_op x -> R.Case ("Expo_op",
          map_exponential_operator env x
        )
      | `Shift_op x -> R.Case ("Shift_op",
          map_shift_operator env x
        )
      | `Comp_op_6e1517d x -> R.Case ("Comp_op_6e1517d",
          map_complement_operator env x
        )
      | `Bin_and_op x -> R.Case ("Bin_and_op",
          map_binary_and_operator env x
        )
      | `Bin_or_op x -> R.Case ("Bin_or_op",
          map_binary_or_operator env x
        )
      | `Equa_op x -> R.Case ("Equa_op",
          map_equality_operator env x
        )
      | `Comp_op_b5e2455 x -> R.Case ("Comp_op_b5e2455",
          map_comparison_operator env x
        )
      | `Index_op x -> R.Case ("Index_op",
          map_index_operator env x
        )
      | `Pseudo_call x -> R.Case ("Pseudo_call",
          map_pseudo_call env x
        )
      | `Index_call x -> R.Case ("Index_call",
          map_index_call env x
        )
      | `Array_like x -> R.Case ("Array_like",
          map_array_like env x
        )
      | `Hash_like x -> R.Case ("Hash_like",
          map_hash_like env x
        )
      | `Assign x -> R.Case ("Assign",
          map_assign env x
        )
      | `Unin_assign x -> R.Case ("Unin_assign",
          map_uninitialized_assign env x
        )
      | `Op_assign x -> R.Case ("Op_assign",
          map_operator_assign env x
        )
      | `Not x -> R.Case ("Not",
          map_not env x
        )
      | `And x -> R.Case ("And",
          map_and_ env x
        )
      | `Or x -> R.Case ("Or",
          map_or_ env x
        )
      | `Asm x -> R.Case ("Asm",
          map_asm env x
        )
      | `Yield x -> R.Case ("Yield",
          map_yield env x
        )
      | `Typeof x -> R.Case ("Typeof",
          map_typeof env x
        )
      | `Poin x -> R.Case ("Poin",
          map_pointerof env x
        )
      | `Sizeof x -> R.Case ("Sizeof",
          map_sizeof env x
        )
      | `Inst_sizeof x -> R.Case ("Inst_sizeof",
          map_instance_sizeof env x
        )
      | `Alig x -> R.Case ("Alig",
          map_alignof env x
        )
      | `Inst_alig x -> R.Case ("Inst_alig",
          map_instance_alignof env x
        )
      | `Offs x -> R.Case ("Offs",
          map_offsetof env x
        )
      )
    )
  )

and map_fun_param (env : env) ((v1, v2, v3) : CST.fun_param) =
  let v1 = map_anon_choice_id_df17f27 env v1 in
  let v2 = (* type_field_colon *) token env v2 in
  let v3 = map_bare_type env v3 in
  R.Tuple [v1; v2; v3]

and map_fun_param_list (env : env) ((v1, v2, v3) : CST.fun_param_list) =
  let v1 = map_fun_param env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_fun_param env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = map_anon_opt_COMMA_opt_DOTDOTDOT_0392b4d env v3 in
  R.Tuple [v1; v2; v3]

and map_fun_type_param_list (env : env) ((v1, v2, v3) : CST.fun_type_param_list) =
  let v1 = map_anon_choice_fun_param_49d118d env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_fun_param_49d118d env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = map_anon_opt_COMMA_opt_DOTDOTDOT_0392b4d env v3 in
  R.Tuple [v1; v2; v3]

and map_generic_instance_type (env : env) ((v1, v2, v3, v4) : CST.generic_instance_type) =
  let v1 = map_constant env v1 in
  let v2 = map_imm_tok_lpar env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_instance_param_list env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_generic_type (env : env) ((v1, v2, v3, v4) : CST.generic_type) =
  let v1 = map_constant env v1 in
  let v2 = map_imm_tok_lpar env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_param_list env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_global_var (env : env) ((v1, v2, v3, v4, v5) : CST.global_var) =
  let v1 = (* "$" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 =
          (match v2 with
          | `Id tok -> R.Case ("Id",
              (* identifier *) token env tok
            )
          | `Cst x -> R.Case ("Cst",
              map_constant env x
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* type_field_colon *) token env v4 in
  let v5 = map_bare_type env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_hash (env : env) (x : CST.hash) =
  (match x with
  | `Start_of_hash_or_tuple_hash_entry_rep_COMMA_hash_entry_opt_COMMA_RCURL_opt_of_bare_type_EQGT_bare_type (v1, v2, v3, v4, v5, v6) -> R.Case ("Start_of_hash_or_tuple_hash_entry_rep_COMMA_hash_entry_opt_COMMA_RCURL_opt_of_bare_type_EQGT_bare_type",
      let v1 = (* start_of_hash_or_tuple *) token env v1 in
      let v2 = map_hash_entry env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_hash_entry env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* "}" *) token env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 = (* "of" *) token env v1 in
            let v2 = map_bare_type env v2 in
            let v3 = (* "=>" *) token env v3 in
            let v4 = map_bare_type env v4 in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Start_of_hash_or_tuple_RCURL_of_bare_type_EQGT_bare_type (v1, v2, v3, v4, v5, v6) -> R.Case ("Start_of_hash_or_tuple_RCURL_of_bare_type_EQGT_bare_type",
      let v1 = (* start_of_hash_or_tuple *) token env v1 in
      let v2 = (* "}" *) token env v2 in
      let v3 = (* "of" *) token env v3 in
      let v4 = map_bare_type env v4 in
      let v5 = (* "=>" *) token env v5 in
      let v6 = map_bare_type env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_hash_entry (env : env) ((v1, v2, v3) : CST.hash_entry) =
  let v1 = map_expression env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_hash_like (env : env) ((v1, v2) : CST.hash_like) =
  let v1 = map_anon_choice_cst_6df303a env v1 in
  let v2 = map_hash env v2 in
  R.Tuple [v1; v2]

and map_if_ (env : env) ((v1, v2, v3, v4, v5, v6) : CST.if_) =
  let v1 = (* regular_if_keyword *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_terminator env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_then_ env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_elsif_b9e1083 env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_implicit_object_call (env : env) (x : CST.implicit_object_call) =
  (match x with
  | `Impl_obj_call_chai x -> R.Case ("Impl_obj_call_chai",
      map_implicit_object_call_chainable env x
    )
  | `Impl_obj_call_unch (v1, v2) -> R.Case ("Impl_obj_call_unch",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_implicit_object_call_chainable env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Choice_impl_obj_meth_id_arg_list_no_parens (v1, v2) -> R.Case ("Choice_impl_obj_meth_id_arg_list_no_parens",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 = map_argument_list_no_parens env v2 in
            R.Tuple [v1; v2]
          )
        | `Choice_impl_obj_meth_id_arg_list_no_parens_with_blk (v1, v2) -> R.Case ("Choice_impl_obj_meth_id_arg_list_no_parens_with_blk",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 = map_argument_list_no_parens_with_block env v2 in
            R.Tuple [v1; v2]
          )
        | `Choice_impl_obj_meth_id_EQ_arg_list_no_parens (v1, v2, v3) -> R.Case ("Choice_impl_obj_meth_id_EQ_arg_list_no_parens",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 = (* "=" *) token env v2 in
            let v3 = map_argument_list_no_parens env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_implicit_object_call_chainable (env : env) (x : CST.implicit_object_call_chainable) =
  (match x with
  | `Rectype (v1, v2) -> R.Case ("Rectype",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_implicit_object_call_chainable env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Choice_impl_obj_meth_id_opt_arg_list_with_parens (v1, v2) -> R.Case ("Choice_impl_obj_meth_id_opt_arg_list_with_parens",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_argument_list_with_parens env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Choice_impl_obj_meth_id_start_of_index_op_brac_arg_list_choice_RBRACK (v1, v2, v3, v4) -> R.Case ("Choice_impl_obj_meth_id_start_of_index_op_brac_arg_list_choice_RBRACK",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 = (* start_of_index_operator *) token env v2 in
            let v3 = map_bracket_argument_list env v3 in
            let v4 = map_anon_choice_RBRACK_57c1d11 env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        | `Choice_impl_obj_meth_id_EQ_arg_list_with_parens (v1, v2, v3) -> R.Case ("Choice_impl_obj_meth_id_EQ_arg_list_with_parens",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 = (* "=" *) token env v2 in
            let v3 = map_argument_list_with_parens env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Choice_impl_obj_meth_id_opt_choice_arg_list_with_parens_brace_blk (v1, v2, v3) -> R.Case ("Choice_impl_obj_meth_id_opt_choice_arg_list_with_parens_brace_blk",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_control_expressions env x
                ))
              | None -> R.Option None)
            in
            let v3 = map_brace_block env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Choice_impl_obj_meth_id_opt_choice_arg_list_with_parens_do_end_blk (v1, v2, v3) -> R.Case ("Choice_impl_obj_meth_id_opt_choice_arg_list_with_parens_do_end_blk",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_control_expressions env x
                ))
              | None -> R.Option None)
            in
            let v3 = map_do_end_block env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Choice_impl_obj_meth_id_arg_list_with_parens_and_blk (v1, v2) -> R.Case ("Choice_impl_obj_meth_id_arg_list_with_parens_and_blk",
            let v1 = map_anon_choice_impl_obj_meth_id_3a9d445 env v1 in
            let v2 = map_argument_list_with_parens_and_block env v2 in
            R.Tuple [v1; v2]
          )
        | `Impl_obj_ivar tok -> R.Case ("Impl_obj_ivar",
            (* implicit_object_ivar *) token env tok
          )
        | `Impl_obj_index_op x -> R.Case ("Impl_obj_index_op",
            map_implicit_object_index_operator env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_implicit_object_index_operator (env : env) ((v1, v2, v3, v4) : CST.implicit_object_index_operator) =
  let v1 = (* "." *) token env v1 in
  let v2 = (* start_of_index_operator *) token env v2 in
  let v3 = map_bracket_argument_list env v3 in
  let v4 = map_anon_choice_RBRACK_57c1d11 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_in_ (env : env) ((v1, v2, v3, v4, v5) : CST.in_) =
  let v1 = (* "in" *) token env v1 in
  let v2 = map_anon_choice_exp_5f47da4 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_5f47da4 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_anon_choice_then_eac33c8 env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_include_ (env : env) ((v1, v2) : CST.include_) =
  let v1 = (* "include" *) token env v1 in
  let v2 = map_anon_choice_cst_092cda1 env v2 in
  R.Tuple [v1; v2]

and map_index_call (env : env) ((v1, v2, v3, v4, v5) : CST.index_call) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "[" *) token env v3 in
  let v4 = map_bracket_argument_list env v4 in
  let v5 = map_anon_choice_RBRACK_57c1d11 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_index_operator (env : env) ((v1, v2, v3, v4) : CST.index_operator) =
  let v1 = map_expression env v1 in
  let v2 = (* start_of_index_operator *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_bracket_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_anon_choice_RBRACK_57c1d11 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_inline_statement (env : env) (x : CST.inline_statement) =
  (match x with
  | `Modi_if (v1, v2, v3) -> R.Case ("Modi_if",
      let v1 = map_statement env v1 in
      let v2 = (* modifier_if_keyword *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Modi_unless (v1, v2, v3) -> R.Case ("Modi_unless",
      let v1 = map_statement env v1 in
      let v2 = (* modifier_unless_keyword *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Modi_rescue (v1, v2, v3) -> R.Case ("Modi_rescue",
      let v1 = map_statement env v1 in
      let v2 = (* modifier_rescue_keyword *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Modi_ensure (v1, v2, v3) -> R.Case ("Modi_ensure",
      let v1 = map_statement env v1 in
      let v2 = (* modifier_ensure_keyword *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ret (v1, v2) -> R.Case ("Ret",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Next (v1, v2) -> R.Case ("Next",
      let v1 = (* "next" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Brk (v1, v2) -> R.Case ("Brk",
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_control_expressions env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_instance_alignof (env : env) ((v1, v2, v3, v4) : CST.instance_alignof) =
  let v1 = (* "instance_alignof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_bare_type env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_instance_sizeof (env : env) ((v1, v2, v3, v4) : CST.instance_sizeof) =
  let v1 = (* "instance_sizeof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_bare_type env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) =
  let v1 = map_tok_prec_p1_hashlcurl env v1 in
  let v2 =
    (match v2 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Inline_stmt x -> R.Case ("Inline_stmt",
        map_inline_statement env x
      )
    )
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_lhs_splat (env : env) ((v1, v2) : CST.lhs_splat) =
  let v1 = (* "*" *) token env v1 in
  let v2 = map_anon_choice_unde_ca3943d env v2 in
  R.Tuple [v1; v2]

and map_lib_def (env : env) ((v1, v2, v3, v4) : CST.lib_def) =
  let v1 = (* "lib" *) token env v1 in
  let v2 =
    (match v2 with
    | `Cst x -> R.Case ("Cst",
        map_constant env x
      )
    | `Gene_type x -> R.Case ("Gene_type",
        map_generic_type env x
      )
    )
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_lib_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "end" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_lib_statement (env : env) (x : CST.lib_statement) =
  (match x with
  | `Macro_node x -> R.Case ("Macro_node",
      map_macro_node env x
    )
  | `Alias x -> R.Case ("Alias",
      map_alias env x
    )
  | `Fun_def (v1, v2, v3, v4, v5) -> R.Case ("Fun_def",
      let v1 = (* "fun" *) token env v1 in
      let v2 = map_anon_choice_id_df17f27 env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_anon_choice_id_5969394 env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* line_break *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = (* "(" *) token env v2 in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_fun_type_param_list env x
                ))
              | None -> R.Option None)
            in
            let v4 = (* ")" *) token env v4 in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* type_field_colon *) token env v1 in
            let v2 = map_bare_type env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Type_def (v1, v2, v3, v4) -> R.Case ("Type_def",
      let v1 = (* "type" *) token env v1 in
      let v2 = map_constant env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_bare_type env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `C_struct_def (v1, v2, v3, v4) -> R.Case ("C_struct_def",
      let v1 = (* "struct" *) token env v1 in
      let v2 = map_constant env v2 in
      let v3 = map_c_struct_expressions env v3 in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Union_def (v1, v2, v3, v4) -> R.Case ("Union_def",
      let v1 = (* "union" *) token env v1 in
      let v2 = map_constant env v2 in
      let v3 = map_union_expressions env v3 in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Enum_def x -> R.Case ("Enum_def",
      map_enum_def env x
    )
  | `Global_var x -> R.Case ("Global_var",
      map_global_var env x
    )
  | `Const_assign x -> R.Case ("Const_assign",
      map_const_assign env x
    )
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  | `Visi_modi x -> R.Case ("Visi_modi",
      map_visibility_modifier env x
    )
  )

and map_lib_statements (env : env) (x : CST.lib_statements) =
  (match x with
  | `Rep1_choice_lib_stmt_term_opt_lib_stmt (v1, v2) -> R.Case ("Rep1_choice_lib_stmt_term_opt_lib_stmt",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Lib_stmt_term (v1, v2) -> R.Case ("Lib_stmt_term",
              let v1 = map_lib_statement env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            )
          | `SEMI tok -> R.Case ("SEMI",
              (* ";" *) token env tok
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_lib_statement env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Lib_stmt x -> R.Case ("Lib_stmt",
      map_lib_statement env x
    )
  )

and map_macro_content_ (env : env) (x : CST.macro_content_) =
  (match x with
  | `Choice_macro_node x -> R.Case ("Choice_macro_node",
      (match x with
      | `Macro_node x -> R.Case ("Macro_node",
          map_macro_node env x
        )
      | `Macro_var x -> R.Case ("Macro_var",
          map_macro_var env x
        )
      | `Macro_lit_content x -> R.Case ("Macro_lit_content",
          map_macro_literal_content env x
        )
      | `Term x -> R.Case ("Term",
          map_terminator env x
        )
      )
    )
  | `RCURLRCURL tok -> R.Case ("RCURLRCURL",
      (* "}}" *) token env tok
    )
  )

and map_macro_def (env : env) ((v1, v2, v3, v4) : CST.macro_def) =
  let v1 = map_macro_signature env v1 in
  let v2 = (* macro_start *) token env v2 in
  let v3 =
    (match v3 with
    | Some xs -> R.Option (Some (
        R.List (List.map (map_macro_def_content env) xs)
      ))
    | None -> R.Option None)
  in
  let v4 = (* "end" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_macro_def_content (env : env) (x : CST.macro_def_content) =
  (match x with
  | `Macro_node x -> R.Case ("Macro_node",
      map_macro_node env x
    )
  | `Macro_var x -> R.Case ("Macro_var",
      map_macro_var env x
    )
  | `Macro_def_lit_content x -> R.Case ("Macro_def_lit_content",
      map_macro_def_literal_content env x
    )
  | `Term x -> R.Case ("Term",
      map_terminator env x
    )
  )

and map_macro_def_literal_content (env : env) (x : CST.macro_def_literal_content) =
  (match x with
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Macro_content_nest tok -> R.Case ("Macro_content_nest",
      (* macro_content_nesting *) token env tok
    )
  | `Choice_pat_b22a502_rep_macro_def_content_end (v1, v2, v3) -> R.Case ("Choice_pat_b22a502_rep_macro_def_content_end",
      let v1 =
        (match v1 with
        | `Pat_b22a502 x -> R.Case ("Pat_b22a502",
            map_pat_b22a502 env x
          )
        | `Pat_6ebc02d x -> R.Case ("Pat_6ebc02d",
            map_pat_6ebc02d env x
          )
        | `Anno tok -> R.Case ("Anno",
            (* "annotation" *) token env tok
          )
        | `Begin tok -> R.Case ("Begin",
            (* "begin" *) token env tok
          )
        | `Case tok -> R.Case ("Case",
            (* "case" *) token env tok
          )
        | `Class tok -> R.Case ("Class",
            (* "class" *) token env tok
          )
        | `Def tok -> R.Case ("Def",
            (* "def" *) token env tok
          )
        | `Do tok -> R.Case ("Do",
            (* "do" *) token env tok
          )
        | `Enum tok -> R.Case ("Enum",
            (* "enum" *) token env tok
          )
        | `Fun tok -> R.Case ("Fun",
            (* "fun" *) token env tok
          )
        | `If tok -> R.Case ("If",
            (* "if" *) token env tok
          )
        | `Lib tok -> R.Case ("Lib",
            (* "lib" *) token env tok
          )
        | `Macro tok -> R.Case ("Macro",
            (* "macro" *) token env tok
          )
        | `Module tok -> R.Case ("Module",
            (* "module" *) token env tok
          )
        | `Select tok -> R.Case ("Select",
            (* "select" *) token env tok
          )
        | `Struct tok -> R.Case ("Struct",
            (* "struct" *) token env tok
          )
        | `Union tok -> R.Case ("Union",
            (* "union" *) token env tok
          )
        | `Unless tok -> R.Case ("Unless",
            (* "unless" *) token env tok
          )
        | `Until tok -> R.Case ("Until",
            (* "until" *) token env tok
          )
        | `While tok -> R.Case ("While",
            (* "while" *) token env tok
          )
        )
      in
      let v2 = R.List (List.map (map_macro_def_content env) v2) in
      let v3 = (* "end" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_macro_else (env : env) ((v1, v2) : CST.macro_else) =
  let v1 = map_macro_else_keyword env v1 in
  let v2 = R.List (List.map (map_macro_content_ env) v2) in
  R.Tuple [v1; v2]

and map_macro_elsif_cond (env : env) ((v1, v2, v3, v4) : CST.macro_elsif_cond) =
  let v1 = (* macro_delimiter_elsif *) token env v1 in
  let v2 = (* "elsif" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "%}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_macro_expression (env : env) ((v1, v2, v3) : CST.macro_expression) =
  let v1 = (* "{{" *) token env v1 in
  let v2 =
    (match v2 with
    | `Splat x -> R.Case ("Splat",
        map_splat env x
      )
    | `Double_splat x -> R.Case ("Double_splat",
        map_double_splat env x
      )
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Inline_stmt x -> R.Case ("Inline_stmt",
        map_inline_statement env x
      )
    )
  in
  let v3 = (* "}}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_macro_for_expr (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.macro_for_expr) =
  let v1 = (* "{%" *) token env v1 in
  let v2 = (* "for" *) token env v2 in
  let v3 = map_anon_choice_unde_76e38d1 env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_unde_76e38d1 env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* "in" *) token env v5 in
  let v6 =
    (match v6 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Splat x -> R.Case ("Splat",
        map_splat env x
      )
    | `Double_splat x -> R.Case ("Double_splat",
        map_double_splat env x
      )
    )
  in
  let v7 = (* "%}" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_macro_if_cond (env : env) ((v1, v2, v3, v4) : CST.macro_if_cond) =
  let v1 = (* "{%" *) token env v1 in
  let v2 = (* regular_if_keyword *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "%}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_macro_literal_content (env : env) (x : CST.macro_literal_content) =
  (match x with
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Macro_content tok -> R.Case ("Macro_content",
      (* macro_content *) token env tok
    )
  )

and map_macro_node (env : env) (x : CST.macro_node) =
  (match x with
  | `Macro_exp x -> R.Case ("Macro_exp",
      map_macro_expression env x
    )
  | `Macro_stmt (v1, v2, v3) -> R.Case ("Macro_stmt",
      let v1 = (* "{%" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_statements env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "%}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Macro_begin (v1, v2, v3) -> R.Case ("Macro_begin",
      let v1 = map_macro_begin_keyword env v1 in
      let v2 = R.List (List.map (map_macro_content_ env) v2) in
      let v3 = map_macro_end_keyword env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Macro_if (v1, v2, v3, v4) -> R.Case ("Macro_if",
      let v1 = map_macro_if_cond env v1 in
      let v2 = R.List (List.map (map_macro_content_ env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_macro_elsif_8f09864 env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_macro_end_keyword env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Macro_unless (v1, v2, v3, v4) -> R.Case ("Macro_unless",
      let v1 = map_macro_unless_cond env v1 in
      let v2 = R.List (List.map (map_macro_content_ env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_macro_else env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_macro_end_keyword env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Macro_for (v1, v2, v3) -> R.Case ("Macro_for",
      let v1 = map_macro_for_expr env v1 in
      let v2 = R.List (List.map (map_macro_content_ env) v2) in
      let v3 = map_macro_end_keyword env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Macro_verb (v1, v2, v3) -> R.Case ("Macro_verb",
      let v1 = map_macro_verbatim_keyword env v1 in
      let v2 = R.List (List.map (map_macro_content_ env) v2) in
      let v3 = map_macro_end_keyword env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_macro_signature (env : env) ((v1, v2, v3) : CST.macro_signature) =
  let v1 = (* "macro" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* semgrep_metavariable *) token env tok
      )
    | `Id_meth_call tok -> R.Case ("Id_meth_call",
        (* identifier_method_call *) token env tok
      )
    | `Op_tok x -> R.Case ("Op_tok",
        map_operator_token env x
      )
    | `BQUOT tok -> R.Case ("BQUOT",
        (* "`" *) token env tok
      )
    )
  in
  let v3 =
    (match v3 with
    | `LPAR_opt_param_list_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_param_list_RPAR",
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_param_list env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Term x -> R.Case ("Term",
        map_terminator env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_macro_unless_cond (env : env) ((v1, v2, v3, v4) : CST.macro_unless_cond) =
  let v1 = (* "{%" *) token env v1 in
  let v2 = (* regular_unless_keyword *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "%}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_macro_var (env : env) ((v1, v2) : CST.macro_var) =
  let v1 = map_tok_perc_pat_fdcd585_rep_pat_fdcd585 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4, v5, v6) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* start_of_macro_var_exps *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = map_imm_tok_lcurl env v2 in
        let v3 = map_expression env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        let v5 =
          (match v5 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        let v6 = (* "}" *) token env v6 in
        R.Tuple [v1; v2; v3; v4; v5; v6]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_method_def (env : env) ((v1, v2, v3, v4, v5) : CST.method_def) =
  let v1 = map_base_method_def env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_rescue_else_ensure env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_method_proc (env : env) ((v1, v2, v3) : CST.method_proc) =
  let v1 = (* "->" *) token env v1 in
  let v2 =
    (match v2 with
    | `Choice_id_DOT_choice_id (v1, v2, v3) -> R.Case ("Choice_id_DOT_choice_id",
        let v1 =
          (match v1 with
          | `Id tok -> R.Case ("Id",
              (* identifier *) token env tok
            )
          | `Inst_var tok -> R.Case ("Inst_var",
              (* instance_var *) token env tok
            )
          | `Class_var tok -> R.Case ("Class_var",
              (* class_var *) token env tok
            )
          | `Self tok -> R.Case ("Self",
              (* "self" *) token env tok
            )
          | `Cst x -> R.Case ("Cst",
              map_constant env x
            )
          )
        in
        let v2 = (* "." *) token env v2 in
        let v3 =
          (match v3 with
          | `Id tok -> R.Case ("Id",
              (* identifier *) token env tok
            )
          | `Id_meth_call tok -> R.Case ("Id_meth_call",
              (* identifier_method_call *) token env tok
            )
          | `Id_assign tok -> R.Case ("Id_assign",
              (* identifier_assign *) token env tok
            )
          | `Op_tok x -> R.Case ("Op_tok",
              map_operator_token env x
            )
          )
        in
        R.Tuple [v1; v2; v3]
      )
    | `Choice_global_meth x -> R.Case ("Choice_global_meth",
        (match x with
        | `Global_meth x -> R.Case ("Global_meth",
            map_global_method env x
          )
        | `Choice_id x -> R.Case ("Choice_id",
            map_anon_choice_id_910ec16 env x
          )
        )
      )
    )
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_type_instance_param_list env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_module_def (env : env) ((v1, v2, v3, v4) : CST.module_def) =
  let v1 = (* "module" *) token env v1 in
  let v2 = map_anon_choice_semg_meta_ed0b729 env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "end" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_multi_assign (env : env) (x : CST.multi_assign) =
  (match x with
  | `Lhs_splat_EQ_exp (v1, v2, v3) -> R.Case ("Lhs_splat_EQ_exp",
      let v1 = map_lhs_splat env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rep1_choice_choice_unde_COMMA_choice_choice_unde_EQ_exp (v1, v2, v3, v4) -> R.Case ("Rep1_choice_choice_unde_COMMA_choice_choice_unde_EQ_exp",
      let v1 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_anon_choice_choice_unde_01ec478 env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v1)
      in
      let v2 = map_anon_choice_choice_unde_01ec478 env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Lhs_splat_EQ_rep1_exp_COMMA_exp (v1, v2, v3, v4) -> R.Case ("Lhs_splat_EQ_rep1_exp_COMMA_exp",
      let v1 = map_lhs_splat env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Rep1_choice_choice_unde_COMMA_choice_choice_unde_EQ_rep1_exp_COMMA_exp (v1, v2, v3, v4, v5) -> R.Case ("Rep1_choice_choice_unde_COMMA_choice_choice_unde_EQ_rep1_exp_COMMA_exp",
      let v1 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_anon_choice_choice_unde_01ec478 env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v1)
      in
      let v2 = map_anon_choice_choice_unde_01ec478 env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_expression env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_multiplicative_operator (env : env) ((v1, v2, v3) : CST.multiplicative_operator) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `Bin_star tok -> R.Case ("Bin_star",
        (* binary_star *) token env tok
      )
    | `AMPSTAR tok -> R.Case ("AMPSTAR",
        (* "&*" *) token env tok
      )
    | `Bin_slash tok -> R.Case ("Bin_slash",
        (* binary_slash *) token env tok
      )
    | `Bin_double_slash tok -> R.Case ("Bin_double_slash",
        (* binary_double_slash *) token env tok
      )
    | `Modulo_op tok -> R.Case ("Modulo_op",
        (* modulo_operator *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_named_expr (env : env) (x : CST.named_expr) =
  (match x with
  | `Choice_id_imm_tok_colon_choice_exp (v1, v2, v3) -> R.Case ("Choice_id_imm_tok_colon_choice_exp",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `With tok -> R.Case ("With",
            (* "with" *) token env tok
          )
        | `Cst_segm tok -> R.Case ("Cst_segm",
            (* constant_segment *) token env tok
          )
        | `Id_meth_call tok -> R.Case ("Id_meth_call",
            (* identifier_method_call *) token env tok
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        | `Str_perc_lit x -> R.Case ("Str_perc_lit",
            map_string_percent_literal env x
          )
        )
      in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = map_anon_choice_exp_373cb08 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Kw_named_arg_name_choice_exp (v1, v2) -> R.Case ("Kw_named_arg_name_choice_exp",
      let v1 = (* keyword_named_argument_name *) token env v1 in
      let v2 = map_anon_choice_exp_373cb08 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_named_tuple (env : env) ((v1, v2, v3, v4, v5) : CST.named_tuple) =
  let v1 = (* start_of_named_tuple *) token env v1 in
  let v2 = map_named_expr env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_named_expr env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_named_tuple_type (env : env) ((v1, v2, v3, v4, v5) : CST.named_tuple_type) =
  let v1 = (* start_of_named_tuple_type *) token env v1 in
  let v2 = map_named_type env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_named_type env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_named_type (env : env) ((v1, v2, v3) : CST.named_type) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Cst_segm tok -> R.Case ("Cst_segm",
        (* constant_segment *) token env tok
      )
    | `Id_meth_call tok -> R.Case ("Id_meth_call",
        (* identifier_method_call *) token env tok
      )
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    | `Str_perc_lit x -> R.Case ("Str_perc_lit",
        map_string_percent_literal env x
      )
    )
  in
  let v2 = map_imm_tok_colon env v2 in
  let v3 = map_bare_type env v3 in
  R.Tuple [v1; v2; v3]

and map_nilable_constant (env : env) ((v1, v2) : CST.nilable_constant) =
  let v1 =
    (match v1 with
    | `Cst x -> R.Case ("Cst",
        map_constant env x
      )
    | `Nila_cst x -> R.Case ("Nila_cst",
        map_nilable_constant env x
      )
    | `Gene_inst_type x -> R.Case ("Gene_inst_type",
        map_generic_instance_type env x
      )
    )
  in
  let v2 = map_imm_tok_qmark env v2 in
  R.Tuple [v1; v2]

and map_nilable_type (env : env) ((v1, v2) : CST.nilable_type) =
  let v1 = map_type_ env v1 in
  let v2 = (* "?" *) token env v2 in
  R.Tuple [v1; v2]

and map_no_args_proc_type (env : env) ((v1, v2) : CST.no_args_proc_type) =
  let v1 = (* "->" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_not (env : env) ((v1, v2) : CST.not) =
  let v1 = (* "!" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_numeric_type (env : env) (x : CST.numeric_type) =
  (match x with
  | `Int x -> R.Case ("Int",
      map_integer env x
    )
  | `Float x -> R.Case ("Float",
      map_float_ env x
    )
  | `Sizeof x -> R.Case ("Sizeof",
      map_sizeof env x
    )
  | `Inst_sizeof x -> R.Case ("Inst_sizeof",
      map_instance_sizeof env x
    )
  | `Alig x -> R.Case ("Alig",
      map_alignof env x
    )
  | `Inst_alig x -> R.Case ("Inst_alig",
      map_instance_alignof env x
    )
  | `Offs x -> R.Case ("Offs",
      map_offsetof env x
    )
  )

and map_offsetof (env : env) ((v1, v2, v3, v4, v5, v6) : CST.offsetof) =
  let v1 = (* "offsetof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_bare_type env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 =
    (match v5 with
    | `Inst_var tok -> R.Case ("Inst_var",
        (* instance_var *) token env tok
      )
    | `Int x -> R.Case ("Int",
        map_integer env x
      )
    )
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_operator_assign (env : env) ((v1, v2, v3) : CST.operator_assign) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* semgrep_metavariable *) token env tok
      )
    | `Inst_var tok -> R.Case ("Inst_var",
        (* instance_var *) token env tok
      )
    | `Class_var tok -> R.Case ("Class_var",
        (* class_var *) token env tok
      )
    | `Macro_var x -> R.Case ("Macro_var",
        map_macro_var env x
      )
    | `Assign_call x -> R.Case ("Assign_call",
        map_assign_call env x
      )
    | `Index_call x -> R.Case ("Index_call",
        map_index_call env x
      )
    | `Index_op x -> R.Case ("Index_op",
        map_index_operator env x
      )
    )
  in
  let v2 =
    (match v2 with
    | `PLUSEQ tok -> R.Case ("PLUSEQ",
        (* "+=" *) token env tok
      )
    | `AMPPLUSEQ tok -> R.Case ("AMPPLUSEQ",
        (* "&+=" *) token env tok
      )
    | `DASHEQ tok -> R.Case ("DASHEQ",
        (* "-=" *) token env tok
      )
    | `AMPDASHEQ tok -> R.Case ("AMPDASHEQ",
        (* "&-=" *) token env tok
      )
    | `STAREQ tok -> R.Case ("STAREQ",
        (* "*=" *) token env tok
      )
    | `AMPSTAREQ tok -> R.Case ("AMPSTAREQ",
        (* "&*=" *) token env tok
      )
    | `SLASHEQ tok -> R.Case ("SLASHEQ",
        (* "/=" *) token env tok
      )
    | `SLASHSLASHEQ tok -> R.Case ("SLASHSLASHEQ",
        (* "//=" *) token env tok
      )
    | `PERCEQ tok -> R.Case ("PERCEQ",
        (* "%=" *) token env tok
      )
    | `BAREQ tok -> R.Case ("BAREQ",
        (* "|=" *) token env tok
      )
    | `AMPEQ tok -> R.Case ("AMPEQ",
        (* "&=" *) token env tok
      )
    | `HATEQ tok -> R.Case ("HATEQ",
        (* "^=" *) token env tok
      )
    | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
        (* "**=" *) token env tok
      )
    | `LTLTEQ tok -> R.Case ("LTLTEQ",
        (* "<<=" *) token env tok
      )
    | `GTGTEQ tok -> R.Case ("GTGTEQ",
        (* ">>=" *) token env tok
      )
    | `BARBAREQ tok -> R.Case ("BARBAREQ",
        (* "||=" *) token env tok
      )
    | `AMPAMPEQ tok -> R.Case ("AMPAMPEQ",
        (* "&&=" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_or_ (env : env) ((v1, v2, v3) : CST.or_) =
  let v1 = map_expression env v1 in
  let v2 = (* "||" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_out (env : env) ((v1, v2) : CST.out) =
  let v1 = (* "out" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Inst_var tok -> R.Case ("Inst_var",
        (* instance_var *) token env tok
      )
    | `Unde tok -> R.Case ("Unde",
        (* "_" *) token env tok
      )
    | `Macro_var x -> R.Case ("Macro_var",
        map_macro_var env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_param (env : env) ((v1, v2, v3, v4, v5) : CST.param) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_choice_id_2655c0e env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* type_field_colon *) token env v1 in
        let v2 = map_bare_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_param_list (env : env) (x : CST.param_list) =
  (match x with
  | `Choice_param_rep_COMMA_choice_param_opt_COMMA_opt_blk_param (v1, v2, v3) -> R.Case ("Choice_param_rep_COMMA_choice_param_opt_COMMA_opt_blk_param",
      let v1 = map_anon_choice_param_1e36750 env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_anon_choice_param_1e36750 env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_block_param env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Blk_param x -> R.Case ("Blk_param",
      map_block_param env x
    )
  )

and map_parenthesized_expressions (env : env) ((v1, v2, v3) : CST.parenthesized_expressions) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_statements env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_proc_type (env : env) ((v1, v2, v3, v4, v5) : CST.parenthesized_proc_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Spla_type_rep1_COMMA_spla_type_opt_COMMA (v1, v2, v3) -> R.Case ("Spla_type_rep1_COMMA_spla_type_opt_COMMA",
        let v1 = map_splattable_type env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_splattable_type env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Bare_type_COMMA (v1, v2) -> R.Case ("Bare_type_COMMA",
        let v1 = map_bare_type env v1 in
        let v2 = (* "," *) token env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  let v4 = (* "->" *) token env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_bare_type env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pointer_type (env : env) ((v1, v2) : CST.pointer_type) =
  let v1 = map_type_ env v1 in
  let v2 = (* pointer_star *) token env v2 in
  R.Tuple [v1; v2]

and map_pointerof (env : env) ((v1, v2, v3, v4) : CST.pointerof) =
  let v1 = (* "pointerof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_proc (env : env) ((v1, v2, v3, v4) : CST.proc) =
  let v1 = (* "->" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_proc_param_list env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_pat_5eb9c21 env v1 in
        let v2 = map_bare_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | `Do_end_blk x -> R.Case ("Do_end_blk",
        map_do_end_block env x
      )
    | `Brace_blk x -> R.Case ("Brace_blk",
        map_brace_block env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_proc_param_list (env : env) ((v1, v2, v3) : CST.proc_param_list) =
  let v1 = map_param env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_param env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_property_argument_list (env : env) (x : CST.property_argument_list) =
  (match x with
  | `Type_decl x -> R.Case ("Type_decl",
      map_type_declaration env x
    )
  | `Kw_type_decl (v1, v2, v3, v4, v5) -> R.Case ("Kw_type_decl",
      let v1 =
        (match v1 with
        | `Else tok -> R.Case ("Else",
            (* "else" *) token env tok
          )
        | `Then tok -> R.Case ("Then",
            (* "then" *) token env tok
          )
        | `Ensure tok -> R.Case ("Ensure",
            (* "ensure" *) token env tok
          )
        | `Rescue tok -> R.Case ("Rescue",
            (* "rescue" *) token env tok
          )
        )
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_imm_tok_pat_dcdac4f env v3 in
      let v4 = map_bare_type env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Prop_assign_decl (v1, v2, v3) -> R.Case ("Prop_assign_decl",
      let v1 = map_anon_choice_id_0dd119e env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prop_bare_decl v1 -> R.Case ("Prop_bare_decl",
      map_anon_choice_id_0dd119e env v1
    )
  | `Unqu_symb x -> R.Case ("Unqu_symb",
      map_unquoted_symbol env x
    )
  | `Quoted_symb x -> R.Case ("Quoted_symb",
      map_quoted_symbol env x
    )
  | `Op_symb x -> R.Case ("Op_symb",
      map_operator_symbol env x
    )
  )

and map_pseudo_call (env : env) (x : CST.pseudo_call) =
  (match x with
  | `Opt_exp_DOT_choice_as_pseudo_call_arg_list (v1, v2, v3) -> R.Case ("Opt_exp_DOT_choice_as_pseudo_call_arg_list",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_expression env v1 in
            let v2 = (* "." *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `As tok -> R.Case ("As",
            (* "as" *) token env tok
          )
        | `AsQM tok -> R.Case ("AsQM",
            (* "as?" *) token env tok
          )
        | `Is_aQMARK tok -> R.Case ("Is_aQMARK",
            (* "is_a?" *) token env tok
          )
        )
      in
      let v3 = map_pseudo_call_argument_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_exp_DOT_nilQ_opt_LPAR_RPAR (v1, v2, v3) -> R.Case ("Opt_exp_DOT_nilQ_opt_LPAR_RPAR",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_expression env v1 in
            let v2 = (* "." *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 = (* "nil?" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_empty_parens env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_exp_DOT_respos_toQM_pseudo_respos_to_arg_list (v1, v2, v3) -> R.Case ("Opt_exp_DOT_respos_toQM_pseudo_respos_to_arg_list",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_expression env v1 in
            let v2 = (* "." *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 = (* "responds_to?" *) token env v2 in
      let v3 = map_pseudo_responds_to_argument_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_pseudo_call_argument_list (env : env) (x : CST.pseudo_call_argument_list) =
  (match x with
  | `LPAR_bare_type_RPAR x -> R.Case ("LPAR_bare_type_RPAR",
      map_parenthesized_type env x
    )
  | `Bare_type x -> R.Case ("Bare_type",
      map_bare_type env x
    )
  )

and map_range (env : env) ((v1, v2, v3, v4) : CST.range) =
  let v1 = map_expression env v1 in
  let v2 = map_anon_choice_DOTDOT_ed078ec env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* end_of_range *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_regex (env : env) ((v1, v2, v3, v4) : CST.regex) =
  let v1 = (* regex_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_regex_literal_content env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_imm_tok_slash env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* regex_modifier *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_regex_literal_content (env : env) (xs : CST.regex_literal_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Interp x -> R.Case ("Interp",
        map_interpolation env x
      )
    | `Regex_lit_content_ x -> R.Case ("Regex_lit_content_",
        map_regex_literal_content_ env x
      )
    )
  ) xs)

and map_regex_percent_literal (env : env) ((v1, v2, v3, v4) : CST.regex_percent_literal) =
  let v1 = (* regex_percent_literal_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_regex_percent_literal_content env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* percent_literal_end *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* regex_modifier *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_regex_percent_literal_content (env : env) (xs : CST.regex_percent_literal_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Interp x -> R.Case ("Interp",
        map_interpolation env x
      )
    | `Deli_str_content tok -> R.Case ("Deli_str_content",
        (* delimited_string_contents *) token env tok
      )
    )
  ) xs)

and map_rescue (env : env) ((v1, v2, v3, v4) : CST.rescue) =
  let v1 = (* regular_rescue_keyword *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Id_pat_5eb9c21_bare_type (v1, v2, v3) -> R.Case ("Id_pat_5eb9c21_bare_type",
            let v1 = (* identifier *) token env v1 in
            let v2 = map_pat_5eb9c21 env v2 in
            let v3 = map_bare_type env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Bare_type x -> R.Case ("Bare_type",
            map_bare_type env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = map_terminator env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_rescue_else_ensure (env : env) (x : CST.rescue_else_ensure) =
  (match x with
  | `Rep1_rescue_opt_else_opt_ensure (v1, v2, v3) -> R.Case ("Rep1_rescue_opt_else_opt_ensure",
      let v1 = R.List (List.map (map_rescue env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_else_ env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_ensure env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Rep_rescue_else_opt_ensure (v1, v2, v3) -> R.Case ("Rep_rescue_else_opt_ensure",
      let v1 = R.List (List.map (map_rescue env) v1) in
      let v2 = map_else_ env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_ensure env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Rep_rescue_opt_else_ensure (v1, v2, v3) -> R.Case ("Rep_rescue_opt_else_ensure",
      let v1 = R.List (List.map (map_rescue env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_else_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_ensure env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_select (env : env) ((v1, v2, v3, v4) : CST.select) =
  let v1 = (* "select" *) token env v1 in
  let v2 = R.List (List.map (map_when_ env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_else_ env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "end" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_shift_operator (env : env) ((v1, v2, v3) : CST.shift_operator) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `LTLT tok -> R.Case ("LTLT",
        (* "<<" *) token env tok
      )
    | `GTGT tok -> R.Case ("GTGT",
        (* ">>" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_sizeof (env : env) ((v1, v2, v3, v4) : CST.sizeof) =
  let v1 = (* "sizeof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_bare_type env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_splat (env : env) ((v1, v2) : CST.splat) =
  let v1 = (* unary_star *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_splattable_type (env : env) (x : CST.splattable_type) =
  (match x with
  | `Double_splat_type (v1, v2) -> R.Case ("Double_splat_type",
      let v1 = (* unary_double_star *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Splat_type (v1, v2) -> R.Case ("Splat_type",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Inline_stmt x -> R.Case ("Inline_stmt",
      map_inline_statement env x
    )
  | `Const_assign x -> R.Case ("Const_assign",
      map_const_assign env x
    )
  | `Multi_assign x -> R.Case ("Multi_assign",
      map_multi_assign env x
    )
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  | `Anno_def (v1, v2, v3, v4) -> R.Case ("Anno_def",
      let v1 = (* "annotation" *) token env v1 in
      let v2 = map_constant env v2 in
      let v3 = R.List (List.map (map_terminator env) v3) in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Module_def x -> R.Case ("Module_def",
      map_module_def env x
    )
  | `Class_def x -> R.Case ("Class_def",
      map_class_def env x
    )
  | `Struct_def x -> R.Case ("Struct_def",
      map_struct_def env x
    )
  | `Enum_def x -> R.Case ("Enum_def",
      map_enum_def env x
    )
  | `Lib_def x -> R.Case ("Lib_def",
      map_lib_def env x
    )
  | `Alias x -> R.Case ("Alias",
      map_alias env x
    )
  | `Meth_def x -> R.Case ("Meth_def",
      map_method_def env x
    )
  | `Abst_meth_def x -> R.Case ("Abst_meth_def",
      map_abstract_method_def env x
    )
  | `Macro_def x -> R.Case ("Macro_def",
      map_macro_def env x
    )
  | `Top_level_fun_def (v1, v2, v3) -> R.Case ("Top_level_fun_def",
      let v1 = map_base_fun_def env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_statements env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "end" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Visi_modi x -> R.Case ("Visi_modi",
      map_visibility_modifier env x
    )
  | `Requ (v1, v2) -> R.Case ("Requ",
      let v1 = (* "require" *) token env v1 in
      let v2 = map_string_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Incl x -> R.Case ("Incl",
      map_include_ env x
    )
  | `Extend (v1, v2) -> R.Case ("Extend",
      let v1 = (* "extend" *) token env v1 in
      let v2 = map_anon_choice_cst_092cda1 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_statements (env : env) (x : CST.statements) =
  (match x with
  | `Rep1_choice_stmt_term_opt_stmt (v1, v2) -> R.Case ("Rep1_choice_stmt_term_opt_stmt",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Stmt_term (v1, v2) -> R.Case ("Stmt_term",
              let v1 = map_statement env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            )
          | `SEMI tok -> R.Case ("SEMI",
              (* ";" *) token env tok
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_statement env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Stmt x -> R.Case ("Stmt",
      map_statement env x
    )
  )

and map_static_array_type (env : env) ((v1, v2, v3, v4) : CST.static_array_type) =
  let v1 = map_type_ env v1 in
  let v2 = (* start_of_index_operator *) token env v2 in
  let v3 =
    (match v3 with
    | `Cst x -> R.Case ("Cst",
        map_constant env x
      )
    | `Nume_type x -> R.Case ("Nume_type",
        map_numeric_type env x
      )
    )
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* string_literal_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_string_literal_content env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_literal_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_string_literal_content (env : env) (xs : CST.string_literal_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Line_cont_expl x -> R.Case ("Line_cont_expl",
        map_line_continuation_explicit env x
      )
    | `Deli_str_content tok -> R.Case ("Deli_str_content",
        (* delimited_string_contents *) token env tok
      )
    | `Str_esc_seq tok -> R.Case ("Str_esc_seq",
        (* string_escape_sequence *) token env tok
      )
    | `Igno_back tok -> R.Case ("Igno_back",
        (* ignored_backslash *) token env tok
      )
    | `Interp x -> R.Case ("Interp",
        map_interpolation env x
      )
    )
  ) xs)

and map_string_percent_literal (env : env) ((v1, v2, v3) : CST.string_percent_literal) =
  let v1 = (* string_percent_literal_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_string_percent_literal_content env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* percent_literal_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_string_percent_literal_content (env : env) (xs : CST.string_percent_literal_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Deli_str_content tok -> R.Case ("Deli_str_content",
        (* delimited_string_contents *) token env tok
      )
    | `Interp x -> R.Case ("Interp",
        map_interpolation env x
      )
    | `Str_esc_seq tok -> R.Case ("Str_esc_seq",
        (* string_escape_sequence *) token env tok
      )
    | `Igno_back tok -> R.Case ("Igno_back",
        (* ignored_backslash *) token env tok
      )
    )
  ) xs)

and map_struct_def (env : env) ((v1, v2, v3, v4, v5, v6) : CST.struct_def) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "abstract" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "struct" *) token env v2 in
  let v3 = map_anon_choice_semg_meta_ed0b729 env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "<" *) token env v1 in
        let v2 = map_anon_choice_semg_meta_de6d985 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_then_ (env : env) (v1 : CST.then_) =
  map_statements env v1

and map_tuple (env : env) ((v1, v2, v3, v4, v5) : CST.tuple) =
  let v1 = (* start_of_hash_or_tuple *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_tuple_type (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_type) =
  let v1 = (* start_of_tuple_type *) token env v1 in
  let v2 = map_splattable_type env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_splattable_type env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Choice_paren_type x -> R.Case ("Choice_paren_type",
      (match x with
      | `Paren_type x -> R.Case ("Paren_type",
          map_parenthesized_type env x
        )
      | `Cst x -> R.Case ("Cst",
          map_constant env x
        )
      | `Gene_inst_type x -> R.Case ("Gene_inst_type",
          map_generic_instance_type env x
        )
      | `Union_type x -> R.Case ("Union_type",
          map_union_type env x
        )
      | `Tuple_type x -> R.Case ("Tuple_type",
          map_tuple_type env x
        )
      | `Named_tuple_type x -> R.Case ("Named_tuple_type",
          map_named_tuple_type env x
        )
      | `No_args_proc_type x -> R.Case ("No_args_proc_type",
          map_no_args_proc_type env x
        )
      | `Paren_proc_type x -> R.Case ("Paren_proc_type",
          map_parenthesized_proc_type env x
        )
      | `Class_type x -> R.Case ("Class_type",
          map_class_type env x
        )
      | `Unde tok -> R.Case ("Unde",
          (* "_" *) token env tok
        )
      | `Nila_type x -> R.Case ("Nila_type",
          map_nilable_type env x
        )
      | `Poin_type x -> R.Case ("Poin_type",
          map_pointer_type env x
        )
      | `Self tok -> R.Case ("Self",
          (* "self" *) token env tok
        )
      | `Typeof x -> R.Case ("Typeof",
          map_typeof env x
        )
      | `Static_array_type x -> R.Case ("Static_array_type",
          map_static_array_type env x
        )
      )
    )
  )

and map_type_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.type_declaration) =
  let v1 = map_anon_choice_id_2655c0e env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_imm_tok_pat_dcdac4f env v3 in
  let v4 = map_bare_type env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_instance_param_list (env : env) ((v1, v2) : CST.type_instance_param_list) =
  let v1 =
    (match v1 with
    | `Choice_bare_type_rep_COMMA_choice_bare_type (v1, v2) -> R.Case ("Choice_bare_type_rep_COMMA_choice_bare_type",
        let v1 = map_anon_choice_bare_type_7b27a0b env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_bare_type_7b27a0b env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      )
    | `Named_type_rep_COMMA_named_type (v1, v2) -> R.Case ("Named_type_rep_COMMA_named_type",
        let v1 = map_named_type env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_named_type env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_type_param_list (env : env) ((v1, v2, v3) : CST.type_param_list) =
  let v1 = map_anon_choice_cst_10f7578 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_cst_10f7578 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_typeof (env : env) ((v1, v2, v3, v4, v5, v6) : CST.typeof) =
  let v1 = (* "typeof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_unary_additive_operator (env : env) ((v1, v2) : CST.unary_additive_operator) =
  let v1 =
    (match v1 with
    | `Un_plus tok -> R.Case ("Un_plus",
        (* unary_plus *) token env tok
      )
    | `Un_minus tok -> R.Case ("Un_minus",
        (* unary_minus *) token env tok
      )
    | `Un_wrap_plus tok -> R.Case ("Un_wrap_plus",
        (* unary_wrapping_plus *) token env tok
      )
    | `Un_wrap_minus tok -> R.Case ("Un_wrap_minus",
        (* unary_wrapping_minus *) token env tok
      )
    )
  in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_uninitialized_assign (env : env) ((v1, v2, v3) : CST.uninitialized_assign) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* semgrep_metavariable *) token env tok
      )
    | `Inst_var tok -> R.Case ("Inst_var",
        (* instance_var *) token env tok
      )
    | `Class_var tok -> R.Case ("Class_var",
        (* class_var *) token env tok
      )
    | `Global_var x -> R.Case ("Global_var",
        map_global_var env x
      )
    | `Macro_var x -> R.Case ("Macro_var",
        map_macro_var env x
      )
    )
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_uninitialized_var env v3 in
  R.Tuple [v1; v2; v3]

and map_uninitialized_var (env : env) ((v1, v2) : CST.uninitialized_var) =
  let v1 = (* "uninitialized" *) token env v1 in
  let v2 = map_bare_type env v2 in
  R.Tuple [v1; v2]

and map_union_expression (env : env) (x : CST.union_expression) =
  (match x with
  | `Incl x -> R.Case ("Incl",
      map_include_ env x
    )
  | `Union_fields (v1, v2, v3, v4) -> R.Case ("Union_fields",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = (* identifier *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* type_field_colon *) token env v3 in
      let v4 = map_bare_type env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_union_expressions (env : env) (x : CST.union_expressions) =
  (match x with
  | `Rep1_choice_union_exp_term_opt_union_exp (v1, v2) -> R.Case ("Rep1_choice_union_exp_term_opt_union_exp",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Union_exp_term (v1, v2) -> R.Case ("Union_exp_term",
              let v1 = map_union_expression env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            )
          | `SEMI tok -> R.Case ("SEMI",
              (* ";" *) token env tok
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_union_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Union_exp x -> R.Case ("Union_exp",
      map_union_expression env x
    )
  )

and map_union_type (env : env) ((v1, v2) : CST.union_type) =
  let v1 = map_type_ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_unless (env : env) ((v1, v2, v3, v4, v5, v6) : CST.unless) =
  let v1 = (* regular_unless_keyword *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_terminator env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_then_ env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_else_ env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* "end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_until (env : env) ((v1, v2, v3, v4, v5) : CST.until) =
  let v1 = (* "until" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_terminator env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_visibility_modifier (env : env) ((v1, v2) : CST.visibility_modifier) =
  let v1 =
    (match v1 with
    | `Priv tok -> R.Case ("Priv",
        (* "private" *) token env tok
      )
    | `Prot tok -> R.Case ("Prot",
        (* "protected" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | `Call x -> R.Case ("Call",
        map_call env x
      )
    | `Module_def x -> R.Case ("Module_def",
        map_module_def env x
      )
    | `Class_def x -> R.Case ("Class_def",
        map_class_def env x
      )
    | `Struct_def x -> R.Case ("Struct_def",
        map_struct_def env x
      )
    | `Enum_def x -> R.Case ("Enum_def",
        map_enum_def env x
      )
    | `Lib_def x -> R.Case ("Lib_def",
        map_lib_def env x
      )
    | `Meth_def x -> R.Case ("Meth_def",
        map_method_def env x
      )
    | `Abst_meth_def x -> R.Case ("Abst_meth_def",
        map_abstract_method_def env x
      )
    | `Macro_def x -> R.Case ("Macro_def",
        map_macro_def env x
      )
    | `Const_assign x -> R.Case ("Const_assign",
        map_const_assign env x
      )
    | `Alias x -> R.Case ("Alias",
        map_alias env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_when_ (env : env) ((v1, v2, v3, v4, v5) : CST.when_) =
  let v1 = (* "when" *) token env v1 in
  let v2 = map_anon_choice_exp_5f47da4 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_5f47da4 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_anon_choice_then_eac33c8 env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_while_ (env : env) ((v1, v2, v3, v4, v5) : CST.while_) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_terminator env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_yield (env : env) ((v1, v2, v3) : CST.yield) =
  let v1 =
    (match v1 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "with" *) token env v1 in
        let v2 = map_expression env v2 in
        let v3 = (* end_of_with_expression *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v2 = (* "yield" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_control_expressions env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_expressions (env : env) (v1 : CST.expressions) =
  (match v1 with
  | Some x -> R.Option (Some (
      map_statements env x
    ))
  | None -> R.Option None)

let map_loc_pragma_pop (env : env) (tok : CST.loc_pragma_pop) =
  (* loc_pragma_pop *) token env tok

let map_loc_pragma_push (env : env) (tok : CST.loc_pragma_push) =
  (* loc_pragma_push *) token env tok

let map_loc_pragma_location (env : env) (tok : CST.loc_pragma_location) =
  (* loc_pragma_location *) token env tok

let map_comment (env : env) (tok : CST.comment) =
  (* pattern #.* *) token env tok

let map_heredoc_body (env : env) ((v1, v2, v3) : CST.heredoc_body) =
  let v1 = (* heredoc_body_start *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Here_content tok -> R.Case ("Here_content",
          (* heredoc_content *) token env tok
        )
      | `Interp x -> R.Case ("Interp",
          map_interpolation env x
        )
      | `Pat_a84aa85_str_esc_seq (v1, v2) -> R.Case ("Pat_a84aa85_str_esc_seq",
          let v1 = map_pat_a84aa85 env v1 in
          let v2 = (* string_escape_sequence *) token env v2 in
          R.Tuple [v1; v2]
        )
      | `Pat_a84aa85_igno_back (v1, v2) -> R.Case ("Pat_a84aa85_igno_back",
          let v1 = map_pat_a84aa85 env v1 in
          let v2 = (* ignored_backslash *) token env v2 in
          R.Tuple [v1; v2]
        )
      | `Line_cont_expl_ x -> R.Case ("Line_cont_expl_",
          map_line_continuation_explicit_ env x
        )
      )
    ) v2)
  in
  let v3 = (* heredoc_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let dump_tree root =
  map_expressions () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Line_continuation (_loc, x) -> ("line_continuation", "line_continuation", map_line_continuation env x)
  | `Loc_pragma_push (_loc, x) -> ("loc_pragma_push", "loc_pragma_push", map_loc_pragma_push env x)
  | `Loc_pragma_pop (_loc, x) -> ("loc_pragma_pop", "loc_pragma_pop", map_loc_pragma_pop env x)
  | `Loc_pragma_location (_loc, x) -> ("loc_pragma_location", "loc_pragma_location", map_loc_pragma_location env x)
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Heredoc_body (_loc, x) -> ("heredoc_body", "heredoc_body", map_heredoc_body env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
