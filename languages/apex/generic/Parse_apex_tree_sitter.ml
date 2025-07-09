(**
   Boilerplate to be used as a template when mapping the apex CST
   to another type of tree.
*)

(*  open Common *)
(*  open Either_ *)
open Fpath_.Operators
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers
module CST = Tree_sitter_apex.CST

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env
type raw = G.raw_tree

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* NEW *)
let property_navigation (env : env) ((v1, v2) : CST.property_navigation)
    : G.tok option * G.tok =
  let v1 = Option.map ((* "?" *) token env) v1 in
  let v2 = (* "." *) token env v2 in
  v1, v2

(* NEW *)
let dimensions (env : env) (xs : CST.dimensions) : (G.tok * G.tok) list =
  List.map (fun (v1, v2) ->
    let v1 = (* "[" *) token env v1 in
    let v2 = (* "]" *) token env v2 in
    v1, v2)
    xs

(* RAW QUERY *)
let value_comparison_operator (env : env) (x : CST.value_comparison_operator) : G.expr =
  let module R = Raw_tree in
  match x with
  | `EQ tok ->
      let t = (* "=" *) token env tok in
      G.IdSpecial (G.Op G.Eq, t) |> G.e
  | `BANGEQ tok ->
      let t = (* "!=" *) token env tok in
      G.IdSpecial (G.Op G.NotEq, t) |> G.e
  | `LTGT tok ->
      let t = (* "<>" *) token env tok in
      G.IdSpecial (G.Op G.NotEq, t) |> G.e
  | `LT tok ->
      let t = (* "<" *) token env tok in
      G.IdSpecial (G.Op G.Lt, t) |> G.e
  | `LTEQ tok ->
      let t = (* "<=" *) token env tok in
      G.IdSpecial (G.Op G.LtE, t) |> G.e
  | `GT tok ->
      let t = (* ">" *) token env tok in
      G.IdSpecial (G.Op G.Gt, t) |> G.e
  | `GTEQ tok ->
      let t = (* ">=" *) token env tok in
      G.IdSpecial (G.Op G.GtE, t) |> G.e
  | `Pat_like x ->
      let v = (* "like" *) str env x in
      G.RawExpr (R.Token v) |> G.e

(* RAW QUERY *)
let with_highlight (env : env) (x : CST.with_highlight) : G.ident =
  (* "hightlight" *) str env x

(* NEW *)
let super (env : env) (x : CST.super) : G.expr =
  let t = (* "super" *) token env x in
  G.IdSpecial (G.Super, t) |> G.e

(* AUX*)
let super_to_field_name (env : env) (x : CST.super) : G.field_name =
  let i = (* "super" *) str env x in
  G.FN (H2.name_of_id i)

(* AUX *)
let this_to_field_name (env : env) (x : CST.this) : G.field_name =
  let i = (* "this" *) str env x in
  G.FN (H2.name_of_id i)

(* RAW QUERY *)
let order_null_direciton (env : env) (x : CST.order_null_direciton) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_nulls_pat_first (v1, v2) ->
      let v1 = (* "nulls" *) str env v1 in
      let v2 = (* "first" *) str env v2 in
      R.Tuple [R.Token v1; R.Token v2]
  | `Pat_nulls_pat_last (v1, v2) ->
      let v1 = (* "nulls" *) str env v1 in
      let v2 = (* "last" *) str env v2 in
      R.Tuple [R.Token v1; R.Token v2]

(* RAW QUERY *)
let count_expression (env : env) ((v1, v2, v3) : CST.count_expression)
    : G.expr =
  let v1 = str env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* ")" *) token env v3 in
  G.Call (G.N (G.Id (v1, G.empty_id_info ())) |> G.e, (v2, [], v3)) |> G.e

(* RAW QUERY *)
let for_type (env : env) (x : CST.for_type) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_update x
  | `Pat_ref x
  | `Pat_view x ->
      let v = str env x in
      R.Token v

 (* RAW QUERY *)
let update_type (env : env) (x : CST.update_type) : G.expr =
  let v1 =
    match x with
    | `Pat_trac x
    | `Pat_view_ x ->
        str env x
  in
    G.L (G.String (fb v1)) |> G.e

(* NEW QUERY *)
let with_data_cat_filter_type (env : env) (x : CST.with_data_cat_filter_type) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_at x
  | `Pat_above x
  | `Pat_below x
  | `Pat_above_or_below x ->
      R.Token (str env x)

(* RAW QUERY *)
let using_scope_type (env : env) (x : CST.using_scope_type) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_dele x
  | `Pat_ever x
  | `Pat_mine x
  | `Pat_mine_and_my_groups x
  | `Pat_my_terr x
  | `Pat_my_team_terr x
  | `Pat_team x ->
      R.Token (str env x)

(* NEW *)
let this (env : env) (x : CST.this) : G.expr =
  let t = token env x (* "this" *) in
  IdSpecial (This, t) |> G.e

(* RAW QUERY *)
let order_direction (env : env) (x : CST.order_direction) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_asc x
  | `Pat_desc x ->
      R.Token (str env x)

(* NEW *)
let null_literal (env : env) (x : CST.null_literal) : literal =
  G.Null (token env x)

(* RAW QUERY *)
let all_rows_clause (env : env) ((v1, v2) : CST.all_rows_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "all" *) str env v1 in
  let v2 = (* "rows" *) str env v2 in
  R.Tuple [R.Token v1; R.Token v2]

(* RAW QUERY *)
let fields_type (env : env) (x : CST.fields_type) : G.ident =
  match x with
  | `Pat_all x
  | `Pat_custom x
  | `Pat_stan x ->
      str env x

(* RAW QUERY *)
let in_type (env : env) (x : CST.in_type) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_all x
  | `Pat_email x
  | `Pat_name x
  | `Pat_phone x
  | `Pat_side x ->
      R.Token (str env x)

(* NEW *)
let identifier (env : env) (x : CST.identifier) : G.ident =
  match x with
  | `Semg_meta tok (* pattern \$[A-Z_][A-Z_0-9]* *) ->
      str env tok
  | `Apex_id_ tok (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) ->
      str env tok

(* AUX *)
let identifier_to_expression (env : env) (x : CST.identifier) : G.expr =
  G.N (H2.name_of_id (identifier env x)) |> G.e

(* RAW QUERY *)
let set_comparison_operator (env : env) (x : CST.set_comparison_operator) : G.expr =
  let module R = Raw_tree in
  match x with
  | `Pat_in x
  | `Pat_inclus x
  | `Pat_exclus x ->
      let v = str env x in
      G.RawExpr (R.Token v) |> G.e
  | `Pat_not_pat_in (v1, v2) ->
      let v1 = str env v1 in
      let v2 = str env v2 in
      G.RawExpr (R.Tuple [R.Token v1; R.Token v2]) |> G.e

(* NEW *)
let boolean (env : env) (x : CST.boolean) : literal =
  match x with
  | `Pat_true tok -> G.Bool (true, token env tok)
  | `Pat_false tok -> G.Bool (false, token env tok)

(* RAW QUERY *)
let date_literal (env : env) (x : CST.date_literal) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_yest x
  | `Pat_today x
  | `Pat_tomo x
  | `Pat_last_week x
  | `Pat_this_week x
  | `Pat_next_week x
  | `Pat_last_month x
  | `Pat_this_month x
  | `Pat_next_month x
  | `Pat_last_90_days x
  | `Pat_next_90_days x
  | `Pat_this_quar x
  | `Pat_last_quar x
  | `Pat_next_quar x
  | `Pat_this_year x
  | `Pat_last_year x
  | `Pat_next_year x
  | `Pat_this_fiscal_quar x
  | `Pat_last_fiscal_quar x
  | `Pat_next_fiscal_quar x
  | `Pat_this_fiscal_year x
  | `Pat_last_fiscal_year x
  | `Pat_next_fiscal_year x ->
      let v = str env x in
      R.Token v

(* NEW *)
let modifier (env : env) (x : CST.modifier) : G.attribute =
  match x with
  | `Pat_global x ->
      G.OtherAttribute (("Global", token env x), [])
  | `Pat_public x ->
      G.KeywordAttr (G.Public, token env x)
  | `Pat_test x ->
      G.OtherAttribute (("testMethod", token env x), [])
  | `Pat_prot x ->
      G.KeywordAttr (G.Protected, token env x)
  | `Pat_over x ->
      G.KeywordAttr (G.Override, token env x)
  | `Pat_priv x ->
      G.KeywordAttr (G.Private, token env x)
  | `Pat_virt x ->
      G.OtherAttribute (("virtual", token env x), [])
  | `Pat_abst x ->
      G.KeywordAttr (G.Abstract, token env x)
  | `Pat_static x ->
      G.KeywordAttr (G.Static, token env x)
  | `Pat_final x ->
      G.KeywordAttr (G.Final, token env x)
  | `Pat_tran x ->
      G.OtherAttribute (("transient", token env x), [])
  | `Pat_with_pat_shar (v1, v2) ->
      let v1 = token env v1 in
      let v2 = token env v2 in
      G.OtherAttribute (("withSharing", Tok.combine_toks v1 [v2]), [])
  | `Pat_with__pat_shar (v1, v2) ->
      let v1 = token env v1 in
      let v2 = token env v2 in
      G.OtherAttribute (("withoutSharing", Tok.combine_toks v1 [v2]), [])
  | `Pat_inhe_pat_shar (v1, v2) ->
      let v1 = token env v1 in
      let v2 = token env v2 in
      G.OtherAttribute (("inheritSharing", Tok.combine_toks v1 [v2]), [])

(* RAW QUERY *)
let function_name (env : env) (x : CST.function_name) : string wrap =
  match x with
  | `Pat_avg x
  | `Pat_count x
  | `Pat_count_dist x
  | `Pat_min x
  | `Pat_max x
  | `Pat_sum x
  | `Pat_grou x
  | `Pat_format x
  | `Pat_conv x
  | `Pat_tola x
  | `Pat_cale_month x
  | `Pat_cale_quar x
  | `Pat_cale_year x
  | `Pat_day_in_month x
  | `Pat_day_in_week x
  | `Pat_day_in_year x
  | `Pat_day_only x
  | `Pat_fiscal_month x
  | `Pat_fiscal_quar x
  | `Pat_fiscal_year x
  | `Pat_hour_in_day x
  | `Pat_week_in_month x
  | `Pat_week_in_year x ->
      str env x

(* NEW *)
let trigger_event (env : env) (x : CST.trigger_event) : G.argument =
  match x with
  | `Pat_before_pat_insert (v1, v2)
  | `Pat_before_pat_update (v1, v2)
  | `Pat_before_pat_delete (v1, v2)
  | `Pat_after_pat_insert (v1, v2)
  | `Pat_after_pat_update (v1, v2)
  | `Pat_after_pat_delete (v1, v2)
  | `Pat_after_pat_unde (v1, v2) ->
      let e1 = G.N (G.Id (str env v1, G.empty_id_info ())) |> G.e in
      let e2 = G.N (G.Id (str env v2, G.empty_id_info ())) |> G.e in
      G.Arg (G.Container (G.Tuple, fb [e1; e2]) |> G.e)

(* NEW *)
let dml_type (env : env) (x : CST.dml_type) : G.ident =
  match x with
  | `Pat_insert x
  | `Pat_update x
  | `Pat_delete x
  | `Pat_unde x ->
      str env x

(* RAW QUERY *)
let for_clause (env : env) ((v1, v2, v3) : CST.for_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "for" *) str env v1 in
  let v2 = for_type env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = for_type env v2 in
      v2
    ) v3
  in
  R.Tuple [R.Token v1; R.List (v2 :: v3)]

(* RAW QUERY *)
let update_clause (env : env) ((v1, v2, v3) : CST.update_clause) : G.expr list =
  let v1 = (* "update" *) str env v1 in
  let v2 = update_type env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = update_type env v2 in
      v2
    ) v3
  in
  [G.L (G.String (fb v1)) |> G.e;
   G.Container (G.Tuple, fb (v2 :: v3)) |> G.e]

(* RAW QUERY *)
let soql_using_clause (env : env) ((v1, v2, v3) : CST.soql_using_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "using" *) str env v1 in
  let v2 = (* "scope" *) str env v2 in
  let v3 = using_scope_type env v3 in
  R.Tuple [R.Token v1; R.Token v2; v3]

(* RAW QUERY *)
let fields_expression (env : env) ((v1, v2, v3, v4) : CST.fields_expression) =
  let module R = Raw_tree in
  let v1 = (* "fields" *) str env v1 in
  let _v2 = (* "(" *) token env v2 in
  let v3 = fields_type env v3 in
  let v4 = (* ")" *) str env v4 in (* keeping this for ranges *)
  R.Tuple [R.Token v1; R.Token v3; R.Token v4]

(* RAW QUERY *)
let in_clause (env : env) ((v1, v2, v3) : CST.in_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "in" *) str env v1 in
  let v2 = in_type env v2 in
  let v3 = (* "fields" *) str env v3 in
  R.Tuple [R.Token v1; v2; R.Token v3]

(* RAW QUERY *)
let using_clause (env : env) ((v1, v2, v3, v4) : CST.using_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* using *) str env v1 in
  let v2 = (*" listView" *) str env v2 in
  let _v3 = (* "=" *) token env v3 in
  let v4 = identifier env v4 in
  R.Tuple [R.Token v1; R.Token v2; R.Token v4]

(* NEW *)
(* FIXME: what does v2 do? *)
let variable_declarator_id (env : env) ((v1, _v2) : CST.variable_declarator_id) : G.ident =
  identifier env v1

(* NEW *)
let break_statement (env : env) ((v1, v2, v3) : CST.break_statement) : G.stmt =
  let v1 = token env v1 (* "break" *) in
  let v2 = H2.opt_to_label_ident (Option.map (identifier env) v2) in
  let v3 = token env v3 (* ";" *) in
  G.Break (v1, v2, v3) |> G.s

(* NEW *)
let continue_statement (env : env) ((v1, v2, v3) : CST.continue_statement) : G.stmt =
  let v1 = token env v1 (* "break" *) in
  let v2 = H2.opt_to_label_ident (Option.map (identifier env) v2) in
  let v3 = token env v3 (* ";" *) in
  G.Continue (v1, v2, v3) |> G.s

(* NEW *)
let rec name (env : env) (x : CST.name) : G.name =
  match x with
  | `Id x ->
      let i = identifier env x in
      H2.name_of_id i
  | `Scoped_id (v1, v2, v3) ->
      let v1 = name env v1 in
      let _v2 = token env v2 (* "." *) in
      let v3 = identifier env v3 in
      H2.add_id_opt_type_args_to_name v1 (v3, None) (* FIXME: None? *)

(* RAW QUERY *)
let inferred_parameters (env : env) ((v1, v2, v3, v4) : CST.inferred_parameters)
    : G.expr =
  let v1 = (* "(" *) token env v1 in
  let v2 = identifier_to_expression env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = identifier_to_expression env v2 in
      v2
    ) v3
  in
  let es = v2 :: v3 in
  let v4 = (* ")" *) token env v4 in
  G.Container (G.Tuple, (v1, es, v4)) |> G.e

(* AUX *)
let strip_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '\'' && s.[len - 1] = '\''
  then String.sub s 1 (len - 2)
  else s

(* NEW *)
let literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Int tok ->
      G.Int (Parsed_int.parse (str env tok))
  | `Deci_floa_point_lit tok ->
      let s, t = str env tok in
      G.Float (float_of_string_opt s, t)
  | `Bool tok ->
      boolean env tok
  | `Str_lit tok ->
      let s, t = str env tok in
      G.String (G.fake "'", (strip_quotes s, t), G.fake "'") (* FIXME: Isn't string delimiter captured by the parser? *)
  | `Null_lit x ->
      null_literal env x

(* RAW QUERY *)
let with_record_visibility_param (env : env) (x : CST.with_record_visibility_param)
    : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_maxd_EQ_int (v1, v2, v3) ->
      let v1 = (* maxDescriptorPerRecord *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let s, t = (* int *) str env v3 in
      let i = G.L (G.Int (Parsed_int.parse (s, t))) |> G.e in
      R.Tuple [R.Token v1; R.Any (G.E i)]
  | `Pat_suppos_EQ_bool (v1, v2, v3) ->
      let v1 = (* "supportsDomains" *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 = boolean env v3 in
      let b = G.L v3 |> G.e in
      R.Tuple [R.Token v1; R.Any (G.E b)]
  | `Pat_suppos__EQ_bool (v1, v2, v3) ->
      let v1 = (* "supportsDelegates" *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 = boolean env v3 in
      let b = G.L v3 |> G.e in
      R.Tuple [R.Token v1; R.Any (G.E b)]

(* RAW QUERY *)
let soql_literal (env : env) (x : CST.soql_literal) : G.expr =
  let module R = Raw_tree in
  match x with
  | `Int tok ->
      let s, t = (* int *) str env tok in
      G.L (G.Int (Parsed_int.parse (s, t))) |> G.e
  | `Deci tok ->
      let s, t = (* pattern -?\d+(\.\d+)? *) str env tok in
      G.L (G.Float (float_of_string_opt s, t)) |> G.e
  | `Str_lit tok ->
      let s, t = (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) str env tok in
      G.L (G.String (fb (s, t))) |> G.e
  | `Date tok ->
      let v = (* pattern [1-4][0-9]{3}-(?:0[1-9]|1[0-2])-(?:[0-2][1-9]|[1-2]0|3[0-1]) *) str env tok in
      G.RawExpr (R.Token v) |> G.e
  | `Date_time tok ->
      let v = (* pattern [1-4][0-9]{3}-(?:0[1-9]|1[0-2])-(?:[0-2][1-9]|[1-2]0|3[0-1])T([0-1]\d|2[0-3]):[0-5]\d:[0-5]\d(?:\.\d\d?\d?)?(?:Z|[+-][0-1]\d:[0-5]\d) *) str env tok in
      G.RawExpr (R.Token v) |> G.e
  | `Bool x ->
      G.L (boolean env x) |> G.e
  | `Date_lit x ->
      G.RawExpr (date_literal env x) |> G.e
  | `Date_lit_with_param (v1, v2, v3) ->
      let v1 = str env v1 in
      let _v2 = (* ":" *) token env v2 in
      let s, t = (* int *) str env v3 in
      let ie = G.L (G.Int (Parsed_int.parse (s, t))) |> G.e in
      G.RawExpr (R.Tuple [R.Token v1; R.Any (G.E ie)]) |> G.e
  | `Curr_lit tok ->
      let v = (* pattern \w{3}\d+(\.\d+)? *) str env tok in
      G.RawExpr (R.Token v) |> G.e
  | `Null_lit x ->
      G.L (null_literal env x) |> G.e

(* RAW QUERY *)
let field_identifier (env : env) (x : CST.field_identifier) : G.expr =
  match x with
  | `Id x ->
      let i = identifier env x in
      G.N (H2.name_of_id i) |> G.e
  | `Dotted_id (v1, v2) ->
      let v1 = (identifier env v1, None) in
      let v2 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "." *) token env v1 in
          let v2 = identifier env v2 in
          (v2, None)
        ) v2
      in
      let ids = v1 :: v2 in
      let id = H2.name_of_ids_with_opt_typeargs ids in
      G.N id |> G.e

(* RAW QUERY *)
let with_data_cat_filter (env : env) ((v1, v2, v3) : CST.with_data_cat_filter) : raw =
  let module R = Raw_tree in
  let v1 = identifier env v1 in
  let v2 = with_data_cat_filter_type env v2 in
  let v3 =
    match v3 with
    | `Id x ->
        identifier_to_expression env x
    | `LPAR_id_rep_COMMA_id_RPAR x ->
        inferred_parameters env x
  in
  R.Tuple [R.Token v1; v2; R.Any (G.E v3)]

(* RAW QUERY *)
let storage_identifier (env : env) (x : CST.storage_identifier) : G.expr =
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      G.Ellipsis t |> G.e
  | `Semg_meta_ellips tok ->
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *)
      let id = str env tok in
      G.N (H2.name_of_id id) |> G.e
  | `Choice_id x ->
      field_identifier env x

(* RAW QUERY *)
let field_list (env : env) ((v1, v2) : CST.field_list) : G.expr list =
  let v1 = field_identifier env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = field_identifier env v2 in
      v2
    ) v2
  in
  v1 :: v2

(* RAW QUERY *)
let with_data_cat_expression (env : env) ((v1, v2, v3, v4) : CST.with_data_cat_expression)
    : raw =
  let module R = Raw_tree in
  let v1 = (* "data" *) str env v1 in
  let v2 = (* "category" *) str env v2 in
  let v3 = with_data_cat_filter env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 in
      let v2 = with_data_cat_filter env v2 in
      v2
    ) v4
  in
  R.Tuple [R.Token v1; R.Token v2; R.List (v3 :: v4)]

(* RAW QUERY *)
let when_expression (env : env) ((v1, v2, v3, v4) : CST.when_expression) =
  let module R = Raw_tree in
  let v1 = (* "when" *) str env v1 in
  let v2 = identifier_to_expression env v2 in
  let v3 = (* "then" *) str env v3 in
  let v4 = field_list env v4 in
  R.Tuple
    [ R.Token v1;
      R.Any (G.E v2);
      R.Token v3;
      R.List (List.map (fun e -> R.Any (G.E e)) v4)]

(* RAW QUERY *)
let else_expression (env : env) ((v1, v2) : CST.else_expression) =
  let module R = Raw_tree in
  let v1 = (* "else" *) str env v1 in
  let v2 = field_list env v2 in
  R.Tuple [R.Token v1; R.List (List.map (fun e -> R.Any (G.E e)) v2)]

(* RAW QUERY *)
let soql_with_type (env : env) (x : CST.soql_with_type) : raw =
  let module R = Raw_tree in
  match x with
  | `Pat_secu_enfo x
  | `Pat_user_mode x
  | `Pat_system_mode x ->
      R.Token (str env x)
  | `With_data_cat_exp x ->
      with_data_cat_expression env x
  | `With_record_visi_exp (v1, v2, v3, v4, v5) ->
      let v1 = (* "recordVisibilityContext" *) str env v1 in
      let _v2 = (* "(" *) token env v2 in
      let v3 = with_record_visibility_param env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "," *) token env v1 in
          let v2 = with_record_visibility_param env v2 in
          v2
        ) v4
      in
      let _v5 = (* ")" *) token env v5 in
      R.Tuple [R.Token v1; R.List (v3 :: v4)]
  | `With_user_id_type (v1, v2, v3) ->
      let v1 = (* "userId" *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) str env v3
      in
      R.Tuple [R.Token v1; R.Token v3]

(* RAW QUERY *)
let anon_choice_stor_id_355c95c (env : env) (x : CST.anon_choice_stor_id_355c95c)
    : G.expr =
  match x with
  | `Stor_id x ->
      storage_identifier env x
  | `Stor_alias (v1, v2, v3) ->
      let v1 = storage_identifier env v1 in
      (* v2 is a token "as" *)
      let v3 = identifier env v3 in
      G.LetPattern (G.PatId (v3, G.empty_id_info ()), v1) |> G.e

(* RAW QUERY *)
let type_of_clause (env : env) ((v1, v2, v3, v4, v5) : CST.type_of_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "typeof" *) str env v1 in
  let v2 = identifier_to_expression env v2 in
  let v3 = List.map (when_expression env) v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        else_expression env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) str env v5 in
  R.Tuple
    [ R.Token v1;
      R.Any (G.E v2);
      R.List v3;
      v4;
      R.Token v5]

(* RAW QUERY *)
let soql_with_clause (env : env) ((v1, v2) : CST.soql_with_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "with" *) str env v1 in
  let v2 = soql_with_type env v2 in
  R.Tuple [R.Token v1; v2]

(* RAW QUERY *)
let from_clause (env : env) ((v1, v2, v3) : CST.from_clause) :raw =
  let module R = Raw_tree in
  let v1 = (* "str" *) str env v1 in
  let v2 = anon_choice_stor_id_355c95c env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = anon_choice_stor_id_355c95c env v2 in
      v2
    ) v3
  in
  let c = G.Container (G.List, fb (v2 :: v3)) |> G.e in
  R.Tuple [R.Token v1; R.Any (G.E c)]

(* NEW *)
let rec accessor_declaration (env : env) ((v1, v2, v3) : CST.accessor_declaration)
    : G.attribute list * G.ident * G.function_body =
  let v1 =
    match v1 with
    | Some x ->
        modifiers env x
    | None -> []
  in
  let a, (_, t as v2) =
    match v2 with
    | `Pat_get tok ->
        G.Getter, str env tok
    | `Pat_set tok ->
        G.Setter, str env tok
  in
  let v3 = anon_choice_trig_body_f78fea4 env v3 in
  G.KeywordAttr (a, t) :: v1, v2, v3

(* NEW *)
and accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list)
    : (G.attribute list * G.ident * G.function_body) list bracket =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map (accessor_declaration env) v2
  in
  let v3 = (* "}" *) token env v3 in
  v1, v2, v3

(* RAW QUERY *)
and alias_expression (env : env) ((v1, v2, v3) : CST.alias_expression) : G.expr =
  let v1 = value_expression env v1 in
  (* TODO: seems we can ignore v2 *)
  let v3 = identifier env v3 in
  G.LetPattern (G.PatId (v3, G.empty_id_info ()), v1) |> G.e

(* NEW *)
and annotation (env : env) ((v1, v2, v3) : CST.annotation) : G.attribute =
  let v1 = (* "@" *) token env v1 in
  let v2 = name env v2 in
  match v3 with
  | Some x ->
      let args = annotation_argument_list env x in
      G.NamedAttr (v1, v2, args)
  | None ->
      G.NamedAttr (v1, v2, fb [])

(* NEW *)
and annotation_argument_list (env : env) ((v1, v2, v3) : CST.annotation_argument_list)
  : G.arguments =
  let v1 = (* "(" *) token env v1 in
  let v2 (* : G.argument list *) =
    match v2 with
    | `Elem_value x ->
        [G.Arg (element_value env x)]
    | `Anno_key_value_rep_opt_COMMA_anno_key_value (v1, v2) ->
        let v1 = annotation_key_value env v1 in
        let v2 =
          List.map (fun (_v1 (* "," *), v2) ->
            annotation_key_value env v2
          ) v2
        in
        v1 :: v2
  in
  let v3 = (* ")" *) token env v3 in
  v1, v2, v3

(* NEW *)
and annotation_key_value (env : env) (x : CST.annotation_key_value)
  : G.argument =
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      G.Arg (G.Ellipsis t |> G.e)
  | `Id_EQ_elem_value (v1, v2, v3) ->
      let v1 = identifier env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 = element_value env v3 in
      G.ArgKwd (v1, v3)

(* RAW QUERY *)
and anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78 (env : env) ((v1, v2, v3, v4) : CST.anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78)
    : G.expr =
  let v1 = (* "(" *) token env v1 in
  let v2 = anon_choice_soql_lit_3019e24 env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = anon_choice_soql_lit_3019e24 env v2 in
      v2
    ) v3
  in
  let es = v2 :: v3 in
  let v4 = (* ")" *) token env v4 in
  G.Container (G.Set, (v1, es, v4)) |> G.e

(* RAW QUERY *)
and anon_choice_field_id_cb081aa (env : env) (x : CST.anon_choice_field_id_cb081aa)
    : G.expr =
  match x with
  | `Field_id x ->
      field_identifier env x
  | `Func_exp x ->
      function_expression env x

(* RAW QUERY *)
and anon_choice_int_1466488 (env : env) (x : CST.anon_choice_int_1466488) : G.expr =
  match x with
  | `Int tok ->
      let s, t = (* int *) str env tok in
      G.L (G.Int (Parsed_int.parse (s, t))) |> G.e
  | `Bound_apex_exp x ->
      bound_apex_expression env x

(* NEW *)
and anon_choice_prim_exp_bbf4eda (env : env) (x : CST.anon_choice_prim_exp_bbf4eda) : G.expr =
  match x with
  | `Prim_exp x ->
      primary_expression env x
  | `Super x ->
      super env x

(* NEW *)
and anon_choice_semg_ellips_d10ab47 (env : env) (x : CST.anon_choice_semg_ellips_d10ab47)
    : G.argument =
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      Arg (G.Ellipsis t |> G.e)
  | `Semg_meta_ellips tok ->
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *)
      let id = str env tok in
      G.Arg (G.N (H2.name_of_id id) |> G.e)
  | `Exp x ->
      Arg (expression env x)

(* RAW QUERY *)
and anon_choice_soql_lit_3019e24 (env : env) (x : CST.anon_choice_soql_lit_3019e24)
    : G.expr=
  match x with
  | `Soql_lit x ->
      soql_literal env x
  | `Bound_apex_exp x ->
      bound_apex_expression env x

(* NEW *)
and anon_choice_trig_body_f78fea4 (env : env) (x : CST.anon_choice_trig_body_f78fea4) : G.function_body =
  match x with
  | `Blk x ->
      G.FBStmt (trigger_body env x)
  | `SEMI tok ->
      let _t = (* ";" *) token env tok in
      FBNothing

(* NEW *)
and anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c)
  : G.expr * ((G.tok * G.expr) list) =
  let v1 = expression env v1 in
  let vs =
    List.map (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = expression env v2 in
        v1, v2)
      v2
  in
  v1, vs

(* NEW *)
and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) : G.arguments =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = anon_choice_semg_ellips_d10ab47 env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = anon_choice_semg_ellips_d10ab47 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  v1, v2, v3

(* NEW *)
and array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) : G.expr =
  let v1 = primary_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = expression env v3 in
  let v4 = (* "]" *) token env v4 in
  G.ArrayAccess (v1, (v2, v3, v4)) |> G.e

(* NEW *)
and array_creation_expression (env : env) ((v1, v2, v3) : CST.array_creation_expression)
  : G.expr =
  let v1 = token env v1 in
  let t0 = simple_type env v2 |> G.t in
  let t1, v3 =
    match v3 with
    | `Rep1_dimens_expr_opt_dimens (w1, w2) ->
        let dim_exprs =
          List.map (dimensions_expr env) w1
          |> List.map (fun (t1, e, t2) -> (t1, Some e, t2))
        in
        let dims =
          (match w2 with
          | Some x -> dimensions env x
          | None -> [])
          |> List.map (fun (t1, t2) -> (t1, None, t2))
        in
        let nt =
          List.fold_left
            (fun t (l,e,r) -> TyArray ((l, e, r), t) |> G.t) t0
            (dim_exprs @ dims)
        in
        nt, fb []
    | `Dimens_array_init (w1, w2) ->
        let dims = dimensions env w1 in
        let nt =
          List.fold_left
            (fun t (l,r) -> TyArray ((l, None, r), t) |> G.t) t0 dims
        in
        let c = array_initializer_content env w2 in
        (nt, c)
    | `Array_init x ->
        t0, array_initializer_content env x
  in
  let lb, _, rb = v3 in
  let args = (lb, [ Arg (G.Container (G.Tuple, v3) |> G.e) ], rb) in
  New (v1, t1, empty_id_info (), args) |> G.e

(* NEW *)
and array_initializer (env : env) (x : CST.array_initializer) : G.expr =
  let v1, v2, v3 = array_initializer_content env x in
  Container (Array, (v1, v2, v3)) |> G.e

(* AUX *)
and array_initializer_content (env : env) ((v1, v2, v3) : CST.array_initializer)
  : expr list bracket =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some (w1, w2) ->
        let z1 = variable_initializer env w1 in
        let z2 =
          List.map (fun (y1, y2) ->
            let _r1 = (* "," *) token env y1 in
            let r2 = variable_initializer env y2 in
            r2
          ) w2
        in
        z1 :: z2
    | None -> []
  in
  let v3 = (* "}" *) token env v3 in
  v1, v2, v3

(* NEW *)
and binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op And, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Or, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op ASR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSL, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitAnd, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitXor, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitOr, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Plus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Minus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mult, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Div, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mod, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Lt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Eq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op GtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Gt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "===" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op PhysEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!==" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotPhysEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTGT_exp (v1, v2, v3) -> (* FIXME: not in Apex documentation! *)
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e

(* NEW *)
and block (env : env) ((v1, v2, v3) : CST.block) : G.stmt =
  let v1 = token env v1 (* "{" *) in
  let v2 = List_.map (statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  G.Block (v1, v2, v3) |> G.s

(* RAW QUERY *)
and boolean_expression (env : env) (x : CST.boolean_expression) : G.expr =
  match x with
  | `And_exp (v1, v2) ->
      let v1 = condition_expression env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "and" *) token env v1 in
          let v2 = condition_expression env v2 in
          v2
        ) v2
      in
      let es = v1 :: v2 in
      let args = List.map (fun e -> G.Arg e) es in
      G.Call (G.IdSpecial (G.Op G.And, fake "") |> G.e, fb args) |> G.e
  | `Or_exp (v1, v2) ->
      let v1 = condition_expression env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "or" *) token env v1 in
          let v2 = condition_expression env v2 in
          v2
        ) v2
      in
      let es = v1 :: v2 in
      let args = List.map (fun e -> G.Arg e) es in
      G.Call (G.IdSpecial (G.Op G.Or, fake "") |> G.e, fb args) |> G.e
  | `Not_exp (v1, v2) ->
      let v1 = (* "not" *) token env v1 in
      let v2 = condition_expression env v2 in
      G.Call (G.IdSpecial (G.Op G.Not, v1) |> G.e, fb [G.Arg v2]) |> G.e
  | `Cond_exp x ->
      condition_expression env x

(* RAW QUERY *)
and bound_apex_expression (env : env) ((v1, v2) : CST.bound_apex_expression)
    : G.expr =
  let _v1 = (* ":" *) token env v1 in
  let v2 = expression env v2 in
  v2

(* NEW *)
and catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) : G.catch =
  let v1 = token env v1 (* "catch" *)in
  let _v2 = (* "(" *) token env v2 in
  let v3 = catch_formal_parameter env v3 in
  let _v4 = (* ")" *) token env v4 in
  let v5 = trigger_body env v5 in
  v1, v3, v5

(* NEW *)
and catch_formal_parameter (env : env) (x : CST.catch_formal_parameter) : G.catch_exn =
  match x with
  | `Semg_ellips tok ->
      let t = token env tok (* "..." *) in
      G.CatchPattern (G.PatEllipsis t)
  | `Opt_modifs_unan_type_var_decl_id (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some x -> modifiers env x
        | None -> []
      in
      let v2 = make_type v1 (unannotated_type env v2) in
      let v3 = variable_declarator_id env v3 in
      G.CatchParam (G.param_of_type v2 ?pname:(Some v3))

(* NEW *)
and class_body (env : env) ((v1, v2, v3) : CST.class_body) : G.stmt list bracket =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.filter_map (class_body_declaration env) v2
  in
  let v3 = (* "}" *) token env v3 in
  v1, v2, v3

(* NEW *)
and class_body_declaration (env : env) (x : CST.class_body_declaration) : G.stmt option =
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      let v2 = G.sc in
      Some (G.ExprStmt (G.Ellipsis t |> G.e, v2) |> G.s)
  | `Choice_field_decl x ->
      match x with
      | `Field_decl x ->
          Some (field_declaration env x)
      | `Meth_decl x ->
          Some (method_declaration env x)
      | `Class_decl x ->
          Some (class_declaration env x)
      | `Inte_decl x ->
          Some (interface_declaration env x)
      | `Enum_decl x ->
          Some (enum_declaration env x)
      | `Blk x ->
          Some (block env x)
      | `Static_init x ->
          Some (static_initializer env x)
      | `Cons_decl x ->
          Some (constructor_declaration env x)
      | `SEMI tok ->
          let _t = (* ";" *) token env tok
          in None

(* NEW *)
and class_declaration (env : env) ((h, b) : CST.class_declaration) : G.stmt =
  let ent, d = class_header env h in
  let lb, b, rb = class_body env b in
  let fields = List_.map (fun x -> G.F x) b in
  G.DefStmt
    (ent,
     G.ClassDef { d with cbody = (lb, fields, rb); })
  |> G.s

(* NEW *)
and class_header (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_header) : G.entity * G.class_definition =
  let v1 =
    match v1 with
    | Some x -> modifiers env x
    | None -> []
  in
  let v2 = (* "class" *) token env v2 in
  let v3 = identifier env v3 in
  let v4 = Option.map (type_parameters env) v4 in
  let v5 =
    match v5 with
    | Some v5 -> superclass env v5
    | None -> []
  in
  let v6 =
    match v6 with
    | Some v6 -> interfaces env v6
    | None -> []
  in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v3, idinfo)); attrs = v1; tparams = v4 } in
  ent,
  {
    ckind = (G.Class, v2);
    cextends = v5;
    cimplements = v6;
    cmixins = [];
    cparams = fb [];
    cbody = fb [];
  }

(* NEW *)
and class_literal (env : env) ((v1, v2, v3) : CST.class_literal) : G.expr =
  let v1 = unannotated_type env v1 |> G.t in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "class" *) token env v3 in
  Call (IdSpecial (Typeof, v2) |> G.e, fb [ ArgType v1 ]) |> G.e

(* RAW QUERY *)
and comparison (env : env) (e1 : G.expr) (x : CST.comparison) : G.expr =
  match x with
  | `Value_comp (v1, v2) ->
      let v1 = value_comparison_operator env v1 in
      let v2 = anon_choice_soql_lit_3019e24 env v2 in
      G.Call (v1, fb [G.Arg e1; G.Arg v2]) |> G.e
  | `Set_comp (v1, v2) ->
      let v1 = set_comparison_operator env v1 in
      let v2 =
        match v2 with
        | `Subq x ->
            G.RawExpr (subquery env x) |> G.e
        | `LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR x ->
            anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78 env x
        | `Bound_apex_exp x ->
            bound_apex_expression env x
      in
      G.Call (v1, fb [G.Arg e1; G.Arg v2]) |> G.e

(* RAW QUERY *)
and comparison_expression (env : env) ((v1, v2) : CST.comparison_expression) : G.expr =
  let v1 = value_expression env v1 in
  let v2 = comparison env v1 v2 in
  v2

(* RAW QUERY *)
and condition_expression (env : env) (x : CST.condition_expression) : G.expr =
  match x with
  | `Semg_meta tok ->
      let v = (* pattern \$[A-Z_][A-Z_0-9]* *) str env tok in
      G.N (G.Id (v, empty_id_info ())) |> G.e
  | `Choice_LPAR_bool_exp_RPAR x ->
      (match x with
      | `LPAR_bool_exp_RPAR (v1, v2, v3) ->
          let _v1 = (* "(" *) token env v1 in
          let v2 = boolean_expression env v2 in
          let _v3 = (* ")" *) token env v3 in
          v2
      | `Comp_exp x ->
          comparison_expression env x
      )

(* NEW *)
and constant_declaration (env : env) ((v1, v2, v3, v4) : CST.constant_declaration)
    : G.stmt =
  local_variable_declaration env (v1, v2, v3, v4)

(* NEW *)
and constructor_body (env : env) ((v1, v2, v3, v4) : CST.constructor_body)
    : G.stmt =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> [explicit_constructor_invocation env x]
    | None -> []
  in
  let v3 = List.map (statement env) v3 in
  let v4 = (* "}" *) token env v4 in
  G.Block (v1, v2 @ v3, v4) |> G.s

(* NEW *)
and constructor_declaration (env : env) ((v1, v2, v3) : CST.constructor_declaration)
    : G.stmt =
  let tparams, (_, t as i), fparams = constructor_declarator env v2 in
  let attrs =
    G.KeywordAttr (G.Ctor, t) ::
    match v1 with
    | Some x -> modifiers env x
    | None -> []
  in
  let fbody = G.FBStmt (constructor_body env v3) in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (i, idinfo)); attrs; tparams } in
  let def =
    G.FuncDef
    {
      fkind = (G.Method, t);
      fparams;
      frettype = None;
      fbody;
    }
  in
  G.DefStmt (ent, def) |> G.s

(* NEW *)
and constructor_declarator (env : env) ((v1, v2, v3) : CST.constructor_declarator)
    : G.type_parameters option * G.ident * G.parameters =
  let v1 = Option.map (type_parameters env) v1 in
  let v2 = identifier env v2 in
  let v3 = formal_parameters env v3 in
  v1, v2, v3

(* NEW *)
and declaration (env : env) (x : CST.declaration) : G.stmt =
  match x with
  | `Class_decl x ->
      class_declaration env x
  | `Trig_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "trigger" *) token env v1 in
      let v2 = identifier env v2 in
      let _v3 = (* "token" *) token env v3 in
      let v4 = identifier env v4 in
      let v5 = (* "(" *) token env v5 in
      let v6 = trigger_event env v6 in
      let v7 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = trigger_event env v2 in
          v2
        ) v7
      in
      let events = v6 :: v7 in
      let v8 = (* ")" *) token env v8 in
      let v9 = trigger_body env v9 in
      let entity =
        { name = G.EN (G.Id (v2, G.empty_id_info ()));
          attrs = [G.NamedAttr
                     (fake "@",
                      G.Id (("__trigger_events", fake ""), empty_id_info ()),
                      (v5, events, v8));
                   G.NamedAttr
                     (fake "@",
                      G.Id (("__trigger_object", fake ""), empty_id_info ()),
                      fb [G.Arg (G.N (G.Id (v4, empty_id_info ())) |> G.e)])];
          tparams = None }
      in
      let f =
        { fkind = (G.Function, v1);
          fparams = fb [];
          frettype = None;
          fbody = G.FBStmt v9
        }
      in
      G.DefStmt (entity, G.FuncDef f) |> G.s
  | `Inte_decl x ->
      interface_declaration env x
  | `Enum_decl x ->
      enum_declaration env x

(* NEW *)
and dimensions_expr (env : env) ((v1, v2, v3) : CST.dimensions_expr)
  : G.expr bracket =
  let v1 = (* "[" *) token env v1 in
  let v2 = expression env v2 in
  let v3 = (* "]" *) token env v3 in
  v1, v2, v3

(* NEW *)
and dml_expression (env : env) (x : CST.dml_expression) : G.expr =
  match x with
  | `Dml_type_exp (v1, v2) ->
      let v1 = G.N (dml_type env v1 |> H2.name_of_id) |> G.e in
      let v2 = expression env v2 in
      G.Call (v1, fb [Arg v2]) |> G.e
  | `Pat_upsert_exp_opt_unan_type (v1, v2, v3) ->
      let v1 = G.N ((* "upsert" *) str env v1 |> H2.name_of_id) |> G.e in
      let v2 = expression env v2 in
      (match v3 with
      | Some x ->
          let t = unannotated_type env x in
          G.Call (v1, fb [Arg v2; ArgType (G.t t)]) |> G.e
      | None ->
          G.Call (v1, fb [Arg v2]) |> G.e)
  | `Pat_merge_exp_SPACE_exp (v1, v2, v3, v4) ->
      let v1 = G.N ((* "merge" *) str env v1 |> H2.name_of_id) |> G.e in
      let v2 = expression env v2 in
      let v3 = (* " " *) token env v3 in
      let v4 = expression env v4 in
      G.Call (v1, fb [Arg v2; Arg v4]) |> G.e

(* NEW *)
and do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) : G.stmt =
  let v1 = token env v1 (* "do" *) in
  let v2 = statement env v2 in
  let _v3 = token env v3 (* "while" *) in
  let v4 = parenthesized_expression env v4 in
  let _v7 = token env v5 (* ";" *) in
  DoWhile (v1, v2, v4) |> G.s

(* NEW *)
and element_value (env : env) (x : CST.element_value) : G.expr =
  match x with
  | `Exp x ->
      expression env x
  | `Elem_value_array_init (v1, v2, _v3, v4) -> (* v3 is optional comma token *)
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = G.Arg (element_value env v1) |> H2.argument_to_expr in
            let v2 =
              List.map (fun (v1, v2) ->
                let _v1 = (* "," *) token env v1 in
                let v2 = G.Arg (element_value env v2) |> H2.argument_to_expr in
                v2
              ) v2
            in
            v1 :: v2
        | None -> [])
      in
      let v4 = (* "}" *) token env v4 in
      G.Container (G.Array, (v1, v2, v4)) |> G.e
  | `Anno x -> (* TODO: not sure what this does *)
      let a = annotation env x in
      G.RawExpr (Raw_tree.Any (G.At a)) |> G.e

(* NEW *)
and enhanced_for_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.enhanced_for_statement) : G.stmt =
  let v1 = (* "for" *) token env v1 in
  let _v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> modifiers env x
    | None -> []
  in
  let v4 = unannotated_type env v4 in
  let ty = make_type v3 v4 in
  let v5 = variable_declarator_id env v5 in
  let pat = G.PatId (v5, empty_id_info ()) in
  let v6 = (* ":" *) token env v6 in
  let v7 = expression env v7 in
  let _v8 = (* ")" *) token env v8 in
  let v9 = statement env v9 in
  G.For (v1, G.ForEach (G.PatTyped (pat, ty), v6, v7), v9) |> G.s

(* NEW *)
and enum_body (env : env) ((v1, v2, v3) : CST.enum_body) : G.or_type_element list =
  let _v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = enum_constant env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = enum_constant env v2 in
            v2
          ) v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = (* "}" *) token env v3 in
  v2

(* NEW *)
and enum_constant (env : env) (x : CST.enum_constant) : G.or_type_element =
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      OrEllipsis t
  | `Opt_modifs_id (v1, v2) ->
      (* FIXME: Not sure where to put the modifiers *)
      let _m = Option.map (modifiers env) v1 in
      let i = identifier env v2 in
      OrEnum (i, None)

(* NEW *)
and enum_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration) : G.stmt =
  let v1 =
    match v1 with
    | Some x -> modifiers env x
    | None -> []
  in
  let v2 = token (* "enum" *) env v2 in
  let v3 = identifier env v3 in
  (* FIXME: not sure where to put it. This part is ignored e.g. in C# *)
  let _v4 =
    match v4 with
    | Some x -> []
    | None -> []
  in
  let v5 = enum_body env v5 in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v3, idinfo)); attrs = v1; tparams = None } in
  G.DefStmt (ent, G.TypeDef { tbody = OrType v5 }) |> G.s

 (* NEW *)
and explicit_constructor_invocation (env : env) ((v1, v2, v3) : CST.explicit_constructor_invocation)
    : G.stmt =
  let v1 =
    match v1 with
    | `Opt_type_args_choice_this (v1, v2) ->
        (* FIXME: Not sure where to put type args here: Application of type args is
         * possible in QualifiedId, but here we have SpecialId. *)
        let _v1 = Option.map (type_arguments env) v1 in
        let v2 =
          match v2 with
          | `This x ->
              this env x
          | `Super x ->
              super env x
        in
        v2
    | `Choice_prim_exp_DOT_opt_type_args_super (v1, v2, v3, v4) ->
        let v1 =
          match v1 with
          | `Prim_exp x ->
              primary_expression env x
        in
        let v2 = (* "." *) token env v2 in
        let _v3 = Option.map (type_arguments env) v3 in
        let v4 = super_to_field_name env v4 in
        G.DotAccess (v1, v2, v4) |> G.e
  in
  let v2 = argument_list env v2 in
  let v3 = (* ";" *) token env v3 in
  G.ExprStmt (G.Call (v1, v2) |> G.e, v3) |> G.s

(* NEW *)
and expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id x -> identifier_to_expression env x
        | `Field_access x -> field_access env x
        | `Array_access x -> array_access env x
        )
      in
      let v2 = assignment_operator env v2 in
      let v3 = expression env v3 in
      G.AssignOp (v1, v2, v3) |> G.e
  | `Bin_exp x ->
      binary_expression env x
  | `Inst_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "is" *) in
      let v3 = type_ env v3 in
      Call (IdSpecial (Instanceof, v2) |> G.e, fb [ Arg v1; ArgType v3 ]) |> G.e
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = expression env v5 in
      G.Conditional (v1, v3, v5) |> G.e
  | `Update_exp x ->
      update_expression env x
  | `Prim_exp x ->
      primary_expression env x
  | `Un_exp x ->
      unary_expression env x
  | `Cast_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = type_ env v2 in
      let _v3 = token env v3 (* ")" *) in
      let v4 = expression env v4 in
      G.Cast (v2, v1, v4) |> G.e
  | `Dml_exp x ->
      dml_expression env x
  | `Switch_exp x ->
      switch_expression env x
      |> G.stmt_to_expr (* FIXME: "switch" is expr in parser, while it should be stmt *)

(* AUX *)
and assignment_operator (env : env) x : G.operator G.wrap =
  match x with
  | `EQ tok -> (Eq, token env tok) (* "=" *)
  | `PLUSEQ tok -> (Plus, token env tok) (* "+=" *)
  | `DASHEQ tok -> (Minus, token env tok) (* "-=" *)
  | `STAREQ tok -> (Mult, token env tok) (* "*=" *)
  | `SLASHEQ tok -> (Div, token env tok) (* "/=" *)
  | `AMPEQ tok -> (BitAnd, token env tok) (* "&=" *)
  | `BAREQ tok -> (BitOr, token env tok) (* "|=" *)
  | `HATEQ tok -> (BitXor, token env tok) (* "^=" *)
  | `PERCEQ tok -> (Mod, token env tok) (* "%=" *)
  | `LTLTEQ tok -> (LSL, token env tok) (* "<<=" *)
  | `GTGTEQ tok -> (ASR, token env tok) (* ">>=" *)
  | `GTGTGTEQ tok -> (LSR, token env tok) (* ">>>=" *)

(* NEW *)
and expression_statement (env : env) ((v1, v2) : CST.expression_statement) : G.stmt =
  let v1 = expression env v1 in
  let v2 = (* ";" *) token env v2 in
  G.ExprStmt (v1, v2) |> G.s

(* NEW *)
and extends_interfaces (env : env) ((v1, v2) : CST.extends_interfaces) : G.type_ list =
  let v1 = (* "extends "*) token env v1 in
  let v2 = type_list env v2 in
  v2

(* NEW *)
and field_access (env : env) ((v1, v2, v3, v4) : CST.field_access) : G.expr =
  let v1 = anon_choice_prim_exp_bbf4eda env v1 in
  let v =
    match v2 with
    | Some (w1, w2) ->
        let e, t = property_navigation env w1 in
        let v1 = add_elvis e v1 in
        let sup = (* "super" *) super_to_field_name env w2 in
        G.DotAccess (v1, t, sup) |> G.e
    | None -> v1
  in
  let e, t = property_navigation env v3 in
  let v = add_elvis e v in
  let v4 = match v4 with
    | `Id x -> G.FN (H2.name_of_id (identifier env x ))
    | `This x -> (* "this"*) this_to_field_name env x
  in
  G.DotAccess (v, t, v4) |> G.e

(* NEW *)
and field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration) : G.stmt =
match v4 with
  | `SEMI tok ->
      local_variable_declaration env (v1, v2, v3, tok)
  | `Acce_list x ->
      let fname, _ = v3 |> fst |> fst |> fst |> identifier env in
      let attrs = Option.map (modifiers env) v1 in
      let typ = unannotated_type env v2 in
      let ptype = Some (make_type_opt attrs typ) in
      let varStmt = local_variable_declaration_data_only env (v1, v2, v3) |> var_def_stmt (fake ";") in
      let lb, accessors, rb = accessor_list env x in
      let funcs =
        accessors |>
        List.map
          (fun (attrs, id, fbody) ->
            let iname, itok = id in
            let iname = String.lowercase_ascii iname in
            let has_params = iname <> "get" in
            let has_return = iname = "get" in
            let ent = basic_entity (iname ^ "_" ^ fname, itok) ~attrs in
            let funcdef =
              FuncDef
                {
                  fkind = (Method, itok);
                  fparams =
                    fb
                      (if has_params then
                          [
                            Param
                              {
                                pname = Some ("value", fake "value");
                                ptype;
                                pdefault = None;
                                pattrs = [];
                                pinfo = empty_id_info ();
                              };
                          ]
                        else []);
                  frettype = (if has_return then ptype else None);
                  (* TODO Should this be "void"? *)
                  fbody;
                }
            in
            DefStmt (ent, funcdef) |> G.s)
      in
      Block (lb, varStmt :: funcs, rb)
      |> G.s

(* NEW *)
and finally_clause (env : env) ((v1, v2) : CST.finally_clause) : G.finally =
  let v1 = token env v1 (* "finally" *) in
  let v2 = trigger_body env v2 in
  v1, v2

(* RAW QUERY *)
and find_clause (env : env) ((v1, v2) : CST.find_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "find" *) str env v1 in
  let v2 =
    (match v2 with
    | `Bound_apex_exp x ->
        bound_apex_expression env x
    | `Term_sepa_start_term_term_sepa_end (v1, v2, v3) ->
        let v1 = (* "'" *) token env v1 in
        let v2 = (* pattern "(\\\\\\'|[^'])+" *) str env v2 in
        let v3 = (* "'" *) token env v3 in
        G.L (G.String (v1, v2 ,v3)) |> G.e
    )
  in
  R.Tuple [R.Token v1; R.Any (G.E v2)]

(* NEW *)
and for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) : G.stmt =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | `Semg_ellips tok ->
        let t = (* "..." *) token env tok in
        G.ForEllipsis t
    | `Choice_local_var_decl_opt_exp_SEMI_opt_exp_rep_COMMA_exp (v1, v2, v3, v4) ->
        let v1 (* : G.for_var_or_expr list *) =
          match v1 with
          | `Local_var_decl (v1, v2, v3, _) ->
               local_variable_declaration_data_only env (v1, v2, v3)
               |> List.map (fun (t1, t2) -> G.ForInitVar (t1, t2))
          | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) ->
              let v1 =
                match v1 with
                | Some x ->
                    let e, tes = anon_exp_rep_COMMA_exp_0bb260c env x in
                    (e :: List.map snd tes)
                    |> List.map (fun f -> ForInitExpr f)
                | None -> []
              in
              let _v2 = (* ";" *) token env v2 in
              v1
        in
        let v2 = Option.map (expression env) v2 in
        let _v3 = (* ";" *) token env v3 in
        let v4 =
          match v4 with
          | Some x ->
              (* FIXME: What to do with the other expressions here? *)
              let e, _ = anon_exp_rep_COMMA_exp_0bb260c env x in
              Some e
          | None -> None
        in
        G.ForClassic (v1, v2, v4)
  in
  let v4 = (* ")" *) token env v4 in
  let v5 = statement env v5 in
  For (v1, v3, v5) |> G.s

(* NEW *)
and formal_parameter (env : env) (x : CST.formal_parameter) : G.parameter =
  match x with
  | `Opt_modifs_unan_type_var_decl_id (v1, v2, v3) ->
      let t_attrs =
        match v1 with
        | Some x -> modifiers env x
        | None -> []
      in
      let t = unannotated_type env v2 in
      let v3 = variable_declarator_id env v3 in
      G.Param (G.param_of_id v3 ~ptype:{t; t_attrs})
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      G.ParamEllipsis t
  | `Semg_meta_ellips tok ->
      (* FIXME: double-check if this is correct *)
      let id = str env tok in
      G.Param (G.param_of_id id)

(* NEW *)
and formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters)
    : G.parameter list bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = formal_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = formal_parameter env v2 in
            v2
          ) v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  v1, v2, v3

(* RAW QUERY *)
and function_expression (env : env) (x : CST.function_expression) : G.expr =
  let module R = Raw_tree in
  match x with
  | `Pat_dist_LPAR_choice_field_id_COMMA_geo_loca_type_COMMA_str_lit_RPAR (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = str env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 (* : expr *) =
        match v3 with
        | `Field_id x ->
            field_identifier env x
        | `Bound_apex_exp x ->
            bound_apex_expression env x
      in
      let v4 = (* "," *) token env v4 in
      let v5 = geo_location_type env v5 in
      let v6 = (* "," *) token env v6 in
      let v7 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) str env v7
      in
      let v7e = G.N (H2.name_of_id v7) |> G.e in
      let v8 = (* ")" *) token env v8 in
      G.Call
        (G.N (H2.name_of_id v1) |> G.e,
         (v2, [G.Arg v3; G.Arg v5; G.Arg v7e], v8))
      |> G.e
  | `Func_name_LPAR_value_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = function_name env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = value_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      G.Call
        (G.N (H2.name_of_id v1) |> G.e,
         (v2, [G.Arg v3], v4))
      |> G.e

(* NEW *)
and generic_type (env : env) ((v1, v2) : CST.generic_type) : (G.ident * G.type_arguments option) list =
  match v1 with
    | `Id x ->
        let i = identifier env x in
        let ta = type_arguments env v2 in
        [i, Some ta]
    | `Scoped_type_id x ->
        scoped_type_identifier env x

(* RAW QUERY*)
  and geo_location_type (env : env) (x : CST.geo_location_type) : G.expr =
  match x with
  | `Field_id x ->
      field_identifier env x
  | `Bound_apex_exp x ->
      bound_apex_expression env x
  | `Pat_geol_LPAR_deci_COMMA_deci_RPAR (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "geolocation" *) str env v1 in
      let v2 = (* "(" *) token env v2 in
      let s3, v3 = (* pattern -?\d+(\.\d+)? *) str env v3 in
      let v4 = (* "," *) token env v4 in
      let s5, v5 = (* pattern -?\d+(\.\d+)? *) str env v5 in
      let v6 = (* ")" *) token env v6 in
      G.Call
        (G.N (H2.name_of_id v1) |> G.e,
         (v2, [G.Arg (G.L (G.Float (float_of_string_opt s3, v3)) |> G.e);
               G.Arg (G.L (G.Float (float_of_string_opt s5, v5)) |> G.e)], v6))
      |> G.e

(* RAW QUERY *)
and group_by_clause (env : env) ((v1, v2, v3, v4) : CST.group_by_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "group" *) str env v1 in
  let v2 = (* "by" *) str env v2 in
  let v3 = group_by_expression env v3 in
  let v4 =
    match v4 with
    | Some x -> R.Option (Some (
        having_clause env x
      ))
    | None -> R.Option None
  in
  R.Tuple [R.Token v1; R.Token v2; v3; v4]

(* RAW QUERY *)
and group_by_expression (env : env) (x : CST.group_by_expression) : raw =
  let module R = Raw_tree in
  match x with
  | `Choice_field_id_rep_COMMA_choice_field_id (v1, v2) ->
      let v1 = anon_choice_field_id_cb081aa env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "," *) token env v1 in
          let v2 = anon_choice_field_id_cb081aa env v2 in
          v2
        ) v2
      in
      let es = v1 :: v2 in
      R.List (List.map (fun e -> R.Any (G.E e)) es)
  | `Choice_pat_rollup_LPAR_field_id_rep_COMMA_field_id_RPAR (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | `Pat_rollup x
        | `Pat_cube x ->
            str env x
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = field_identifier env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "," *) token env v1 in
          let v2 = field_identifier env v2 in
          v2
        ) v4
      in
      let es = v3 :: v4 in
      let v5 = (* ")" *) token env v5 in
      R.Any (G.E
        (G.Call
          (G.N (H2.name_of_id v1) |> G.e,
           (v2, List.map (fun e -> G.Arg e) es, v5))
        |> G.e))

(* RAW QUERY *)
and having_boolean_expression (env : env) (x : CST.having_boolean_expression)
    : G.expr =
  match x with
  | `Having_and_exp (v1, v2) ->
      let v1 = having_condition_expression env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "and" *) token env v1 in
          let v2 = having_condition_expression env v2 in
          v2
        ) v2
      in
      let es = v1 :: v2 in
      let args = List.map (fun e -> G.Arg e) es in
      G.Call (G.IdSpecial (G.Op G.And, fake "") |> G.e, fb args) |> G.e
  | `Having_or_exp (v1, v2) ->
      let v1 = having_condition_expression env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let _v1 = (* "or" *) token env v1 in
          let v2 = having_condition_expression env v2 in
          v2
        ) v2
      in
      let es = v1 :: v2 in
      let args = List.map (fun e -> G.Arg e) es in
      G.Call (G.IdSpecial (G.Op G.Or, fake "") |> G.e, fb args) |> G.e
  | `Having_not_exp (v1, v2) ->
      let v1 = (* "not" *) token env v1 in
      let v2 = having_condition_expression env v2 in
      G.Call (G.IdSpecial (G.Op G.Not, v1) |> G.e, fb [G.Arg v2]) |> G.e
  | `Having_cond_exp x ->
      having_condition_expression env x

(* RAW QUERY *)
and having_clause (env : env) ((v1, v2) : CST.having_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "having" *) str env v1 in
  let v2 = having_boolean_expression env v2 in
  R.Tuple [R.Token v1; R.Any (G.E v2)]

(* RAW QUERY *)
and having_comparison (env : env) (e1 : G.expr) (x : CST.having_comparison) : G.expr =
  match x with
  | `Having_value_comp (v1, v2) ->
      let v1 = value_comparison_operator env v1 in
      let v2 = anon_choice_soql_lit_3019e24 env v2 in
      G.Call (v1, fb [G.Arg e1; G.Arg v2]) |> G.e
  | `Having_set_comp (v1, v2) ->
      let v1 = set_comparison_operator env v1 in
      let v2 =
        match v2 with
        | `LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR x ->
            anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78 env x
        | `Bound_apex_exp x ->
            bound_apex_expression env x
      in
      G.Call (v1, fb [G.Arg e1; G.Arg v2]) |> G.e

(* RAW QUERY *)
and having_condition_expression (env : env) (x : CST.having_condition_expression)
    : G.expr =
  match x with
  | `LPAR_having_bool_exp_RPAR (v1, v2, v3) ->
      let _v1 = (* "(" *) token env v1 in
      let v2 = having_boolean_expression env v2 in
      let _v3 = (* ")" *) token env v3 in
      v2
  | `Having_comp_exp (v1, v2) ->
      let v1 = function_expression env v1 in
      let v2 = having_comparison env v1 v2 in
      v2

(* NEW *)
and if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) : G.stmt =
  let v1 = token env v1 (* "if" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = statement env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "else" *) in
        let v2 = statement env v2 in
        Some v2
    | None -> None
  in
  G.If (v1, G.Cond v2, v3, v4) |> G.s

(* NEW *)
and interface_body (env : env) ((v1, v2, v3) : CST.interface_body)
    : G.stmt list bracket =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.filter_map (fun x ->
      match x with
      | `Semg_ellips tok ->
          let t = (* "..." *) token env tok in
          let v2 = G.sc in
          Some (G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s)
      | `Cst_decl x ->
          Some (constant_declaration env x)
      | `Enum_decl x ->
          Some (enum_declaration env x)
      | `Meth_decl x ->
          Some (method_declaration env x)
      | `Class_decl x ->
          Some (class_declaration env x)
      | `Inte_decl x ->
          Some (interface_declaration env x)
      | `SEMI tok ->
          let _t = (* ";" *) token env tok
          in None
    ) v2
  in
  let v3 = (* "}" *) token env v3 in
  v1, v2, v3

(* NEW *)
and interface_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.interface_declaration) : G.stmt =
  let v1 =
    match v1 with
    | Some x -> modifiers env x
    | None -> []
  in
  let v2 = (* "interface" *) token env v2 in
  let v3 = identifier env v3 in
  let v4 = Option.map (type_parameters env) v4 in
  let v5 =
    match v5 with
    | Some x -> extends_interfaces env x
                |> List.map (fun x -> (x, None))
    | None -> []
  in
  let lb, v6, rb = interface_body env v6 in
  let fields = List_.map (fun x -> G.F x) v6 in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v3, idinfo)); attrs = v1; tparams = v4 } in
  G.DefStmt
    ( ent,
      G.ClassDef
        {
          ckind = (Interface, v2);
          cextends = v5;
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody = (lb, fields, rb);
        } )
  |> G.s

(* NEW *)
and interfaces (env : env) ((v1, v2) : CST.interfaces) : G.type_ list =
  let _v1 = (* "implementes" *) token env v1 in
  let v2 = type_list env v2 in
  v2

(* NEW *)
and labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) : G.stmt =
  let v1 = identifier env v1 (* identifier *) in
  let _v2 = token env v2 (* ":" *) in
  let v3 = statement env v3 in
  G.Label (v1, v3) |> G.s

(* RAW QUERY *)
and limit_clause (env : env) ((v1, v2) : CST.limit_clause) : G.expr list =
  let v1 = (* "limit" *) str env v1 in
  let v2 = anon_choice_int_1466488 env v2 in
  [G.L (G.String (fb v1)) |> G.e; v2]

(* NEW *)
and local_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.local_variable_declaration) : G.stmt =
  let d = local_variable_declaration_data_only env (v1, v2, v3) in
  let v4 = (* ";" *) token env v4 in
  var_def_stmt v4 d

(* AUX *)
(* without semicolon *)
and local_variable_declaration_data_only (env : env) ((v1, v2, v3))
    : (entity * variable_definition) list =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  List_.map
    (fun (ent, vardef) ->
      (ent, { vinit = vardef.vinit; vtype = Some (make_type v1 v2); vtok = G.no_sc }))
    v3

(* AUX *)
and var_def_stmt (sc : Tok.t)
    (decls : (entity * variable_definition) list) : G.stmt =
  let stmts =
    decls
    |> H2.add_semicolon_to_last_var_def_and_convert_to_stmts sc
  in
  G.stmt1 stmts

(* NEW *)
and new_map_creation_expression (env : env) ((v1, v2, v3) : CST.map_creation_expression)
    : G.expr =
  let _v1 = (* "new" *) token env v1 in
  let _v2 = simple_type env v2 in
  let v3 = new_map_initializer env v3 in
  v3

(* NEW *)
and new_map_initializer (env : env) ((v1, v2, v3) : CST.map_initializer)
    : G.expr =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = new_map_initializer_ env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = new_map_initializer_ env v2 in
            v2
          ) v2
        in
        v1 :: v2
    | None -> [])
  in
  let v3 = (* "}" *) token env v3 in
  G.Container (G.Dict, (v1, v2, v3)) |> G.e

(* NEW *)
and new_map_initializer_ (env : env) ((v1, v2, v3) : CST.map_initializer_)
    : G.expr =
  let v1 = expression env v1 in
  let _v2 = (* "=>" *) token env v2 in
  let v3 = expression env v3 in
  G.Container (G.Tuple, fb [v1; v3]) |> G.e

(* NEW *)
and method_declaration (env : env) ((v1, v2, v3) : CST.method_declaration) : G.stmt =
  let v1 =
    match v1 with
    | Some x -> modifiers env x
    | None -> []
  in
  let body =
    match v3 with
    | `Blk x ->
        G.FBStmt (trigger_body env x)
    | `SEMI tok ->
        let t = (* ";" *) token env tok in
        G.FBDecl t
  in
  make_method env v1 body v2

(* NEW *)
and method_declarator (env : env) ((v1, v2, v3) : CST.method_declarator)
    : G.ident * G.parameter list bracket * (G.tok * G.tok) list =
  let v1 = identifier env v1 in
  let v2 = formal_parameters env v2 in
  let v3 =
    match v3 with
    | Some x -> dimensions env x
    | None -> []
  in
  v1, v2, v3

(* NEW *)
and method_header (env : env) ((v1, v2, v3) : CST.method_header)
  : G.entity * G.function_definition =
  let tparams, annots =
    match v1 with
    | None -> None, []
    | Some (v1, v2) ->
        let tp = type_parameters env v1 in
        Some tp, List.map (annotation env) v2
  in
  let v2 = unannotated_type env v2 in
  (* FIXME: What do dimensions do here? *)
  let i, params, _dim = method_declarator env v3 in
  let _, tok = i in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (i, idinfo)); attrs = []; tparams } in
  ent,
  {
    fkind = (G.Method, tok);
    fparams = params;
    frettype = Some (make_type annots v2);
    fbody = G.FBNothing;
  }

(* NEW *)
and make_method (env : env) (mods : G.attribute list) (body : G.function_body)
    (h : CST.method_header) : G.stmt =
  let ent, d = method_header env h in
  let ent = { ent with attrs = mods } in
  let def = G.FuncDef { d with fbody = body } in
  G.DefStmt (ent, def) |> G.s

(* NEW *)
and method_invocation (env : env) ((v1, v2) : CST.method_invocation) =
  let v1 =
    match v1 with
    | `Id x -> identifier_to_expression env x
    | `Choice_prim_exp_prop_navi_opt_super_prop_navi_opt_type_args_id (v1, v2, v3, v4, v5) ->
        let v1 = anon_choice_prim_exp_bbf4eda env v1 in
        let e, t = property_navigation env v2 in
        let v1 = add_elvis e v1 in
        let v3, e', t' =
          match v3 with
          | Some (w1, w2) ->
              let s = (* "super" *) super_to_field_name env w1 in
              let e', t' = property_navigation env v2 in
              G.DotAccess (v1, t', s) |> G.e, e', t'
          | None -> v1, e, t
        in
        let targs =
          match v4 with
          | Some x -> type_arguments env x
          | None -> fb []
        in
        let v5 = identifier env v5 in
        let b = add_elvis e' v3 in
        G.DotAccess (b, t', G.FN (H2.name_of_id v5)) |> G.e
  in
  let v2 = argument_list env v2 in
  G.Call (v1, v2) |> G.e

(* AUX *)
and add_elvis (b : G.tok option) (e : G.expr) : G.expr =
  match b with
  | Some t ->
      G.Call (G.IdSpecial (G.Op G.Elvis, t) |> G.e, fb [ G.Arg e ]) |> G.e
  | None -> e

(* NEW *)
and modifiers (env : env) (xs : CST.modifiers) : attribute list =
  List.map (fun x ->
    match x with
    | `Anno x -> annotation env x (* G.NamedAttr *)
    | `Modi x -> modifier env x (* G.KeywordAttr or G.OtherAttribute *)
  ) xs

(* NEW *)
and object_creation_expression (env : env) (x : CST.object_creation_expression) : G.expr =
  unqualified_object_creation_expression env x

(* RAW QUERY *)
and offset_clause (env : env) ((v1, v2) : CST.offset_clause) : G.expr list =
  let v1 = (* "offset" *) str env v1 in
  let v2 = anon_choice_int_1466488 env v2 in
  [G.L (G.String (fb v1)) |> G.e; v2]

(* RAW QUERY *)
and order_by_clause (env : env) ((v1, v2, v3, v4) : CST.order_by_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "order" *) str env v1 in
  let v2 = (* "by" *) str env v2 in
  let v3 = order_expression env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = order_expression env v2 in
      v2
    ) v4
  in
  R.Tuple [R.Token v1; R.Token v2; R.List (v3 :: v4)]

(* RAW QUERY *)
and order_expression (env : env) ((v1, v2, v3) : CST.order_expression) : raw =
  let module R = Raw_tree in
  let v1 = value_expression env v1 in
  let v2 =
    match v2 with
    | Some x -> R.Option (Some (
        order_direction env x
      ))
    | None -> R.Option None
  in
  let v3 =
    match v3 with
    | Some x -> R.Option (Some (
        order_null_direciton env x
      ))
    | None -> R.Option None
  in
  R.Tuple [R.Any (G.E v1); v2; v3]

(* AUX *)
and adjust_range_of_parenthesized_expr (env : env)
    (l : Tree_sitter_run.Token.t) (r : Tree_sitter_run.Token.t) (e : G.expr) : G.expr =
  let lloc = Tok.unsafe_loc_of_tok (token env l) in
  let rloc = Tok.unsafe_loc_of_tok (token env r) in
  e.e_range <- Some (lloc, rloc);
  e

(* NEW *)
and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : G.expr =
  expression env v2
  |> adjust_range_of_parenthesized_expr env v1 v3

(* NEW *)
and partial_catch (env : env) (x : CST.partial_catch) : G.catch =
  catch_clause env x

(* NEW *)
and partial_finally (env : env) (x : CST.partial_finally) : G.finally =
  finally_clause env x

(* NEW *)
and primary_expression (env : env) (x : CST.primary_expression) : G.expr =
  match x with
  | `Choice_lit x ->
      (match x with
      | `Lit x ->
          let lit = literal env x in
          G.L lit |> G.e
      | `Class_lit x ->
          class_literal env x
      | `This x ->
          this env x
      | `Id x ->
          identifier_to_expression env x
      | `Paren_exp x ->
          parenthesized_expression env x
      | `Obj_crea_exp x ->
          object_creation_expression env x
      | `Field_access x ->
          field_access env x
      | `Array_access x ->
          array_access env x
      | `Meth_invo x ->
          method_invocation env x
      | `Array_crea_exp x ->
          array_creation_expression env x
      | `Map_crea_exp x ->
          new_map_creation_expression env x
      | `Query_exp x -> query_expression env x
      )
  | `Semg_deep_exp (v1, v2, v3) ->
      let v1 = token env v1 in
      let v2 = expression env v2 in
      let v3 = token env v3 in
      G.DeepEllipsis (v1, v2, v3) |> G.e

(* RAW QUERY *)
and query_expression (env : env) ((v1, v2, v3) : CST.query_expression)
    : G.expr =
  let v1 = (* "[" *) str env v1 in
  let v2 =
    match v2 with
    | `Soql_query x ->
        soql_query env x
    | `Sosl_query x ->
        sosl_query env x
  in
  let _v3 = (* "]" *) token env v3 in
  G.Call
    (G.N (Id (v1, G.empty_id_info ())) |> G.e,
     fb (List.map (fun e -> G.Arg e) v2)) |> G.e

(* RAW QUERY *)
and query_expression_ (env : env) (x : CST.query_expression_) : G.expr list =
  sosl_query_body env x

(* NEW *)
and return_statement (env : env) ((v1, v2, v3) : CST.return_statement) : G.stmt =
  let v1 = token env v1 (* "return" *) in
  let v2 = Option.map (expression env) v2 in
  let v3 = token env v3 (* ";" *) in
  Return (v1, v2, v3) |> G.s

(* RAW QUERY *)
and returning_clause (env : env) ((v1, v2, v3) : CST.returning_clause) : raw =
  let module R = Raw_tree in
  let v1 = (* "returning" *) str env v1 in
  let v2 = sobject_return env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = sobject_return env v2 in
      v2
    ) v3
  in
  let srets = v2 :: v3 in
  R.Tuple [R.Token v1; R.List srets]

(* NEW *)
and run_as_statement (env : env) ((v1, v2, v3) : CST.run_as_statement) : G.stmt =
  let v1 = token env v1 (* "System.runAs" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = trigger_body env v3 in
  G.RawStmt (Raw_tree.Case ("", Raw_tree.Any (G.Raw (Raw_tree.Tuple
    [Raw_tree.Any (G.Tk v1); Raw_tree.Any (G.E v2); Raw_tree.Any (G.S v3)])))) |> G.s

(* NEW *)
and scoped_type_identifier (env : env) ((v1, v2, v3, v4) : CST.scoped_type_identifier)
  : (G.ident * G.type_arguments option) list =
  let v1 =
    match v1 with
    | `Id x -> [identifier env x, None]
    | `Scoped_type_id x -> scoped_type_identifier env x
    | `Gene_type x -> generic_type env x
  in
  let v2 = (* "." *) token env v2 in
  (* FIXME: There is no place in the AST to put these attributes. In the AST attrs
   * occur only on the toplevel of a type, while in Apex we can have them in the
   * path in a scoped type, which is represented by G.name (i.e., a list of idents,
   * without attrs). *)
  let _v3 = List.map (annotation env) v3 in
  let v4 = identifier env v4 in
  v1 @ [v4, None]

(* RAW QUERY *)
and select_clause (env : env) ((v1, v2) : CST.select_clause)
    : G.expr list =
  let v1 = (* "select" *) str env v1 in
  let v2 =
    match v2 with
    | `Count_exp x ->
        [count_expression env x]
    | `Sele_exp_rep_COMMA_sele_exp x ->
        selected_fields env x
  in
  (G.L (G.String (fb v1)) |> G.e) :: v2

(* RAW QUERY *)
and selectable_expression (env : env) (x : CST.selectable_expression)
    : G.expr =
  let module R = Raw_tree in
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      G.Ellipsis t |> G.e
  | `Semg_meta_ellips tok ->
      let i = (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) str env tok in
      G.N (G.Id (i, G.empty_id_info ())) |> G.e
  | `Choice_value_exp x ->
      match x with
      | `Value_exp x ->
          value_expression env x
      | `Alias_exp x ->
          alias_expression env x
      | `Type_of_clause x ->
          G.RawExpr (type_of_clause env x) |> G.e
      | `Fields_exp x ->
          G.RawExpr (fields_expression env x) |> G.e
      | `Subq x ->
          G.RawExpr (subquery env x) |> G.e

(* RAW QUERY *)
and selected_fields (env : env) ((v1, v2) : CST.selected_fields) : G.expr list =
  let module R = Raw_tree in
  let v1 = selectable_expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = selectable_expression env v2 in
      v2
    ) v2
  in
  v1 :: v2

(* NEW *)
and simple_type (env : env) (x : CST.simple_type) : G.type_kind =
  match x with
  | `Void_type x ->
      let i = ("void", token env x) in
      let n = H2.name_of_id i in
      TyN n
  | `Bool_type x ->
      let i = ("boolean", token env x) in
      let n = H2.name_of_id i in
      TyN n
  | `Id x ->
      let i = identifier env x in
      let n = H2.name_of_id i in
      TyN n
  | `Scoped_type_id x ->
      let i = scoped_type_identifier env x in
      TyN (H2.name_of_ids_with_opt_typeargs i)
  | `Gene_type x ->
      let i = generic_type env x in
      TyN (H2.name_of_ids_with_opt_typeargs i)

(* RAW QUERY *)
and sobject_return (env : env) ((v1, v2) : CST.sobject_return) : raw =
  let module R = Raw_tree in
  let v1 = identifier env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4, v5, v6, v7, v8) ->
        let v1 = (* "(" *) str env v1 in
        let v2 =
          R.List
            (List.map
               (fun e -> R.Any (G.E e))
               (selected_fields env v2))
        in
        let v3 =
          match v3 with
          | Some x -> R.Option (Some (
              using_clause env x
            ))
          | None -> R.Option None
        in
        let v4 =
          match v4 with
          | Some x ->
              R.List
                (List.map
                  (fun e -> R.Any (G.E e))
                  (where_clause env x))
          | None -> R.Option None
        in
        let v5 =
          match v5 with
          | Some x -> R.Option (Some (
              order_by_clause env x
            ))
          | None -> R.Option None
        in
        let v6 =
          match v6 with
          | Some x -> R.List
              (List.map
                 (fun e -> R.Any (G.E e))
                 (limit_clause env x))
          | None -> R.Option None
        in
        let v7 =
          match v7 with
          | Some x -> R.List
              (List.map
                 (fun e -> R.Any (G.E e))
                 (offset_clause env x))
          | None -> R.Option None
        in
        let v8 = (* ")" *) str env v8 in
        R.Tuple [R.Token v1; v2; v3; v4; v5; v6; v7; R.Token v8]
    | None -> R.Option None
  in
  R.Tuple [R.Token v1; v2]

(* RAW QUERY *)
and soql_query (env : env) (v1 : CST.soql_query) : G.expr list =
  soql_query_expression env v1

(* RAW QUERY *)
and soql_query_body (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) : CST.soql_query_body)
    : G.expr list =
  let module R = Raw_tree in
  let v1 = select_clause env v1 in
  let v2 = [G.RawExpr (from_clause env v2) |> G.e] in
  let v3 =
    match v3 with
    | Some x -> [G.RawExpr (soql_using_clause env x) |> G.e]
    | None -> []
  in
  let v4 =
    match v4 with
    | Some x -> where_clause env x
    | None -> []
  in
  let v5 =
    match v5 with
    | Some x -> [G.RawExpr (soql_with_clause env x) |> G.e]
    | None -> []
     in
  let v6 =
    match v6 with
    | Some x -> [G.RawExpr (group_by_clause env x) |> G.e]
    | None -> []
     in
  let v7 =
    match v7 with
    | Some x -> [G.RawExpr (order_by_clause env x) |> G.e]
    | None -> []
  in
  let v8 =
    match v8 with
    | Some x -> limit_clause env x
    | None -> []
  in
  let v9 =
    match v9 with
    | Some x -> offset_clause env x
    | None -> []
  in
  let v10 =
    match v10 with
    | Some x -> [G.RawExpr (for_clause env x) |> G.e]
    | None -> []
  in
  let v11 =
    match v11 with
    | Some x -> update_clause env x
    | None -> []
  in
  let v12 =
    match v12 with
    | Some x -> [G.RawExpr (all_rows_clause env x) |> G.e]
    | None -> []
  in
  List.concat [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12]

(* RAW QUERY *)
and soql_query_expression (env : env) (x : CST.soql_query_expression) : G.expr list =
  soql_query_body env x

(* RAW QUERY *)
and sosl_query (env : env) (v1 : CST.sosl_query) : G.expr list =
  query_expression_ env v1

(* RAW QUERY *)
and sosl_query_body (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.sosl_query_body)
    : G.expr list =
  let module R = Raw_tree in
  let v1 = [G.RawExpr (find_clause env v1) |> G.e] in
  let v2 =
    match v2 with
    | Some x -> [G.RawExpr (in_clause env x) |> G.e]
    | None -> []
  in
  let v3 =
    match v3 with
    | Some xs -> [G.RawExpr (R.List (List.map (returning_clause env) xs)) |> G.e]
    | None -> []
  in
  let v4 =
    match v4 with
    | Some xs ->
        [G.RawExpr (R.List (List.map (sosl_with_clause env) xs)) |> G.e]
    | None -> []
  in
  let v5 =
    match v5 with
    | Some x -> limit_clause env x
    | None -> []
  in
  let v6 =
    match v6 with
    | Some x -> offset_clause env x
    | None -> []
  in
  let v7 =
    match v7 with
    | Some x -> update_clause env x
    | None -> []
  in
  List.concat [v1; v2; v5; v6; v7]

(* RAW QUERY *)
and sosl_with_clause (env : env) ((v1, v2) : CST.sosl_with_clause) =
  let module R = Raw_tree in
  let v1 = (* "with" *) str env v1 in
  let v2 = sosl_with_type env v2 in
  R.Tuple [R.Token v1; v2]

(* RAW QUERY *)
and sosl_with_type (env : env) (x : CST.sosl_with_type) : raw =
  let module R = Raw_tree in
  match x with
  | `With_data_cat_exp x ->
      with_data_cat_expression env x
  | `With_divi_exp (v1, v2, v3) ->
      let v1 = (* "division" *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 =
        match v3 with
        | `Bound_apex_exp x ->
            bound_apex_expression env x
        | `Str_lit tok ->
            let v = (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) str env tok in
            G.L (G.String (fake "", v, fake "")) |> G.e
      in
      R.Tuple [R.Token v1; R.Any (G.E v3)]
  | `With_high x ->
      R.Token (with_highlight env x)
  | `With_meta_exp (v1, v2, v3) ->
      let v1 = (* "metadata" *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) str env v3
      in
      R.Tuple [R.Token v1; R.Token v3]
  | `With_netw_exp (v1, v2) -> R.Case ("With_netw_exp",
      let v1 = (* "network" *) str env v1 in
      let v2 = comparison env (G.L (G.Unit (fake "")) |> G.e) v2 in
      R.Tuple [R.Token v1; R.Any (G.E v2)]
    )
  | `With_pric_exp (v1, v2, v3) -> R.Case ("With_pric_exp",
      let v1 = (* "priceBookID" *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) str env v3
      in
      R.Tuple [R.Token v1; R.Token v3]
    )
  | `With_snip_exp (v1, v2) ->
      let v1 = (* "snippet" *) str env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
            let _v1 = (* "(" *) token env v1 in
            let v2 = (* "target_length" *) str env v2 in
            let _v3 = (* "=" *) token env v3 in
            let v4 = (* int *) str env v4 in
            let e = G.L (G.Int (Parsed_int.parse v4)) |> G.e in
            let _v5 = (* ")" *) token env v5 in
            R.Tuple [R.Token v2; R.Any (G.E e)]
          ))
        | None -> R.Option None)
      in
      R.Tuple [R.Token v1; v2]
  | `With_spell_corr_exp (v1, v2, v3) ->
      let v1 = (* "specll_correction" *) str env v1 in
      let _v2 = (* "=" *) token env v2 in
      let v3 = boolean env v3 in
      let e = G.L v3 |> G.e in
      R.Tuple [R.Token v1; R.Any (G.E e)]

(* NEW *)
and statement (env : env) (x : CST.statement) : G.stmt =
  match x with
  | `Choice_decl x ->
      (match x with
      | `Decl x ->
          declaration env x
      | `Exp_stmt x ->
          expression_statement env x
      | `Labe_stmt x ->
          labeled_statement env x
      | `If_stmt x ->
          if_statement env x
      | `While_stmt x ->
          while_statement env x
      | `For_stmt x ->
          for_statement env x
      | `Enha_for_stmt x ->
          enhanced_for_statement env x
      | `Blk x ->
          trigger_body env x
      | `SEMI tok ->
          let v1 = token env tok (* ";" *) in
          Block (v1, [], v1) |> G.s
      | `Do_stmt x ->
          do_statement env x
      | `Brk_stmt x ->
          break_statement env x
      | `Cont_stmt x ->
          continue_statement env x
      | `Ret_stmt x ->
          return_statement env x
      | `Switch_exp x ->
          switch_expression env x
      | `Local_var_decl x ->
          local_variable_declaration env x
      | `Throw_stmt x ->
          throw_statement env x
      | `Try_stmt x ->
          try_statement env x
      | `Run_as_stmt x ->
          run_as_statement env x
      )
  | `Semg_ellips tok ->
      let v1 = token env tok (* "..." *) in
      let v2 = G.sc in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s

(* NEW *)
and static_initializer (env : env) ((v1, v2) : CST.static_initializer) : G.stmt =
  let _, t as v1 = str env v1 in
  let v2 = trigger_body env v2 in
  let attrs = [KeywordAttr (G.Static, t)] in
  let ent = basic_entity v1 ~attrs in
  let def =
    G.FuncDef
      { fkind = (G.Method, t); fparams = fb []; frettype = None; fbody = G.FBStmt v2 }
  in
  G.DefStmt (ent, def) |> G.s

(* RAW QUERY *)
and subquery (env : env) ((v1, v2, v3) : CST.subquery) : raw =
  let module R = Raw_tree in
  let _v1 = (* "(" *) token env v1 in
  let v2 = soql_query_expression env v2 in
  let _v3 = (* ")" *) token env v3 in
  R.List (List. map (fun e -> R.Any (G.E e)) v2)

(* NEW *)
and superclass (env : env) ((v1, v2) : CST.superclass) : G.class_parent list =
  let _v1 = (* ""extends *) token env v1 in
  let v2 = type_ env v2 in
  [v2, None]

(* NEW *)
and switch_block (env : env) ((v1, v2, v3) : CST.switch_block) : case_and_body list =
  let _v1 = token env v1 (* "{" *) in
  let v2 = List_.map (switch_rule env) v2 in
  let _v3 = token env v3 (* "}" *) in
  v2

(* NEW *)
and switch_expression (env : env) ((v1, v2, v3, v4) : CST.switch_expression) : G.stmt =
  let v1 = token env v1 (* "switch" *) in
  let v2 = token env v2 (* "on" *) in
  let v3 = expression env v3 in
  let v4 = switch_block env v4 in
  G.Switch (Tok.combine_toks v1 [v2], Some (G.Cond v3), v4) |> G.s

(* NEW *)
and switch_label (env : env) ((v1, v2) : CST.switch_label) : G.case list =
  let t1 = token env v1 in
  match v2 with
  | `Opt_unan_type_id_rep_COMMA_opt_unan_type_id (v1, v2, v3) ->
      let t = Option.map (unannotated_type env) v1 in
      let i = identifier env v2 in
      let tis =
        List.map (fun (w1, w2, w3) ->
          let tok = (* "," *) token env w1 in
          let t = Option.map (unannotated_type env) w2 in
          let i = identifier env v2 in
          tok, t, i) v3
      in
      List.map
        (fun (tok, t,i) -> make_switch_label_obj env tok t i)
        ((t1, t, i) :: tis)
  | `Exp_rep_COMMA_exp x ->
      let c1, cs = anon_exp_rep_COMMA_exp_0bb260c env x in
      G.Case (t1, H2.expr_to_pattern c1) ::
        List.map (fun (t, e) -> G.Case (t, H2.expr_to_pattern e)) cs
  | `Pat_else x ->
      let else_token = token env x in
      [G.Default (Tok.combine_toks t1 [else_token])]

(* AUX *)
and make_switch_label_obj (env : env) (tok : G.tok) (t : G.type_kind option) (i : G.ident)
    : G.case =
  let pat = G.PatId (i, empty_id_info ()) in
  match t with
  | Some t -> G.Case (tok, G.PatTyped (pat, t |> G.t))
  | None -> G.Case (tok, pat)

(* NEW *)
and switch_rule (env : env) (x : CST.switch_rule) : G.case_and_body =
  match x with
  | `Semg_ellips tok (* "..." *) ->
      G.CaseEllipsis (token env tok)
  | `Switch_label_blk (v1, v2) ->
      let v1 = switch_label env v1 in
      let v2 = trigger_body env v2 in
      G.CasesAndBody (v1, v2)

(* NEW *)
and throw_statement (env : env) ((v1, v2, v3) : CST.throw_statement) : G.stmt =
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      let v3 = token env v3 (* ";" *) in
      G.Throw (v1, v2, v3) |> G.s

(* NEW *)
and trigger_body (env : env) (x : CST.trigger_body) : G.stmt =
  block env x

(* NEW *)
and try_statement (env : env) ((v1, v2, v3) : CST.try_statement) : G.stmt =
  let v1 = token env v1 (* "try" *) in
  let v2 = trigger_body env v2 in
  match v3 with
  | `Rep1_catch_clause xs ->
      let cs = List.map (partial_catch env) xs in
      G.Try (v1, v2, cs, None, None) |> G.s
  | `Rep_catch_clause_fina_clause (xs, fly) ->
      let cs = List.map (partial_catch env) xs in
      let fly = partial_finally env fly in
      G.Try (v1, v2, cs, None, Some fly) |> G.s

(* NEW *)
and type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Unan_type x ->
      unannotated_type env x |> G.t
  | `Anno_type (v1, v2) ->
      let v1 = List.map (annotation env) v1 in
      let v2 = unannotated_type env v2 in
      make_type v1 v2

(* AUX *)
and make_type (a : G.attribute list) (t : G.type_kind) : G.type_=
  {G.t = t; G.t_attrs = a}

(* AUX *)
and make_type_opt (a : G.attribute list option) (t : G.type_kind) : G.type_=
  match a with
  | None -> make_type [] t
  | Some a -> make_type a t

(* NEW *)
and type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) : G.type_arguments =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> List.map (fun x -> G.TA x) (type_list env x)
    | None -> [])
  in
  let v3 = (* ">" *) token env v3 in
  v1, v2, v3

(* NEW *)
and type_bound (env : env) ((v1, v2, v3) : CST.type_bound) : G.type_ list =
  let v1 = (* "extends" *) token env v1 in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "&" *) token env v1 in
      let v2 = type_ env v2 in
      v2
    ) v3
  in
  v2 :: v3

(* NEW *)
and type_list (env : env) ((v1, v2) : CST.type_list) : G.type_ list=
  let v1 = type_ env v1 in
  let v2 = List.map (fun (v1, v2) ->
    let _v1 = (* "," *) token env v1 in
    let v2 = type_ env v2 in
    v2
    ) v2
  in
  v1 :: v2

(* NEW *)
and type_parameter (env : env) (x : CST.type_parameter) : G.type_parameter =
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token env tok in
      G.TParamEllipsis t
  | `Rep_anno_id_opt_type_bound (v1, v2, v3) ->
      let v1 = List.map (annotation env) v1 in
      let v2 = identifier env v2 in
      let v3 =
        match Option.map (type_bound env) v3 with
        | Some xs -> xs
        | None -> []
      in
      G.TP
        { tp_id = v2;
          tp_attrs = v1;
          tp_bounds = v3;
          tp_default = None;
          tp_variance = None;
        }

(* NEW *)
and type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters)
    : G.type_parameter list bracket =
  let v1 = (* "<" *) token env v1 in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = (* "," *) token env v1 in
      let v2 = type_parameter env v2 in
      v2
    ) v3
  in
  let v4 = (* ">" *) token env v4 in
  v1, v2 :: v3, v4

(* NEW *)
and unannotated_type (env : env) (x : CST.unannotated_type) : G.type_kind =
  match x with
  | `Choice_void_type x -> simple_type env x
  | `Array_type (v1, v2) ->
      (* FIXME: parser never seems to produce this *)
      let v1 = unannotated_type env v1 in
      let v2 = dimensions env v2 in
      List.fold_left (fun t (t1, t2) -> G.TyArray ((t1, None, t2), G.t t)) v1 v2

(* NEW *)
and unary_expression (env : env) (x : CST.unary_expression) : G.expr =
  match x with
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Plus, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Minus, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Not, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op BitNot, v1) |> G.e, fb [ Arg v2 ]) |> G.e

(* NEW *)
and unqualified_object_creation_expression (env : env) ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) : G.expr =
  let v1 = (* "new" *) token env v1 in
  let v3 = simple_type env v3 in
  let t =
    match v2 with
    | Some x ->
        let targs = type_arguments env x in
        make_type [] (G.TyApply (make_type [] v3, targs))
    | None -> make_type [] v3
  in
  let v4 = argument_list env v4 in
  match v5 with
  | Some v5 ->
      let lb, v5, rb = class_body env v5 in
      let v5 = List.map (fun x -> G.F x) v5 in
      G.AnonClass
            {
              ckind = (G.Class, v1);
              cextends = [];
              cimplements = [];
              cmixins = [];
              cparams = fb [];
              cbody = (lb, v5, rb);
            }
      |> G.e
  | None ->
      New (v1, t, empty_id_info (), v4) |> G.e

(* NEW *)
and update_expression (env : env) (x : CST.update_expression) : G.expr =
  match x with
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "++" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (IncrDecr (Incr, Prefix), v1) |> G.e, fb [ Arg v2 ])
      |> G.e
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token env v1 (* "--" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (IncrDecr (Decr, Prefix), v1) |> G.e, fb [ Arg v2 ])
      |> G.e
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "++" *) in
      Call (IdSpecial (IncrDecr (Incr, Postfix), v2) |> G.e, fb [ Arg v1 ])
      |> G.e
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "--" *) in
      Call (IdSpecial (IncrDecr (Decr, Postfix), v2) |> G.e, fb [ Arg v1 ])
      |> G.e

(* NEW QUERY *)
and value_expression (env : env) (x : CST.value_expression) : G.expr =
  let module R = Raw_tree in
  match x with
  | `Func_exp x ->
      function_expression env x
  | `Field_id x ->
      field_identifier env x

(* NEW *)
and variable_declarator (env : env) ((v1, v2) : CST.variable_declarator)
    : entity * variable_definition =
  let v1 = variable_declarator_id env v1 in
  let vinit =
    Option.map
      (fun (w1, w2) ->
        let _r1 = (* "=" *) token env w1 in
        let r2 = variable_initializer env w2 in
        r2
      )
      v2
  in
  let ent = G.basic_entity v1 in
  let vardef = { vinit; vtype = None; vtok = G.no_sc } in
  ent, vardef

(* NEW *)
and variable_declarator_list (env : env) ((v1, v2) : CST.variable_declarator_list)
    : (entity * variable_definition) list =
  let v1 = variable_declarator env v1 in
  let
    v2 = List.map
      (fun (w1, w2) ->
         let _r1 = token env w1 (* "," *) in
         let r2 = variable_declarator env w2 in
         r2
      )
      v2
  in
  v1 :: v2

(* NEW *)
and variable_initializer (env : env) (x : CST.variable_initializer) : G.expr =
  match x with
  | `Exp x -> expression env x
  | `Array_init x -> array_initializer env x

(* NEW QUERY *)
and where_clause (env : env) ((v1, v2) : CST.where_clause) : G.expr list =
  let v1 = (* "where" *) str env v1 in
  let v2 = boolean_expression env v2 in
  [(G.L (G.String (fb v1)) |> G.e); v2]

(* NEW *)
and while_statement (env : env) ((v1, v2, v3) : CST.while_statement) : G.stmt =
  let v1 = token env v1 (* "while" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = statement env v3 in
  G.While (v1, G.Cond v2, v3) |> G.s

(* NEW *)
let parser_output (env : env) (x : CST.parser_output) : G.any =
  match x with
  | `Rep_stmt xs ->
      G.Ss (List.map (statement env) xs)
  | `Cons_decl x ->
      G.S (constructor_declaration env x)
  | `Exp x ->
      G.E (expression env x)
  | `Anno x ->
      G.At (annotation env x)
  | `Meth_decl x ->
      G.S (method_declaration env x)
  | `Local_var_decl x ->
      G.S (local_variable_declaration env x)
  | `Class_header x ->
      let ent, d = class_header env x in
      G.Partial (G.PartialDef (ent, G.ClassDef d))
  | `Full_meth_header (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> modifiers env x
        | None -> []
      in
      let ent, d = method_header env v2 in
      let ent = { ent with attrs = v1 } in
      G.Partial (G.PartialDef (ent, G.FuncDef d))
  | `Part_if (v1, v2) ->
      let t = token env v1 in
      let s = parenthesized_expression env v2 in
      G.Partial (G.PartialIf (t, s))
  | `Part_try (v1, v2) ->
      let t = token env v1 in
      let s = trigger_body env v2 in
      G.Partial (G.PartialTry (t, s))
  | `Part_catch x ->
      G.Partial (G.PartialCatch (partial_catch env x))
  | `Part_fina x ->
      let t, s = partial_finally env x in
      G.Partial (G.PartialFinally (t, s))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_apex.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match parser_output env cst with
      | G.S s -> [s]
      | G.Ss ss -> ss
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_apex.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      parser_output env cst)
