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
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token_ = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_pat_snip (env : env) (tok : CST.pat_snip) =
  (* pattern [sS][nN][iI][pP][pP][eE][tT] *) token env tok

let map_pat_data (env : env) (tok : CST.pat_data) =
  (* pattern [dD][aA][tT][aA] *) token env tok

let map_pat_first (env : env) (tok : CST.pat_first) =
  (* pattern [fF][iI][rR][sS][tT] *) token env tok

let map_pat_suppos (env : env) (tok : CST.pat_suppos) =
  (* pattern [sS][uU][pP][pP][oO][rR][tT][sS][dD][oO][mM][aA][iI][nN][sS] *) token env tok

let map_pat_view (env : env) (tok : CST.pat_view) =
  (* pattern [vV][iI][eE][wW] *) token env tok

let map_pat_find (env : env) (tok : CST.pat_find) =
  (* pattern [fF][iI][nN][dD] *) token env tok

let map_pat_suppos_ (env : env) (tok : CST.pat_suppos_) =
  (* pattern [sS][uU][pP][pP][oO][rR][tT][sS][dD][eE][lL][eE][gG][aA][tT][eE][sS] *) token env tok

let map_pat_final (env : env) (tok : CST.pat_final) =
  (* pattern [fF][iI][nN][aA][lL] *) token env tok

let map_pat_my_team_terr (env : env) (tok : CST.pat_my_team_terr) =
  (* pattern [mM][yY][__][tT][eE][aA][mM][__][tT][eE][rR][rR][iI][tT][oO][rR][yY] *) token env tok

let map_currency_literal (env : env) (tok : CST.currency_literal) =
  (* pattern \w{3}\d+(\.\d+)? *) token env tok

let map_pat_virt (env : env) (tok : CST.pat_virt) =
  (* pattern [vV][iI][rR][tT][uU][aA][lL] *) token env tok

let map_pat_shar (env : env) (tok : CST.pat_shar) =
  (* pattern [sS][hH][aA][rR][iI][nN][gG] *) token env tok

let map_pat_in (env : env) (tok : CST.pat_in) =
  (* pattern [iI][nN] *) token env tok

let map_pat_meta (env : env) (tok : CST.pat_meta) =
  (* pattern [mM][eE][tT][aA][dD][aA][tT][aA] *) token env tok

let map_pat_user_mode (env : env) (tok : CST.pat_user_mode) =
  (* pattern [uU][sS][eE][rR][__][mM][oO][dD][eE] *) token env tok

let map_pat_prot (env : env) (tok : CST.pat_prot) =
  (* pattern [pP][rR][oO][tT][eE][cC][tT][eE][dD] *) token env tok

let map_pat_next_fiscal_year (env : env) (tok : CST.pat_next_fiscal_year) =
  (* pattern [nN][eE][xX][tT][__][fF][iI][sS][cC][aA][lL][__][yY][eE][aA][rR] *) token env tok

let map_pat_tola (env : env) (tok : CST.pat_tola) =
  (* pattern [tT][oO][lL][aA][bB][eE][lL] *) token env tok

let map_block_comment_explicit (env : env) (() : CST.block_comment_explicit) =
  R.Tuple []

let map_pat_offset (env : env) (tok : CST.pat_offset) =
  (* pattern [oO][fF][fF][sS][eE][tT] *) token env tok

let map_pat_today (env : env) (tok : CST.pat_today) =
  (* pattern [tT][oO][dD][aA][yY] *) token env tok

let map_semgrep_metavar_ellipsis (env : env) (tok : CST.semgrep_metavar_ellipsis) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_pat_then (env : env) (tok : CST.pat_then) =
  (* pattern [tT][hH][eE][nN] *) token env tok

let map_pat_above (env : env) (tok : CST.pat_above) =
  (* pattern [aA][bB][oO][vV][eE] *) token env tok

let map_pat_insert (env : env) (tok : CST.pat_insert) =
  (* pattern [iI][nN][sS][eE][rR][tT] *) token env tok

let map_pat_merge (env : env) (tok : CST.pat_merge) =
  (* pattern [mM][eE][rR][gG][eE] *) token env tok

let map_pat_count_dist (env : env) (tok : CST.pat_count_dist) =
  (* pattern [cC][oO][uU][nN][tT][__][dD][iI][sS][tT][iI][nN][cC][tT] *) token env tok

let map_pat_nulls (env : env) (tok : CST.pat_nulls) =
  (* pattern [nN][uU][lL][lL][sS] *) token env tok

let map_date (env : env) (tok : CST.date) =
  (* pattern [1-4][0-9]{3}-(?:0[1-9]|1[0-2])-(?:[0-2][1-9]|[1-2]0|3[0-1]) *) token env tok

let map_decimal_floating_point_literal (env : env) (tok : CST.decimal_floating_point_literal) =
  (* decimal_floating_point_literal *) token env tok

let map_pat_last_month (env : env) (tok : CST.pat_last_month) =
  (* pattern [lL][aA][sS][tT][__][mM][oO][nN][tT][hH] *) token env tok

let map_property_navigation (env : env) ((v1, v2) : CST.property_navigation) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "." *) token env v2 in
  R.Tuple [v1; v2]

let map_pat_cale_month (env : env) (tok : CST.pat_cale_month) =
  (* pattern [cC][aA][lL][eE][nN][dD][aA][rR][__][mM][oO][nN][tT][hH] *) token env tok

let map_pat_unde (env : env) (tok : CST.pat_unde) =
  (* pattern [uU][nN][dD][eE][lL][eE][tT][eE] *) token env tok

let map_pat_inclus (env : env) (tok : CST.pat_inclus) =
  (* pattern [iI][nN][cC][lL][uU][dD][eE][sS] *) token env tok

let map_pat_side (env : env) (tok : CST.pat_side) =
  (* pattern [sS][iI][dD][eE][bB][aA][rR] *) token env tok

let map_pat_dele (env : env) (tok : CST.pat_dele) =
  (* pattern [dD][eE][lL][eE][gG][aA][tT][eE][dD] *) token env tok

let map_pat_fields (env : env) (tok : CST.pat_fields) =
  (* pattern [fF][iI][eE][lL][dD][sS] *) token env tok

let map_pat_fiscal_month (env : env) (tok : CST.pat_fiscal_month) =
  (* pattern [fF][iI][sS][cC][aA][lL][__][mM][oO][nN][tT][hH] *) token env tok

let map_pat_when (env : env) (tok : CST.pat_when) =
  (* pattern [wW][hH][eE][nN] *) token env tok

let map_pat_like (env : env) (tok : CST.pat_like) =
  (* pattern [lL][iI][kK][eE] *) token env tok

let map_pat_avg (env : env) (tok : CST.pat_avg) =
  (* pattern [aA][vV][gG] *) token env tok

let map_pat_scope (env : env) (tok : CST.pat_scope) =
  (* pattern [sS][cC][oO][pP][eE] *) token env tok

let map_pat_sum (env : env) (tok : CST.pat_sum) =
  (* pattern [sS][uU][mM] *) token env tok

let map_pat_target_len (env : env) (tok : CST.pat_target_len) =
  (* pattern [tT][aA][rR][gG][eE][tT][__][lL][eE][nN][gG][tT][hH] *) token env tok

let map_pat_try (env : env) (tok : CST.pat_try) =
  (* pattern [tT][rR][yY] *) token env tok

let map_pat_inst (env : env) (tok : CST.pat_inst) =
  (* pattern [iI][nN][sS][tT][aA][nN][cC][eE][oO][fF] *) token env tok

let map_pat_mine (env : env) (tok : CST.pat_mine) =
  (* pattern [mM][iI][nN][eE] *) token env tok

let map_date_time (env : env) (tok : CST.date_time) =
  (* pattern [1-4][0-9]{3}-(?:0[1-9]|1[0-2])-(?:[0-2][1-9]|[1-2]0|3[0-1])T([0-1]\d|2[0-3]):[0-5]\d:[0-5]\d(?:\.\d\d?\d?)?(?:Z|[+-][0-1]\d:[0-5]\d) *) token env tok

let map_pat_while (env : env) (tok : CST.pat_while) =
  (* pattern [wW][hH][iI][lL][eE] *) token env tok

let map_pat_ret (env : env) (tok : CST.pat_ret) =
  (* pattern [rR][eE][tT][uU][rR][nN] *) token env tok

let map_pat_with (env : env) (tok : CST.pat_with) =
  (* pattern [wW][iI][tT][hH] *) token env tok

let map_pat_where (env : env) (tok : CST.pat_where) =
  (* pattern [wW][hH][eE][rR][eE] *) token env tok

let map_pat_this_month (env : env) (tok : CST.pat_this_month) =
  (* pattern [tT][hH][iI][sS][__][mM][oO][nN][tT][hH] *) token env tok

let map_pat_high (env : env) (tok : CST.pat_high) =
  (* pattern [hH][iI][gG][hH][lL][iI][gG][hH][tT] *) token env tok

let map_pat_tran (env : env) (tok : CST.pat_tran) =
  (* pattern [tT][rR][aA][nN][sS][iI][eE][nN][tT] *) token env tok

let map_pat_as (env : env) (tok : CST.pat_as) =
  (* pattern [aA][sS] *) token env tok

let map_pat_next_90_days (env : env) (tok : CST.pat_next_90_days) =
  (* pattern [nN][eE][xX][tT][__][99][00][__][dD][aA][yY][sS] *) token env tok

let map_pat_super (env : env) (tok : CST.pat_super) =
  (* pattern [sS][uU][pP][eE][rR] *) token env tok

let map_pat_on (env : env) (tok : CST.pat_on) =
  (* pattern [oO][nN] *) token env tok

let map_pat_min (env : env) (tok : CST.pat_min) =
  (* pattern [mM][iI][nN] *) token env tok

let map_pat_mine_and_my_groups (env : env) (tok : CST.pat_mine_and_my_groups) =
  (* pattern [mM][iI][nN][eE][__][aA][nN][dD][__][mM][yY][__][gG][rR][oO][uU][pP][sS] *) token env tok

let map_pat_list (env : env) (tok : CST.pat_list) =
  (* pattern [lL][iI][sS][tT][vV][iI][eE][wW] *) token env tok

let map_string_literal (env : env) (tok : CST.string_literal) =
  (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env tok

let map_pat_not (env : env) (tok : CST.pat_not) =
  (* pattern [nN][oO][tT] *) token env tok

let map_pat_test (env : env) (tok : CST.pat_test) =
  (* pattern [tT][eE][sS][tT][mM][eE][tT][hH][oO][dD] *) token env tok

let map_pat_typeof (env : env) (tok : CST.pat_typeof) =
  (* pattern [tT][yY][pP][eE][oO][fF] *) token env tok

let map_pat_desc (env : env) (tok : CST.pat_desc) =
  (* pattern [dD][eE][sS][cC] *) token env tok

let map_pat_day_only (env : env) (tok : CST.pat_day_only) =
  (* pattern [dD][aA][yY][__][oO][nN][lL][yY] *) token env tok

let map_pat_spell_corr (env : env) (tok : CST.pat_spell_corr) =
  (* pattern [sS][pP][eE][lL][lL][__][cC][oO][rR][rR][eE][cC][tT][iI][oO][nN] *) token env tok

let map_pat_ref (env : env) (tok : CST.pat_ref) =
  (* pattern [rR][eE][fF][eE][rR][eE][nN][cC][eE] *) token env tok

let map_pat_public (env : env) (tok : CST.pat_public) =
  (* pattern [pP][uU][bB][lL][iI][cC] *) token env tok

let map_pat_last (env : env) (tok : CST.pat_last) =
  (* pattern [lL][aA][sS][tT] *) token env tok

let map_pat_upsert (env : env) (tok : CST.pat_upsert) =
  (* pattern [uU][pP][sS][eE][rR][tT] *) token env tok

let map_pat_void (env : env) (tok : CST.pat_void) =
  (* pattern [vV][oO][iI][dD] *) token env tok

let map_pat_netw (env : env) (tok : CST.pat_netw) =
  (* pattern [nN][eE][tT][wW][oO][rR][kK] *) token env tok

let map_pat_below (env : env) (tok : CST.pat_below) =
  (* pattern [bB][eE][lL][oO][wW] *) token env tok

let map_line_comment_explicit (env : env) (() : CST.line_comment_explicit) =
  R.Tuple []

let map_apex_identifier_ (env : env) (tok : CST.apex_identifier_) =
  (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok

let map_pat_imples (env : env) (tok : CST.pat_imples) =
  (* pattern [iI][mM][pP][lL][eE][mM][eE][nN][tT][sS] *) token env tok

let map_pat_stan (env : env) (tok : CST.pat_stan) =
  (* pattern [sS][tT][aA][nN][dD][aA][rR][dD] *) token env tok

let map_pat_or (env : env) (tok : CST.pat_or) =
  (* pattern [oO][rR] *) token env tok

let map_pat_cate (env : env) (tok : CST.pat_cate) =
  (* pattern [cC][aA][tT][eE][gG][oO][rR][yY] *) token env tok

let map_pat_before (env : env) (tok : CST.pat_before) =
  (* pattern [bB][eE][fF][oO][rR][eE] *) token env tok

let map_pat_email (env : env) (tok : CST.pat_email) =
  (* pattern [eE][mM][aA][iI][lL] *) token env tok

let map_pat_if (env : env) (tok : CST.pat_if) =
  (* pattern [iI][fF] *) token env tok

let map_pat_rows (env : env) (tok : CST.pat_rows) =
  (* pattern [rR][oO][wW][sS] *) token env tok

let map_pat_else (env : env) (tok : CST.pat_else) =
  (* pattern [eE][lL][sS][eE] *) token env tok

let map_pat_week_in_year (env : env) (tok : CST.pat_week_in_year) =
  (* pattern [wW][eE][eE][kK][__][iI][nN][__][yY][eE][aA][rR] *) token env tok

let map_pat_limit (env : env) (tok : CST.pat_limit) =
  (* pattern [lL][iI][mM][iI][tT] *) token env tok

let map_pat_next_month (env : env) (tok : CST.pat_next_month) =
  (* pattern [nN][eE][xX][tT][__][mM][oO][nN][tT][hH] *) token env tok

let map_pat_trac (env : env) (tok : CST.pat_trac) =
  (* pattern [tT][rR][aA][cC][kK][iI][nN][gG] *) token env tok

let map_block_comment (env : env) (tok : CST.block_comment) =
  (* block_comment *) token env tok

let map_pat_dist (env : env) (tok : CST.pat_dist) =
  (* pattern [dD][iI][sS][tT][aA][nN][cC][eE] *) token env tok

let map_pat_rollup (env : env) (tok : CST.pat_rollup) =
  (* pattern [rR][oO][lL][lL][uU][pP] *) token env tok

let map_pat_inte (env : env) (tok : CST.pat_inte) =
  (* pattern [iI][nN][tT][eE][rR][fF][aA][cC][eE] *) token env tok

let map_pat_set (env : env) (tok : CST.pat_set) =
  (* pattern [sS][eE][tT] *) token env tok

let map_decimal (env : env) (tok : CST.decimal) =
  (* pattern -?\d+(\.\d+)? *) token env tok

let map_pat_last_fiscal_quar (env : env) (tok : CST.pat_last_fiscal_quar) =
  (* pattern [lL][aA][sS][tT][__][fF][iI][sS][cC][aA][lL][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_pat_for (env : env) (tok : CST.pat_for) =
  (* pattern [fF][oO][rR] *) token env tok

let map_pat_pric (env : env) (tok : CST.pat_pric) =
  (* pattern [pP][rR][iI][cC][eE][bB][oO][oO][kK][iI][dD] *) token env tok

let map_pat_last_fiscal_year (env : env) (tok : CST.pat_last_fiscal_year) =
  (* pattern [lL][aA][sS][tT][__][fF][iI][sS][cC][aA][lL][__][yY][eE][aA][rR] *) token env tok

let map_pat_this_quar (env : env) (tok : CST.pat_this_quar) =
  (* pattern [tT][hH][iI][sS][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_pat_at (env : env) (tok : CST.pat_at) =
  (* pattern [aA][tT] *) token env tok

let map_pat_static (env : env) (tok : CST.pat_static) =
  (* pattern [sS][tT][aA][tT][iI][cC] *) token env tok

let map_pat_fiscal_quar (env : env) (tok : CST.pat_fiscal_quar) =
  (* pattern [fF][iI][sS][cC][aA][lL][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_pat_tomo (env : env) (tok : CST.pat_tomo) =
  (* pattern [tT][oO][mM][oO][rR][rR][oO][wW] *) token env tok

let map_pat_geol (env : env) (tok : CST.pat_geol) =
  (* pattern [gG][eE][oO][lL][oO][cC][aA][tT][iI][oO][nN] *) token env tok

let map_pat_day_in_week (env : env) (tok : CST.pat_day_in_week) =
  (* pattern [dD][aA][yY][__][iI][nN][__][wW][eE][eE][kK] *) token env tok

let map_pat_last_week (env : env) (tok : CST.pat_last_week) =
  (* pattern [lL][aA][sS][tT][__][wW][eE][eE][kK] *) token env tok

let map_pat_divi (env : env) (tok : CST.pat_divi) =
  (* pattern [dD][iI][vV][iI][sS][iI][oO][nN] *) token env tok

let map_pat_abst (env : env) (tok : CST.pat_abst) =
  (* pattern [aA][bB][sS][tT][rR][aA][cC][tT] *) token env tok

let map_pat_custom (env : env) (tok : CST.pat_custom) =
  (* pattern [cC][uU][sS][tT][oO][mM] *) token env tok

let map_pat_userid (env : env) (tok : CST.pat_userid) =
  (* pattern [uU][sS][eE][rR][iI][dD] *) token env tok

let map_pat_over (env : env) (tok : CST.pat_over) =
  (* pattern [oO][vV][eE][rR][rR][iI][dD][eE] *) token env tok

let map_pat_fina (env : env) (tok : CST.pat_fina) =
  (* pattern [fF][iI][nN][aA][lL][lL][yY] *) token env tok

let map_pat_trig (env : env) (tok : CST.pat_trig) =
  (* pattern [tT][rR][iI][gG][gG][eE][rR] *) token env tok

let map_pat_cale_quar (env : env) (tok : CST.pat_cale_quar) =
  (* pattern [cC][aA][lL][eE][nN][dD][aA][rR][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_pat_this_week (env : env) (tok : CST.pat_this_week) =
  (* pattern [tT][hH][iI][sS][__][wW][eE][eE][kK] *) token env tok

let map_pat_throw (env : env) (tok : CST.pat_throw) =
  (* pattern [tT][hH][rR][oO][wW] *) token env tok

let map_pat_ever (env : env) (tok : CST.pat_ever) =
  (* pattern [eE][vV][eE][rR][yY][tT][hH][iI][nN][gG] *) token env tok

let map_pat_cube (env : env) (tok : CST.pat_cube) =
  (* pattern [cC][uU][bB][eE] *) token env tok

let map_pat_hour_in_day (env : env) (tok : CST.pat_hour_in_day) =
  (* pattern [hH][oO][uU][rR][__][iI][nN][__][dD][aA][yY] *) token env tok

let map_pat_day_in_month (env : env) (tok : CST.pat_day_in_month) =
  (* pattern [dD][aA][yY][__][iI][nN][__][mM][oO][nN][tT][hH] *) token env tok

let map_pat_last_90_days (env : env) (tok : CST.pat_last_90_days) =
  (* pattern [lL][aA][sS][tT][__][99][00][__][dD][aA][yY][sS] *) token env tok

(* OLD *)
let map_dimensions (env : env) (xs : CST.dimensions) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = (* "[" *) token env v1 in
    let v2 = (* "]" *) token env v2 in
    R.Tuple [v1; v2]
  ) xs)

(* NEW *)
let dimensions (env : env) (xs : CST.dimensions) : (G.tok * G.tok) list =
  List.map (fun (v1, v2) ->
    let v1 = (* "[" *) token_ env v1 in
    let v2 = (* "]" *) token_ env v2 in
    v1, v2)
    xs

let map_pat_cale_year (env : env) (tok : CST.pat_cale_year) =
  (* pattern [cC][aA][lL][eE][nN][dD][aA][rR][__][yY][eE][aA][rR] *) token env tok

let map_pat_last_year (env : env) (tok : CST.pat_last_year) =
  (* pattern [lL][aA][sS][tT][__][yY][eE][aA][rR] *) token env tok

let map_pat_end (env : env) (tok : CST.pat_end) =
  (* pattern [eE][nN][dD] *) token env tok

let map_pat_format (env : env) (tok : CST.pat_format) =
  (* pattern [fF][oO][rR][mM][aA][tT] *) token env tok

let map_pat_switch (env : env) (tok : CST.pat_switch) =
  (* pattern [sS][wW][iI][tT][cC][hH] *) token env tok

let map_pat_priv (env : env) (tok : CST.pat_priv) =
  (* pattern [pP][rR][iI][vV][aA][tT][eE] *) token env tok

let map_pat_count (env : env) (tok : CST.pat_count) =
  (* pattern [cC][oO][uU][nN][tT] *) token env tok

let map_pat_this_fiscal_quar (env : env) (tok : CST.pat_this_fiscal_quar) =
  (* pattern [tT][hH][iI][sS][__][fF][iI][sS][cC][aA][lL][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_term (env : env) (tok : CST.term) =
  (* pattern "(\\\\\\'|[^'])+" *) token env tok

let map_pat_after (env : env) (tok : CST.pat_after) =
  (* pattern [aA][fF][tT][eE][rR] *) token env tok

let map_pat_next_year (env : env) (tok : CST.pat_next_year) =
  (* pattern [nN][eE][xX][tT][__][yY][eE][aA][rR] *) token env tok

let map_pat_extends (env : env) (tok : CST.pat_extends) =
  (* pattern [eE][xX][tT][eE][nN][dD][sS] *) token env tok

let map_pat_update (env : env) (tok : CST.pat_update) =
  (* pattern [uU][pP][dD][aA][tT][eE] *) token env tok

let map_pat_this_year (env : env) (tok : CST.pat_this_year) =
  (* pattern [tT][hH][iI][sS][__][yY][eE][aA][rR] *) token env tok

let map_pat_view_ (env : env) (tok : CST.pat_view_) =
  (* pattern [vV][iI][eE][wW][sS][tT][aA][tT] *) token env tok

let map_pat_fiscal_year (env : env) (tok : CST.pat_fiscal_year) =
  (* pattern [fF][iI][sS][cC][aA][lL][__][yY][eE][aA][rR] *) token env tok

let map_pat_my_terr (env : env) (tok : CST.pat_my_terr) =
  (* pattern [mM][yY][__][tT][eE][rR][rR][iI][tT][oO][rR][yY] *) token env tok

let map_tok_choice_pat_last_n_days (env : env) (tok : CST.tok_choice_pat_last_n_days) =
  (* tok_choice_pat_last_n_days *) token env tok

let map_pat_do (env : env) (tok : CST.pat_do) =
  (* pattern [dD][oO] *) token env tok

let map_pat_true (env : env) (tok : CST.pat_true) =
  (* pattern [tT][rR][uU][eE] *) token env tok

let map_pat_by (env : env) (tok : CST.pat_by) =
  (* pattern [bB][yY] *) token env tok

let map_pat_above_or_below (env : env) (tok : CST.pat_above_or_below) =
  (* pattern [aA][bB][oO][vV][eE][__][oO][rR][__][bB][eE][lL][oO][wW] *) token env tok

let map_pat_last_quar (env : env) (tok : CST.pat_last_quar) =
  (* pattern [lL][aA][sS][tT][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_pat_from (env : env) (tok : CST.pat_from) =
  (* pattern [fF][rR][oO][mM] *) token env tok

let map_pat_team (env : env) (tok : CST.pat_team) =
  (* pattern [tT][eE][aA][mM] *) token env tok

let map_pat_and (env : env) (tok : CST.pat_and) =
  (* pattern [aA][nN][dD] *) token env tok

let map_pat_phone (env : env) (tok : CST.pat_phone) =
  (* pattern [pP][hH][oO][nN][eE] *) token env tok

let map_pat_this (env : env) (tok : CST.pat_this) =
  (* pattern [tT][hH][iI][sS] *) token env tok

let map_pat_week_in_month (env : env) (tok : CST.pat_week_in_month) =
  (* pattern [wW][eE][eE][kK][__][iI][nN][__][mM][oO][nN][tT][hH] *) token env tok

let map_pat_next_week (env : env) (tok : CST.pat_next_week) =
  (* pattern [nN][eE][xX][tT][__][wW][eE][eE][kK] *) token env tok

let map_pat_having (env : env) (tok : CST.pat_having) =
  (* pattern [hH][aA][vV][iI][nN][gG] *) token env tok

let map_pat_next_quar (env : env) (tok : CST.pat_next_quar) =
  (* pattern [nN][eE][xX][tT][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_pat_next_fiscal_quar (env : env) (tok : CST.pat_next_fiscal_quar) =
  (* pattern [nN][eE][xX][tT][__][fF][iI][sS][cC][aA][lL][__][qQ][uU][aA][rR][tT][eE][rR] *) token env tok

let map_pat_asc (env : env) (tok : CST.pat_asc) =
  (* pattern [aA][sS][cC] *) token env tok

let map_pat_null (env : env) (tok : CST.pat_null) =
  (* pattern [nN][uU][lL][lL] *) token env tok

let map_pat_global (env : env) (tok : CST.pat_global) =
  (* pattern [gG][lL][oO][bB][aA][lL] *) token env tok

let map_pat_max (env : env) (tok : CST.pat_max) =
  (* pattern [mM][aA][xX] *) token env tok

let map_pat_all (env : env) (tok : CST.pat_all) =
  (* pattern [aA][lL][lL] *) token env tok

let map_pat_reco (env : env) (tok : CST.pat_reco) =
  (* pattern [rR][eE][cC][oO][rR][dD][vV][iI][sS][iI][bB][iI][lL][iI][tT][yY][cC][oO][nN][tT][eE][xX][tT] *) token env tok

let map_pat_cont (env : env) (tok : CST.pat_cont) =
  (* pattern [cC][oO][nN][tT][iI][nN][uU][eE] *) token env tok

let map_pat_get (env : env) (tok : CST.pat_get) =
  (* pattern [gG][eE][tT] *) token env tok

let map_pat_select (env : env) (tok : CST.pat_select) =
  (* pattern [sS][eE][lL][eE][cC][tT] *) token env tok

let map_pat_conv (env : env) (tok : CST.pat_conv) =
  (* pattern [cC][oO][nN][vV][eE][rR][tT][cC][uU][rR][rR][eE][nN][cC][yY] *) token env tok

let map_pat_inhe (env : env) (tok : CST.pat_inhe) =
  (* pattern [iI][nN][hH][eE][rR][iI][tT][eE][dD] *) token env tok

let map_pat_maxd (env : env) (tok : CST.pat_maxd) =
  (* pattern [mM][aA][xX][dD][eE][sS][cC][rR][iI][pP][tT][oO][rR][pP][eE][rR][rR][eE][cC][oO][rR][dD] *) token env tok

let map_pat_name (env : env) (tok : CST.pat_name) =
  (* pattern [nN][aA][mM][eE] *) token env tok

let map_pat_catch (env : env) (tok : CST.pat_catch) =
  (* pattern [cC][aA][tT][cC][hH] *) token env tok

let map_semgrep_metavar (env : env) (tok : CST.semgrep_metavar) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_pat_retu (env : env) (tok : CST.pat_retu) =
  (* pattern [rR][eE][tT][uU][rR][nN][iI][nN][gG] *) token env tok

let map_pat_system_mode (env : env) (tok : CST.pat_system_mode) =
  (* pattern [sS][yY][sS][tT][eE][mM][__][mM][oO][dD][eE] *) token env tok

let map_pat_using (env : env) (tok : CST.pat_using) =
  (* pattern [uU][sS][iI][nN][gG] *) token env tok

let map_pat_class (env : env) (tok : CST.pat_class) =
  (* pattern [cC][lL][aA][sS][sS] *) token env tok

let map_pat_e8c36c5 (env : env) (tok : CST.pat_e8c36c5) =
  (* pattern [sS][yY][sS][tT][eE][mM][..][rR][uU][nN][aA][sS] *) token env tok

let map_pat_exclus (env : env) (tok : CST.pat_exclus) =
  (* pattern [eE][xX][cC][lL][uU][dD][eE][sS] *) token env tok

let map_pat_order (env : env) (tok : CST.pat_order) =
  (* pattern [oO][rR][dD][eE][rR] *) token env tok

let map_pat_yest (env : env) (tok : CST.pat_yest) =
  (* pattern [yY][eE][sS][tT][eE][rR][dD][aA][yY] *) token env tok

let map_pat_false (env : env) (tok : CST.pat_false) =
  (* pattern [fF][aA][lL][sS][eE] *) token env tok

let map_pat_grou (env : env) (tok : CST.pat_grou) =
  (* pattern [gG][rR][oO][uU][pP][iI][nN][gG] *) token env tok

let map_pat_enum (env : env) (tok : CST.pat_enum) =
  (* pattern [eE][nN][uU][mM] *) token env tok

let map_pat_new (env : env) (tok : CST.pat_new) =
  (* pattern [nN][eE][wW] *) token env tok

let map_pat_this_fiscal_year (env : env) (tok : CST.pat_this_fiscal_year) =
  (* pattern [tT][hH][iI][sS][__][fF][iI][sS][cC][aA][lL][__][yY][eE][aA][rR] *) token env tok

let map_pat_group (env : env) (tok : CST.pat_group) =
  (* pattern [gG][rR][oO][uU][pP] *) token env tok

let map_int_ (env : env) (tok : CST.int_) =
  (* int *) token env tok

let map_pat_with_ (env : env) (tok : CST.pat_with_) =
  (* pattern [wW][iI][tT][hH][oO][uU][tT] *) token env tok

let map_pat_secu_enfo (env : env) (tok : CST.pat_secu_enfo) =
  (* pattern [sS][eE][cC][uU][rR][iI][tT][yY][__][eE][nN][fF][oO][rR][cC][eE][dD] *) token env tok

let map_pat_brk (env : env) (tok : CST.pat_brk) =
  (* pattern [bB][rR][eE][aA][kK] *) token env tok

let map_pat_day_in_year (env : env) (tok : CST.pat_day_in_year) =
  (* pattern [dD][aA][yY][__][iI][nN][__][yY][eE][aA][rR] *) token env tok

let map_line_comment (env : env) (tok : CST.line_comment) =
  (* line_comment *) token env tok

let map_pat_delete (env : env) (tok : CST.pat_delete) =
  (* pattern [dD][eE][lL][eE][tT][eE] *) token env tok

let map_value_comparison_operator (env : env) (x : CST.value_comparison_operator) =
  (match x with
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `LTGT tok -> R.Case ("LTGT",
      (* "<>" *) token env tok
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
  | `Pat_like x -> R.Case ("Pat_like",
      map_pat_like env x
    )
  )

let map_with_highlight (env : env) (x : CST.with_highlight) =
  map_pat_high env x

let map_super (env : env) (x : CST.super) =
  map_pat_super env x

let map_order_null_direciton (env : env) (x : CST.order_null_direciton) =
  (match x with
  | `Pat_nulls_pat_first (v1, v2) -> R.Case ("Pat_nulls_pat_first",
      let v1 = map_pat_nulls env v1 in
      let v2 = map_pat_first env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_nulls_pat_last (v1, v2) -> R.Case ("Pat_nulls_pat_last",
      let v1 = map_pat_nulls env v1 in
      let v2 = map_pat_last env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_void_type (env : env) (x : CST.void_type) =
  map_pat_void env x

let map_count_expression (env : env) ((v1, v2, v3) : CST.count_expression) =
  let v1 = map_pat_count env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_for_type (env : env) (x : CST.for_type) =
  (match x with
  | `Pat_update x -> R.Case ("Pat_update",
      map_pat_update env x
    )
  | `Pat_ref x -> R.Case ("Pat_ref",
      map_pat_ref env x
    )
  | `Pat_view x -> R.Case ("Pat_view",
      map_pat_view env x
    )
  )

let map_update_type (env : env) (x : CST.update_type) =
  (match x with
  | `Pat_trac x -> R.Case ("Pat_trac",
      map_pat_trac env x
    )
  | `Pat_view_ x -> R.Case ("Pat_view_",
      map_pat_view_ env x
    )
  )

let map_with_data_cat_filter_type (env : env) (x : CST.with_data_cat_filter_type) =
  (match x with
  | `Pat_at x -> R.Case ("Pat_at",
      map_pat_at env x
    )
  | `Pat_above x -> R.Case ("Pat_above",
      map_pat_above env x
    )
  | `Pat_below x -> R.Case ("Pat_below",
      map_pat_below env x
    )
  | `Pat_above_or_below x -> R.Case ("Pat_above_or_below",
      map_pat_above_or_below env x
    )
  )

let map_using_scope_type (env : env) (x : CST.using_scope_type) =
  (match x with
  | `Pat_dele x -> R.Case ("Pat_dele",
      map_pat_dele env x
    )
  | `Pat_ever x -> R.Case ("Pat_ever",
      map_pat_ever env x
    )
  | `Pat_mine x -> R.Case ("Pat_mine",
      map_pat_mine env x
    )
  | `Pat_mine_and_my_groups x -> R.Case ("Pat_mine_and_my_groups",
      map_pat_mine_and_my_groups env x
    )
  | `Pat_my_terr x -> R.Case ("Pat_my_terr",
      map_pat_my_terr env x
    )
  | `Pat_my_team_terr x -> R.Case ("Pat_my_team_terr",
      map_pat_my_team_terr env x
    )
  | `Pat_team x -> R.Case ("Pat_team",
      map_pat_team env x
    )
  )

(* OLD *)
let map_this (env : env) (x : CST.this) =
  map_pat_this env x

(* NEW *)
let this (env : env) (x : CST.this) : G.expr =
  let t = token_ env x (* "this" *) in
  IdSpecial (This, t) |> G.e

let map_order_direction (env : env) (x : CST.order_direction) =
  (match x with
  | `Pat_asc x -> R.Case ("Pat_asc",
      map_pat_asc env x
    )
  | `Pat_desc x -> R.Case ("Pat_desc",
      map_pat_desc env x
    )
  )

(* OLD *)
let map_null_literal (env : env) (x : CST.null_literal) =
  map_pat_null env x

(* NEW *)
let null_literal (env : env) (x : CST.null_literal) : literal =
  G.Null (token_ env x)

let map_all_rows_clause (env : env) ((v1, v2) : CST.all_rows_clause) =
  let v1 = map_pat_all env v1 in
  let v2 = map_pat_rows env v2 in
  R.Tuple [v1; v2]

let map_fields_type (env : env) (x : CST.fields_type) =
  (match x with
  | `Pat_all x -> R.Case ("Pat_all",
      map_pat_all env x
    )
  | `Pat_custom x -> R.Case ("Pat_custom",
      map_pat_custom env x
    )
  | `Pat_stan x -> R.Case ("Pat_stan",
      map_pat_stan env x
    )
  )

let map_in_type (env : env) (x : CST.in_type) =
  (match x with
  | `Pat_all x -> R.Case ("Pat_all",
      map_pat_all env x
    )
  | `Pat_email x -> R.Case ("Pat_email",
      map_pat_email env x
    )
  | `Pat_name x -> R.Case ("Pat_name",
      map_pat_name env x
    )
  | `Pat_phone x -> R.Case ("Pat_phone",
      map_pat_phone env x
    )
  | `Pat_side x -> R.Case ("Pat_side",
      map_pat_side env x
    )
  )

(* OLD *)
let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Apex_id_ tok -> R.Case ("Apex_id_",
      (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
    )
  )

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

let map_set_comparison_operator (env : env) (x : CST.set_comparison_operator) =
  (match x with
  | `Pat_in x -> R.Case ("Pat_in",
      map_pat_in env x
    )
  | `Pat_not_pat_in (v1, v2) -> R.Case ("Pat_not_pat_in",
      let v1 = map_pat_not env v1 in
      let v2 = map_pat_in env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_inclus x -> R.Case ("Pat_inclus",
      map_pat_inclus env x
    )
  | `Pat_exclus x -> R.Case ("Pat_exclus",
      map_pat_exclus env x
    )
  )

(* OLD *)
let map_boolean (env : env) (x : CST.boolean) =
  (match x with
  | `Pat_true x -> R.Case ("Pat_true",
      map_pat_true env x
    )
  | `Pat_false x -> R.Case ("Pat_false",
      map_pat_false env x
    )
  )

(* NEW *)
let boolean (env : env) (x : CST.boolean) : literal =
  match x with
  | `Pat_true tok -> G.Bool (true, token_ env tok)
  | `Pat_false tok -> G.Bool (false, token_ env tok)

let map_date_literal (env : env) (x : CST.date_literal) =
  (match x with
  | `Pat_yest x -> R.Case ("Pat_yest",
      map_pat_yest env x
    )
  | `Pat_today x -> R.Case ("Pat_today",
      map_pat_today env x
    )
  | `Pat_tomo x -> R.Case ("Pat_tomo",
      map_pat_tomo env x
    )
  | `Pat_last_week x -> R.Case ("Pat_last_week",
      map_pat_last_week env x
    )
  | `Pat_this_week x -> R.Case ("Pat_this_week",
      map_pat_this_week env x
    )
  | `Pat_next_week x -> R.Case ("Pat_next_week",
      map_pat_next_week env x
    )
  | `Pat_last_month x -> R.Case ("Pat_last_month",
      map_pat_last_month env x
    )
  | `Pat_this_month x -> R.Case ("Pat_this_month",
      map_pat_this_month env x
    )
  | `Pat_next_month x -> R.Case ("Pat_next_month",
      map_pat_next_month env x
    )
  | `Pat_last_90_days x -> R.Case ("Pat_last_90_days",
      map_pat_last_90_days env x
    )
  | `Pat_next_90_days x -> R.Case ("Pat_next_90_days",
      map_pat_next_90_days env x
    )
  | `Pat_this_quar x -> R.Case ("Pat_this_quar",
      map_pat_this_quar env x
    )
  | `Pat_last_quar x -> R.Case ("Pat_last_quar",
      map_pat_last_quar env x
    )
  | `Pat_next_quar x -> R.Case ("Pat_next_quar",
      map_pat_next_quar env x
    )
  | `Pat_this_year x -> R.Case ("Pat_this_year",
      map_pat_this_year env x
    )
  | `Pat_last_year x -> R.Case ("Pat_last_year",
      map_pat_last_year env x
    )
  | `Pat_next_year x -> R.Case ("Pat_next_year",
      map_pat_next_year env x
    )
  | `Pat_this_fiscal_quar x -> R.Case ("Pat_this_fiscal_quar",
      map_pat_this_fiscal_quar env x
    )
  | `Pat_last_fiscal_quar x -> R.Case ("Pat_last_fiscal_quar",
      map_pat_last_fiscal_quar env x
    )
  | `Pat_next_fiscal_quar x -> R.Case ("Pat_next_fiscal_quar",
      map_pat_next_fiscal_quar env x
    )
  | `Pat_this_fiscal_year x -> R.Case ("Pat_this_fiscal_year",
      map_pat_this_fiscal_year env x
    )
  | `Pat_last_fiscal_year x -> R.Case ("Pat_last_fiscal_year",
      map_pat_last_fiscal_year env x
    )
  | `Pat_next_fiscal_year x -> R.Case ("Pat_next_fiscal_year",
      map_pat_next_fiscal_year env x
    )
  )

(* OLD *)
let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Pat_global x -> R.Case ("Pat_global",
      map_pat_global env x
    )
  | `Pat_public x -> R.Case ("Pat_public",
      map_pat_public env x
    )
  | `Pat_test x -> R.Case ("Pat_test",
      map_pat_test env x
    )
  | `Pat_prot x -> R.Case ("Pat_prot",
      map_pat_prot env x
    )
  | `Pat_over x -> R.Case ("Pat_over",
      map_pat_over env x
    )
  | `Pat_priv x -> R.Case ("Pat_priv",
      map_pat_priv env x
    )
  | `Pat_virt x -> R.Case ("Pat_virt",
      map_pat_virt env x
    )
  | `Pat_abst x -> R.Case ("Pat_abst",
      map_pat_abst env x
    )
  | `Pat_static x -> R.Case ("Pat_static",
      map_pat_static env x
    )
  | `Pat_final x -> R.Case ("Pat_final",
      map_pat_final env x
    )
  | `Pat_tran x -> R.Case ("Pat_tran",
      map_pat_tran env x
    )
  | `Pat_with_pat_shar (v1, v2) -> R.Case ("Pat_with_pat_shar",
      let v1 = map_pat_with env v1 in
      let v2 = map_pat_shar env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_with__pat_shar (v1, v2) -> R.Case ("Pat_with__pat_shar",
      let v1 = map_pat_with_ env v1 in
      let v2 = map_pat_shar env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_inhe_pat_shar (v1, v2) -> R.Case ("Pat_inhe_pat_shar",
      let v1 = map_pat_inhe env v1 in
      let v2 = map_pat_shar env v2 in
      R.Tuple [v1; v2]
    )
  )

(* NEW *)
let modifier (env : env) (x : CST.modifier) : G.attribute =
  match x with
  | `Pat_global x ->
      G.OtherAttribute (("Global", token_ env x), [])
  | `Pat_public x ->
      G.KeywordAttr (G.Public, token_ env x)
  | `Pat_test x ->
      G.OtherAttribute (("testMethod", token_ env x), [])
  | `Pat_prot x ->
      G.KeywordAttr (G.Protected, token_ env x)
  | `Pat_over x ->
      G.KeywordAttr (G.Override, token_ env x)
  | `Pat_priv x ->
      G.KeywordAttr (G.Private, token_ env x)
  | `Pat_virt x ->
      G.OtherAttribute (("virtual", token_ env x), [])
  | `Pat_abst x ->
      G.KeywordAttr (G.Abstract, token_ env x)
  | `Pat_static x ->
      G.KeywordAttr (G.Static, token_ env x)
  | `Pat_final x ->
      G.KeywordAttr (G.Final, token_ env x)
  | `Pat_tran x ->
      G.OtherAttribute (("transient", token_ env x), [])
  | `Pat_with_pat_shar (v1, v2) ->
      let v1 = token_ env v1 in
      let v2 = token_ env v2 in
      G.OtherAttribute (("withSharing", Tok.combine_toks v1 [v2]), [])
  | `Pat_with__pat_shar (v1, v2) ->
      let v1 = token_ env v1 in
      let v2 = token_ env v2 in
      G.OtherAttribute (("withoutSharing", Tok.combine_toks v1 [v2]), [])
  | `Pat_inhe_pat_shar (v1, v2) ->
      let v1 = token_ env v1 in
      let v2 = token_ env v2 in
      G.OtherAttribute (("inheritSharing", Tok.combine_toks v1 [v2]), [])

let map_function_name (env : env) (x : CST.function_name) =
  (match x with
  | `Pat_avg x -> R.Case ("Pat_avg",
      map_pat_avg env x
    )
  | `Pat_count x -> R.Case ("Pat_count",
      map_pat_count env x
    )
  | `Pat_count_dist x -> R.Case ("Pat_count_dist",
      map_pat_count_dist env x
    )
  | `Pat_min x -> R.Case ("Pat_min",
      map_pat_min env x
    )
  | `Pat_max x -> R.Case ("Pat_max",
      map_pat_max env x
    )
  | `Pat_sum x -> R.Case ("Pat_sum",
      map_pat_sum env x
    )
  | `Pat_grou x -> R.Case ("Pat_grou",
      map_pat_grou env x
    )
  | `Pat_format x -> R.Case ("Pat_format",
      map_pat_format env x
    )
  | `Pat_conv x -> R.Case ("Pat_conv",
      map_pat_conv env x
    )
  | `Pat_tola x -> R.Case ("Pat_tola",
      map_pat_tola env x
    )
  | `Pat_cale_month x -> R.Case ("Pat_cale_month",
      map_pat_cale_month env x
    )
  | `Pat_cale_quar x -> R.Case ("Pat_cale_quar",
      map_pat_cale_quar env x
    )
  | `Pat_cale_year x -> R.Case ("Pat_cale_year",
      map_pat_cale_year env x
    )
  | `Pat_day_in_month x -> R.Case ("Pat_day_in_month",
      map_pat_day_in_month env x
    )
  | `Pat_day_in_week x -> R.Case ("Pat_day_in_week",
      map_pat_day_in_week env x
    )
  | `Pat_day_in_year x -> R.Case ("Pat_day_in_year",
      map_pat_day_in_year env x
    )
  | `Pat_day_only x -> R.Case ("Pat_day_only",
      map_pat_day_only env x
    )
  | `Pat_fiscal_month x -> R.Case ("Pat_fiscal_month",
      map_pat_fiscal_month env x
    )
  | `Pat_fiscal_quar x -> R.Case ("Pat_fiscal_quar",
      map_pat_fiscal_quar env x
    )
  | `Pat_fiscal_year x -> R.Case ("Pat_fiscal_year",
      map_pat_fiscal_year env x
    )
  | `Pat_hour_in_day x -> R.Case ("Pat_hour_in_day",
      map_pat_hour_in_day env x
    )
  | `Pat_week_in_month x -> R.Case ("Pat_week_in_month",
      map_pat_week_in_month env x
    )
  | `Pat_week_in_year x -> R.Case ("Pat_week_in_year",
      map_pat_week_in_year env x
    )
  )

let map_trigger_event (env : env) (x : CST.trigger_event) =
  (match x with
  | `Pat_before_pat_insert (v1, v2) -> R.Case ("Pat_before_pat_insert",
      let v1 = map_pat_before env v1 in
      let v2 = map_pat_insert env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_before_pat_update (v1, v2) -> R.Case ("Pat_before_pat_update",
      let v1 = map_pat_before env v1 in
      let v2 = map_pat_update env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_before_pat_delete (v1, v2) -> R.Case ("Pat_before_pat_delete",
      let v1 = map_pat_before env v1 in
      let v2 = map_pat_delete env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_after_pat_insert (v1, v2) -> R.Case ("Pat_after_pat_insert",
      let v1 = map_pat_after env v1 in
      let v2 = map_pat_insert env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_after_pat_update (v1, v2) -> R.Case ("Pat_after_pat_update",
      let v1 = map_pat_after env v1 in
      let v2 = map_pat_update env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_after_pat_delete (v1, v2) -> R.Case ("Pat_after_pat_delete",
      let v1 = map_pat_after env v1 in
      let v2 = map_pat_delete env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_after_pat_unde (v1, v2) -> R.Case ("Pat_after_pat_unde",
      let v1 = map_pat_after env v1 in
      let v2 = map_pat_unde env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_dml_type (env : env) (x : CST.dml_type) =
  (match x with
  | `Pat_insert x -> R.Case ("Pat_insert",
      map_pat_insert env x
    )
  | `Pat_update x -> R.Case ("Pat_update",
      map_pat_update env x
    )
  | `Pat_delete x -> R.Case ("Pat_delete",
      map_pat_delete env x
    )
  | `Pat_unde x -> R.Case ("Pat_unde",
      map_pat_unde env x
    )
  )

let map_for_clause (env : env) ((v1, v2, v3) : CST.for_clause) =
  let v1 = map_pat_for env v1 in
  let v2 = map_for_type env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_for_type env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_update_clause (env : env) ((v1, v2, v3) : CST.update_clause) =
  let v1 = map_pat_update env v1 in
  let v2 = map_update_type env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_update_type env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_soql_using_clause (env : env) ((v1, v2, v3) : CST.soql_using_clause) =
  let v1 = map_pat_using env v1 in
  let v2 = map_pat_scope env v2 in
  let v3 = map_using_scope_type env v3 in
  R.Tuple [v1; v2; v3]

let map_fields_expression (env : env) ((v1, v2, v3, v4) : CST.fields_expression) =
  let v1 = map_pat_fields env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_fields_type env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_in_clause (env : env) ((v1, v2, v3) : CST.in_clause) =
  let v1 = map_pat_in env v1 in
  let v2 = map_in_type env v2 in
  let v3 = map_pat_fields env v3 in
  R.Tuple [v1; v2; v3]

let map_using_clause (env : env) ((v1, v2, v3, v4) : CST.using_clause) =
  let v1 = map_pat_using env v1 in
  let v2 = map_pat_list env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_identifier env v4 in
  R.Tuple [v1; v2; v3; v4]

(* OLD *)
let map_variable_declarator_id (env : env) ((v1, v2) : CST.variable_declarator_id) =
  let v1 = map_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_dimensions env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

(* NEW *)
(* FIXME: what does v2 do? *)
let variable_declarator_id (env : env) ((v1, _v2) : CST.variable_declarator_id) : G.ident =
  identifier env v1

(* OLD *)
let map_break_statement (env : env) ((v1, v2, v3) : CST.break_statement) =
  let v1 = map_pat_brk env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_identifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
let break_statement (env : env) ((v1, v2, v3) : CST.break_statement) : G.stmt =
  let v1 = token_ env v1 (* "break" *) in
  let v2 = H2.opt_to_label_ident (Option.map (identifier env) v2) in
  let v3 = token_ env v3 (* ";" *) in
  G.Break (v1, v2, v3) |> G.s

(* OLD *)
let map_continue_statement (env : env) ((v1, v2, v3) : CST.continue_statement) =
  let v1 = map_pat_cont env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_identifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
let continue_statement (env : env) ((v1, v2, v3) : CST.continue_statement) : G.stmt =
  let v1 = token_ env v1 (* "break" *) in
  let v2 = H2.opt_to_label_ident (Option.map (identifier env) v2) in
  let v3 = token_ env v3 (* ";" *) in
  G.Continue (v1, v2, v3) |> G.s

(* OLD *)
let rec map_name (env : env) (x : CST.name) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Scoped_id (v1, v2, v3) -> R.Case ("Scoped_id",
      let v1 = map_name env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

(* NEW *)
let rec name (env : env) (x : CST.name) : G.name =
  match x with
  | `Id x ->
      let i = identifier env x in
      H2.name_of_id i
  | `Scoped_id (v1, v2, v3) ->
      let v1 = name env v1 in
      let _v2 = token_ env v2 (* "." *) in
      let v3 = identifier env v3 in
      H2.add_id_opt_type_args_to_name v1 (v3, None) (* FIXME: None? *)

let map_inferred_parameters (env : env) ((v1, v2, v3, v4) : CST.inferred_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_identifier env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_identifier env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

(* OLD *)
let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* int *) token env tok
    )
  | `Deci_floa_point_lit tok -> R.Case ("Deci_floa_point_lit",
      (* decimal_floating_point_literal *) token env tok
    )
  | `Bool x -> R.Case ("Bool",
      map_boolean env x
    )
  | `Str_lit tok -> R.Case ("Str_lit",
      (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env tok
    )
  | `Null_lit x -> R.Case ("Null_lit",
      map_null_literal env x
    )
  )

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
      G.String (G.fake "'", str env tok, G.fake "'") (* FIXME: Isn't string delimiter captured by the parser? *)
  | `Null_lit x ->
      null_literal env x

let map_with_record_visibility_param (env : env) (x : CST.with_record_visibility_param) =
  (match x with
  | `Pat_maxd_EQ_int (v1, v2, v3) -> R.Case ("Pat_maxd_EQ_int",
      let v1 = map_pat_maxd env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = (* int *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_suppos_EQ_bool (v1, v2, v3) -> R.Case ("Pat_suppos_EQ_bool",
      let v1 = map_pat_suppos env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_boolean env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_suppos__EQ_bool (v1, v2, v3) -> R.Case ("Pat_suppos__EQ_bool",
      let v1 = map_pat_suppos_ env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_boolean env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_soql_literal (env : env) (x : CST.soql_literal) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* int *) token env tok
    )
  | `Deci tok -> R.Case ("Deci",
      (* pattern -?\d+(\.\d+)? *) token env tok
    )
  | `Str_lit tok -> R.Case ("Str_lit",
      (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env tok
    )
  | `Date tok -> R.Case ("Date",
      (* pattern [1-4][0-9]{3}-(?:0[1-9]|1[0-2])-(?:[0-2][1-9]|[1-2]0|3[0-1]) *) token env tok
    )
  | `Date_time tok -> R.Case ("Date_time",
      (* pattern [1-4][0-9]{3}-(?:0[1-9]|1[0-2])-(?:[0-2][1-9]|[1-2]0|3[0-1])T([0-1]\d|2[0-3]):[0-5]\d:[0-5]\d(?:\.\d\d?\d?)?(?:Z|[+-][0-1]\d:[0-5]\d) *) token env tok
    )
  | `Bool x -> R.Case ("Bool",
      map_boolean env x
    )
  | `Date_lit x -> R.Case ("Date_lit",
      map_date_literal env x
    )
  | `Date_lit_with_param (v1, v2, v3) -> R.Case ("Date_lit_with_param",
      let v1 = map_tok_choice_pat_last_n_days env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = (* int *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Curr_lit tok -> R.Case ("Curr_lit",
      (* pattern \w{3}\d+(\.\d+)? *) token env tok
    )
  | `Null_lit x -> R.Case ("Null_lit",
      map_null_literal env x
    )
  )

let map_field_identifier (env : env) (x : CST.field_identifier) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Dotted_id (v1, v2) -> R.Case ("Dotted_id",
      let v1 = map_identifier env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "." *) token env v1 in
          let v2 = map_identifier env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

let map_anon_choice_id_73106c9 (env : env) (x : CST.anon_choice_id_73106c9) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Apex_meth_id (v1, v2, v3) -> R.Case ("Apex_meth_id",
      let v1 = map_identifier env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_with_data_cat_filter (env : env) ((v1, v2, v3) : CST.with_data_cat_filter) =
  let v1 = map_identifier env v1 in
  let v2 = map_with_data_cat_filter_type env v2 in
  let v3 =
    (match v3 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `LPAR_id_rep_COMMA_id_RPAR x -> R.Case ("LPAR_id_rep_COMMA_id_RPAR",
        map_inferred_parameters env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_storage_identifier (env : env) (x : CST.storage_identifier) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_meta_ellips tok -> R.Case ("Semg_meta_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_field_identifier env x
    )
  )

let map_field_list (env : env) ((v1, v2) : CST.field_list) =
  let v1 = map_field_identifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_field_identifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_with_data_cat_expression (env : env) ((v1, v2, v3, v4) : CST.with_data_cat_expression) =
  let v1 = map_pat_data env v1 in
  let v2 = map_pat_cate env v2 in
  let v3 = map_with_data_cat_filter env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_pat_and env v1 in
      let v2 = map_with_data_cat_filter env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_when_expression (env : env) ((v1, v2, v3, v4) : CST.when_expression) =
  let v1 = map_pat_when env v1 in
  let v2 = map_identifier env v2 in
  let v3 = map_pat_then env v3 in
  let v4 = map_field_list env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_else_expression (env : env) ((v1, v2) : CST.else_expression) =
  let v1 = map_pat_else env v1 in
  let v2 = map_field_list env v2 in
  R.Tuple [v1; v2]

let map_soql_with_type (env : env) (x : CST.soql_with_type) =
  (match x with
  | `Pat_secu_enfo x -> R.Case ("Pat_secu_enfo",
      map_pat_secu_enfo env x
    )
  | `Pat_user_mode x -> R.Case ("Pat_user_mode",
      map_pat_user_mode env x
    )
  | `Pat_system_mode x -> R.Case ("Pat_system_mode",
      map_pat_system_mode env x
    )
  | `With_record_visi_exp (v1, v2, v3, v4, v5) -> R.Case ("With_record_visi_exp",
      let v1 = map_pat_reco env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_with_record_visibility_param env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_with_record_visibility_param env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `With_data_cat_exp x -> R.Case ("With_data_cat_exp",
      map_with_data_cat_expression env x
    )
  | `With_user_id_type (v1, v2, v3) -> R.Case ("With_user_id_type",
      let v1 = map_pat_userid env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_choice_stor_id_355c95c (env : env) (x : CST.anon_choice_stor_id_355c95c) =
  (match x with
  | `Stor_id x -> R.Case ("Stor_id",
      map_storage_identifier env x
    )
  | `Stor_alias (v1, v2, v3) -> R.Case ("Stor_alias",
      let v1 = map_storage_identifier env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_pat_as env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_type_of_clause (env : env) ((v1, v2, v3, v4, v5) : CST.type_of_clause) =
  let v1 = map_pat_typeof env v1 in
  let v2 = map_identifier env v2 in
  let v3 = R.List (List.map (map_when_expression env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_else_expression env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_end env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_soql_with_clause (env : env) ((v1, v2) : CST.soql_with_clause) =
  let v1 = map_pat_with env v1 in
  let v2 = map_soql_with_type env v2 in
  R.Tuple [v1; v2]

let map_from_clause (env : env) ((v1, v2, v3) : CST.from_clause) =
  let v1 = map_pat_from env v1 in
  let v2 = map_anon_choice_stor_id_355c95c env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_stor_id_355c95c env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let rec map_accessor_declaration (env : env) ((v1, v2, v3) : CST.accessor_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Pat_get x -> R.Case ("Pat_get",
        map_pat_get env x
      )
    | `Pat_set x -> R.Case ("Pat_set",
        map_pat_set env x
      )
    )
  in
  let v3 = map_anon_choice_trig_body_f78fea4 env v3 in
  R.Tuple [v1; v2; v3]

and map_accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_accessor_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_alias_expression (env : env) ((v1, v2, v3) : CST.alias_expression) =
  let v1 = map_value_expression env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_pat_as env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_identifier env v3 in
  R.Tuple [v1; v2; v3]

(* OLD *)
and map_annotation (env : env) ((v1, v2, v3) : CST.annotation) =
  let v1 = (* "@" *) token env v1 in
  let v2 = map_name env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_annotation_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

(* NEW *)
and annotation (env : env) ((v1, v2, v3) : CST.annotation) : G.attribute =
  let v1 = (* "@" *) token_ env v1 in
  let v2 = name env v2 in
  match v3 with
  | Some x ->
      let args = annotation_argument_list env x in
      G.NamedAttr (v1, v2, args)
  | None ->
      G.NamedAttr (v1, v2, fb [])

(* OLD *)
and map_annotation_argument_list (env : env) ((v1, v2, v3) : CST.annotation_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Elem_value x -> R.Case ("Elem_value",
        map_element_value env x
      )
    | `Anno_key_value_rep_opt_COMMA_anno_key_value (v1, v2) -> R.Case ("Anno_key_value_rep_opt_COMMA_anno_key_value",
        let v1 = map_annotation_key_value env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = map_annotation_key_value env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and annotation_argument_list (env : env) ((v1, v2, v3) : CST.annotation_argument_list) : G.arguments =
  let v1 = (* "(" *) token_ env v1 in
  let v2 (* : G.argument list *) =
    match v2 with
    | `Elem_value x -> R.Case ("Elem_value",
        map_element_value env x
      )
    | `Anno_key_value_rep_opt_COMMA_anno_key_value (v1, v2) -> R.Case ("Anno_key_value_rep_opt_COMMA_anno_key_value",
        let v1 = map_annotation_key_value env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = map_annotation_key_value env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      )
  in
  let v3 = (* ")" *) token_ env v3 in
  (v1, failwith "NOT IMPLEMENTED (annotation_argument_list)", v3)

and map_annotation_key_value (env : env) (x : CST.annotation_key_value) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Id_EQ_elem_value (v1, v2, v3) -> R.Case ("Id_EQ_elem_value",
      let v1 = map_identifier env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_element_value env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78 (env : env) ((v1, v2, v3, v4) : CST.anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_anon_choice_soql_lit_3019e24 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_soql_lit_3019e24 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_anon_choice_field_id_cb081aa (env : env) (x : CST.anon_choice_field_id_cb081aa) =
  (match x with
  | `Field_id x -> R.Case ("Field_id",
      map_field_identifier env x
    )
  | `Func_exp x -> R.Case ("Func_exp",
      map_function_expression env x
    )
  )

and map_anon_choice_int_1466488 (env : env) (x : CST.anon_choice_int_1466488) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* int *) token env tok
    )
  | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
      map_bound_apex_expression env x
    )
  )

and map_anon_choice_prim_exp_bbf4eda (env : env) (x : CST.anon_choice_prim_exp_bbf4eda) =
  (match x with
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Super x -> R.Case ("Super",
      map_super env x
    )
  )

and map_anon_choice_semg_ellips_d10ab47 (env : env) (x : CST.anon_choice_semg_ellips_d10ab47) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_meta_ellips tok -> R.Case ("Semg_meta_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_anon_choice_soql_lit_3019e24 (env : env) (x : CST.anon_choice_soql_lit_3019e24) =
  (match x with
  | `Soql_lit x -> R.Case ("Soql_lit",
      map_soql_literal env x
    )
  | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
      map_bound_apex_expression env x
    )
  )

and map_anon_choice_trig_body_f78fea4 (env : env) (x : CST.anon_choice_trig_body_f78fea4) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_trigger_body env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

(* OLD *)
and map_anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

(* NEW *)
and anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c)
  : G.expr * ((G.tok * G.expr) list) =
  let v1 = expression env v1 in
  let vs =
    List.map (fun (v1, v2) ->
        let v1 = (* "," *) token_ env v1 in
        let v2 = expression env v2 in
        v1, v2)
      v2
  in
  v1, vs

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_semg_ellips_d10ab47 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_semg_ellips_d10ab47 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* OLD *)
and map_array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) =
  let v1 = map_primary_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

(* NEW *)
and array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) : G.expr =
  let v1 = primary_expression env v1 in
  let v2 = (* "[" *) token_ env v2 in
  let v3 = expression env v3 in
  let v4 = (* "]" *) token_ env v4 in
  G.ArrayAccess (v1, (v2, v3, v4)) |> G.e

(* OLD *)
and map_array_creation_expression (env : env) ((v1, v2, v3) : CST.array_creation_expression) =
  let v1 = map_pat_new env v1 in
  let v2 = map_simple_type env v2 in
  let v3 =
    (match v3 with
    | `Rep1_dimens_expr_opt_dimens (v1, v2) -> R.Case ("Rep1_dimens_expr_opt_dimens",
        let v1 = R.List (List.map (map_dimensions_expr env) v1) in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_dimensions env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Dimens_array_init (v1, v2) -> R.Case ("Dimens_array_init",
        let v1 = map_dimensions env v1 in
        let v2 = map_array_initializer env v2 in
        R.Tuple [v1; v2]
      )
    | `Array_init v1 -> R.Case ("Array_init",
        map_array_initializer env v1
      )
    )
  in
  R.Tuple [v1; v2; v3]

(* NEW *)
and array_creation_expression (env : env) ((v1, v2, v3) : CST.array_creation_expression)
  : G.expr =
  let v1 = token_ env v1 in
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

(* OLD *)
and map_array_initializer (env : env) ((v1, v2, v3) : CST.array_initializer) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_variable_initializer env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_variable_initializer env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and array_initializer (env : env) (x : CST.array_initializer) : G.expr =
  let v1, v2, v3 = array_initializer_content env x in
  Container (Array, (v1, v2, v3)) |> G.e

(* AUX *)
and array_initializer_content (env : env) ((v1, v2, v3) : CST.array_initializer)
  : expr list bracket =
  let v1 = (* "{" *) token_ env v1 in
  let v2 =
    match v2 with
    | Some (w1, w2) ->
        let z1 = variable_initializer env w1 in
        let z2 =
          List.map (fun (y1, y2) ->
            let _r1 = (* "," *) token_ env y1 in
            let r2 = variable_initializer env y2 in
            r2
          ) w2
        in
        z1 :: z2
    | None -> []
  in
  let v3 = (* "}" *) token_ env v3 in
  v1, v2, v3

(* OLD *)
and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTGT_exp (v1, v2, v3) -> R.Case ("Exp_LTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

(* NEW *)
and binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op And, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "||" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Or, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op ASR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSR, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LSL, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "&" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitAnd, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "^" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitXor, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "|" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op BitOr, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "+" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Plus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "-" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Minus, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "*" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mult, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "/" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Div, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "%" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Mod, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "<" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Lt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op LtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "==" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Eq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op GtE, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* ">" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op Gt, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "===" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op PhysEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "!==" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotPhysEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e
  | `Exp_LTGT_exp (v1, v2, v3) -> (* FIXME: not in Apex documentation! *)
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "<>" *) in
      let v3 = expression env v3 in
      Call (IdSpecial (Op NotEq, v2) |> G.e, fb [ Arg v1; Arg v3 ]) |> G.e

(* OLD *)
and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and block (env : env) ((v1, v2, v3) : CST.block) : G.stmt =
  let v1 = token_ env v1 (* "{" *) in
  let v2 = List_.map (statement env) v2 in
  let v3 = token_ env v3 (* "}" *) in
  G.Block (v1, v2, v3) |> G.s

and map_boolean_expression (env : env) (x : CST.boolean_expression) =
  (match x with
  | `And_exp (v1, v2) -> R.Case ("And_exp",
      let v1 = map_condition_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_pat_and env v1 in
          let v2 = map_condition_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Or_exp (v1, v2) -> R.Case ("Or_exp",
      let v1 = map_condition_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_pat_or env v1 in
          let v2 = map_condition_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Not_exp (v1, v2) -> R.Case ("Not_exp",
      let v1 = map_pat_not env v1 in
      let v2 = map_condition_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Cond_exp x -> R.Case ("Cond_exp",
      map_condition_expression env x
    )
  )

and map_bound_apex_expression (env : env) ((v1, v2) : CST.bound_apex_expression) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

(* OLD *)
and map_catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) =
  let v1 = map_pat_catch env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_catch_formal_parameter env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_trigger_body env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

(* NEW *)
and catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) : G.catch =
  let v1 = token_ env v1 (* "catch" *)in
  let _v2 = (* "(" *) token env v2 in
  let v3 = catch_formal_parameter env v3 in
  let _v4 = (* ")" *) token env v4 in
  let v5 = trigger_body env v5 in
  (v1, v3, v5)

(* OLD *)
and map_catch_formal_parameter (env : env) (x : CST.catch_formal_parameter) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Opt_modifs_unan_type_var_decl_id (v1, v2, v3) -> R.Case ("Opt_modifs_unan_type_var_decl_id",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_id env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

(* NEW *)
and catch_formal_parameter (env : env) (x : CST.catch_formal_parameter) : G.catch_exn =
  match x with
  | `Semg_ellips tok ->
      let t = token_ env tok (* "..." *) in
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

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_class_body_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_class_body_declaration (env : env) (x : CST.class_body_declaration) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Choice_field_decl x -> R.Case ("Choice_field_decl",
      (match x with
      | `Field_decl x -> R.Case ("Field_decl",
          map_field_declaration env x
        )
      | `Meth_decl x -> R.Case ("Meth_decl",
          map_method_declaration env x
        )
      | `Class_decl x -> R.Case ("Class_decl",
          map_class_declaration env x
        )
      | `Inte_decl x -> R.Case ("Inte_decl",
          map_interface_declaration env x
        )
      | `Enum_decl x -> R.Case ("Enum_decl",
          map_enum_declaration env x
        )
      | `Blk x -> R.Case ("Blk",
          map_trigger_body env x
        )
      | `Static_init x -> R.Case ("Static_init",
          map_static_initializer env x
        )
      | `Cons_decl x -> R.Case ("Cons_decl",
          map_constructor_declaration env x
        )
      | `SEMI tok -> R.Case ("SEMI",
          (* ";" *) token env tok
        )
      )
    )
  )

(* OLD *)
and map_class_declaration (env : env) ((v1, v2) : CST.class_declaration) =
  let v1 = map_class_header env v1 in
  let v2 = map_class_body env v2 in
  R.Tuple [v1; v2]

(* NEW *)
and class_declaration (env : env) ((v1, v2) : CST.class_declaration) : G.stmt =
  failwith "NOT IMPLEMENTED (class_declaration)"

and map_class_header (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_header) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_pat_class env v2 in
  let v3 = map_identifier env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_superclass env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_interfaces env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_literal (env : env) ((v1, v2, v3) : CST.class_literal) =
  let v1 = map_unannotated_type env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = map_pat_class env v3 in
  R.Tuple [v1; v2; v3]

and map_comparison (env : env) (x : CST.comparison) =
  (match x with
  | `Value_comp (v1, v2) -> R.Case ("Value_comp",
      let v1 = map_value_comparison_operator env v1 in
      let v2 = map_anon_choice_soql_lit_3019e24 env v2 in
      R.Tuple [v1; v2]
    )
  | `Set_comp (v1, v2) -> R.Case ("Set_comp",
      let v1 = map_set_comparison_operator env v1 in
      let v2 =
        (match v2 with
        | `Subq x -> R.Case ("Subq",
            map_subquery env x
          )
        | `LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR x -> R.Case ("LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR",
            map_anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78 env x
          )
        | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
            map_bound_apex_expression env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_comparison_expression (env : env) ((v1, v2) : CST.comparison_expression) =
  let v1 = map_value_expression env v1 in
  let v2 = map_comparison env v2 in
  R.Tuple [v1; v2]

and map_condition_expression (env : env) (x : CST.condition_expression) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Choice_LPAR_bool_exp_RPAR x -> R.Case ("Choice_LPAR_bool_exp_RPAR",
      (match x with
      | `LPAR_bool_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_bool_exp_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_boolean_expression env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Comp_exp x -> R.Case ("Comp_exp",
          map_comparison_expression env x
        )
      )
    )
  )

and map_constant_declaration (env : env) ((v1, v2, v3, v4) : CST.constant_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_variable_declarator_list env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_constructor_body (env : env) ((v1, v2, v3, v4) : CST.constructor_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_explicit_constructor_invocation env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_statement env) v3) in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_constructor_declaration (env : env) ((v1, v2, v3) : CST.constructor_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_constructor_declarator env v2 in
  let v3 = map_constructor_body env v3 in
  R.Tuple [v1; v2; v3]

and map_constructor_declarator (env : env) ((v1, v2, v3) : CST.constructor_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_identifier env v2 in
  let v3 = map_formal_parameters env v3 in
  R.Tuple [v1; v2; v3]

(* OLD *)
and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Trig_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Trig_decl",
      let v1 = map_pat_trig env v1 in
      let v2 = map_identifier env v2 in
      let v3 = map_pat_on env v3 in
      let v4 = map_identifier env v4 in
      let v5 = (* "(" *) token env v5 in
      let v6 = map_trigger_event env v6 in
      let v7 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_trigger_event env v2 in
          R.Tuple [v1; v2]
        ) v7)
      in
      let v8 = (* ")" *) token env v8 in
      let v9 = map_trigger_body env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Inte_decl x -> R.Case ("Inte_decl",
      map_interface_declaration env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  )

(* NEW *)
and declaration (env : env) (x : CST.declaration) : G.stmt =
  match x with
  | `Class_decl x ->
      class_declaration env x
  | `Trig_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      failwith "NOT IMPLEMENTED (declaration)"
  | `Inte_decl x ->
      interface_declaration env x
  | `Enum_decl x ->
      enum_declaration env x

(* OLD *)
and map_dimensions_expr (env : env) ((v1, v2, v3) : CST.dimensions_expr) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and dimensions_expr (env : env) ((v1, v2, v3) : CST.dimensions_expr)
  : G.expr bracket =
  let v1 = (* "[" *) token_ env v1 in
  let v2 = expression env v2 in
  let v3 = (* "]" *) token_ env v3 in
  v1, v2, v3

and map_dml_expression (env : env) (x : CST.dml_expression) =
  (match x with
  | `Dml_type_exp (v1, v2) -> R.Case ("Dml_type_exp",
      let v1 = map_dml_type env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_upsert_exp_opt_unan_type (v1, v2, v3) -> R.Case ("Pat_upsert_exp_opt_unan_type",
      let v1 = map_pat_upsert env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_unannotated_type env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_merge_exp_SPACE_exp (v1, v2, v3, v4) -> R.Case ("Pat_merge_exp_SPACE_exp",
      let v1 = map_pat_merge env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* " " *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

(* OLD *)
and map_do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) =
  let v1 = map_pat_do env v1 in
  let v2 = map_statement env v2 in
  let v3 = map_pat_while env v3 in
  let v4 = map_parenthesized_expression env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

(* OLD *)
and do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) : G.stmt =
  let v1 = token_ env v1 (* "do" *) in
  let v2 = statement env v2 in
  let _v3 = token_ env v3 (* "while" *) in
  let v4 = parenthesized_expression env v4 in
  let _v7 = token_ env v5 (* ";" *) in
  DoWhile (v1, v2, v4) |> G.s

and map_element_value (env : env) (x : CST.element_value) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Elem_value_array_init (v1, v2, v3, v4) -> R.Case ("Elem_value_array_init",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_element_value env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_element_value env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  )

(* OLD *)
and map_enhanced_for_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.enhanced_for_statement) =
  let v1 = map_pat_for env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_unannotated_type env v4 in
  let v5 = map_variable_declarator_id env v5 in
  let v6 = (* ":" *) token env v6 in
  let v7 = map_expression env v7 in
  let v8 = (* ")" *) token env v8 in
  let v9 = map_statement env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

(* NEW *)
and enhanced_for_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.enhanced_for_statement) : G.stmt =
  let v1 = (* "for" *) token_ env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> modifiers env x
    | None -> []
  in
  let v4 = unannotated_type env v4 in
  let ty = make_type v3 v4 in
  let v5 = variable_declarator_id env v5 in
  let pat = G.PatId (v5, empty_id_info ()) in
  let v6 = (* ":" *) token_ env v6 in
  let v7 = expression env v7 in
  let v8 = (* ")" *) token_ env v8 in
  let v9 = statement env v9 in
  G.For (v1, G.ForEach (G.PatTyped (pat, ty), v6, v7), v9) |> G.s

(* OLD *)
and map_enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_enum_constant env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_enum_constant env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and enum_body (env : env) ((v1, v2, v3) : CST.enum_body) : G.or_type_element list =
  let _v1 = (* "{" *) token_ env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = enum_constant env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = (* "," *) token_ env v1 in
            let v2 = enum_constant env v2 in
            v2
          ) v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 = (* "}" *) token_ env v3 in
  v2

(* OLD *)
and map_enum_constant (env : env) (x : CST.enum_constant) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Opt_modifs_id (v1, v2) -> R.Case ("Opt_modifs_id",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_identifier env v2 in
      R.Tuple [v1; v2]
    )
  )

(* NEW *)
and enum_constant (env : env) (x : CST.enum_constant) : G.or_type_element =
  match x with
  | `Semg_ellips tok ->
      let t = (* "..." *) token_ env tok in
      OrEllipsis t
  | `Opt_modifs_id (v1, v2) ->
      let m = Option.map (modifiers env) v1 in
      let i = identifier env v2 in
      OrEnum (m, i)

(* OLD *)
and map_enum_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration) =
  let v1 =
    match v1 with
    | Some x -> modifiers env x
    | None -> []
  in
  let v2 = token_ (* "enum" *) env v2 in
  let v3 = identifier env v3 in
  (* FIXME: not sure where to put it. This part is ignored e.g. in C# *)
  let v4 =
    match v4 with
    | Some x -> [] (* interfaces env x *)
    | None -> []
  in
  let v5 = enum_body env v5 in
  let idinfo = empty_id_info () in
  let ent = { name = EN (Id (v3, idinfo)); attrs = v1; tparams = None } in
  G.DefStmt (ent, G.TypeDef { tbody = OrType v5 }) |> G.s

and enum_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration) : G.stmt =
  failwith "NOT IMPLEMENTED (enum_declaration)"

and map_explicit_constructor_invocation (env : env) ((v1, v2, v3) : CST.explicit_constructor_invocation) =
  let v1 =
    (match v1 with
    | `Opt_type_args_choice_this (v1, v2) -> R.Case ("Opt_type_args_choice_this",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_type_arguments env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | `This x -> R.Case ("This",
              map_this env x
            )
          | `Super x -> R.Case ("Super",
              map_super env x
            )
          )
        in
        R.Tuple [v1; v2]
      )
    | `Choice_prim_exp_DOT_opt_type_args_super (v1, v2, v3, v4) -> R.Case ("Choice_prim_exp_DOT_opt_type_args_super",
        let v1 =
          (match v1 with
          | `Prim_exp x -> R.Case ("Prim_exp",
              map_primary_expression env x
            )
          )
        in
        let v2 = (* "." *) token env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_type_arguments env x
            ))
          | None -> R.Option None)
        in
        let v4 = map_super env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v2 = map_argument_list env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* OLD  *)
and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 =
        (match v1 with
        | `Id x -> R.Case ("Id",
            map_identifier env x
          )
        | `Field_access x -> R.Case ("Field_access",
            map_field_access env x
          )
        | `Array_access x -> R.Case ("Array_access",
            map_array_access env x
          )
        )
      in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `GTGTGTEQ tok -> R.Case ("GTGTGTEQ",
            (* ">>>=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Inst_exp (v1, v2, v3) -> R.Case ("Inst_exp",
      let v1 = map_expression env v1 in
      let v2 = map_pat_inst env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tern_exp (v1, v2, v3, v4, v5) -> R.Case ("Tern_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Update_exp x -> R.Case ("Update_exp",
      map_update_expression env x
    )
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Cast_exp (v1, v2, v3, v4) -> R.Case ("Cast_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Dml_exp x -> R.Case ("Dml_exp",
      map_dml_expression env x
    )
  | `Switch_exp x -> R.Case ("Switch_exp",
      map_switch_expression env x
    )
  )

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
      let v2 = token_ env v2 (* "is" *) in
      let v3 = type_ env v3 in
      Call (IdSpecial (Instanceof, v2) |> G.e, fb [ Arg v1; ArgType v3 ]) |> G.e
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let v2 = (* "?" *) token_ env v2 in
      let v3 = expression env v3 in
      let v4 = (* ":" *) token_ env v4 in
      let v5 = expression env v5 in
      G.Conditional (v1, v3, v5) |> G.e
  | `Update_exp x ->
      update_expression env x
  | `Prim_exp x ->
      primary_expression env x
  | `Un_exp x ->
      unary_expression env x
  | `Cast_exp (v1, v2, v3, v4) ->
      let v1 = token_ env v1 (* "(" *) in
      let v2 = type_ env v2 in
      let _v3 = token env v3 (* ")" *) in
      let v4 = expression env v4 in
      G.Cast (v2, v1, v4) |> G.e
  | `Dml_exp x ->
      failwith "NOT IMPLEMENTED (expression)"
  | `Switch_exp x ->
      switch_expression env x
      |> G.stmt_to_expr (* FIXME: "switch" is expr in parser, while it should be stmt *)

(* AUX *)
and assignment_operator (env : env) x : G.operator G.wrap =
  match x with
  | `EQ tok -> (Eq, token_ env tok) (* "=" *)
  | `PLUSEQ tok -> (Plus, token_ env tok) (* "+=" *)
  | `DASHEQ tok -> (Minus, token_ env tok) (* "-=" *)
  | `STAREQ tok -> (Mult, token_ env tok) (* "*=" *)
  | `SLASHEQ tok -> (Div, token_ env tok) (* "/=" *)
  | `AMPEQ tok -> (BitAnd, token_ env tok) (* "&=" *)
  | `BAREQ tok -> (BitOr, token_ env tok) (* "|=" *)
  | `HATEQ tok -> (BitXor, token_ env tok) (* "^=" *)
  | `PERCEQ tok -> (Mod, token_ env tok) (* "%=" *)
  | `LTLTEQ tok -> (LSL, token_ env tok) (* "<<=" *)
  | `GTGTEQ tok -> (ASR, token_ env tok) (* ">>=" *)
  | `GTGTGTEQ tok -> (LSR, token_ env tok) (* ">>>=" *)

(* OLD *)
and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expression env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

(* NEW *)
and expression_statement (env : env) ((v1, v2) : CST.expression_statement) : G.stmt =
  let v1 = expression env v1 in
  let v2 = (* ";" *) token_ env v2 in
  G.ExprStmt (v1, v2) |> G.s

and map_extends_interfaces (env : env) ((v1, v2) : CST.extends_interfaces) =
  let v1 = map_pat_extends env v1 in
  let v2 = map_type_list env v2 in
  R.Tuple [v1; v2]

(* OLD *)
and map_field_access (env : env) ((v1, v2, v3, v4) : CST.field_access) =
  let v1 = map_anon_choice_prim_exp_bbf4eda env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_property_navigation env v1 in
        let v2 = map_super env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_property_navigation env v3 in
  let v4 =
    (match v4 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `This x -> R.Case ("This",
        map_this env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

(* NEW *)
and field_access (env : env) ((v1, v2, v3, v4) : CST.field_access) : G.expr =
  failwith "NOT IMPLEMENTED (field_access)"

and map_field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_variable_declarator_list env v3 in
  let v4 =
    (match v4 with
    | `Acce_list x -> R.Case ("Acce_list",
        map_accessor_list env x
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

(* OLD *)
and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = map_pat_fina env v1 in
  let v2 = map_trigger_body env v2 in
  R.Tuple [v1; v2]

(* NEW *)
and finally_clause (env : env) ((v1, v2) : CST.finally_clause) : G.finally =
  let v1 = token_ env v1 (* "finally" *) in
  let v2 = trigger_body env v2 in
  (v1, v2)

and map_find_clause (env : env) ((v1, v2) : CST.find_clause) =
  let v1 = map_pat_find env v1 in
  let v2 =
    (match v2 with
    | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
        map_bound_apex_expression env x
      )
    | `Term_sepa_start_term_term_sepa_end (v1, v2, v3) -> R.Case ("Term_sepa_start_term_term_sepa_end",
        let v1 = (* "'" *) token env v1 in
        let v2 = (* pattern "(\\\\\\'|[^'])+" *) token env v2 in
        let v3 = (* "'" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  R.Tuple [v1; v2]

(* OLD *)
and map_for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) =
  let v1 = map_pat_for env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | `Semg_ellips tok -> R.Case ("Semg_ellips",
        (* "..." *) token env tok
      )
    | `Choice_local_var_decl_opt_exp_SEMI_opt_exp_rep_COMMA_exp (v1, v2, v3, v4) -> R.Case ("Choice_local_var_decl_opt_exp_SEMI_opt_exp_rep_COMMA_exp",
        let v1 =
          (match v1 with
          | `Local_var_decl x -> R.Case ("Local_var_decl",
              map_local_variable_declaration env x
            )
          | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) -> R.Case ("Opt_exp_rep_COMMA_exp_SEMI",
              let v1 =
                (match v1 with
                | Some x -> R.Option (Some (
                    map_anon_exp_rep_COMMA_exp_0bb260c env x
                  ))
                | None -> R.Option None)
              in
              let v2 = (* ";" *) token env v2 in
              R.Tuple [v1; v2]
            )
          )
        in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_expression env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* ";" *) token env v3 in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_exp_rep_COMMA_exp_0bb260c env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

(* NEW *)
and for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) : G.stmt =
  let v1 = (* "for" *) token_ env v1 in
  let v2 = (* "(" *) token_ env v2 in
  let v3 =
    match v3 with
    | `Semg_ellips tok ->
        let t = (* "..." *) token_ env tok in
        G.ForEllipsis t
    | `Choice_local_var_decl_opt_exp_SEMI_opt_exp_rep_COMMA_exp (v1, v2, v3, v4) ->
        let v1 (* : G.for_var_or_expr list *) =
          match v1 with
          | `Local_var_decl x ->
               local_variable_declaration_data_only env x
               |> List.map (fun (t1, t2) -> G.ForInitVar (t1, t2))
          | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) ->
              let v1 =
                match v1 with
                | Some x ->
                    let (e, tes) = anon_exp_rep_COMMA_exp_0bb260c env x in
                    (e :: List.map snd tes)
                    |> List.map (fun f -> ForInitExpr f)
                | None -> []
              in
              let _v2 = (* ";" *) token env v2 in
              v1
        in
        let v2 = Option.map (expression env) v2 in
        let v3 = (* ";" *) token env v3 in
        let v4 =
          match v4 with
          | Some x ->
              (* FIXME: What to do with the other expressions here? *)
              let (e, _) = anon_exp_rep_COMMA_exp_0bb260c env x in
              Some e
          | None -> None
        in
        G.ForClassic (v1, v2, v4)
  in
  let v4 = (* ")" *) token_ env v4 in
  let v5 = statement env v5 in
  For (v1, v3, v5) |> G.s

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Opt_modifs_unan_type_var_decl_id (v1, v2, v3) -> R.Case ("Opt_modifs_unan_type_var_decl_id",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_id env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_meta_ellips tok -> R.Case ("Semg_meta_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_formal_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_formal_parameter env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_function_expression (env : env) (x : CST.function_expression) =
  (match x with
  | `Pat_dist_LPAR_choice_field_id_COMMA_geo_loca_type_COMMA_str_lit_RPAR (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Pat_dist_LPAR_choice_field_id_COMMA_geo_loca_type_COMMA_str_lit_RPAR",
      let v1 = map_pat_dist env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Field_id x -> R.Case ("Field_id",
            map_field_identifier env x
          )
        | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
            map_bound_apex_expression env x
          )
        )
      in
      let v4 = (* "," *) token env v4 in
      let v5 = map_geo_location_type env v5 in
      let v6 = (* "," *) token env v6 in
      let v7 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env v7
      in
      let v8 = (* ")" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Func_name_LPAR_value_exp_RPAR (v1, v2, v3, v4) -> R.Case ("Func_name_LPAR_value_exp_RPAR",
      let v1 = map_function_name env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_value_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

(* OLD *)
and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Scoped_type_id x -> R.Case ("Scoped_type_id",
        map_scoped_type_identifier env x
      )
    )
  in
  let v2 = map_type_arguments env v2 in
  R.Tuple [v1; v2]

(* NEW *)
and generic_type (env : env) ((v1, v2) : CST.generic_type) : (G.ident * G.type_arguments option) list =
  match v1 with
    | `Id x ->
        let i = identifier env x in
        let ta = type_arguments env v2 in
        [i, Some ta]
    | `Scoped_type_id x ->
        scoped_type_identifier env x

and map_geo_location_type (env : env) (x : CST.geo_location_type) =
  (match x with
  | `Field_id x -> R.Case ("Field_id",
      map_field_identifier env x
    )
  | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
      map_bound_apex_expression env x
    )
  | `Pat_geol_LPAR_deci_COMMA_deci_RPAR (v1, v2, v3, v4, v5, v6) -> R.Case ("Pat_geol_LPAR_deci_COMMA_deci_RPAR",
      let v1 = map_pat_geol env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* pattern -?\d+(\.\d+)? *) token env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 = (* pattern -?\d+(\.\d+)? *) token env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_group_by_clause (env : env) ((v1, v2, v3, v4) : CST.group_by_clause) =
  let v1 = map_pat_group env v1 in
  let v2 = map_pat_by env v2 in
  let v3 = map_group_by_expression env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_having_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_group_by_expression (env : env) (x : CST.group_by_expression) =
  (match x with
  | `Choice_field_id_rep_COMMA_choice_field_id (v1, v2) -> R.Case ("Choice_field_id_rep_COMMA_choice_field_id",
      let v1 = map_anon_choice_field_id_cb081aa env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_anon_choice_field_id_cb081aa env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_pat_rollup_LPAR_field_id_rep_COMMA_field_id_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Choice_pat_rollup_LPAR_field_id_rep_COMMA_field_id_RPAR",
      let v1 =
        (match v1 with
        | `Pat_rollup x -> R.Case ("Pat_rollup",
            map_pat_rollup env x
          )
        | `Pat_cube x -> R.Case ("Pat_cube",
            map_pat_cube env x
          )
        )
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_field_identifier env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_field_identifier env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_having_boolean_expression (env : env) (x : CST.having_boolean_expression) =
  (match x with
  | `Having_and_exp (v1, v2) -> R.Case ("Having_and_exp",
      let v1 = map_having_condition_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_pat_and env v1 in
          let v2 = map_having_condition_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Having_or_exp (v1, v2) -> R.Case ("Having_or_exp",
      let v1 = map_having_condition_expression env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_pat_or env v1 in
          let v2 = map_having_condition_expression env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Having_not_exp (v1, v2) -> R.Case ("Having_not_exp",
      let v1 = map_pat_not env v1 in
      let v2 = map_having_condition_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Having_cond_exp x -> R.Case ("Having_cond_exp",
      map_having_condition_expression env x
    )
  )

and map_having_clause (env : env) ((v1, v2) : CST.having_clause) =
  let v1 = map_pat_having env v1 in
  let v2 = map_having_boolean_expression env v2 in
  R.Tuple [v1; v2]

and map_having_comparison (env : env) (x : CST.having_comparison) =
  (match x with
  | `Having_value_comp (v1, v2) -> R.Case ("Having_value_comp",
      let v1 = map_value_comparison_operator env v1 in
      let v2 = map_anon_choice_soql_lit_3019e24 env v2 in
      R.Tuple [v1; v2]
    )
  | `Having_set_comp (v1, v2) -> R.Case ("Having_set_comp",
      let v1 = map_set_comparison_operator env v1 in
      let v2 =
        (match v2 with
        | `LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR x -> R.Case ("LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR",
            map_anon_LPAR_choice_soql_lit_rep_COMMA_choice_soql_lit_RPAR_bea6d78 env x
          )
        | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
            map_bound_apex_expression env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_having_condition_expression (env : env) (x : CST.having_condition_expression) =
  (match x with
  | `LPAR_having_bool_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_having_bool_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_having_boolean_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Having_comp_exp (v1, v2) -> R.Case ("Having_comp_exp",
      let v1 = map_function_expression env v1 in
      let v2 = map_having_comparison env v2 in
      R.Tuple [v1; v2]
    )
  )

(* OLD *)
and map_if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) =
  let v1 = map_pat_if env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_pat_else env v1 in
        let v2 = map_statement env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

(* NEW *)
and if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) : G.stmt =
  let v1 = token_ env v1 (* "if" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = statement env v3 in
  let v4 =
    match v4 with
    | Some (v1, v2) ->
        let _v1 = token_ env v1 (* "else" *) in
        let v2 = statement env v2 in
        Some v2
    | None -> None
  in
  G.If (v1, G.Cond v2, v3, v4) |> G.s

and map_interface_body (env : env) ((v1, v2, v3) : CST.interface_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Semg_ellips tok -> R.Case ("Semg_ellips",
          (* "..." *) token env tok
        )
      | `Cst_decl x -> R.Case ("Cst_decl",
          map_constant_declaration env x
        )
      | `Enum_decl x -> R.Case ("Enum_decl",
          map_enum_declaration env x
        )
      | `Meth_decl x -> R.Case ("Meth_decl",
          map_method_declaration env x
        )
      | `Class_decl x -> R.Case ("Class_decl",
          map_class_declaration env x
        )
      | `Inte_decl x -> R.Case ("Inte_decl",
          map_interface_declaration env x
        )
      | `SEMI tok -> R.Case ("SEMI",
          (* ";" *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* OLD *)
and map_interface_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.interface_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_pat_inte env v2 in
  let v3 = map_identifier env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_extends_interfaces env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_interface_body env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

(* NEW *)
and interface_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.interface_declaration) : G.stmt =
  failwith "NOT IMPLEMENTED (interface_declaration)"

and map_interfaces (env : env) ((v1, v2) : CST.interfaces) =
  let v1 = map_pat_imples env v1 in
  let v2 = map_type_list env v2 in
  R.Tuple [v1; v2]

(* OLD *)
and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 = map_identifier env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) : G.stmt =
  let v1 = identifier env v1 (* identifier *) in
  let _v2 = token_ env v2 (* ":" *) in
  let v3 = statement env v3 in
  G.Label (v1, v3) |> G.s

and map_limit_clause (env : env) ((v1, v2) : CST.limit_clause) =
  let v1 = map_pat_limit env v1 in
  let v2 = map_anon_choice_int_1466488 env v2 in
  R.Tuple [v1; v2]

(* OLD *)
and map_local_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.local_variable_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_variable_declarator_list env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

(* NEW *)
and local_variable_declaration (env : env) ((v1, v2, v3, v4) as v : CST.local_variable_declaration) : G.stmt =
  let d = local_variable_declaration_data_only env v in
  let v4 = (* ";" *) token_ env v4 in
  var_def_stmt v4 d

(* AUX *)
(* data-only = without semicolon *)
and local_variable_declaration_data_only (env : env) ((v1, v2, v3, v4) : CST.local_variable_declaration)
    : (entity * variable_definition) list =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  let v4 = (* ";" *) token_ env v4 in
  List_.map
    (fun (ent, vardef) ->
      (ent, { vinit = vardef.vinit; vtype = Some (make_type v1 v2); vtok = G.no_sc }))
    v3 (* t *)

(* AUX *)
and var_def_stmt (sc : Tok.t)
    (decls : (entity * variable_definition) list) : G.stmt =
  let stmts =
    decls
    |> H2.add_semicolon_to_last_var_def_and_convert_to_stmts sc
  in
  G.stmt1 stmts

and map_map_creation_expression (env : env) ((v1, v2, v3) : CST.map_creation_expression) =
  let v1 = map_pat_new env v1 in
  let v2 = map_simple_type env v2 in
  let v3 = map_map_initializer env v3 in
  R.Tuple [v1; v2; v3]

and map_map_initializer (env : env) ((v1, v2, v3) : CST.map_initializer) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_map_initializer_ env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_map_initializer_ env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_map_initializer_ (env : env) ((v1, v2, v3) : CST.map_initializer_) =
  let v1 = map_expression env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_method_declaration (env : env) ((v1, v2, v3) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_method_header env v2 in
  let v3 = map_anon_choice_trig_body_f78fea4 env v3 in
  R.Tuple [v1; v2; v3]

and map_method_declarator (env : env) ((v1, v2, v3) : CST.method_declarator) =
  let v1 = map_identifier env v1 in
  let v2 = map_formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_dimensions env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_method_header (env : env) ((v1, v2, v3) : CST.method_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_type_parameters env v1 in
        let v2 = R.List (List.map (map_annotation env) v2) in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_method_declarator env v3 in
  R.Tuple [v1; v2; v3]

and map_method_invocation (env : env) ((v1, v2) : CST.method_invocation) =
  let v1 =
    (match v1 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Choice_prim_exp_prop_navi_opt_super_prop_navi_opt_type_args_id (v1, v2, v3, v4, v5) -> R.Case ("Choice_prim_exp_prop_navi_opt_super_prop_navi_opt_type_args_id",
        let v1 = map_anon_choice_prim_exp_bbf4eda env v1 in
        let v2 = map_property_navigation env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_super env v1 in
              let v2 = map_property_navigation env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_type_arguments env x
            ))
          | None -> R.Option None)
        in
        let v5 = map_identifier env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    )
  in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

(* OLD *)
and map_modifiers (env : env) (xs : CST.modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Anno x -> R.Case ("Anno",
        map_annotation env x
      )
    | `Modi x -> R.Case ("Modi",
        map_modifier env x
      )
    )
  ) xs)

(* NEW *)
and modifiers (env : env) (xs : CST.modifiers) : attribute list =
  List.map (fun x ->
    match x with
    | `Anno x -> annotation env x (* G.NamedAttr *)
    | `Modi x -> modifier env x (* G.KeywordAttr or G.OtherAttribute *)
  ) xs

 (* OLD *)
and map_object_creation_expression (env : env) (x : CST.object_creation_expression) =
  map_unqualified_object_creation_expression env x

(* NEW *)
and object_creation_expression (env : env) (x : CST.object_creation_expression) : G.expr =
  unqualified_object_creation_expression env x

and map_offset_clause (env : env) ((v1, v2) : CST.offset_clause) =
  let v1 = map_pat_offset env v1 in
  let v2 = map_anon_choice_int_1466488 env v2 in
  R.Tuple [v1; v2]

and map_order_by_clause (env : env) ((v1, v2, v3, v4) : CST.order_by_clause) =
  let v1 = map_pat_order env v1 in
  let v2 = map_pat_by env v2 in
  let v3 = map_order_expression env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_order_expression env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_order_expression (env : env) ((v1, v2, v3) : CST.order_expression) =
  let v1 = map_value_expression env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_order_direction env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_order_null_direciton env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

(* OLD *)
and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* AUX *)
and adjust_range_of_parenthesized_expr (env : env)
    (l : Tree_sitter_run.Token.t) (r : Tree_sitter_run.Token.t) (e : G.expr) : G.expr =
  let lloc = Tok.unsafe_loc_of_tok (token_ env l) in
  let rloc = Tok.unsafe_loc_of_tok (token_ env r) in
  e.e_range <- Some (lloc, rloc);
  e
(* NEW *)
and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : G.expr =
  expression env v2
  |> adjust_range_of_parenthesized_expr env v1 v3

(* OLD *)
and map_partial_catch (env : env) (x : CST.partial_catch) =
  map_catch_clause env x

(* NEW *)
and partial_catch (env : env) (x : CST.partial_catch) : G.catch =
  catch_clause env x

(* OLD *)
and map_partial_finally (env : env) (x : CST.partial_finally) =
  map_finally_clause env x

(* NEW *)
and partial_finally (env : env) (x : CST.partial_finally) : G.finally =
  finally_clause env x

(* OLD *)
and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Choice_lit x -> R.Case ("Choice_lit",
      (match x with
      | `Lit x -> R.Case ("Lit",
          map_literal env x
        )
      | `Class_lit x -> R.Case ("Class_lit",
          map_class_literal env x
        )
      | `This x -> R.Case ("This",
          map_this env x
        )
      | `Id x -> R.Case ("Id",
          map_identifier env x
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      | `Obj_crea_exp x -> R.Case ("Obj_crea_exp",
          map_object_creation_expression env x
        )
      | `Field_access x -> R.Case ("Field_access",
          map_field_access env x
        )
      | `Array_access x -> R.Case ("Array_access",
          map_array_access env x
        )
      | `Meth_invo x -> R.Case ("Meth_invo",
          map_method_invocation env x
        )
      | `Array_crea_exp x -> R.Case ("Array_crea_exp",
          map_array_creation_expression env x
        )
      | `Map_crea_exp x -> R.Case ("Map_crea_exp",
          map_map_creation_expression env x
        )
      | `Query_exp x -> R.Case ("Query_exp",
          map_query_expression env x
        )
      )
    )
  | `Semg_deep_exp (v1, v2, v3) -> R.Case ("Semg_deep_exp",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

(* NEW *)
and primary_expression (env : env) (x : CST.primary_expression) : G.expr =
  match x with
  | `Choice_lit x ->
      (match x with
      | `Lit x ->
          let lit = literal env x in
          G.L lit |> G.e
      | `Class_lit x -> failwith "NOT IMPLEMENTED (primary_expression)"
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
      | `Meth_invo x -> failwith "NOT IMPLEMENTED (primary_expression)"
      | `Array_crea_exp x ->
          array_creation_expression env x
      | `Map_crea_exp x -> failwith "NOT IMPLEMENTED (primary_expression)"
      | `Query_exp x -> failwith "NOT IMPLEMENTED (primary_expression)"
      )
  | `Semg_deep_exp (v1, v2, v3) ->
      let v1 = token_ env v1 in
      let v2 = expression env v2 in
      let v3 = token_ env v3 in
      G.DeepEllipsis (v1, v2, v3) |> G.e

and map_query_expression (env : env) ((v1, v2, v3) : CST.query_expression) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | `Soql_query x -> R.Case ("Soql_query",
        map_soql_query env x
      )
    | `Sosl_query x -> R.Case ("Sosl_query",
        map_sosl_query env x
      )
    )
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_query_expression_ (env : env) (x : CST.query_expression_) =
  map_sosl_query_body env x

(* OLD *)
and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = map_pat_ret env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and return_statement (env : env) ((v1, v2, v3) : CST.return_statement) : G.stmt =
  let v1 = token_ env v1 (* "return" *) in
  let v2 = Option.map (expression env) v2 in
  let v3 = token_ env v3 (* ";" *) in
  Return (v1, v2, v3) |> G.s

and map_returning_clause (env : env) ((v1, v2, v3) : CST.returning_clause) =
  let v1 = map_pat_retu env v1 in
  let v2 = map_sobject_return env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_sobject_return env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

(* OLD *)
and map_run_as_statement (env : env) ((v1, v2, v3) : CST.run_as_statement) =
  let v1 = map_pat_e8c36c5 env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_trigger_body env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and run_as_statement (env : env) ((v1, v2, v3) : CST.run_as_statement) : G.stmt =
  let v1 = token_ env v1 (* "System.runAs" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = trigger_body env v3 in
  G.RawStmt (Raw_tree.Case ("System.runAs", Raw_tree.Any (G.Raw (Raw_tree.Tuple
    [Raw_tree.Any (G.Tk v1); Raw_tree.Any (G.E v2); Raw_tree.Any (G.S v3)])))) |> G.s

(* OLD *)
and map_scoped_type_identifier (env : env) ((v1, v2, v3, v4) : CST.scoped_type_identifier) =
  let v1 =
    (match v1 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Scoped_type_id x -> R.Case ("Scoped_type_id",
        map_scoped_type_identifier env x
      )
    | `Gene_type x -> R.Case ("Gene_type",
        map_generic_type env x
      )
    )
  in
  let v2 = (* "." *) token env v2 in
  let v3 = R.List (List.map (map_annotation env) v3) in
  let v4 = map_identifier env v4 in
  R.Tuple [v1; v2; v3; v4]

(* NEW *)
and scoped_type_identifier (env : env) ((v1, v2, v3, v4) : CST.scoped_type_identifier)
  : (G.ident * G.type_arguments option) list =
  let v1 =
    match v1 with
    | `Id x -> [identifier env x, None]
    | `Scoped_type_id x -> scoped_type_identifier env x
    | `Gene_type x -> generic_type env x
  in
  let v2 = (* "." *) token_ env v2 in
  (* FIXME: There is no place in the AST to put these attributes, as in AST attrs
   * occur only on the toplevel of a type, while in Apex we can have them in the
   * path in a scoped type, whic is represented by G.name, in which we only keep
   * a list of idents, without attrs. *)
  let _v3 = List.map (annotation env) v3 in
  let v4 = identifier env v4 in
  v1 @ [v4, None]

and map_select_clause (env : env) ((v1, v2) : CST.select_clause) =
  let v1 = map_pat_select env v1 in
  let v2 =
    (match v2 with
    | `Count_exp x -> R.Case ("Count_exp",
        map_count_expression env x
      )
    | `Sele_exp_rep_COMMA_sele_exp x -> R.Case ("Sele_exp_rep_COMMA_sele_exp",
        map_selected_fields env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_selectable_expression (env : env) (x : CST.selectable_expression) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_meta_ellips tok -> R.Case ("Semg_meta_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Choice_value_exp x -> R.Case ("Choice_value_exp",
      (match x with
      | `Value_exp x -> R.Case ("Value_exp",
          map_value_expression env x
        )
      | `Alias_exp x -> R.Case ("Alias_exp",
          map_alias_expression env x
        )
      | `Type_of_clause x -> R.Case ("Type_of_clause",
          map_type_of_clause env x
        )
      | `Fields_exp x -> R.Case ("Fields_exp",
          map_fields_expression env x
        )
      | `Subq x -> R.Case ("Subq",
          map_subquery env x
        )
      )
    )
  )

and map_selected_fields (env : env) ((v1, v2) : CST.selected_fields) =
  let v1 = map_selectable_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_selectable_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

(* old *)
and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Void_type x -> R.Case ("Void_type",
      map_void_type env x
    )
  | `Bool_type tok -> R.Case ("Bool_type",
      (* "boolean" *) token env tok
    )
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Scoped_type_id x -> R.Case ("Scoped_type_id",
      map_scoped_type_identifier env x
    )
  | `Gene_type x -> R.Case ("Gene_type",
      map_generic_type env x
    )
  )

(* new *)
and simple_type (env : env) (x : CST.simple_type) : G.type_kind =
  match x with
  | `Void_type x ->
      let i = ("void", token_ env x) in
      let n = H2.name_of_id i in
      TyN n
  | `Bool_type x ->
      let i = ("boolean", token_ env x) in
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

and map_sobject_return (env : env) ((v1, v2) : CST.sobject_return) =
  let v1 = map_identifier env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_selected_fields env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_using_clause env x
            ))
          | None -> R.Option None)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_where_clause env x
            ))
          | None -> R.Option None)
        in
        let v5 =
          (match v5 with
          | Some x -> R.Option (Some (
              map_order_by_clause env x
            ))
          | None -> R.Option None)
        in
        let v6 =
          (match v6 with
          | Some x -> R.Option (Some (
              map_limit_clause env x
            ))
          | None -> R.Option None)
        in
        let v7 =
          (match v7 with
          | Some x -> R.Option (Some (
              map_offset_clause env x
            ))
          | None -> R.Option None)
        in
        let v8 = (* ")" *) token env v8 in
        R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_soql_query (env : env) (v1 : CST.soql_query) =
  map_soql_query_expression env v1

and map_soql_query_body (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) : CST.soql_query_body) =
  let v1 = map_select_clause env v1 in
  let v2 = map_from_clause env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_soql_using_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_where_clause env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_soql_with_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_group_by_clause env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_order_by_clause env x
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_limit_clause env x
      ))
    | None -> R.Option None)
  in
  let v9 =
    (match v9 with
    | Some x -> R.Option (Some (
        map_offset_clause env x
      ))
    | None -> R.Option None)
  in
  let v10 =
    (match v10 with
    | Some x -> R.Option (Some (
        map_for_clause env x
      ))
    | None -> R.Option None)
  in
  let v11 =
    (match v11 with
    | Some x -> R.Option (Some (
        map_update_clause env x
      ))
    | None -> R.Option None)
  in
  let v12 =
    (match v12 with
    | Some x -> R.Option (Some (
        map_all_rows_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12]

and map_soql_query_expression (env : env) (x : CST.soql_query_expression) =
  map_soql_query_body env x

and map_sosl_query (env : env) (v1 : CST.sosl_query) =
  map_query_expression_ env v1

and map_sosl_query_body (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.sosl_query_body) =
  let v1 = map_find_clause env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_in_clause env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some xs -> R.Option (Some (
        R.List (List.map (map_returning_clause env) xs)
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some xs -> R.Option (Some (
        R.List (List.map (map_sosl_with_clause env) xs)
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_limit_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_offset_clause env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_update_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_sosl_with_clause (env : env) ((v1, v2) : CST.sosl_with_clause) =
  let v1 = map_pat_with env v1 in
  let v2 = map_sosl_with_type env v2 in
  R.Tuple [v1; v2]

and map_sosl_with_type (env : env) (x : CST.sosl_with_type) =
  (match x with
  | `With_data_cat_exp x -> R.Case ("With_data_cat_exp",
      map_with_data_cat_expression env x
    )
  | `With_divi_exp (v1, v2, v3) -> R.Case ("With_divi_exp",
      let v1 = map_pat_divi env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Bound_apex_exp x -> R.Case ("Bound_apex_exp",
            map_bound_apex_expression env x
          )
        | `Str_lit tok -> R.Case ("Str_lit",
            (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `With_high x -> R.Case ("With_high",
      map_with_highlight env x
    )
  | `With_meta_exp (v1, v2, v3) -> R.Case ("With_meta_exp",
      let v1 = map_pat_meta env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `With_netw_exp (v1, v2) -> R.Case ("With_netw_exp",
      let v1 = map_pat_netw env v1 in
      let v2 = map_comparison env v2 in
      R.Tuple [v1; v2]
    )
  | `With_pric_exp (v1, v2, v3) -> R.Case ("With_pric_exp",
      let v1 = map_pat_pric env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (* pattern "'(\\\\[nNrRtTbBfFuU\"'_%\\\\]|[^\\\\'])*'" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `With_snip_exp (v1, v2) -> R.Case ("With_snip_exp",
      let v1 = map_pat_snip env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
            let v1 = (* "(" *) token env v1 in
            let v2 = map_pat_target_len env v2 in
            let v3 = (* "=" *) token env v3 in
            let v4 = (* int *) token env v4 in
            let v5 = (* ")" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `With_spell_corr_exp (v1, v2, v3) -> R.Case ("With_spell_corr_exp",
      let v1 = map_pat_spell_corr env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_boolean env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

(* OLD *)
and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_decl x -> R.Case ("Choice_decl",
      (match x with
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Exp_stmt x -> R.Case ("Exp_stmt",
          map_expression_statement env x
        )
      | `Labe_stmt x -> R.Case ("Labe_stmt",
          map_labeled_statement env x
        )
      | `If_stmt x -> R.Case ("If_stmt",
          map_if_statement env x
        )
      | `While_stmt x -> R.Case ("While_stmt",
          map_while_statement env x
        )
      | `For_stmt x -> R.Case ("For_stmt",
          map_for_statement env x
        )
      | `Enha_for_stmt x -> R.Case ("Enha_for_stmt",
          map_enhanced_for_statement env x
        )
      | `Blk x -> R.Case ("Blk",
          map_trigger_body env x
        )
      | `SEMI tok -> R.Case ("SEMI",
          (* ";" *) token env tok
        )
      | `Do_stmt x -> R.Case ("Do_stmt",
          map_do_statement env x
        )
      | `Brk_stmt x -> R.Case ("Brk_stmt",
          map_break_statement env x
        )
      | `Cont_stmt x -> R.Case ("Cont_stmt",
          map_continue_statement env x
        )
      | `Ret_stmt x -> R.Case ("Ret_stmt",
          map_return_statement env x
        )
      | `Switch_exp x -> R.Case ("Switch_exp",
          map_switch_expression env x
        )
      | `Local_var_decl x -> R.Case ("Local_var_decl",
          map_local_variable_declaration env x
        )
      | `Throw_stmt x -> R.Case ("Throw_stmt",
          map_throw_statement env x
        )
      | `Try_stmt x -> R.Case ("Try_stmt",
          map_try_statement env x
        )
      | `Run_as_stmt x -> R.Case ("Run_as_stmt",
          map_run_as_statement env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

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
          let v1 = token_ env tok (* ";" *) in
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
      let v1 = token_ env tok (* "..." *) in
      let v2 = G.sc in
      G.ExprStmt (G.Ellipsis v1 |> G.e, v2) |> G.s

and map_static_initializer (env : env) ((v1, v2) : CST.static_initializer) =
  let v1 = map_pat_static env v1 in
  let v2 = map_trigger_body env v2 in
  R.Tuple [v1; v2]

and map_subquery (env : env) ((v1, v2, v3) : CST.subquery) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_soql_query_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_superclass (env : env) ((v1, v2) : CST.superclass) =
  let v1 = map_pat_extends env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

(* OLD *)
and map_switch_block (env : env) ((v1, v2, v3) : CST.switch_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_switch_rule env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and switch_block (env : env) ((v1, v2, v3) : CST.switch_block) : case_and_body list =
  let _v1 = token env v1 (* "{" *) in
  let v2 = List_.map (switch_rule env) v2 in
  let _v3 = token env v3 (* "}" *) in
  v2

(* OLD *)
and map_switch_expression (env : env) ((v1, v2, v3, v4) : CST.switch_expression) =
  let v1 = map_pat_switch env v1 in
  let v2 = map_pat_on env v2 in
  let v3 = map_expression env v3 in
  let v4 = map_switch_block env v4 in
  R.Tuple [v1; v2; v3; v4]

(* NEW *)
and switch_expression (env : env) ((v1, v2, v3, v4) : CST.switch_expression) : G.stmt =
  let v1 = token_ env v1 (* "switch" *) in
  let v2 = token_ env v2 (* "on" *) in
  let v3 = expression env v3 in
  let v4 = switch_block env v4 in
  G.Switch (Tok.combine_toks v1 [v2], Some (G.Cond v3), v4)
  |> G.s

 (* OLD *)
and map_switch_label (env : env) ((v1, v2) : CST.switch_label) =
  let v1 = map_pat_when env v1 in
  let v2 =
    (match v2 with
    | `Opt_unan_type_id_rep_COMMA_opt_unan_type_id (v1, v2, v3) -> R.Case ("Opt_unan_type_id_rep_COMMA_opt_unan_type_id",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_unannotated_type env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_identifier env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_unannotated_type env x
                ))
              | None -> R.Option None)
            in
            let v3 = map_identifier env v3 in
            R.Tuple [v1; v2; v3]
          ) v3)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Exp_rep_COMMA_exp x -> R.Case ("Exp_rep_COMMA_exp",
        map_anon_exp_rep_COMMA_exp_0bb260c env x
      )
    | `Pat_else x -> R.Case ("Pat_else",
        map_pat_else env x
      )
    )
  in
  R.Tuple [v1; v2]

(* NEW *)
and switch_label (env : env) ((v1, v2) : CST.switch_label) : G.case list =
  let v1 = token_ env v1 in
  match v2 with
  | `Opt_unan_type_id_rep_COMMA_opt_unan_type_id (v1, v2, v3) ->
      failwith "NOT IMPLEMENTED (switch_label)"
  | `Exp_rep_COMMA_exp x ->
      let c1, cs = anon_exp_rep_COMMA_exp_0bb260c env x in
      G.Case (v1, H2.expr_to_pattern c1) ::
        List.map (fun (t, e) -> G.Case (t, H2.expr_to_pattern e)) cs
  | `Pat_else x ->
      let else_token = token_ env x in
      [G.Default (Tok.combine_toks v1 [else_token])]

(* OLD *)
and map_switch_rule (env : env) (x : CST.switch_rule) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Switch_label_blk (v1, v2) -> R.Case ("Switch_label_blk",
      let v1 = map_switch_label env v1 in
      let v2 = map_trigger_body env v2 in
      R.Tuple [v1; v2]
    )
  )

(* NEW *)
and switch_rule (env : env) (x : CST.switch_rule) : G.case_and_body =
  match x with
  | `Semg_ellips tok (* "..." *) ->
      G.CaseEllipsis (token_ env tok)
  | `Switch_label_blk (v1, v2) ->
      let v1 = switch_label env v1 in
      let v2 = trigger_body env v2 in
      G.CasesAndBody (v1, v2)

(* OLD *)
and map_throw_statement (env : env) ((v1, v2, v3) : CST.throw_statement) =
  let v1 = map_pat_throw env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and throw_statement (env : env) ((v1, v2, v3) : CST.throw_statement) : G.stmt =
      let v1 = token_ env v1 (* "throw" *) in
      let v2 = expression env v2 in
      let v3 = token_ env v3 (* ";" *) in
      G.Throw (v1, v2, v3) |> G.s

(* OLD *)
and map_trigger_body (env : env) (x : CST.trigger_body) =
  map_block env x

(* NEW *)
and trigger_body (env : env) (x : CST.trigger_body) : G.stmt =
  block env x

(* OLD *)
and map_try_statement (env : env) ((v1, v2, v3) : CST.try_statement) =
  let v1 = map_pat_try env v1 in
  let v2 = map_trigger_body env v2 in
  let v3 =
    (match v3 with
    | `Rep1_catch_clause xs -> R.Case ("Rep1_catch_clause",
        R.List (List.map (map_partial_catch env) xs)
      )
    | `Rep_catch_clause_fina_clause (v1, v2) -> R.Case ("Rep_catch_clause_fina_clause",
        let v1 = R.List (List.map (map_partial_catch env) v1) in
        let v2 = map_partial_finally env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3]

(* NEW *)
and try_statement (env : env) ((v1, v2, v3) : CST.try_statement) : G.stmt =
  let v1 = token_ env v1 (* "try" *) in
  let v2 = trigger_body env v2 in
  match v3 with
  | `Rep1_catch_clause xs ->
      let cs = List.map (partial_catch env) xs in
      G.Try (v1, v2, cs, None, None) |> G.s
  | `Rep_catch_clause_fina_clause (xs, fly) ->
      let cs = List.map (partial_catch env) xs in
      let fly = partial_finally env fly in
      G.Try (v1, v2, cs, None, Some fly) |> G.s

(* OLD *)
and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Unan_type x -> R.Case ("Unan_type",
      map_unannotated_type env x
    )
  | `Anno_type (v1, v2) -> R.Case ("Anno_type",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = map_unannotated_type env v2 in
      R.Tuple [v1; v2]
    )
  )

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

(* OLD *)
and map_type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) : G.type_arguments =
  let v1 = (* "<" *) token_ env v1 in
  let v2 =
    (match v2 with
    | Some x -> List.map (fun x -> G.TA x) (type_list env x)
    | None -> [])
  in
  let v3 = (* ">" *) token_ env v3 in
  v1, v2, v3

and map_type_bound (env : env) ((v1, v2, v3) : CST.type_bound) =
  let v1 = map_pat_extends env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "&" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

(* OLD *)
and map_type_list (env : env) ((v1, v2) : CST.type_list) =
  let v1 = map_type_ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

(* NEW *)
and type_list (env : env) ((v1, v2) : CST.type_list) : G.type_ list=
  let v1 = type_ env v1 in
  let v2 = List.map (fun (v1, v2) ->
    let _v1 = (* "," *) token_ env v1 in
    let v2 = type_ env v2 in
    v2
    ) v2
  in
  v1 :: v2

and map_type_parameter (env : env) (x : CST.type_parameter) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Rep_anno_id_opt_type_bound (v1, v2, v3) -> R.Case ("Rep_anno_id_opt_type_bound",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = map_identifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_bound env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

(* OLD *)
and map_unannotated_type (env : env) (x : CST.unannotated_type) =
  (match x with
  | `Choice_void_type x -> R.Case ("Choice_void_type",
      map_simple_type env x
    )
  | `Array_type (v1, v2) -> R.Case ("Array_type",
      let v1 = map_unannotated_type env v1 in
      let v2 = map_dimensions env v2 in
      R.Tuple [v1; v2]
    )
  )

(* NEW *)
and unannotated_type (env : env) (x : CST.unannotated_type) : G.type_kind =
  match x with
  | `Choice_void_type x -> simple_type env x
  | `Array_type (v1, v2) ->
      (* FIXME: parser never seems to produce this *)
      let v1 = unannotated_type env v1 in
      let v2 = dimensions env v2 in
      List.fold_left (fun t (t1, t2) -> G.TyArray ((t1, None, t2), G.t t)) v1 v2

(* OLD *)
and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `PLUS_exp (v1, v2) -> R.Case ("PLUS_exp",
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASH_exp (v1, v2) -> R.Case ("DASH_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `BANG_exp (v1, v2) -> R.Case ("BANG_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `TILDE_exp (v1, v2) -> R.Case ("TILDE_exp",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

(* NEW *)
and unary_expression (env : env) (x : CST.unary_expression) : G.expr =
  match x with
  | `PLUS_exp (v1, v2) ->
      let v1 = token_ env v1 (* "+" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Plus, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `DASH_exp (v1, v2) ->
      let v1 = token_ env v1 (* "-" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Minus, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `BANG_exp (v1, v2) ->
      let v1 = token_ env v1 (* "!" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op Not, v1) |> G.e, fb [ Arg v2 ]) |> G.e
  | `TILDE_exp (v1, v2) ->
      let v1 = token_ env v1 (* "~" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (Op BitNot, v1) |> G.e, fb [ Arg v2 ]) |> G.e

(* OLD *)
and map_unqualified_object_creation_expression (env : env) ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) =
  let v1 = map_pat_new env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_simple_type env v3 in
  let v4 = map_argument_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_class_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

(* NEW *)
and unqualified_object_creation_expression (env : env) ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) : G.expr =
  failwith "NOT IMPLEMENTED (unqualified_object_creation_expression)"

(* OLD *)
and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_PLUSPLUS (v1, v2) -> R.Case ("Exp_PLUSPLUS",
      let v1 = map_expression env v1 in
      let v2 = (* "++" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_DASHDASH (v1, v2) -> R.Case ("Exp_DASHDASH",
      let v1 = map_expression env v1 in
      let v2 = (* "--" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `PLUSPLUS_exp (v1, v2) -> R.Case ("PLUSPLUS_exp",
      let v1 = (* "++" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASHDASH_exp (v1, v2) -> R.Case ("DASHDASH_exp",
      let v1 = (* "--" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

(* NEW *)
and update_expression (env : env) (x : CST.update_expression) : G.expr =
  match x with
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token_ env v1 (* "++" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (IncrDecr (Incr, Prefix), v1) |> G.e, fb [ Arg v2 ])
      |> G.e
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token_ env v1 (* "--" *) in
      let v2 = expression env v2 in
      Call (IdSpecial (IncrDecr (Decr, Prefix), v1) |> G.e, fb [ Arg v2 ])
      |> G.e
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "++" *) in
      Call (IdSpecial (IncrDecr (Incr, Postfix), v2) |> G.e, fb [ Arg v1 ])
      |> G.e
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token_ env v2 (* "--" *) in
      Call (IdSpecial (IncrDecr (Decr, Postfix), v2) |> G.e, fb [ Arg v1 ])
      |> G.e

and map_value_expression (env : env) (x : CST.value_expression) =
  (match x with
  | `Func_exp x -> R.Case ("Func_exp",
      map_function_expression env x
    )
  | `Field_id x -> R.Case ("Field_id",
      map_field_identifier env x
    )
  )

(* OLD *)
and map_variable_declarator (env : env) ((v1, v2) : CST.variable_declarator) =
  let v1 = map_variable_declarator_id env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_variable_initializer env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

(* NEW *)
and variable_declarator (env : env) ((v1, v2) : CST.variable_declarator)
    : entity * variable_definition =
  let v1 = variable_declarator_id env v1 in
  let vinit =
    Option.map
      (fun (w1, w2) ->
        let _r1 = (* "=" *) token_ env w1 in
        let r2 = variable_initializer env w2 in
        r2
      )
      v2
  in
  let ent = G.basic_entity v1 in
  let vardef = { vinit; vtype = None; vtok = G.no_sc } in
  (ent, vardef)

(* OLD *)
and map_variable_declarator_list (env : env) ((v1, v2) : CST.variable_declarator_list) =
  let v1 = map_variable_declarator env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

(* NEW *)
and variable_declarator_list (env : env) ((v1, v2) : CST.variable_declarator_list)
    : (entity * variable_definition) list =
  let v1 = variable_declarator env v1 in
  let
    v2 = List.map
      (fun (w1, w2) ->
         let _r1 = token_ env w1 (* "," *) in
         let r2 = variable_declarator env w2 in
         r2
      )
      v2
  in
  v1 :: v2

(* OLD *)
and map_variable_initializer (env : env) (x : CST.variable_initializer) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Array_init x -> R.Case ("Array_init",
      map_array_initializer env x
    )
  )

(* NEW *)
and variable_initializer (env : env) (x : CST.variable_initializer) : G.expr =
  match x with
  | `Exp x -> expression env x
  | `Array_init x -> array_initializer env x

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = map_pat_where env v1 in
  let v2 = map_boolean_expression env v2 in
  R.Tuple [v1; v2]

(* OLD *)
and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 = map_pat_while env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

(* NEW *)
and while_statement (env : env) ((v1, v2, v3) : CST.while_statement) : G.stmt =
  let v1 = token_ env v1 (* "while" *) in
  let v2 = parenthesized_expression env v2 in
  let v3 = statement env v3 in
  G.While (v1, G.Cond v2, v3) |> G.s

(* OLD *)
let map_parser_output (env : env) (x : CST.parser_output) =
  (match x with
  | `Rep_stmt xs -> R.Case ("Rep_stmt",
      R.List (List.map (map_statement env) xs)
    )
  | `Cons_decl x -> R.Case ("Cons_decl",
      map_constructor_declaration env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Local_var_decl x -> R.Case ("Local_var_decl",
      map_local_variable_declaration env x
    )
  | `Class_header x -> R.Case ("Class_header",
      map_class_header env x
    )
  | `Full_meth_header (v1, v2) -> R.Case ("Full_meth_header",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_method_header env v2 in
      R.Tuple [v1; v2]
    )
  | `Part_if (v1, v2) -> R.Case ("Part_if",
      let v1 = map_pat_if env v1 in
      let v2 = map_parenthesized_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Part_try (v1, v2) -> R.Case ("Part_try",
      let v1 = map_pat_try env v1 in
      let v2 = map_trigger_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Part_catch x -> R.Case ("Part_catch",
      map_partial_catch env x
    )
  | `Part_fina x -> R.Case ("Part_fina",
      map_partial_finally env x
    )
  )

(* NEW *)
let parser_output (env : env) (x : CST.parser_output) : G.any =
  match x with
  | `Rep_stmt xs ->
      G.Ss (List.map (statement env) xs)
  | `Cons_decl x -> failwith "NOT IMPLEMENTED (c_dcl_)"
  | `Exp x ->
      G.E (expression env x)
  | `Anno x -> failwith "NOT IMPLEMENTED (anno)"
  | `Meth_decl x -> failwith "NOT IMPLEMENTED (met_decl)"
  | `Local_var_decl x -> failwith "NOT IMPLEMENTED (lvar_decl)"
  | `Class_header x -> failwith "NOT IMPLEMENTED"
  | `Full_meth_header (v1, v2) -> failwith "NOT IMPLEMENTED"
  | `Part_if (v1, v2) -> failwith "NOT IMPLEMENTED"
  | `Part_try (v1, v2) -> failwith "NOT IMPLEMENTED"
  | `Part_catch x -> failwith "NOT IMPLEMENTED"
  | `Part_fina x -> failwith "NOT IMPLEMENTED"

 (* FIXME: what does this do? *)
(*
let dump_tree root =
  map_parser_output () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Line_comment (_loc, x) -> ("line_comment", "line_comment", map_line_comment env x)
  | `Block_comment (_loc, x) -> ("block_comment", "block_comment", map_block_comment env x)

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
*)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_apex.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match parser_output env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")


let parse_expr file =
  H.wrap_parser
    (fun () -> Tree_sitter_apex.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match parser_output env cst with
      | G.E xs -> xs
      | _ -> failwith "not an expression")

let parse_stmt file =
  H.wrap_parser
    (fun () -> Tree_sitter_apex.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match parser_output env cst with
      | G.Ss xs -> xs
      | _ -> failwith "not a statement")

 (*
let parse_pattern_aux str =
  (* ugly: coupling: see grammar.js of csharp.
   * todo: will need to adjust position information in parsing errors! *)
  let expr_str = "__SEMGREP_EXPRESSION " ^ str in
  (* If possible, we always prefer to parse a pattern as an expression than
   * as a program, since an expression is also a statement, but a statement
   * is not an expression! E.g., `Foo()` as an statement will not match
   * `if (null == Foo()) ...` whereas as an expression it does. *)
  let res = Tree_sitter_apex.Parse.string expr_str in
  match res.errors with
  | [] -> res
  | _ -> Tree_sitter_apex.Parse.string str

let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_pattern_aux str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      compilation_unit env cst)
*)
