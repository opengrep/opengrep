open Fpath_.Operators
module CST = Tree_sitter_crystal.CST
module Boilerplate = Tree_sitter_crystal.Boilerplate
module G = AST_generic
module H = Parse_tree_sitter_helpers
module GH = AST_generic_helpers

[@@@warning "-26-27-32-39"]

type mode = Pattern | Target
type env = mode H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket
let fake = G.fake

let name_of_id id = G.Id (id, G.empty_id_info ())
let expr_of_id id = G.N (name_of_id id) |> G.e
let expr_of_name name = G.N name |> G.e
let todo_stmt env label tok =
  G.DirectiveStmt { d = G.OtherDirective ((label, token env tok), []); d_attrs = [] } |> G.s
let opaque_expr label =
  G.OtherExpr ((label, fake label), []) |> G.e
let str_of_constant_segment env = function
  | `Cst_segm tok -> str env tok
  | `Macro_inte_cst_segm (tok, _macro) ->
      let s, t = str env tok in
      (s ^ "{{...}}", t)
let name_of_constant env ((top, first, rest) : CST.constant) : G.name =
  let first = str_of_constant_segment env first in
  let rest =
    List_.map
      (fun (_tok, x) -> (str_of_constant_segment env x, None))
      rest
  in
  match (top, rest) with
  | None, [] -> name_of_id first
  | top, rest ->
      let middle, last =
        match rest with
        | [] -> ([], first)
        | xs -> ([ (first, None) ] @ List.rev (List.tl (List.rev xs)), fst (List.hd (List.rev xs)))
      in
      G.IdQualified
        {
          name_top = Option.map (token env) top;
          name_middle = Some (G.QDots middle);
          name_last = (last, None);
          name_info = G.empty_id_info ();
        }

let id_of_method_name env = function
  | `Id tok
  | `Semg_meta tok
  | `Id_meth_call tok
  | `Id_assign tok
  | `BQUOT tok ->
      str env tok
  | `Op_tok op ->
      let tok =
        match op with
        | `PLUS tok | `DASH tok | `STAR tok | `SLASH tok | `SLASHSLASH tok
        | `PERC tok | `AMP tok | `BAR tok | `HAT tok | `STARSTAR tok
        | `GTGT tok | `LTLT tok | `EQEQ tok | `BANGEQ tok | `LT tok
        | `LTEQ tok | `GT tok | `GTEQ tok | `LTEQGT tok | `EQEQEQ tok
        | `LBRACKRBRACK tok | `LBRACKRBRACKQMARK tok
        | `LBRACKRBRACKEQ tok | `BANG tok | `TILDE tok | `BANGTILDE tok
        | `EQTILDE tok | `AMPPLUS tok | `AMPDASH tok | `AMPSTAR tok
        | `AMPSTARSTAR tok ->
            tok
      in
      str env tok

let rec map_type env (x : CST.type_) : G.type_ =
  match x with
  | `Semg_meta tok -> G.TyN (name_of_id (str env tok)) |> G.t
  | `Choice_paren_type x -> map_type_inner env x

and map_type_inner env = function
  | `Cst x -> G.TyN (name_of_constant env x) |> G.t
  | `Gene_inst_type (cst, lp, args, rp) ->
      let name = G.TyN (name_of_constant env cst) |> G.t in
      let args =
        match args with
        | None -> []
        | Some (xs, _trailing_comma) -> (
            match xs with
            | `Choice_bare_type_rep_COMMA_choice_bare_type (x, xs) ->
                (x :: List_.map snd xs) |> List_.map (map_type_arg env)
            | `Named_type_rep_COMMA_named_type (x, xs) ->
                (x :: List_.map snd xs)
                |> List_.map (fun (_name, _colon, ty) -> G.TA (map_bare_type env ty)))
      in
      G.TyApply (name, (token env lp, args, token env rp)) |> G.t
  | `Nila_type (ty, tok) ->
      G.OtherType (("Nilable", token env tok), [ G.T (map_type env ty) ]) |> G.t
  | `Union_type (ty, rest) ->
      let tys = map_type env ty :: List_.map (fun (_bar, ty) -> map_type env ty) rest in
      G.OtherType (("Union", fake "|"), List_.map (fun ty -> G.T ty) tys) |> G.t
  | `Paren_type (_lp, ty, _rp) -> map_bare_type env ty
  | `Self tok -> G.TyN (name_of_id (str env tok)) |> G.t
  | `Unde tok -> G.OtherType (("Underscore", token env tok), []) |> G.t
  | `Tuple_type _ | `Named_tuple_type _ | `No_args_proc_type _
  | `Paren_proc_type _ | `Class_type _ | `Poin_type _ | `Typeof _
  | `Static_array_type _ ->
      G.OtherType (("Type", fake "type"), []) |> G.t

and map_nilable_constant env (base, tok) =
  G.OtherType (("Nilable", token env tok), [ G.T (map_nilable_constant_base env base) ]) |> G.t

and map_nilable_constant_base env = function
  | `Cst c -> G.TyN (name_of_constant env c) |> G.t
  | `Nila_cst c -> map_nilable_constant env c
  | `Gene_inst_type x -> map_type env (`Choice_paren_type (`Gene_inst_type x))

and map_type_arg env = function
  | `Bare_type x -> G.TA (map_bare_type env x)
  | `Nume_type _ -> G.OtherTypeArg (("NumericType", fake "type"), [])

and map_bare_type env (x : CST.bare_type) : G.type_ =
  match x with
  | `Spla_type (`Type ty) -> map_type env ty
  | `Spla_type (`Splat_type (_tok, ty))
  | `Spla_type (`Double_splat_type (_tok, ty)) ->
      map_type env ty
  | `Proc_type _ -> G.OtherType (("ProcType", fake "->"), []) |> G.t

let map_return_type env = function
  | None -> None
  | Some (_colon, ty) -> Some (map_bare_type env ty)

let rec map_string_content env = function
  | None -> []
  | Some xs ->
      List_.map
        (function
          | `Deli_str_content tok | `Str_esc_seq tok | `Igno_back tok ->
              str env tok
          | `Line_cont_expl _ -> ("", fake "\\")
          | `Interp _ -> ("", fake "#{...}"))
        xs

let map_string env ((l, contents, r) : CST.string_) =
  let l = token env l in
  let r = token env r in
  let has_interpolation =
    match contents with
    | None -> false
    | Some xs ->
        List.exists
          (function
            | `Interp _ -> true
            | _ -> false)
          xs
  in
  if not has_interpolation then
    G.L (G.String (G.string_ (l, map_string_content env contents, r))) |> G.e
  else
    let parts =
      match contents with
      | None -> []
      | Some xs ->
          xs
          |> List_.map (function
               | `Deli_str_content tok | `Str_esc_seq tok | `Igno_back tok ->
                   G.L (G.String (fb (str env tok))) |> G.e
               | `Line_cont_expl _ -> G.L (G.String (fb ("", fake "\\"))) |> G.e
               | `Interp (start, _body, end_) ->
                   let tok = token env start in
                   let e =
                     G.OtherExpr (("Interpolation", tok), []) |> G.e
                   in
                   G.Call
                     ( G.IdSpecial (G.InterpolatedElement, tok) |> G.e,
                       (tok, [ G.Arg e ], token env end_) )
                   |> G.e)
    in
    G.Call
      ( G.IdSpecial (G.ConcatString G.InterpolatedConcat, l) |> G.e,
        (l, List_.map G.arg parts, r) )
    |> G.e

let map_regex_content env = function
  | None -> ""
  | Some xs ->
      xs
      |> List_.map (function
           | `Interp _ -> "#{...}"
           | `Regex_lit_content_ xs ->
               xs
	               |> List_.map (function
	                    | `Imm_tok_prec_p1_pat_ab3dea2 tok
	                    | `Imm_tok_pat_245b2d0 tok ->
	                        fst (str env tok)
	                    | `Line_cont_expl2 _ -> "\\\n")
	               |> String.concat "")
      |> String.concat ""

let map_regex_percent_content env = function
  | None -> ""
  | Some xs ->
      xs
      |> List_.map (function
           | `Interp _ -> "#{...}"
           | `Deli_str_content tok -> fst (str env tok))
      |> String.concat ""

let pseudo_constant_tok = function
  | `X___LINE__ tok | `X___END_LINE__ tok | `X___FILE__ tok | `X___DIR__ tok ->
      tok

let map_integer env = function
  | `Tok_choice_0b_rep_pat_78bc49d_choice_choice_u8 tok ->
      Parsed_int.parse (str env tok)
  | `Choice_un_minus_imm_tok_choice_0b_rep_pat_78bc49d_choice_choice_u8
      (sign, tok) ->
      let s, t = str env tok in
      let sign_s =
        match sign with
        | `Un_minus tok -> fst (str env tok)
        | `Un_plus _ -> ""
      in
      Parsed_int.parse (sign_s ^ s, t)

let map_float env = function
  | `Tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32
      tok ->
      let s, t = str env tok in
      (float_of_string_opt s, t)
  | `Choice_un_minus_imm_tok_choice_choice_pat_b628393_rep_pat_6c14acd_pat_b591095_rep_pat_6c14acd_choice_pat_e_choice_choice_pat_dece469_rep1_pat_6c14acd_choice_choice_f32
      (sign, tok) ->
      let s, t = str env tok in
      let sign_s =
        match sign with
        | `Un_minus tok -> fst (str env tok)
        | `Un_plus _ -> ""
      in
      (float_of_string_opt (sign_s ^ s), t)

let map_operator env = function
  | `Bin_plus tok | `Bin_wrap_plus tok -> (G.Plus, token env tok)
  | `Bin_minus tok | `Bin_wrap_minus tok -> (G.Minus, token env tok)

let map_mult_operator env = function
  | `Bin_star tok | `AMPSTAR tok -> (G.Mult, token env tok)
  | `Bin_slash tok -> (G.Div, token env tok)
  | `Bin_double_slash tok -> (G.FloorDiv, token env tok)
  | `Modulo_op tok -> (G.Mod, token env tok)

let import_stmt tok module_name =
  G.DirectiveStmt
    { d = G.ImportAs (tok, module_name, None); d_attrs = [] }
  |> G.s

let rec map_lhs_name env = function
  | `Unde tok -> expr_of_id (str env tok)
  | `Id tok | `Semg_meta tok | `Inst_var tok | `Class_var tok | `Spec_var tok | `Prop tok ->
      expr_of_id (str env tok)
  | `Macro_var (tok, _) -> expr_of_id (str env tok)
  | `Assign_call x ->
      let recv, dot, id = x in
      G.DotAccess (map_expression env recv, token env dot, G.FN (name_of_id (str env id)))
      |> G.e
  | `Index_call x -> map_index_call env x
  | `Index_op x -> map_index_operator env x

and map_op_lhs_name env = function
  | `Id tok | `Semg_meta tok | `Inst_var tok | `Class_var tok ->
      expr_of_id (str env tok)
  | `Macro_var (tok, _) -> expr_of_id (str env tok)
  | `Assign_call x ->
      let recv, dot, id = x in
      G.DotAccess (map_expression env recv, token env dot, G.FN (name_of_id (str env id)))
      |> G.e
  | `Index_call x -> map_index_call env x
  | `Index_op x -> map_index_operator env x

and map_statement (env : env) (x : CST.statement) : G.stmt =
  match x with
  | `Exp e -> map_expression env e |> G.exprstmt
  | `Inline_stmt x -> map_inline_statement env x
  | `Const_assign (name, eq, rhs) ->
      let ent = G.basic_entity ~attrs:[ G.attr G.Const (token env eq) ] (name_to_id env name) in
      G.DefStmt (ent, G.VarDef { vinit = Some (map_statement_expr env rhs); vtype = None; vtok = Some (token env eq) })
      |> G.s
  | `Module_def (tok, name, body, _end) ->
      let ent = G.basic_entity (id_of_type_name env name) in
      let items = Option.value ~default:[] (Option.map (map_statements env) body) in
      G.DefStmt (ent, G.ModuleDef { mbody = G.ModuleStruct (None, items) }) |> G.s
  | `Class_def (abstract, tok, name, parent, body, _end)
  | `Struct_def (abstract, tok, name, parent, body, _end) ->
      let attrs = Option.to_list abstract |> List_.map (fun tok -> G.unhandled_keywordattr (str env tok)) in
      let ent = G.basic_entity ~attrs (id_of_type_name env name) in
      let ckind = (G.Class, token env tok) in
      let cextends =
        match parent with
        | None -> []
        | Some (_lt, ty) -> [ (map_type_name env ty, None) ]
      in
      let body = Option.value ~default:[] (Option.map (map_statements env) body) in
      let cbody = fb (List_.map (fun st -> G.F st) body) in
      let def = { G.ckind; cextends; cimplements = []; cmixins = []; cparams = fb []; cbody } in
      G.DefStmt (ent, G.ClassDef def) |> G.s
  | `Meth_def x ->
      let def = map_method_def env x in
      G.DefStmt def |> G.s
  | `Top_level_fun_def (base, body, _end) ->
      let def = map_fun_def env base body in
      G.DefStmt def |> G.s
  | `Macro_def x ->
      let def = map_macro_def env x in
      G.DefStmt def |> G.s
  | `Multi_assign x -> map_multi_assign env x
  | `Requ (tok, s) ->
      let module_name =
        match map_string env s with
        | { G.e = G.L (G.String (_, (path, _), _)); _ } ->
            G.FileName (path, token env tok)
        | _ -> G.FileName ("", token env tok)
      in
      import_stmt (token env tok) module_name
  | `Incl (tok, x) ->
      G.Call
        (expr_of_id (str env tok), fb [ G.ArgType (map_include_type_name env x) ])
      |> G.e |> G.exprstmt
  | `Alias (tok, name, _eq, ty) ->
      let ent = G.basic_entity (name_to_id env name) in
      let def = G.TypeDef { tbody = G.AliasType (map_bare_type env ty) } in
      G.DefStmt (ent, def) |> G.s
  | `Anno (tok, name, args, _) ->
      let args =
        match args with
        | None -> []
        | Some (_lp, None, _rp) -> []
        | Some (_lp, Some args, _rp) ->
            map_bracket_argument_list env args |> List_.map (fun arg -> G.Ar arg)
      in
      G.DirectiveStmt
        {
          d =
            G.OtherDirective
              ( ("Annotation", token env tok),
                G.Name (name_of_constant env name) :: args );
          d_attrs = [];
        }
      |> G.s
  | `Anno_def (tok, name, _, _) ->
      let ent = G.basic_entity (name_to_id env name) in
      G.DefStmt
        (ent, G.TypeDef { tbody = G.OtherTypeKind (("Annotation", token env tok), []) })
      |> G.s
  | `Enum_def (tok, name, _, _, _) ->
      let ent = G.basic_entity (name_to_id env name) in
      G.DefStmt (ent, G.TypeDef { tbody = G.OtherTypeKind (("Enum", token env tok), []) }) |> G.s
  | `Lib_def (tok, name, _, _) ->
      let ent =
        match name with
        | `Cst c -> G.basic_entity (name_to_id env c)
        | `Gene_type (c, _lp, _params, _rp) -> G.basic_entity (name_to_id env c)
      in
      G.DefStmt (ent, G.ModuleDef { mbody = G.ModuleStruct (None, []) }) |> G.s
  | `Abst_meth_def (tok, base, _) ->
      let ent, def = map_base_method_def env base None in
      G.DefStmt
        ({ ent with G.attrs = G.attr G.Abstract (token env tok) :: ent.G.attrs }, def)
      |> G.s
  | `Visi_modi (modifier, _) ->
      let tok =
        match modifier with
        | `Priv tok | `Prot tok -> tok
      in
      todo_stmt env "Visibility" tok
  | `Extend (tok, x) ->
      G.Call
        (expr_of_id (str env tok), fb [ G.ArgType (map_include_type_name env x) ])
      |> G.e |> G.exprstmt

and map_statement_expr env (x : CST.statement) : G.expr =
  match x with
  | `Exp e -> map_expression env e
  | _ -> G.stmt_to_expr (map_statement env x)

and map_statements env (x : CST.statements) : G.stmt list =
  match x with
  | `Stmt st -> [ map_statement env st ]
  | `Rep1_choice_stmt_term_opt_stmt (xs, last) ->
      let xs =
        List_.filter_map
          (function
            | `SEMI _ -> None
            | `Stmt_term (st, _term) -> Some (map_statement env st))
          xs
      in
      xs @ Option.to_list (Option.map (map_statement env) last)

and map_inline_statement env = function
  | `Ret (tok, args) ->
      G.Return (token env tok, Option.map (map_control_exprs_to_expr env) args, G.sc) |> G.s
  | `Next (tok, _args) -> G.Continue (token env tok, G.LNone, G.sc) |> G.s
  | `Brk (tok, _args) -> G.Break (token env tok, G.LNone, G.sc) |> G.s
  | `Modi_if (st, tok, cond) ->
      G.If (token env tok, G.Cond (map_expression env cond), map_statement env st, None) |> G.s
  | `Modi_unless (st, tok, cond) ->
      let cond = G.opcall (G.Not, token env tok) [ map_expression env cond ] in
      G.If (token env tok, G.Cond cond, map_statement env st, None) |> G.s
  | `Modi_rescue (st, tok, rhs)
  | `Modi_ensure (st, tok, rhs) ->
      G.OtherStmtWithStmt (G.OSWS_Block ("Modifier", token env tok), [ G.E (map_expression env rhs) ], map_statement env st)
      |> G.s

and map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Semg_meta tok -> expr_of_id (str env tok)
  | `Semg_ellips tok -> G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips (l, e, r) ->
      G.DeepEllipsis (token env l, map_expression env e, token env r) |> G.e
  | `Choice_macro_node x -> map_expression_inner env x

and map_expression_inner env x =
  match x with
  | `Nil tok -> G.L (G.Null (token env tok)) |> G.e
  | `True tok -> G.L (G.Bool (true, token env tok)) |> G.e
  | `False tok -> G.L (G.Bool (false, token env tok)) |> G.e
  | `Int x -> G.L (G.Int (map_integer env x)) |> G.e
  | `Float x -> G.L (G.Float (map_float env x)) |> G.e
  | `Char (_l, x, _r) ->
      let x =
        match x with
        | `Imm_tok_prec_p1_pat_e77da7f tok
        | `Char_esc_seq tok ->
            str env tok
      in
      G.L (G.Char x) |> G.e
  | `Str x -> map_string env x
  | `Chai_str (x, xs) ->
      let strings = x :: xs |> List_.map (map_string env) in
      G.Call (G.IdSpecial (G.ConcatString G.SequenceConcat, fake "") |> G.e, fb (List_.map G.arg strings)) |> G.e
  | `Op_symb (tok, op) ->
      G.L (G.Atom (token env tok, str env op)) |> G.e
  | `Unqu_symb (tok, value) ->
      G.L (G.Atom (token env tok, str env value)) |> G.e
  | `Quoted_symb (l, contents, r) ->
      let value =
        contents
        |> List_.map (function
             | `Imm_tok_prec_p1_pat_f2207b3 tok
             | `Str_esc_seq tok
             | `Igno_back tok ->
                 fst (str env tok))
        |> String.concat ""
      in
      G.L (G.Atom (token env l, (value, token env r))) |> G.e
  | `Array x -> map_array env x
  | `Hash x -> map_hash env x
  | `Named_tuple x -> map_named_tuple env x
  | `Tuple (l, e, rest, _comma, r) ->
      G.Container (G.Tuple, (token env l, map_expression env e :: List_.map (fun (_c, e) -> map_expression env e) rest, token env r))
      |> G.e
  | `Self tok | `Id tok | `Inst_var tok | `Class_var tok | `Spec_var tok
  | `Global_match_data_index tok ->
      expr_of_id (str env tok)
  | `Cst c -> expr_of_name (name_of_constant env c)
  | `Gene_inst_type (c, _lp, _args, _rp) -> expr_of_name (name_of_constant env c)
  | `Nila_cst c -> G.OtherExpr (("NilableConstant", fake "?"), [ G.T (map_nilable_constant env c) ]) |> G.e
  | `Pseudo_cst tok -> expr_of_id (str env (pseudo_constant_tok tok))
  | `Range (lhs, op, _end_marker, rhs) ->
      let op, op_tok =
        match op with
        | `DOTDOT tok -> (G.RangeInclusive, token env tok)
        | `DOTDOTDOT tok -> (G.Range, token env tok)
      in
      let rhs = Option.value ~default:(G.L (G.Unit op_tok) |> G.e) (Option.map (map_expression env) rhs) in
      G.opcall (op, op_tok) [ map_expression env lhs; rhs ]
  | `Begi_range (op, _end_marker, rhs) ->
      let op, tok =
        match op with
        | `DOTDOT tok -> (G.RangeInclusive, token env tok)
        | `DOTDOTDOT tok -> (G.Range, token env tok)
      in
      let lhs = G.L (G.Unit tok) |> G.e in
      let rhs = Option.value ~default:(G.L (G.Unit tok) |> G.e) (Option.map (map_expression env) rhs) in
      G.opcall (op, tok) [ lhs; rhs ]
  | `Paren_expres (_lp, stmts, _rp) -> G.stmt_to_expr (G.Block (fb (map_statements env stmts)) |> G.s)
  | `Call c -> map_call env c
  | `Assign (lhs, eq, rhs) ->
      G.Assign (map_lhs_name env lhs, token env eq, map_assign_rhs env rhs) |> G.e
  | `Op_assign (lhs, op, rhs) ->
      G.AssignOp (map_op_lhs_name env lhs, map_assign_operator env op, map_expression env rhs) |> G.e
  | `Addi_op (lhs, op, rhs) -> G.opcall (map_operator env op) [ map_expression env lhs; map_expression env rhs ]
  | `Mult_op (lhs, op, rhs) -> G.opcall (map_mult_operator env op) [ map_expression env lhs; map_expression env rhs ]
  | `Expo_op (lhs, op, rhs) ->
      let tok =
        match op with
        | `Bin_double_star tok | `AMPSTARSTAR tok -> token env tok
      in
      G.opcall (G.Pow, tok) [ map_expression env lhs; map_expression env rhs ]
  | `Shift_op (lhs, op, rhs) ->
      let op =
        match op with
        | `LTLT tok -> (G.LSL, token env tok)
        | `GTGT tok -> (G.ASR, token env tok)
      in
      G.opcall op [ map_expression env lhs; map_expression env rhs ]
  | `Bin_and_op (lhs, tok, rhs) ->
      G.opcall (G.BitAnd, token env tok) [ map_expression env lhs; map_expression env rhs ]
  | `Bin_or_op (lhs, op, rhs) ->
      let op =
        match op with
        | `BAR tok -> (G.BitOr, token env tok)
        | `HAT tok -> (G.BitXor, token env tok)
      in
      G.opcall op [ map_expression env lhs; map_expression env rhs ]
  | `Equa_op (lhs, op, rhs) ->
      G.opcall (map_equality_operator env op) [ map_expression env lhs; map_expression env rhs ]
  | `Comp_op_b5e2455 (lhs, op, rhs) ->
      G.opcall (map_comparison_operator env op) [ map_expression env lhs; map_expression env rhs ]
  | `And (lhs, tok, rhs) ->
      G.opcall (G.And, token env tok) [ map_expression env lhs; map_expression env rhs ]
  | `Or (lhs, tok, rhs) ->
      G.opcall (G.Or, token env tok) [ map_expression env lhs; map_expression env rhs ]
  | `Not (tok, e) | `Comp_op_6e1517d (tok, e) ->
      G.opcall (G.Not, token env tok) [ map_expression env e ]
  | `Un_addi_op (op, e) ->
      let op =
        match op with
        | `Un_plus tok | `Un_wrap_plus tok -> (G.Plus, token env tok)
        | `Un_minus tok | `Un_wrap_minus tok -> (G.Minus, token env tok)
      in
      G.opcall op [ map_expression env e ]
  | `Index_op x -> map_index_operator env x
  | `Index_call x -> map_index_call env x
  | `Cond (cond, _q, then_, _colon, else_) ->
      G.Conditional (map_expression env cond, map_expression env then_, map_expression env else_) |> G.e
  | `If x -> G.stmt_to_expr (map_if env x)
  | `Unless x -> G.stmt_to_expr (map_unless env x)
  | `Case x -> G.stmt_to_expr (map_case env x)
  | `Exha_case x -> G.stmt_to_expr (map_exhaustive_case env x)
  | `While (tok, cond, _term, body, _end) ->
      let body = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s in
      G.stmt_to_expr (G.While (token env tok, G.Cond (map_expression env cond), body) |> G.s)
  | `Until (tok, cond, _term, body, _end) ->
      let tok = token env tok in
      let cond = G.opcall (G.Not, tok) [ map_expression env cond ] in
      let body = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s in
      G.stmt_to_expr (G.While (tok, G.Cond cond, body) |> G.s)
  | `Yield (_with, tok, args) ->
      G.Yield (token env tok, Option.map (map_control_exprs_to_expr env) args, false) |> G.e
  | `Macro_node x ->
      let tok = macro_node_tok x in
      G.OtherExpr (("Macro", token env tok), [ G.Tk (token env tok) ]) |> G.e
  | `Macro_var (tok, _) -> expr_of_id (str env tok)
  | `Type_decl x -> map_type_declaration env x
  | `Pseudo_call x -> map_pseudo_call env x
  | `Regex (l, contents, r, mods) ->
      G.L (G.Regexp ((token env l, (map_regex_content env contents, token env r), token env r), Option.map (str env) mods)) |> G.e
  | `Regex_perc_lit (l, contents, r, mods) ->
      G.L (G.Regexp ((token env l, (map_regex_percent_content env contents, token env r), token env r), Option.map (str env) mods)) |> G.e
  | `Begin (tok, _term, body, _rescue, end_) ->
      G.stmt_to_expr (G.Block (token env tok, Option.value ~default:[] (Option.map (map_statements env) body), token env end_) |> G.s)
  | `Empty_parens (l, r) ->
      G.Container (G.Tuple, (token env l, [], token env r)) |> G.e
  | `Str_perc_lit _ -> opaque_expr "PercentString"
  | `Str_array_perc_lit _ -> opaque_expr "PercentStringArray"
  | `Symb_array_perc_lit _ -> opaque_expr "PercentSymbolArray"
  | `Here_start _ -> opaque_expr "HeredocStart"
  | `Proc _ -> opaque_expr "ProcLiteral"
  | `Meth_proc _ -> opaque_expr "MethodProc"
  | `Cmd _ -> opaque_expr "Command"
  | `Cmd_perc_lit _ -> opaque_expr "PercentCommand"
  | `Select _ -> opaque_expr "Select"
  | `Array_like _ -> opaque_expr "ArrayLike"
  | `Hash_like _ -> opaque_expr "HashLike"
  | `Unin_assign _ -> opaque_expr "UninitializedAssign"
  | `Asm _ -> opaque_expr "Asm"
  | `Typeof _ -> opaque_expr "Typeof"
  | `Poin _ -> opaque_expr "PointerOf"
  | `Sizeof _ -> opaque_expr "Sizeof"
  | `Inst_sizeof _ -> opaque_expr "InstanceSizeof"
  | `Alig _ -> opaque_expr "Alignof"
  | `Inst_alig _ -> opaque_expr "InstanceAlignof"
  | `Offs _ -> opaque_expr "Offsetof"

and map_assign_rhs env = function
  | `Exp e -> map_expression env e
  | `Prop_id tok -> expr_of_id (str env tok)

and map_array env = function
  | `LBRACK_exp_rep_COMMA_exp_opt_COMMA_RBRACK_opt_of_bare_type (l, e, rest, _comma, r, _ty) ->
      let xs = map_expression env e :: List_.map (fun (_comma, e) -> map_expression env e) rest in
      G.Container (G.Array, (token env l, xs, token env r)) |> G.e
  | `LBRACK_RBRACK_of_bare_type (l, r, _of_, _ty) ->
      G.Container (G.Array, (token env l, [], token env r)) |> G.e

and map_hash env = function
  | `Start_of_hash_or_tuple_hash_entry_rep_COMMA_hash_entry_opt_COMMA_RCURL_opt_of_bare_type_EQGT_bare_type
      (l, entry, rest, _comma, r, _ty) ->
      let entries = entry :: List_.map snd rest |> List_.map (map_hash_entry env) in
      G.Container (G.Dict, (token env l, entries, token env r)) |> G.e
  | `Start_of_hash_or_tuple_RCURL_of_bare_type_EQGT_bare_type (l, r, _of_, _k, _arrow, _v) ->
      G.Container (G.Dict, (token env l, [], token env r)) |> G.e

and map_hash_entry env (k, arrow, v) =
  G.keyval (map_expression env k) (token env arrow) (map_expression env v)

and map_named_tuple env (l, entry, rest, _comma, r) =
  let entries = entry :: List_.map snd rest |> List_.map (map_named_tuple_entry env) in
  G.Container (G.Dict, (token env l, entries, token env r)) |> G.e

and map_named_tuple_entry env = function
  | `Choice_id_imm_tok_colon_choice_exp (name, colon, value) ->
      let key =
        match name with
        | `Id tok | `With tok | `Cst_segm tok | `Id_meth_call tok -> expr_of_id (str env tok)
        | `Str s -> map_string env s
        | `Str_perc_lit _ -> G.OtherExpr (("PercentStringKey", fake "string"), []) |> G.e
      in
      G.keyval key (token env colon) (map_named_expr_value env value)
  | `Kw_named_arg_name_choice_exp (name, value) ->
      G.keyval
        (expr_of_id (id_of_keyword_named_argument env name))
        (token env name)
        (map_named_expr_value env value)

and map_call env (x : CST.call) : G.expr =
  let callee, args = call_parts env x in
  G.Call (callee, fb args) |> G.e

and map_call_id env = function
  | `Id tok | `Semg_meta tok -> expr_of_id (str env tok)

and tok_of_property_bare_declaration = function
  | `Id tok | `Id_meth_call tok | `Else tok | `Then tok | `Ensure tok | `Rescue tok ->
      tok

and map_property_argument env = function
  | `Type_decl (name, colon, _space, ty, value) ->
      G.Arg
        (G.OtherExpr
           ( ("PropertyArgument", token env colon),
             [
               G.I (id_of_property_type_declaration_name env name);
               G.T (map_bare_type env ty);
             ]
             @
             match value with
             | None -> []
             | Some (_eq, e) -> [ G.E (map_expression env e) ] )
        |> G.e)
  | `Kw_type_decl (name, colon, _space, ty, value) ->
      G.Arg
        (G.OtherExpr
           ( ("PropertyArgument", token env colon),
             [
               G.I (id_of_property_keyword_name env name);
               G.T (map_bare_type env ty);
             ]
             @
             match value with
             | None -> []
             | Some (_eq, e) -> [ G.E (map_expression env e) ] )
        |> G.e)
  | `Prop_assign_decl (name, eq, _value) ->
      G.ArgKwd
        ( id_of_property_bare_declaration env name,
          G.OtherExpr (("PropertyInitializer", token env eq), []) |> G.e )
  | `Prop_bare_decl name ->
      G.Arg (expr_of_id (str env (tok_of_property_bare_declaration name)))
  | `Unqu_symb sym ->
      G.Arg (map_expression_inner env (`Unqu_symb sym))
  | `Quoted_symb sym ->
      G.Arg (map_expression_inner env (`Quoted_symb sym))
  | `Op_symb sym ->
      G.Arg (map_expression_inner env (`Op_symb sym))

and call_parts env = function
  | `Choice_id_choice_arg_list_with_parens (id, args) ->
      (map_call_id env id, map_control_exprs env args)
  | `Macro_exp_arg_list_with_parens ((l, _body, _r), args) ->
      (G.OtherExpr (("MacroCallee", token env l), []) |> G.e, map_argument_list_with_parens env args)
  | `Choice_prop_prop_arg_list (prop, arg) ->
      let tok =
        match prop with
        | `Prop_1a8db4c tok | `Prop_b128b0f tok -> tok
      in
      (expr_of_id (str env tok), [ map_property_argument env arg ])
  | `Choice_prop_prop_arg_list_brace_blk (prop, arg, block) ->
      let tok =
        match prop with
        | `Prop_1a8db4c tok | `Prop_b128b0f tok -> tok
      in
      (expr_of_id (str env tok), [ map_property_argument env arg; map_block_arg env (`Brace block) ])
  | `Choice_prop_arg_list_with_parens (prop, args) ->
      let tok =
        match prop with
        | `Prop_1a8db4c tok | `Prop_b128b0f tok -> tok
      in
      (expr_of_id (str env tok), map_argument_list_with_parens env args)
  | `Choice_prop_arg_list_with_parens_brace_blk (prop, args, block) ->
      let tok =
        match prop with
        | `Prop_1a8db4c tok | `Prop_b128b0f tok -> tok
      in
      (expr_of_id (str env tok), map_argument_list_with_parens env args @ [ map_block_arg env (`Brace block) ])
  | `Choice_id_opt_choice_arg_list_with_parens_brace_blk (id, args, block) ->
      (map_call_id env id, map_opt_control_exprs env args @ [ map_block_arg env (`Brace block) ])
  | `Choice_id_opt_choice_arg_list_with_parens_do_end_blk (id, args, block) ->
      (map_call_id env id, map_opt_control_exprs env args @ [ map_block_arg env (`Do block) ])
  | `Choice_id_choice_arg_list_with_parens_and_blk (id, args) ->
      (map_call_id env id, map_args_with_block env args)
  | `Choice_dot_call_opt_choice_arg_list_with_parens (callee, args) ->
      (map_dot_call env callee, map_opt_control_exprs env args)
  | `Choice_dot_call_opt_choice_arg_list_with_parens_brace_blk (callee, args, block) ->
      (map_dot_call env callee, map_opt_control_exprs env args @ [ map_block_arg env (`Brace block) ])
  | `Choice_dot_call_opt_choice_arg_list_with_parens_do_end_blk (callee, args, block) ->
      (map_dot_call env callee, map_opt_control_exprs env args @ [ map_block_arg env (`Do block) ])
  | `Choice_dot_call_choice_arg_list_with_parens_and_blk (callee, args) ->
      (map_dot_call env callee, map_args_with_block env args)

and map_dot_call env = function
  | `Dot_call (recv, dot, name) ->
      let field =
        match name with
        | `Id tok | `Semg_meta tok | `Id_meth_call tok | `Inst_var tok -> G.FN (name_of_id (str env tok))
        | `Cst c -> G.FN (name_of_constant env c)
        | `Op_tok op -> G.FN (name_of_id (id_of_method_name env (`Op_tok op)))
      in
      G.DotAccess (map_expression env recv, token env dot, field) |> G.e
  | `Id_meth_call tok -> expr_of_id (str env tok)
  | `Global_meth (_coloncolon, id) ->
      let id =
        match id with
        | `Id tok | `Id_meth_call tok | `Id_assign tok -> str env tok
      in
      expr_of_id id

and map_args_with_block env = function
  | `Arg_list_with_parens_and_blk args -> map_argument_list_with_parens_and_block env args
  | `Arg_list_no_parens_with_blk args -> map_argument_list_no_parens_with_block env args

and map_control_exprs env = function
  | `Arg_list_with_parens args -> map_argument_list_with_parens env args
  | `Arg_list_no_parens args -> map_argument_list_no_parens env args

and map_opt_control_exprs env = function
  | None -> []
  | Some x -> map_control_exprs env x

and map_control_exprs_to_expr env args =
  match map_control_exprs env args with
  | [ G.Arg e ] -> e
  | args -> G.OtherExpr (("Arguments", fake "args"), List_.map (fun a -> G.Ar a) args) |> G.e

and map_argument_list_with_parens env (_lp, args, _rp) =
  match args with
  | None -> []
  | Some (first, rest, _comma) -> (first :: List_.map snd rest) |> List_.map (map_argument_with_property env)

and map_argument_list_no_parens env (_start, first, rest) =
  (first :: List_.map snd rest) |> List_.map (map_argument env)

and map_argument_list_with_parens_and_block env (_lp, args, block, _rp) =
  let args =
    match args with
    | None -> []
    | Some (first, rest, _comma) -> (first :: List_.map snd rest) |> List_.map (map_argument_with_property env)
  in
  args @ [ map_block_argument env block ]

and map_argument_list_no_parens_with_block env (_start, args, block) =
  let args =
    match args with
    | None -> []
    | Some (first, rest, _comma) -> (first :: List_.map snd rest) |> List_.map (map_argument env)
  in
  args @ [ map_block_argument env block ]

and map_bracket_argument_list env (first, rest, _comma) =
  (first :: List_.map snd rest) |> List_.map (map_argument env)

and map_argument env = function
  | `Exp e -> G.Arg (map_expression env e)
  | `Splat (tok, e) | `Double_splat (tok, e) ->
      G.Arg (G.Call (G.IdSpecial (G.Spread, token env tok) |> G.e, fb [ G.Arg (map_expression env e) ]) |> G.e)
  | `Named_expr named_expr ->
      let id, e = map_named_expr_argument env named_expr in
      G.ArgKwd (id, e)
  | `Out _ -> G.OtherArg (("Out", fake "out"), [])

and map_argument_with_property env = function
  | `Prop_id tok -> G.Arg (expr_of_id (str env tok))
  | `Exp e -> G.Arg (map_expression env e)
  | `Splat (tok, e) | `Double_splat (tok, e) ->
      G.Arg (G.Call (G.IdSpecial (G.Spread, token env tok) |> G.e, fb [ G.Arg (map_expression env e) ]) |> G.e)
  | `Named_expr named_expr ->
      let id, e = map_named_expr_argument env named_expr in
      G.ArgKwd (id, e)
  | `Out _ -> G.OtherArg (("Out", fake "out"), [])

and map_block_argument env (_amp, value) =
  match value with
  | `Exp e -> G.Arg (map_expression env e)
  | `Impl_obj_call _ -> G.OtherArg (("ImplicitObjectCall", fake "block"), [])

and map_block_arg env = function
  | `Brace (l, params, body, r) ->
      let body = Option.value ~default:[] (Option.map (map_statements env) body) in
      let fparams = map_block_params env params in
      G.Arg (G.Lambda { fkind = (G.LambdaKind, token env l); fparams; frettype = None; fbody = G.FBStmt (G.Block (token env l, body, token env r) |> G.s) } |> G.e)
  | `Do (l, params, body, _rescue, r) ->
      let body = Option.value ~default:[] (Option.map (map_statements env) body) in
      let fparams = map_block_params env params in
      G.Arg (G.Lambda { fkind = (G.LambdaKind, token env l); fparams; frettype = None; fbody = G.FBStmt (G.Block (token env l, body, token env r) |> G.s) } |> G.e)

and map_block_params env = function
  | None -> fb []
  | Some (l, params, r) -> (token env l, map_block_param_list env params, token env r)

and map_block_param_list env (first, rest, _comma) =
  first :: List_.map snd rest |> List_.map (map_block_body_param_choice env)

and map_block_body_param_choice env = function
  | `Blk_body_param p -> map_block_body_param env p
  | `Blk_body_splat_param (tok, p) ->
      G.ParamRest (token env tok, param_classic_of_block_body_param env p)
  | `Semg_ellips tok -> G.ParamEllipsis (token env tok)
  | `Blk_body_nested_param (l, p, rest, _comma, _r) ->
      let pats =
        p :: List_.map snd rest
        |> List_.map (fun p -> param_pattern_of_block_body_param_choice env p)
      in
      G.ParamPattern (G.PatTuple (fb pats), G.implicit_param_classic (token env l))

and map_block_body_param env p =
  G.Param (param_classic_of_block_body_param env p)

and param_classic_of_block_body_param env = function
  | `Semg_meta tok -> G.param_of_id (str env tok)
  | `Id tok -> G.param_of_id (str env tok)
  | `Unde tok -> G.param_of_id ("_", token env tok)

and param_pattern_of_block_body_param_choice env = function
  | `Blk_body_param (`Semg_meta tok) -> G.PatId (str env tok, G.empty_id_info ())
  | `Blk_body_param (`Id tok) -> G.PatId (str env tok, G.empty_id_info ())
  | `Blk_body_param (`Unde tok) -> G.PatWildcard (token env tok)
  | `Blk_body_splat_param (tok, p) ->
      G.OtherPat (("Splat", token env tok), [ G.P (param_pattern_of_block_body_param_choice env (`Blk_body_param p)) ])
  | `Blk_body_nested_param (l, p, rest, _comma, r) ->
      let pats =
        p :: List_.map snd rest
        |> List_.map (fun p -> param_pattern_of_block_body_param_choice env p)
      in
      G.PatTuple (token env l, pats, token env r)

and map_index_operator env (recv, l, args, r) =
  let args =
    match args with
    | None -> []
    | Some args -> map_bracket_argument_list env args |> List_.filter_map (function G.Arg e -> Some e | _ -> None)
  in
  let arg =
    match args with
    | [ x ] -> x
    | xs -> G.Container (G.Tuple, fb xs) |> G.e
  in
  let r =
    match r with
    | `RBRACK tok | `RBRACKQMARK tok -> token env tok
  in
  G.ArrayAccess (map_expression env recv, (token env l, arg, r)) |> G.e

and map_index_call env (recv, _dot, l, args, r) =
  let r =
    match r with
    | `RBRACK tok | `RBRACKQMARK tok -> token env tok
  in
  let args = map_bracket_argument_list env args |> List_.filter_map (function G.Arg e -> Some e | _ -> None) in
  let arg =
    match args with
    | [ x ] -> x
    | xs -> G.Container (G.Tuple, fb xs) |> G.e
  in
  G.ArrayAccess (map_expression env recv, (token env l, arg, r)) |> G.e

and map_assign_operator env = function
  | `PLUSEQ tok | `AMPPLUSEQ tok -> (G.Plus, token env tok)
  | `DASHEQ tok | `AMPDASHEQ tok -> (G.Minus, token env tok)
  | `STAREQ tok | `AMPSTAREQ tok -> (G.Mult, token env tok)
  | `SLASHEQ tok -> (G.Div, token env tok)
  | `SLASHSLASHEQ tok -> (G.FloorDiv, token env tok)
  | `PERCEQ tok -> (G.Mod, token env tok)
  | `BAREQ tok -> (G.BitOr, token env tok)
  | `AMPEQ tok -> (G.BitAnd, token env tok)
  | `HATEQ tok -> (G.BitXor, token env tok)
  | `STARSTAREQ tok -> (G.Pow, token env tok)
  | `LTLTEQ tok -> (G.LSL, token env tok)
  | `GTGTEQ tok -> (G.ASR, token env tok)
  | `BARBAREQ tok -> (G.Or, token env tok)
  | `AMPAMPEQ tok -> (G.And, token env tok)

and map_equality_operator env = function
  | `EQEQ tok -> (G.Eq, token env tok)
  | `BANGEQ tok -> (G.NotEq, token env tok)
  | `EQTILDE tok -> (G.RegexpMatch, token env tok)
  | `BANGTILDE tok -> (G.NotMatch, token env tok)
  | `EQEQEQ tok -> (G.PhysEq, token env tok)

and map_comparison_operator env = function
  | `LT tok -> (G.Lt, token env tok)
  | `LTEQ tok -> (G.LtE, token env tok)
  | `GT tok -> (G.Gt, token env tok)
  | `GTEQ tok -> (G.GtE, token env tok)
  | `LTEQGT tok -> (G.Cmp, token env tok)

and map_if env (tok, cond, _term, then_, else_, _end) =
  let then_ = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) then_))) |> G.s in
  let else_ = Option.map (map_else env) else_ in
  G.If (token env tok, G.Cond (map_expression env cond), then_, else_) |> G.s

and map_unless env (tok, cond, _term, then_, else_, _end) =
  let tok = token env tok in
  let cond = G.opcall (G.Not, tok) [ map_expression env cond ] in
  let then_ = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) then_))) |> G.s in
  let else_ = Option.map (fun (_, body) -> G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s) else_ in
  G.If (tok, G.Cond cond, then_, else_) |> G.s

and map_case env (tok, cond, whens, else_, _end) =
  let cond = Option.map (fun e -> G.Cond (map_expression env e)) cond in
  let cases = List_.map (map_when env) whens @ Option.to_list (Option.map (map_case_else env) else_) in
  G.Switch (token env tok, cond, cases) |> G.s

and map_exhaustive_case env (tok, cond, ins, _end) =
  let cond = Some (G.Cond (map_expression env cond)) in
  let cases = List_.map (map_in_case env) ins in
  G.Switch (token env tok, cond, cases) |> G.s

and map_when env (tok, first, rest, _then, body) =
  let cases =
    first :: List_.map snd rest
    |> List_.map (fun x -> G.Case (token env tok, GH.expr_to_pattern (map_case_expr env x)))
  in
  let body = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s in
  G.CasesAndBody (cases, body)

and map_case_expr env = function
  | `Exp e -> map_expression env e
  | `Impl_obj_call _ -> G.OtherExpr (("ImplicitObjectCall", fake "call"), []) |> G.e
  | `Impl_obj_tuple (l, _prefix, _mid, _rest, _comma, r) ->
      G.Container (G.Tuple, (token env l, [], token env r)) |> G.e

and map_case_else env (tok, body) =
  let body = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s in
  G.CasesAndBody ([ G.Default (token env tok) ], body)

and map_in_case env (tok, first, rest, _then, body) =
  let cases =
    first :: List_.map snd rest
    |> List_.map (fun x -> G.Case (token env tok, GH.expr_to_pattern (map_case_expr env x)))
  in
  let body = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s in
  G.CasesAndBody (cases, body)

and map_else env = function
  | `Else (_tok, body) -> G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s
  | `Elsif (tok, cond, term, then_, nested) -> map_if env (tok, cond, term, then_, nested, tok)

and map_multi_assign env x =
  let lhs, eq, rhs = map_multi_assign_parts env x in
  G.Assign (lhs, token env eq, rhs) |> G.e |> G.exprstmt

and map_method_def env (base, _term, body, _rescue, _end) =
  let def = map_base_method_def env base body in
  def

and map_base_method_def env (tok, recv, name, params, ret, _forall) body =
  let id =
    match recv with
    | None -> id_of_method_name env name
    | Some (recv, _dot) ->
        let id, tok = id_of_method_name env name in
        (id_of_method_receiver env recv ^ "." ^ id, tok)
  in
  let ent = G.basic_entity id in
  let fparams =
    match params with
    | None -> fb []
    | Some (lp, params, rp) -> (token env lp, Option.value ~default:[] (Option.map (map_param_list env) params), token env rp)
  in
  let body = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s in
  (ent, G.FuncDef { fkind = (G.Method, token env tok); fparams; frettype = map_return_type env ret; fbody = G.FBStmt body })

and map_fun_def env (tok, name, _extern, params, ret) body =
  let id =
    match name with
    | `Id tok | `Id_meth_call tok -> str env tok
  in
  let ent = G.basic_entity id in
  let fparams =
    match params with
    | None -> fb []
    | Some (lp, params, rp) -> (token env lp, Option.value ~default:[] (Option.map (map_fun_param_list env) params), token env rp)
  in
  let body = G.Block (fb (Option.value ~default:[] (Option.map (map_statements env) body))) |> G.s in
  (ent, G.FuncDef { fkind = (G.Function, token env tok); fparams; frettype = map_return_type env ret; fbody = G.FBStmt body })

and map_param_list env = function
  | `Choice_param_rep_COMMA_choice_param_opt_COMMA_opt_blk_param (first, rest, block) ->
      map_param_choice env first
      :: List_.map (fun (_comma, p) -> map_param_choice env p) rest
      @ Option.value ~default:[] (Option.map (fun (_comma, p) -> Option.to_list (Option.map (fun p -> map_block_param env p) p)) block)
  | `Blk_param p -> [ map_block_param env p ]

and map_param_choice env = function
  | `Param p -> map_param env p
  | `Splat_param (_attrs, tok, id, ty) ->
      let id = Option.value ~default:("*", token env tok) (Option.map (map_param_id env) id) in
      G.ParamRest (token env tok, G.param_of_id ?ptype:(Option.map (fun (_c, ty) -> map_bare_type env ty) ty) id)
  | `Double_splat_param (_attrs, tok, id, ty) ->
      let id = map_param_id env id in
      G.ParamRest (token env tok, G.param_of_id ?ptype:(Option.map (fun (_c, ty) -> map_bare_type env ty) ty) id)
  | `Semg_ellips tok -> G.ParamEllipsis (token env tok)

and map_block_param env (_attrs, tok, id, ty) =
  let id = Option.value ~default:("&", token env tok) (Option.map (map_param_id env) id) in
  G.ParamRest (token env tok, G.param_of_id ?ptype:(Option.map (fun (_c, ty) -> map_bare_type env ty) ty) id)

and map_param env (_attrs, _external_name, id, ty, default) =
  let id = map_regular_param_id env id in
  let ptype = Option.map (fun (_colon, ty) -> map_bare_type env ty) ty in
  let pdefault = Option.map (fun (_eq, e) -> map_expression env e) default in
  G.Param (G.param_of_id ?ptype ?pdefault id)

and map_fun_param_list env (first, rest, _dots) =
  map_fun_param env first :: List_.map (fun (_comma, p) -> map_fun_param env p) rest

and map_fun_param env (id, _colon, ty) =
  G.Param (G.param_of_id ~ptype:(map_bare_type env ty) (map_fun_param_id env id))

and map_param_id env = function
  | `Id tok | `Semg_meta tok | `Inst_var tok | `Class_var tok -> str env tok
  | `Macro_var (tok, _) -> str env tok

and map_regular_param_id env = function
  | `Id tok | `Semg_meta tok | `Id_meth_call tok | `Inst_var tok | `Class_var tok -> str env tok
  | `Macro_var (tok, _) -> str env tok

and map_fun_param_id env = function
  | `Id tok | `Id_meth_call tok -> str env tok
  | `Cst c -> name_to_id env c

and map_macro_def env ((tok, name, _params), _start, _content, _end) =
  let id = id_of_method_name env name in
  let ent = G.basic_entity id in
  (ent, G.MacroDef { macroparams = []; macrobody = [ G.Tk (token env tok) ] })

and map_type_declaration env (lhs, colon, _space, ty, init) =
  let lhs = GH.expr_to_pattern (map_typed_lhs env lhs) in
  let pat = G.PatTyped (lhs, map_bare_type env ty) in
  match init with
  | Some (_eq, e) -> G.LetPattern (pat, map_expression env e) |> G.e
  | None -> G.OtherExpr (("TypeDeclaration", token env colon), [ G.P pat ]) |> G.e

and map_typed_lhs env = function
  | `Id tok | `Semg_meta tok | `Id_meth_call tok | `Inst_var tok | `Class_var tok -> expr_of_id (str env tok)
  | `Macro_var (tok, _) -> expr_of_id (str env tok)

and map_multi_lhs_name env = function
  | `Unde tok -> expr_of_id (str env tok)
  | `Id tok | `Semg_meta tok | `Inst_var tok | `Class_var tok ->
      expr_of_id (str env tok)
  | `Macro_var (tok, _) -> expr_of_id (str env tok)
  | `Assign_call x ->
      let recv, dot, id = x in
      G.DotAccess (map_expression env recv, token env dot, G.FN (name_of_id (str env id)))
      |> G.e
  | `Index_call x -> map_index_call env x
  | `Index_op x -> map_index_operator env x

and map_multi_assign_lhs env = function
  | `Choice_unde lhs -> map_multi_lhs_name env lhs
  | `Lhs_splat (tok, lhs) ->
      G.Call
        (G.IdSpecial (G.Spread, token env tok) |> G.e, fb [ G.Arg (map_multi_lhs_name env lhs) ])
      |> G.e

and map_multi_assign_lhs_tuple env leading last =
  let xs = List_.map fst leading @ [ last ] |> List_.map (map_multi_assign_lhs env) in
  G.Container (G.Tuple, fb xs) |> G.e

and map_multi_assign_rhs_tuple env leading last =
  let xs = List_.map fst leading @ [ last ] |> List_.map (map_expression env) in
  G.Container (G.Tuple, fb xs) |> G.e

and map_multi_assign_parts env = function
  | `Lhs_splat_EQ_exp (lhs, eq, rhs) ->
      (map_multi_assign_lhs env (`Lhs_splat lhs), eq, map_expression env rhs)
  | `Rep1_choice_choice_unde_COMMA_choice_choice_unde_EQ_exp (lhses, lhs, eq, rhs) ->
      (map_multi_assign_lhs_tuple env lhses lhs, eq, map_expression env rhs)
  | `Lhs_splat_EQ_rep1_exp_COMMA_exp (lhs, eq, rhses, rhs) ->
      (map_multi_assign_lhs env (`Lhs_splat lhs), eq, map_multi_assign_rhs_tuple env rhses rhs)
  | `Rep1_choice_choice_unde_COMMA_choice_choice_unde_EQ_rep1_exp_COMMA_exp (lhses, lhs, eq, rhses, rhs) ->
      (map_multi_assign_lhs_tuple env lhses lhs, eq, map_multi_assign_rhs_tuple env rhses rhs)

and map_pseudo_call env = function
  | `Opt_exp_DOT_choice_as_pseudo_call_arg_list (recv, op, ty) ->
      let recv = Option.map (fun (e, _dot) -> map_expression env e) recv |> Option.value ~default:(expr_of_id ("self", fake "self")) in
      let ty = map_pseudo_call_type env ty in
      (match op with
      | `As tok | `AsQM tok -> G.Cast (ty, token env tok, recv) |> G.e
      | `Is_aQMARK tok -> G.Call (expr_of_id (str env tok), fb [ G.Arg recv; G.ArgType ty ]) |> G.e)
  | `Opt_exp_DOT_nilQ_opt_LPAR_RPAR (recv, tok, _parens) ->
      let recv = Option.map (fun (e, _dot) -> map_expression env e) recv |> Option.value ~default:(expr_of_id ("self", fake "self")) in
      G.Call (expr_of_id (str env tok), fb [ G.Arg recv ]) |> G.e
  | `Opt_exp_DOT_respos_toQM_pseudo_respos_to_arg_list (recv, tok, _args) ->
      let recv = Option.map (fun (e, _dot) -> map_expression env e) recv |> Option.value ~default:(expr_of_id ("self", fake "self")) in
      G.Call (expr_of_id (str env tok), fb [ G.Arg recv ]) |> G.e

and map_pseudo_call_type env = function
  | `LPAR_bare_type_RPAR (_l, ty, _r) -> map_bare_type env ty
  | `Bare_type ty -> map_bare_type env ty

and map_type_name env x =
  match x with
  | `Semg_meta tok -> G.TyN (name_of_id (str env tok)) |> G.t
  | `Cst c -> G.TyN (name_of_constant env c) |> G.t
  | `Gene_inst_type x -> map_type env (`Choice_paren_type (`Gene_inst_type x))

and id_of_property_type_declaration_name env = function
  | `Id tok | `Semg_meta tok | `Id_meth_call tok | `Inst_var tok | `Class_var tok ->
      str env tok
  | `Macro_var (tok, _) -> str env tok

and id_of_property_keyword_name env = function
  | `Else tok | `Then tok | `Ensure tok | `Rescue tok -> str env tok

and id_of_property_bare_declaration env = function
  | `Id tok | `Id_meth_call tok | `Else tok | `Then tok | `Ensure tok | `Rescue tok ->
      str env tok

and map_include_type_name env (x : CST.anon_choice_cst_092cda1) =
  match x with
  | `Cst c -> G.TyN (name_of_constant env c) |> G.t
  | `Self tok -> G.TyN (name_of_id (str env tok)) |> G.t
  | `Gene_inst_type x -> map_type env (`Choice_paren_type (`Gene_inst_type x))

and id_of_type_name env = function
  | `Semg_meta tok -> str env tok
  | `Cst c -> name_to_id env c
  | `Gene_type (c, _lp, _params, _rp) -> name_to_id env c

and id_of_method_receiver env = function
  | `Semg_meta tok -> fst (str env tok)
  | `Self tok -> fst (str env tok)
  | `Cst c -> fst (name_to_id env c)

and name_to_id env c =
  match name_of_constant env c with
  | G.Id (id, _) -> id
  | G.IdQualified q -> fst q.name_last

and map_named_expr_argument env = function
  | `Choice_id_imm_tok_colon_choice_exp (name, _colon, value) ->
      (id_of_named_expr_name env name, map_named_expr_value env value)
  | `Kw_named_arg_name_choice_exp (name, value) ->
      (id_of_keyword_named_argument env name, map_named_expr_value env value)

and map_named_expr_value env = function
  | `Exp e -> map_expression env e
  | `Out _ -> G.OtherExpr (("Out", fake "out"), []) |> G.e

and id_of_keyword_named_argument env tok =
  let s, t = str env tok in
  let len = String.length s in
  let s = if len > 0 && s.[len - 1] = ':' then String.sub s 0 (len - 1) else s in
  (s, t)

and id_of_named_expr_name env = function
  | `Id tok | `With tok | `Cst_segm tok | `Id_meth_call tok -> str env tok
  | `Str _ -> ("", fake "string")
  | `Str_perc_lit _ -> ("", fake "string")

and macro_node_tok = function
  | `Macro_exp (tok, _, _) | `Macro_stmt (tok, _, _) -> tok
  | `Macro_begin ((tok, _, _), _, _)
  | `Macro_if ((tok, _, _, _), _, _, _)
  | `Macro_unless ((tok, _, _, _), _, _, _)
  | `Macro_for ((tok, _, _, _, _, _, _), _, _)
  | `Macro_verb ((tok, _, _, _), _, _) ->
      tok

let map_program env = function
  | None -> []
  | Some stmts -> map_statements env stmts

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_crystal.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Target } in
      map_program env cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_crystal.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern } in
      match map_program env cst with
      | [ st ] -> G.S st
      | xs -> G.Ss xs)
