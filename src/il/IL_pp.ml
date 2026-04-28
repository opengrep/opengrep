(* IL pretty-printer in C-like syntax.
 *
 * Prints IL constructs without exposing raw AST_generic internal
 * representations; all AST_generic values are formatted into readable C-like
 * syntax strings.
 *)
open IL
module G = AST_generic

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let indent_str = "  "

let indent level = String.concat "" (List.init level (fun _ -> indent_str))

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let pp_type_ (ty : G.type_) =
  match ty.t with
  | G.TyN (G.Id ((s, _), _)) -> s
  | G.TyN (G.IdQualified { name_last = ((s, _), _); _ }) -> s
  | G.TyArray (_, inner) -> (
      match inner.t with
      | G.TyN (G.Id ((s, _), _)) -> s ^ "[]"
      | _ -> "<type>[]")
  | G.TyVar (s, _) -> s
  | G.TyAny _ -> "auto"
  | _ -> "<type>"

(*****************************************************************************)
(* Operators *)
(*****************************************************************************)

let pp_operator op =
  match op with
  | G.Plus -> "+"
  | G.Minus -> "-"
  | G.Mult -> "*"
  | G.Div -> "/"
  | G.Mod -> "%"
  | G.Pow -> "**"
  | G.FloorDiv -> "//"
  | G.MatMult -> "@"
  | G.LSL -> "<<"
  | G.LSR -> ">>"
  | G.ASR -> ">>>"
  | G.BitOr -> "|"
  | G.BitXor -> "^"
  | G.BitAnd -> "&"
  | G.BitNot -> "~"
  | G.BitClear -> "&^"
  | G.And -> "&&"
  | G.Or -> "||"
  | G.Xor -> "xor"
  | G.Not -> "!"
  | G.Eq -> "=="
  | G.NotEq -> "!="
  | G.PhysEq -> "==="
  | G.NotPhysEq -> "!=="
  | G.Lt -> "<"
  | G.LtE -> "<="
  | G.Gt -> ">"
  | G.GtE -> ">="
  | G.Cmp -> "<=>"
  | G.Concat -> "++"
  | G.Append -> "+="
  | G.RegexpMatch -> "=~"
  | G.NotMatch -> "!~"
  | G.Range -> ".."
  | G.RangeInclusive -> "..="
  | G.NotNullPostfix -> "!"
  | G.Length -> "#"
  | G.Elvis -> "?:"
  | G.Nullish -> "??"
  | G.In -> "in"
  | G.NotIn -> "!in"
  | G.Is -> "is"
  | G.NotIs -> "!is"
  | G.Background -> "&"
  | G.Pipe -> "|>"
  | G.LDA -> "<=="
  | G.RDA -> "==>"
  | G.LSA -> "<--"
  | G.RSA -> "-->"

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

let pp_literal (lit : G.literal) =
  match lit with
  | G.Bool (b, _) -> string_of_bool b
  | G.Int pi -> (
      match Parsed_int.to_string_opt pi with
      | None -> "<int>"
      | Some s -> s)
  | G.Float (Some f, _) -> string_of_float f
  | G.Float (None, _) -> "<float>"
  | G.Char (s, _) -> Printf.sprintf "'%s'" s
  | G.String (_, (s, _), _) -> Printf.sprintf "\"%s\"" s
  | G.Regexp ((_, (pat, _), _), flags_opt) ->
      let flags =
        match flags_opt with
        | None -> ""
        | Some (s, _) -> s
      in
      Printf.sprintf "/%s/%s" pat flags
  | G.Atom (_, (s, _)) -> Printf.sprintf ":%s" s
  | G.Unit _ -> "()"
  | G.Null _ -> "null"
  | G.Undefined _ -> "undefined"
  | G.Imag (s, _) -> s
  | G.Ratio (s, _) -> s

(*****************************************************************************)
(* Names and labels *)
(*****************************************************************************)

let pp_name name =
  let s = fst name.ident in
  if G.SId.is_unsafe_default name.sid then s
  else Printf.sprintf "%s_%d" s (G.SId.to_int name.sid)

let pp_label ((ident, _sid) : label) = fst ident

(*****************************************************************************)
(* Lvalues *)
(*****************************************************************************)

let pp_var_special vs =
  match vs with
  | This -> "this"
  | Super -> "super"
  | Self -> "self"
  | Parent -> "parent"

let rec pp_exp (e : exp) = pp_exp_kind e.e

and pp_exp_kind ek =
  match ek with
  | Fetch lv -> pp_lval lv
  | Literal lit -> pp_literal lit
  | Composite (ck, (_, exps, _)) -> pp_composite ck exps
  | RecordOrDict fields -> pp_record_or_dict fields
  | Cast (ty, e) -> Printf.sprintf "(%s)%s" (pp_type_ ty) (pp_exp e)
  | Operator ((op, _), args) -> pp_operator_call op args
  | FixmeExp (_, _, Some e) -> Printf.sprintf "<fixme>(%s)" (pp_exp e)
  | FixmeExp _ -> "<fixme>"

and pp_lval { base; rev_offset } =
  let base_str =
    match base with
    | Var name -> pp_name name
    | VarSpecial (vs, _) -> pp_var_special vs
    | Mem e -> Printf.sprintf "*(%s)" (pp_exp e)
  in
  let offsets = List.rev rev_offset in
  List.fold_left
    (fun acc off ->
      match off.o with
      | Dot name -> acc ^ "." ^ pp_name name
      | Index e -> Printf.sprintf "%s[%s]" acc (pp_exp e)
      | Slice lo -> Printf.sprintf "%s[%d..]" acc lo)
    base_str offsets

and pp_composite ck exps =
  let items = String.concat ", " (List.map pp_exp exps) in
  match ck with
  | CTuple -> Printf.sprintf "(%s)" items
  | CArray -> Printf.sprintf "[%s]" items
  | CList -> Printf.sprintf "[%s]" items
  | CSet -> Printf.sprintf "{%s}" items
  | Constructor name -> Printf.sprintf "%s(%s)" (pp_name name) items
  | Regexp -> Printf.sprintf "/%s/" items

and pp_record_or_dict fields =
  let pp_field = function
    | Field (name, e) -> Printf.sprintf "%s: %s" (pp_name name) (pp_exp e)
    | Entry (k, v) -> Printf.sprintf "%s: %s" (pp_exp k) (pp_exp v)
    | Spread e -> Printf.sprintf "...%s" (pp_exp e)
  in
  "{" ^ String.concat ", " (List.map pp_field fields) ^ "}"

and pp_operator_call op args =
  match args with
  | [ Unnamed e ] ->
      (* unary prefix *)
      Printf.sprintf "%s%s" (pp_operator op) (pp_exp e)
  | [ Unnamed e1; Unnamed e2 ] ->
      Printf.sprintf "(%s %s %s)" (pp_exp e1) (pp_operator op) (pp_exp e2)
  | _ ->
      let args_str =
        String.concat ", "
          (List.map
             (function
               | Unnamed e -> pp_exp e
               | Named ((s, _), e) -> Printf.sprintf "%s: %s" s (pp_exp e))
             args)
      in
      Printf.sprintf "__op_%s__(%s)" (pp_operator op) args_str

(*****************************************************************************)
(* Call special *)
(*****************************************************************************)

let pp_call_special cs =
  match cs with
  | Eval -> "__eval__"
  | Typeof -> "typeof"
  | Instanceof -> "instanceof"
  | Sizeof -> "sizeof"
  | Concat -> "__concat__"
  | SpreadFn -> "__spread__"
  | Yield -> "yield"
  | Await -> "await"
  | Delete -> "delete"
  | Assert -> "assert"
  | Ref -> "__ref__"
  | ForeachNext -> "__foreach_next__"
  | ForeachHasNext -> "__foreach_has_next__"
  | Require -> "require"

(*****************************************************************************)
(* Arguments *)
(*****************************************************************************)

let pp_arg pp_val arg =
  match arg with
  | Unnamed v -> pp_val v
  | Named ((s, _), v) -> Printf.sprintf "%s: %s" s (pp_val v)

let pp_args args = String.concat ", " (List.map (pp_arg pp_exp) args)

(*****************************************************************************)
(* Instructions *)
(*****************************************************************************)

let pp_instr_kind ik =
  match ik with
  | Assign (lv, e) -> Printf.sprintf "%s = %s;" (pp_lval lv) (pp_exp e)
  | AssignAnon (lv, Lambda _) ->
      Printf.sprintf "%s = <lambda>;" (pp_lval lv)
  | AssignAnon (lv, AnonClass _) ->
      Printf.sprintf "%s = <class>;" (pp_lval lv)
  | Call (lv_opt, func, args) ->
      let call_str =
        Printf.sprintf "%s(%s)" (pp_exp func) (pp_args args)
      in
      (match lv_opt with
      | None -> call_str ^ ";"
      | Some lv -> Printf.sprintf "%s = %s;" (pp_lval lv) call_str)
  | CallSpecial (lv_opt, (cs, _), args) ->
      let call_str =
        Printf.sprintf "%s(%s)" (pp_call_special cs) (pp_args args)
      in
      (match lv_opt with
      | None -> call_str ^ ";"
      | Some lv -> Printf.sprintf "%s = %s;" (pp_lval lv) call_str)
  | New (lv, ty, _cons, args) ->
      Printf.sprintf "%s = new %s(%s);" (pp_lval lv) (pp_type_ ty)
        (pp_args args)
  | FixmeInstr _ -> "/* fixme_instr */;"

let pp_instr instr = pp_instr_kind instr.i

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

let rec pp_stmt ?(level = 0) stmt =
  let ind = indent level in
  match stmt.s with
  | Instr i -> ind ^ pp_instr i
  | If (_, cond, then_stmts, else_stmts) ->
      let cond_str = pp_exp cond in
      let then_str = pp_block ~level then_stmts in
      let else_part =
        match else_stmts with
        | [] -> ""
        | _ -> Printf.sprintf " else %s" (pp_block ~level else_stmts)
      in
      Printf.sprintf "%sif (%s) %s%s" ind cond_str then_str else_part
  | Loop (_, cond, body) ->
      Printf.sprintf "%swhile (%s) %s" ind (pp_exp cond) (pp_block ~level body)
  | Return (_, e) ->
      Printf.sprintf "%sreturn %s;" ind (pp_exp e)
  | Goto (_, lbl) ->
      Printf.sprintf "%sgoto %s;" ind (pp_label lbl)
  | Label lbl ->
      (* Labels are dedented by one level conventionally *)
      Printf.sprintf "%s%s:" (if level > 0 then indent (level - 1) else "") (pp_label lbl)
  | Try (body, catches, else_stmts, finally) ->
      pp_try ~level body catches else_stmts finally
  | Throw (_, e) ->
      Printf.sprintf "%sthrow %s;" ind (pp_exp e)
  | MiscStmt (Noop s) ->
      Printf.sprintf "%s/* noop: %s */" ind s
  | MiscStmt (DefStmt _) ->
      Printf.sprintf "%s/* def */" ind
  | MiscStmt (DirectiveStmt _) ->
      Printf.sprintf "%s/* directive */" ind
  | FixmeStmt _ ->
      Printf.sprintf "%s/* fixme_stmt */" ind

and pp_block ~level stmts =
  let body =
    String.concat "\n" (List.map (pp_stmt ~level:(level + 1)) stmts)
  in
  Printf.sprintf "{\n%s\n%s}" body (indent level)

and pp_try ~level body catches else_stmts finally =
  let ind = indent level in
  let try_part = Printf.sprintf "%stry %s" ind (pp_block ~level body) in
  let catch_parts =
    List.map
      (fun (name, stmts) ->
        Printf.sprintf " catch (%s) %s" (pp_name name) (pp_block ~level stmts))
      catches
  in
  let else_part =
    match else_stmts with
    | [] -> ""
    | _ -> Printf.sprintf " else %s" (pp_block ~level else_stmts)
  in
  let finally_part =
    match finally with
    | [] -> ""
    | _ -> Printf.sprintf " finally %s" (pp_block ~level finally)
  in
  try_part ^ String.concat "" catch_parts ^ else_part ^ finally_part

(*****************************************************************************)
(* Function definitions *)
(*****************************************************************************)

let pp_function_kind fk =
  match fk with
  | G.Function -> "function"
  | G.Method -> "method"
  | G.Arrow -> "=>"
  | G.LambdaKind -> "lambda"
  | G.BlockCases -> "block"

let pp_param p =
  match p with
  | Param { pname; _ } -> pp_name pname
  | ParamRest { pname; _ } -> "..." ^ pp_name pname
  | ParamPattern ({ pname; _ }, _) -> "<pattern>" ^ pp_name pname
  | ParamFixme -> "<fixme>"

let pp_function_definition ~name (fdef : function_definition) =
  let kind = pp_function_kind (fst fdef.fkind) in
  let ret =
    match fdef.frettype with
    | None -> ""
    | Some ty -> ": " ^ pp_type_ ty
  in
  let params = String.concat ", " (List.map pp_param fdef.fparams) in
  let body = pp_block ~level:0 fdef.fbody in
  Printf.sprintf "%s %s(%s)%s %s" kind name params ret body

(*****************************************************************************)
(* Top-level entry points *)
(*****************************************************************************)

let pp_stmts stmts =
  String.concat "\n" (List.map (pp_stmt ~level:0) stmts)
