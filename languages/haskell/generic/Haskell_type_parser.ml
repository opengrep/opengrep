(* Haskell Type Parser - Converts type signature strings to AST_generic types *)
(* Handles: Int -> Int, [a] -> [a], Ord a => [a] -> [a], etc. *)

module G = AST_generic

(*******************************************************************************)
(* Internal representation of Haskell types *)
(*******************************************************************************)

type haskell_type =
  | HTyVar of string                                      (* a, b, c *)
  | HTyCon of string                                      (* Int, String, IO *)
  | HTyApp of haskell_type * haskell_type                 (* Maybe Int, [a] *)
  | HTyFun of haskell_type * haskell_type                 (* a -> b *)
  | HTyTuple of haskell_type list                         (* (a, b, c) *)
  | HTyConstraint of (string * haskell_type) list * haskell_type
                                                          (* Ord a => [a] -> [a] *)

(*******************************************************************************)
(* Tokenizer *)
(*******************************************************************************)

type token =
  | TIdent of string      (* identifiers: Int, String, a, b *)
  | TArrow                (* -> *)
  | TDoubleArrow          (* => *)
  | TLParen               (* ( *)
  | TRParen               (* ) *)
  | TLBracket             (* [ *)
  | TRBracket             (* ] *)
  | TComma                (* , *)
  | TEOF

let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '\''

let is_digit c = c >= '0' && c <= '9'

let is_ident_char c = is_alpha c || is_digit c

let rec tokenize_helper s pos acc =
  if pos >= String.length s then
    List.rev (TEOF :: acc)
  else
    match s.[pos] with
    | ' ' | '\t' | '\n' | '\r' ->
        tokenize_helper s (pos + 1) acc
    | '(' ->
        tokenize_helper s (pos + 1) (TLParen :: acc)
    | ')' ->
        tokenize_helper s (pos + 1) (TRParen :: acc)
    | '[' ->
        tokenize_helper s (pos + 1) (TLBracket :: acc)
    | ']' ->
        tokenize_helper s (pos + 1) (TRBracket :: acc)
    | ',' ->
        tokenize_helper s (pos + 1) (TComma :: acc)
    | '-' when pos + 1 < String.length s && s.[pos + 1] = '>' ->
        tokenize_helper s (pos + 2) (TArrow :: acc)
    | '=' when pos + 1 < String.length s && s.[pos + 1] = '>' ->
        tokenize_helper s (pos + 2) (TDoubleArrow :: acc)
    | c when is_alpha c ->
        (* Read identifier *)
        let start = pos in
        let rec read_ident p =
          if p < String.length s && is_ident_char s.[p] then
            read_ident (p + 1)
          else
            p
        in
        let end_pos = read_ident (pos + 1) in
        let ident = String.sub s start (end_pos - start) in
        tokenize_helper s end_pos (TIdent ident :: acc)
    | _ ->
        (* Skip unknown characters *)
        tokenize_helper s (pos + 1) acc

let tokenize s = tokenize_helper s 0 []

(*******************************************************************************)
(* Parser *)
(*******************************************************************************)

exception ParseError of string

type parser_state = {
  tokens : token list;
  pos : int;
}

let peek state =
  if state.pos < List.length state.tokens then
    List.nth state.tokens state.pos
  else
    TEOF

let advance state =
  { state with pos = state.pos + 1 }

let expect tok state =
  let current = peek state in
  if current = tok then
    advance state
  else
    raise (ParseError (Printf.sprintf "Expected token but got different token at pos %d" state.pos))

(* Parse type variable or constructor *)
let rec parse_type_atom state =
  match peek state with
  | TIdent name ->
      let state = advance state in
      (* Check if lowercase (type variable) or uppercase (constructor) *)
      if String.length name > 0 && name.[0] >= 'a' && name.[0] <= 'z' then
        (HTyVar name, state)
      else
        (HTyCon name, state)
  | TLParen ->
      (* Tuple or parenthesized type *)
      let state = advance state in
      let (types, state) = parse_type_list state [] in
      let state = expect TRParen state in
      (match types with
       | [t] -> (t, state)  (* Parenthesized type *)
       | _ -> (HTyTuple types, state))  (* Tuple *)
  | TLBracket ->
      (* List type [a] *)
      let state = advance state in
      let (inner, state) = parse_type state in
      let state = expect TRBracket state in
      (HTyApp (HTyCon "[]", inner), state)
  | _ ->
      raise (ParseError "Expected type atom")

(* Parse type list for tuples *)
and parse_type_list state acc =
  let (ty, state) = parse_type state in
  let acc = ty :: acc in
  match peek state with
  | TComma ->
      let state = advance state in
      parse_type_list state acc
  | _ ->
      (List.rev acc, state)

(* Parse type application: Maybe Int, IO () *)
and parse_type_app state =
  let (base, state) = parse_type_atom state in
  let rec parse_args state base =
    match peek state with
    | TIdent _ | TLParen | TLBracket ->
        let (arg, state) = parse_type_atom state in
        parse_args state (HTyApp (base, arg))
    | _ ->
        (base, state)
  in
  parse_args state base

(* Parse function type: a -> b -> c *)
and parse_type_arrow state =
  let (lhs, state) = parse_type_app state in
  match peek state with
  | TArrow ->
      let state = advance state in
      let (rhs, state) = parse_type_arrow state in
      (HTyFun (lhs, rhs), state)
  | _ ->
      (lhs, state)

(* Parse constraint: Ord a => ... *)
and parse_type_constraint state =
  (* Try to parse constraints *)
  let (ty, state_after) = parse_type_arrow state in
  match peek state_after with
  | TDoubleArrow ->
      (* It's a constraint! Convert ty to constraints *)
      let state = advance state_after in
      let constraints = extract_constraints ty in
      let (body, state) = parse_type state in
      (HTyConstraint (constraints, body), state)
  | _ ->
      (* No constraint *)
      (ty, state_after)

and parse_type state = parse_type_constraint state

(* Extract constraints from parsed type (before =>) *)
and extract_constraints ty =
  match ty with
  | HTyApp (HTyCon class_name, HTyVar var) ->
      [(class_name, HTyVar var)]
  | HTyTuple types ->
      List.concat_map (fun t ->
        match t with
        | HTyApp (HTyCon class_name, HTyVar var) -> [(class_name, HTyVar var)]
        | _ -> []
      ) types
  | _ -> []

(*******************************************************************************)
(* Public API *)
(*******************************************************************************)

let parse_haskell_type (s: string) : haskell_type =
  try
    let tokens = tokenize s in
    let state = { tokens; pos = 0 } in
    let (ty, _state) = parse_type state in
    ty
  with ParseError _msg ->
    (* Fallback to simple constructor if parsing fails *)
    HTyCon s

(*******************************************************************************)
(* Conversion to AST_generic *)
(*******************************************************************************)

let fake_tok s = Tok.unsafe_fake_tok s

let rec haskell_type_to_generic ?(resolve_name = fun s -> s) (ht: haskell_type) : G.type_ =
  match ht with
  | HTyVar name ->
      let name = resolve_name name in
      { G.t = G.TyVar ((name, fake_tok name)); G.t_attrs = [] }

  | HTyCon name ->
      let name = resolve_name name in
      let id = G.Id ((name, fake_tok name), G.empty_id_info ()) in
      { G.t = G.TyN id; G.t_attrs = [] }

  | HTyApp (con, arg) ->
      let base = haskell_type_to_generic ~resolve_name con in
      let arg_type = haskell_type_to_generic ~resolve_name arg in
      { G.t = G.TyApply (base, Tok.unsafe_fake_bracket [G.TA arg_type]); G.t_attrs = [] }

  | HTyFun (from_type, to_type) ->
      (* Convert from_type to parameter *)
      let param_type = haskell_type_to_generic ~resolve_name from_type in
      let param = G.Param {
        G.pname = None;  (* Haskell doesn't name function type params *)
        G.ptype = Some param_type;
        G.pdefault = None;
        G.pattrs = [];
        G.pinfo = G.empty_id_info ();
      } in
      let return_type = haskell_type_to_generic ~resolve_name to_type in
      { G.t = G.TyFun ([param], return_type); G.t_attrs = [] }

  | HTyTuple types ->
      let converted = List.map (haskell_type_to_generic ~resolve_name) types in
      { G.t = G.TyTuple (Tok.unsafe_fake_bracket converted); G.t_attrs = [] }

  | HTyConstraint (_constraints, body) ->
      (* For now, just convert the body type *)
      (* Full constraint support would require OtherType *)
      haskell_type_to_generic ~resolve_name body

(*******************************************************************************)
(* Main entry point *)
(*******************************************************************************)

let parse_and_convert (type_string: string) : G.type_ =
  let haskell_ty = parse_haskell_type type_string in
  haskell_type_to_generic haskell_ty

let parse_and_convert_with_metavars (type_string: string) (resolve : string -> string) : G.type_ =
  let haskell_ty = parse_haskell_type type_string in
  haskell_type_to_generic ~resolve_name:resolve haskell_ty
