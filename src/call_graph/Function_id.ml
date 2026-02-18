(** Function identifiers  **)

(* This module defines a type used to identify functions globally. 
   It is used as the type of nodes in the call graph and as keys in
   the signature database. It is abstract and can be constructed from
   an IL.name that is used in _function definition_, but not a name
   used to refer to that function elsewhere: you need to use the call
   graph to translate the call-site identifier to the proper id of the
   function that it referes to. *)

(* TODO: Keep the file paths always normalized, which should
   speed up comparison. *)

type t = IL.ident
 
(* Helper to extract normalized key for node comparison.
   Resolves file paths to canonical form to handle different representations
   of the same file (e.g., /foo/bar vs /foo/baz/../bar). *)
let normalize_file (file : Fpath.t) : string =
  Fpath.to_string (Fpath.normalize file)

let key ((id, tok) : t) =
  (* For Go lambda names (starting with "_tmp"), extract position even from fake
   * tokens that have position info. This is important for distinguishing
   * different lambdas that all have "_tmp" names.
   * For other languages or regular functions with fake tokens, use empty key
   * to preserve the original matching behavior. *)
  let is_lambda_name = String.length id >= 4 && String.sub id 0 4 = "_tmp" in
  let is_go_file loc =
    let langs = Lang.langs_of_filename loc.Tok.pos.file in
    List.mem Lang.Go langs
  in
  if Tok.is_fake tok then
    if is_lambda_name then
      match Tok.loc_of_tok tok with
      | Ok loc when is_go_file loc ->
          (id, normalize_file loc.Tok.pos.file, loc.Tok.pos.line, loc.Tok.pos.column)
      | _ ->
          (id, "", 0, 0)
    else
      (id, "", 0, 0)
  else
    let file = Tok.file_of_tok tok in
    let line = Tok.line_of_tok tok in
    let col = Tok.col_of_tok tok in
    (id, normalize_file file, line, col)

let hash (v : t) = Hashtbl.hash (key v)

(* TODO: mind the followinf definition from Sig_and_shape *)
(* Compare IL.name by string name and position to differentiate functions with
   the same name in different modules/classes. This matches FuncVertex
   in Function_call_graph.ml. *)
(*
let compare_func_key n1 n2 =
  let open Tok in
  let st = String.compare (fst n1.IL.ident) (fst n2.IL.ident) in
  if st <> 0 then st
  else
    match (snd n1.IL.ident, snd n2.IL.ident) with
    | FakeTok _, FakeTok _ -> 0
    | FakeTok _, _ -> -1
    | _, FakeTok _ -> 1
    | _ -> Tok.compare_pos (snd n1.IL.ident) (snd n2.IL.ident)
*)

let compare (n1 : t) (n2 : t) : int =
  compare (key n1) (key n2)

let equal (n1 : t) (n2 : t) : bool =
  key n1 = key n2

let show ((id, _) : t) : string =
  id

let show_debug (id, tok) : string =
  Printf.sprintf "%s (%s)" id (Tok.stringpos_of_tok tok) 

let of_il_name (n : IL.name) : t =
  n.IL.ident

let to_file_line_col ((_, tok) : t) : string * int * int =
  if Tok.is_fake tok then ("unknown", 0, 0)
  else (normalize_file (Tok.file_of_tok tok), Tok.line_of_tok tok, Tok.col_of_tok tok)
