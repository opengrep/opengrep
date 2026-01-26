(* TODO: Keep the file paths normalized from the construction *)

type t = IL.ident

let normalize_file (file : Fpath.t) : string =
  try Unix.realpath (Fpath.to_string file)
  with Unix.Unix_error _ -> Fpath.to_string (Fpath.normalize file)

let key ((id, tok) : t) =
  if Tok.is_fake tok then
    (id, "", 0, 0)
  else
    let file = Tok.file_of_tok tok in
    let line = Tok.line_of_tok tok in
    let col = Tok.col_of_tok tok in
    (id, normalize_file file, line, col)

let hash (v : t) = Hashtbl.hash (key v)

(* TODO: mid the definition from Sig_and_shape *)
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
