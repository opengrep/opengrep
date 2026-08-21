(** Function identifiers  **)

(* This module defines a type used to identify functions globally.
   It is used as the type of nodes in the call graph and as keys in
   the signature database. It is abstract and can be constructed from
   an IL.name that is used in _function definition_, but not a name
   used to refer to that function elsewhere: you need to use the call
   graph to translate the call-site identifier to the proper id of the
   function that it referes to. *)

(* Comparison key precomputed so hash/compare/equal stay Fpath-free in hot loops. *)
type t = {
  ident : IL.ident;
  key : string * string * int * int;
}

(* Helper to extract normalized key for node comparison.
   Resolves file paths to canonical form to handle different representations
   of the same file (e.g., /foo/bar vs /foo/baz/../bar). *)
let normalize_file (file : Fpath.t) : string =
  Fpath.to_string (Fpath.normalize file)

let compute_key ((id, tok) : IL.ident) : string * string * int * int =
  (* Lambdas/[<top_level>] bake the file into the key so vertices don't collide on name; other fake tokens keep the empty key. *)
  let is_lambda_name = String.starts_with ~prefix:"_tmp_lambda" id in
  let is_top_level = String.equal id "<top_level>" in
  if Tok.is_fake tok then
    if is_lambda_name || is_top_level then
      match Tok.loc_of_tok tok with
      | Ok loc ->
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

let hash (v : t) : int = Hashtbl.hash v.key

let compare_key ((n1, f1, l1, c1) : string * string * int * int)
    ((n2, f2, l2, c2) : string * string * int * int) : int =
  let st = String.compare n1 n2 in
  if st <> 0 then st
  else
    let sf = String.compare f1 f2 in
    if sf <> 0 then sf
    else
      let sl = Int.compare l1 l2 in
      if sl <> 0 then sl
      else Int.compare c1 c2

let compare (n1 : t) (n2 : t) : int =
  compare_key n1.key n2.key

let equal (n1 : t) (n2 : t) : bool =
  Int.equal (compare_key n1.key n2.key) 0

let tok ({ident = (_, tok); _} : t) : Tok.t = tok

let show (v : t) : string =
  fst v.ident

let show_debug (v : t) : string =
  let (kid, kfile, kline, kcol) = v.key in
  Printf.sprintf "%s [key=(%s,%s,%d,%d)] (%s)"
    (fst v.ident) kid kfile kline kcol
    (Tok.stringpos_of_tok (snd v.ident))

let of_il_name (n : IL.name) : t =
  let ident = n.IL.ident in
  { ident; key = compute_key ident }

let of_string_and_tok (name : string) (tok : Tok.t) : t =
  let ident = (name, tok) in
  { ident; key = compute_key ident }

(* Key rebuilt from the sid so it compares equal to [of_il_name] of the same def. *)
let of_sid (sid : AST_generic.sid) : t =
  let (name, file, line, col) = AST_generic.SId.to_loc sid in
  let key =
    if String.equal file "" then (name, "", line, col)
    else (name, normalize_file (Fpath.v file), line, col)
  in
  { ident = (name, Tok.unsafe_fake_tok name); key }

let key (v : t) = v.key

let to_file_line_col (v : t) : string * int * int =
  let (_name, file_str, line, col) = v.key in
  if String.equal file_str "" then ("unknown", 0, 0)
  else (file_str, line, col)

let file_of (v : t) : Fpath.t option =
  let (_name, file_str, _line, _col) = v.key in
  if String.equal file_str "" then None
  else Some (Fpath.v file_str)

(* Key-based: absolutifies any id whose key carries a relative file —
   real tokens, located fakes, and key-only ids rebuilt by [of_sid]
   (whose placeless token has no location to fix). The token is
   re-located too when it has a position, so token-derived lookups
   agree with the key. *)
let make_absolute (project_root : Fpath.t) (v : t) : t =
  let (name, tok) = v.ident in
  let (kname, kfile, kline, kcol) = v.key in
  if String.equal kfile "" || Fpath.is_abs (Fpath.v kfile) then v
  else
    let abs_file = Fpath.(project_root // Fpath.v kfile) |> Fpath.normalize in
    let new_tok =
      match Tok.loc_of_tok tok with
      | Ok loc ->
        let new_loc =
          { loc with Tok.pos = { loc.Tok.pos with Pos.file = abs_file } }
        in
        (* [Tok.fix_location] leaves located fakes UNCHANGED; rebuild via
           [fake_tok_loc] so the new location survives. *)
        if Tok.is_fake tok then Tok.fake_tok_loc new_loc name
        else Tok.fix_location (fun _ -> new_loc) tok
      | Error _ -> tok
    in
    { ident = (name, new_tok);
      key = (kname, normalize_file abs_file, kline, kcol) }
