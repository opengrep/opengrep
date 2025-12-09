module T = Vbnet_token
module Lexer = Vbnet_lexer

let rec collect_tokens lexer lexbuf =
  match lexer lexbuf with
  | { T.kind = T.EOF; _} as t -> [t]
  | t -> t :: collect_tokens lexer lexbuf

(* ocamllex doesn't track line/col very well, hence we update them
 * post-factum; see the comment above complete_position in Pos.ml
 *)

(* Copy-paste from Parsing_helpers.ml, tokenize_and_adjust_pos *)
let complete_location_in_token (t : Tok.t) (filename : Fpath.t) (table : Pos.bytepos_linecol_converters) =
  Tok.(
    match t with
    | OriginTok pi -> OriginTok (complete_location filename table pi)
    | ExpandedTok (pi, vloc) ->
        ExpandedTok (complete_location filename table pi, vloc)
    | FakeTok (s, vpi_opt) -> FakeTok (s, vpi_opt)
    | Ab -> raise Common.Impossible)

let fix_pos (s : string) (filename : Fpath.t) (ts : T.t list) : T.t list =
  let table = Pos.full_converters_str s in
  let one_tok (t : T.t) =
    { t with tok = complete_location_in_token t.tok filename table }
  in
  List.map one_tok ts

(* The parser needs its own kind of tokens, which also include kinds,
 * so we don't tokenize into Tok.t list (still, Tok.t is a field in T.t)
 *)
(* See the note in Vbnet_lexer.mll about lexer state *)
let tokenize ?(filepath=Fpath.v "<pattern>") (s : string) : T.t list =
  let state = ref Lexer.initial_state in
  Lexing.from_string s
  |> collect_tokens (Lexer.token state)
  |> fix_pos s filepath
