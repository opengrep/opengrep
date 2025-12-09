def text():
  return """type token_name = string

module T = Vbnet_token
module G = AST_generic
module RT = Raw_tree

type token = T.t

let token_match = T.token_match

let token_ghost = T.token_ghost

type 'a parsing_result = {
  next : token list;
  value : 'a
  }

type 'a parser =
  (* next *) token list ->
  ('a parsing_result) Seq.t

let run (p : 'a parser) (ts : token list): 'a parsing_result list =
  p ts |> Seq.take 1 |> List.of_seq

let ( let/ ) xs f = Seq.concat_map f xs

let empty = Seq.empty

let single a = Seq.cons a Seq.empty

let is_empty = Seq.is_empty

let cut s = Seq.take 1 s

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  fun next ->
    let/ { next = next'; value = v } = p next in
    f v next'

let ( let* ) p f = bind p f

let pure (value : 'a) : 'a parser =
  fun next ->
    single { next; value }

let fail : 'a parser =
  fun _next -> empty

let rec token (t : token_name) : token parser =
  fun next ->
    match next with
    | w :: ws when token_ghost w ->
        token t ws
    | w :: ws when token_match t w ->
        single { next = ws; value = w }
    | _ -> empty

let rec token_type (t : T.token_kind) : token parser =
  fun next ->
    match next with
    | w :: ws when token_ghost w ->
        token_type t ws
    | w :: ws when w.kind = t ->
        single { next = ws; value = w }
    | _ -> empty

let choice (ps : 'a parser list) : 'a parser =
  fun next ->
    let/ p = List.to_seq ps in
    p next

let look_ahead (t : token_name) : unit parser =
  fun next ->
    match next with
    | w :: _ when token_match t w ->
        single { next; value = () }
    | [] when String.equal t "EOF" ->
        single { next; value = () }
    | _ -> empty

let look_ahead_not (t : token_name) : unit parser =
  fun next ->
    match next with
    | w :: _ when token_match t w ->
        empty
    | [] when String.equal t "EOF" ->
        empty
    | _ ->
        single { next; value = () }

let optional (p : 'a parser) : 'a option parser =
  choice
    [ begin
        let* a = p in
        pure (Some a)
      end
    ; pure None
    ]

(* lists don't backtrack for perf reasons *)
let rec list_of_aux (p : 'a parser) (acc : 'a list) : 'a list parser =
  fun next ->
    match Seq.uncons (p next) with
    | None -> pure (List.rev acc) next
    | Some (r, _) ->
        list_of_aux p (r.value :: acc) r.next

let list_of (p : 'a parser) : 'a list parser =
  fun next ->
    list_of_aux p [] next

let ne_list_of (p : 'a parser) : 'a list parser =
  let* x = p in
  let* xs = list_of p in
  pure (x :: xs)

let any_of_raw (r : G.any RT.t) : G.any =
  G.Raw r

let raw_of_any (a : G.any) : G.any RT.t =
  RT.Any a

let xToken (t : token) : G.any =
  G.Raw (RT.Token (t.content, t.tok))

let xRule (_rule_name : string) (_prod_idex : int) (rs : G.any list) : G.any =
  RT.Tuple (List.map raw_of_any rs) |> any_of_raw

let xGroup (rs : G.any list) : G.any =
  RT.List (List.map raw_of_any rs) |> any_of_raw

let xOptional (r : G.any option) : G.any =
  RT.Option (Option.map raw_of_any r) |> any_of_raw

let xList (rs : G.any list) : G.any =
  RT.List (List.map raw_of_any rs) |> any_of_raw

"""
