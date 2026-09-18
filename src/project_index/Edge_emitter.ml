module FA = Graph_from_AST

type edge = Function_id.t * Function_id.t * Tok.t

type t = {
  top_level : Function_id.t;
}

let create ~top_level = { top_level }

let emit_call t ~caller_node ~is_toplevel_lambda ~callee ~call_tok : edge list =
  match FA.fn_id_to_node callee, caller_node with
  | Some src, Some dst ->
    let base = [(src, dst, call_tok)] in
    if is_toplevel_lambda && not (Function_id.equal dst t.top_level)
    then (src, t.top_level, call_tok) :: base
    else base
  | _ -> []

let emit_callback t ~caller_node ~is_toplevel_lambda ~callback ~call_tok
    ~tmp : edge list =
  match FA.fn_id_to_node callback, caller_node with
  | Some callback_node, Some caller ->
    let tmp_edge, src_to_caller =
      match tmp with
      | Some tmp_name ->
        let tmp_node = Function_id.of_il_name tmp_name in
        ([(callback_node, tmp_node, call_tok)], tmp_node)
      | None -> ([], callback_node)
    in
    let main = (src_to_caller, caller, call_tok) in
    let lift =
      if is_toplevel_lambda && not (Function_id.equal caller t.top_level)
      then [(src_to_caller, t.top_level, call_tok)]
      else []
    in
    tmp_edge @ (main :: lift)
  | _ -> []

let emit_toplevel t ~callee ~call_tok : edge list =
  match FA.fn_id_to_node callee with
  | Some src -> [(src, t.top_level, call_tok)]
  | None -> []
