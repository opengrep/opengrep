module G = AST_generic

(* Scope path outermost->innermost: [Some cls; Some meth] method, [] anonymous. *)
type fn_id = IL.name option list
[@@deriving show, eq, ord]

type t = {
  fn_id : fn_id;
  entity : G.entity option;
  fdef : G.function_definition;
}

let as_method : fn_id -> (IL.name * IL.name) option = function
  | [Some cls; Some meth] -> Some (cls, meth)
  | _ -> None

let as_free : fn_id -> IL.name option = function
  | [None; Some leaf] -> Some leaf
  | _ -> None

let is_method_of ~(class_name : string) ~(method_name : string)
    (fn_id : fn_id) : bool =
  match as_method fn_id with
  | Some (cls, meth) ->
    String.equal (fst cls.IL.ident) class_name
    && String.equal (fst meth.IL.ident) method_name
  | None -> false

let leaf_name : fn_id -> IL.name option = fun fn_id ->
  match List.rev fn_id with
  | Some name :: _ -> Some name
  | _ -> None

let enclosing_class : fn_id -> IL.name option = function
  | Some cls :: _ -> Some cls
  | _ -> None

let method_id ~(cls : IL.name) ~(meth : IL.name) : fn_id =
  [Some cls; Some meth]

(* File of the def's [fkind] token; anchored fake tokens still carry their
   file.  Indexed methods can have a location-less reconstructed [fkind], so
   fall back to the def's own name tokens (leaf first), which carry the source
   file.  [None] only when no token has a location. *)
let def_file_opt (func_info : t) : Fpath.t option =
  let from_tok tok =
    try Some (Tok.file_of_tok tok) with Tok.NoTokenLocation _ -> None
  in
  match from_tok (snd func_info.fdef.G.fkind) with
  | Some _ as f -> f
  | None ->
    List.find_map
      (function
        | Some (name : IL.name) -> from_tok (snd name.IL.ident)
        | None -> None)
      (List.rev func_info.fn_id)

let free_id (leaf : IL.name) : fn_id = [None; Some leaf]
