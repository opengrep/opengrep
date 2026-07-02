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
  | Some (c, m) ->
    String.equal (fst c.IL.ident) class_name
    && String.equal (fst m.IL.ident) method_name
  | None -> false

let leaf_name : fn_id -> IL.name option = fun fn_id ->
  match List.rev fn_id with
  | Some n :: _ -> Some n
  | _ -> None

let enclosing_class : fn_id -> IL.name option = function
  | Some c :: _ -> Some c
  | _ -> None

let method_id ~(cls : IL.name) ~(meth : IL.name) : fn_id =
  [Some cls; Some meth]

(* File of the def's [fkind] token; anchored fake tokens still carry their
   file, [None] only for location-less tokens. *)
let def_file_opt (f : t) : Fpath.t option =
  try Some (Tok.file_of_tok (snd f.fdef.G.fkind))
  with Tok.NoTokenLocation _ -> None

let free_id (leaf : IL.name) : fn_id = [None; Some leaf]
