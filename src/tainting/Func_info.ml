module G = AST_generic

(* Scope path outermost->innermost: [Some cls; Some meth] method, [] anonymous. *)
type fn_id = IL.name option list
[@@deriving show]

(* Paths name definitions by where they are: two definitions rebinding one
   name share a sid but are distinct functions. *)
let compare_fn_id (a : fn_id) (b : fn_id) : int =
  List.compare
    (Option.compare (fun (n1 : IL.name) (n2 : IL.name) ->
         Function_id.compare (Function_id.of_il_name n1)
           (Function_id.of_il_name n2)))
    a b

let equal_fn_id (a : fn_id) (b : fn_id) : bool =
  Int.equal (compare_fn_id a b) 0

type t = {
  fn_id : fn_id;
  entity : G.entity option;
  fdef : G.function_definition;
}

let entity_qualifier (func : t) : string option =
  Option.bind func.entity (fun (entity : G.entity) ->
      Option.bind
        (AST_generic_helpers.name_of_entity_name entity.G.name)
        Ty_bare_name.qualifier_of_name)

let as_method : fn_id -> (IL.name * IL.name) option = function
  | [Some cls; Some meth] -> Some (cls, meth)
  | _ -> None

let as_free : fn_id -> IL.name option = function
  | [None; Some bare_name] -> Some bare_name
  | _ -> None

let is_method_of ~(class_name : string) ~(method_name : string)
    (fn_id : fn_id) : bool =
  match as_method fn_id with
  | Some (cls, meth) ->
    String.equal (fst cls.IL.ident) class_name
    && String.equal (fst meth.IL.ident) method_name
  | None -> false

let bare_name : fn_id -> IL.name option = fun fn_id ->
  match List.rev fn_id with
  | Some name :: _ -> Some name
  | _ -> None

let has_body (fdef : G.function_definition) : bool =
  match fdef.G.fbody with
  | G.FBDecl _
  | G.FBNothing -> false
  | _ -> true

let enclosing_class : fn_id -> IL.name option = function
  | Some cls :: _ -> Some cls
  | _ -> None

let method_id ~(cls : IL.name) ~(meth : IL.name) : fn_id =
  [Some cls; Some meth]

(* File of the def's [fkind] token; anchored fake tokens still carry their
   file.  Indexed methods can have a location-less reconstructed [fkind], so
   fall back to the def's own name tokens (bare name first), which carry the source
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

let free_id (bare_name : IL.name) : fn_id = [None; Some bare_name]

let prefer ~(keep : t -> bool) (funcs : t list) : t list =
  match List.filter keep funcs with
  | [] -> funcs
  | kept -> kept

