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

(* Narrow one class's method list per method-name group.  Two same-named
   classes in different files land under one bare class name at method
   dispatch, and [pick_by_arity] drops the call on the (class, method, arity)
   collision — a silent cross-file false negative caused by an unrelated
   homonym.  Only a group holding several entries is that collision, so
   narrowing applies per method name, not per class: a uniquely named method
   is kept whatever its file (a TS class-body alias carries the aliased
   function's file, not the class's, and would otherwise be dropped whenever
   the class also declares an ordinary method).  A group [keep] would empty is
   left untouched, so a path-shape mismatch degrades to the un-narrowed set
   rather than erasing the method.  [None] = nothing changed. *)
let narrow_colliding_groups ~(keep : t -> bool) (methods : t list)
    : t list option =
  let leaf (func : t) : string =
    match leaf_name func.fn_id with
    | Some (name : IL.name) -> fst name.IL.ident
    | None -> ""
  in
  let named = List.map (fun func -> (leaf func, func)) methods in
  (* Method names whose group spans several entries and keeps at least one
     survivor; every other name is left alone. *)
  let narrowed_names =
    List.sort_uniq String.compare (List.map fst named)
    |> List.filter (fun name ->
         let group =
           List.filter (fun (n, _) -> String.equal n name) named
         in
         List.length group > 1
         && List.exists (fun (_, func) -> keep func) group)
  in
  let filtered =
    List.filter_map (fun (name, func) ->
      if List.exists (String.equal name) narrowed_names && not (keep func)
      then None
      else Some func)
      named
  in
  if List.length filtered <> List.length methods then Some filtered else None
