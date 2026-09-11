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

let bare_name_key (func : t) : string =
  match bare_name func.fn_id with
  | Some (name : IL.name) -> fst name.IL.ident
  | None -> ""

let has_colliding_bare_names (funcs : t list) : bool =
  let bare_names = List.map bare_name_key funcs in
  not
    (Int.equal
       (List.length (List.sort_uniq String.compare bare_names))
       (List.length bare_names))

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
  let methods_with_bare_name =
    List.map (fun (func : t) -> (bare_name_key func, func)) methods
  in
  let entries_per_name =
    List.fold_left
      (fun (entries_per_name : int Common.SMap.t)
           ((name : string), (_ : t)) ->
        Common.SMap.update name
          (function
            | None -> Some 1
            | Some (count : int) -> Some (count + 1))
          entries_per_name)
      Common.SMap.empty methods_with_bare_name
  in
  let name_is_shared (name : string) : bool =
    Common.SMap.find name entries_per_name > 1
  in
  let named_with_kept_in_shared_group =
    List.map
      (fun ((name : string), (func : t)) ->
        (name, func, name_is_shared name && keep func))
      methods_with_bare_name
  in
  (* Method names whose group spans several entries and keeps at least one
     survivor; every other name is left alone. *)
  let narrowed_names =
    List.fold_left
      (fun (narrowed_names : Common.SSet.t)
           ((name : string), _, (kept_in_shared_group : bool)) ->
        if kept_in_shared_group then Common.SSet.add name narrowed_names
        else narrowed_names)
      Common.SSet.empty named_with_kept_in_shared_group
  in
  let kept_methods =
    List.filter_map
      (fun ((name : string), (func : t), (kept_in_shared_group : bool)) ->
        if Common.SSet.mem name narrowed_names && not kept_in_shared_group
        then None
        else Some func)
      named_with_kept_in_shared_group
  in
  if Int.equal (List.length kept_methods) (List.length methods) then None
  else Some kept_methods
