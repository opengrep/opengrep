module G = AST_generic

open Types

type include_form =
  | Quoted_include of string
  | Angle_include of string

type t = {
  im_visible : Func_info.t list Common.SMap.t Common.SMap.t;
}

let empty : t = { im_visible = Common.SMap.empty }

let include_form_of (specifier : string) : include_form =
  let last = String.length specifier - 1 in
  if
    last >= 1
    && Char.equal specifier.[0] '<'
    && Char.equal specifier.[last] '>'
  then Angle_include (String.sub specifier 1 (last - 1))
  else Quoted_include specifier

let specifiers_of_file (fi : file_info) : string list =
  List.rev
    (Walker.fold_stmts_in_program
       (fun (specifiers : string list) (stmt : G.stmt) ->
         match stmt.G.s with
         | G.DirectiveStmt
             { G.d = G.ImportAll (_, G.FileName ((specifier : string), _), _);
               _ } -> specifier :: specifiers
         | _ -> specifiers)
       [] fi.fi_ast)

let normalised_key (file : Fpath.t) : string =
  Fpath.to_string (Fpath.rem_empty_seg (Fpath.normalize file))

let ends_with_segments ~(suffix : string list) (segments : string list) : bool =
  let extra = List.length segments - List.length suffix in
  extra >= 0
  && List.equal String.equal
       (List.filteri (fun (index : int) _ -> index >= extra) segments) suffix

let unique_suffix_match
    ~(by_basename : (string list * string) list Common.SMap.t)
    (specifier : string) : string option =
  match Fpath.of_string specifier with
  | Error _ -> None
  | Ok (path : Fpath.t) -> (
    let suffix = Fpath.segs (Fpath.normalize path) in
    match List.rev suffix with
    | [] -> None
    | (basename : string) :: _ -> (
      match
        List.filter
          (fun ((segments : string list), _) ->
            ends_with_segments ~suffix segments)
          (Option.value (Common.SMap.find_opt basename by_basename) ~default:[])
      with
      | [ (_, (file_key : string)) ] -> Some file_key
      | _ -> None))

let direct_includes ~(by_path : string Common.SMap.t)
    ~(by_basename : (string list * string) list Common.SMap.t)
    (fi : file_info) : string list =
  let own_key = Fpath.to_string fi.fi_file in
  let resolved (specifier : string) : string option =
    match include_form_of specifier with
    | Angle_include (inner : string) -> unique_suffix_match ~by_basename inner
    | Quoted_include (relative : string) -> (
      match Fpath.of_string relative with
      | Error _ -> None
      | Ok (path : Fpath.t) -> (
        let joined = Fpath.append (Fpath.parent fi.fi_file) path in
        match Common.SMap.find_opt (normalised_key joined) by_path with
        | Some _ as found -> found
        | None -> unique_suffix_match ~by_basename relative))
  in
  List.sort_uniq String.compare
    (List.filter
       (fun (key : string) -> not (String.equal key own_key))
       (List.filter_map resolved (specifiers_of_file fi)))

let rec reachable_from ~(direct : string list Common.SMap.t)
    ~(active : unit Common.SMap.t)
    (memo : unit Common.SMap.t Common.SMap.t) (file : string)
    : unit Common.SMap.t Common.SMap.t * unit Common.SMap.t =
  match Common.SMap.find_opt file memo with
  | Some (reached : unit Common.SMap.t) -> (memo, reached)
  | None ->
    if Common.SMap.mem file active then (memo, Common.SMap.empty)
    else
      let active = Common.SMap.add file () active in
      let memo, reached =
        List.fold_left
          (fun ((memo : unit Common.SMap.t Common.SMap.t),
                (reached : unit Common.SMap.t)) (included : string) ->
            let memo, deeper =
              reachable_from ~direct ~active memo included
            in
            ( memo,
              Common.SMap.union
                (fun _ () () -> Some ())
                (Common.SMap.add included () reached)
                deeper ))
          (memo, Common.SMap.empty)
          (Option.value (Common.SMap.find_opt file direct) ~default:[])
      in
      (Common.SMap.add file reached memo, reached)

let closures ~(direct : string list Common.SMap.t) (files : string list)
    : unit Common.SMap.t Common.SMap.t =
  List.fold_left
    (fun (memo : unit Common.SMap.t Common.SMap.t) (file : string) ->
      fst (reachable_from ~direct ~active:Common.SMap.empty memo file))
    Common.SMap.empty files

let external_names (funcs : Func_info.t list) : (string * Func_info.t) list =
  List.filter_map
    (fun (func : Func_info.t) ->
      if Receiver.is_static func.Func_info.entity then None
      else
        Option.map
          (fun (name : IL.name) -> (fst name.IL.ident, func))
          (Func_info.as_free func.Func_info.fn_id))
    funcs

let file_scope_declarations (ast : G.program) : string list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DefStmt
          (({ G.name = G.EN (G.Id (((name : string), _), _)); _ } as ent),
           G.VarDef
             { G.vtype = Some { G.t = G.TyFun _; _ }; vinit = None; _ })
        when not (Receiver.is_static (Some (ent : G.entity))) -> Some name
      | _ -> None)
    ast

let merge_funcs (kept : Func_info.t list) (added : Func_info.t list)
    : Func_info.t list =
  kept
  @ List.filter
      (fun (func : Func_info.t) ->
        not
          (List.exists
             (fun (earlier : Func_info.t) ->
               Func_info.equal_fn_id earlier.Func_info.fn_id
                 func.Func_info.fn_id)
             kept))
      added

let merge_by_name (kept : Func_info.t list Common.SMap.t)
    (added : Func_info.t list Common.SMap.t) : Func_info.t list Common.SMap.t =
  Common.SMap.union
    (fun _ (first : Func_info.t list) (second : Func_info.t list) ->
      Some (merge_funcs first second))
    kept added

let declared_at ~(closure : unit Common.SMap.t Common.SMap.t)
    ~(declarers : string list Common.SMap.t)
    (exports : (string * Func_info.t) list Common.SMap.t)
    : Func_info.t list Common.SMap.t Common.SMap.t =
  let reaches (declaring : string) (defining : string) : bool =
    String.equal declaring defining
    || Common.SMap.mem declaring
         (Option.value (Common.SMap.find_opt defining closure)
            ~default:Common.SMap.empty)
  in
  Common.SMap.fold
    (fun (defining : string) (named : (string * Func_info.t) list)
         (declared : Func_info.t list Common.SMap.t Common.SMap.t) ->
      List.fold_left
        (fun (declared : Func_info.t list Common.SMap.t Common.SMap.t)
             ((name : string), (func : Func_info.t)) ->
          List.fold_left
            (fun (declared : Func_info.t list Common.SMap.t Common.SMap.t)
                 (declaring : string) ->
              if not (reaches declaring defining) then declared
              else
                Common.SMap.update declaring
                  (fun (bound : Func_info.t list Common.SMap.t option) ->
                    Some
                      (merge_by_name
                         (Option.value bound ~default:Common.SMap.empty)
                         (Common.SMap.singleton name [ func ])))
                  declared)
            declared
            (Option.value (Common.SMap.find_opt name declarers) ~default:[]))
        declared named)
    exports Common.SMap.empty

let rec visible_from ~(direct : string list Common.SMap.t)
    ~(declared : Func_info.t list Common.SMap.t Common.SMap.t)
    ~(active : unit Common.SMap.t)
    (memo : Func_info.t list Common.SMap.t Common.SMap.t) (file : string)
    : Func_info.t list Common.SMap.t Common.SMap.t
      * Func_info.t list Common.SMap.t =
  match Common.SMap.find_opt file memo with
  | Some (bound : Func_info.t list Common.SMap.t) -> (memo, bound)
  | None ->
    if Common.SMap.mem file active then (memo, Common.SMap.empty)
    else
      let active = Common.SMap.add file () active in
      let memo, bound =
        List.fold_left
          (fun ((memo : Func_info.t list Common.SMap.t Common.SMap.t),
                (bound : Func_info.t list Common.SMap.t)) (included : string) ->
            let memo, deeper =
              visible_from ~direct ~declared ~active memo included
            in
            (memo, merge_by_name bound deeper))
          (memo,
           Option.value (Common.SMap.find_opt file declared)
             ~default:Common.SMap.empty)
          (Option.value (Common.SMap.find_opt file direct) ~default:[])
      in
      (Common.SMap.add file bound memo, bound)

let build ~(file_infos : file_info list)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t) : t =
  let file_keys = List.map (fun (fi : file_info) -> Fpath.to_string fi.fi_file)
      file_infos
  in
  let by_path =
    List.fold_left
      (fun (by_path : string Common.SMap.t) (fi : file_info) ->
        Common.SMap.add (normalised_key fi.fi_file)
          (Fpath.to_string fi.fi_file) by_path)
      Common.SMap.empty file_infos
  in
  let by_basename =
    List.fold_left
      (fun (by_basename : (string list * string) list Common.SMap.t)
           (fi : file_info) ->
        let segments = Fpath.segs (Fpath.normalize fi.fi_file) in
        match List.rev segments with
        | [] -> by_basename
        | (basename : string) :: _ ->
          Common.SMap.update basename
            (fun (earlier : (string list * string) list option) ->
              Some
                ((segments, Fpath.to_string fi.fi_file)
                 :: Option.value earlier ~default:[]))
            by_basename)
      Common.SMap.empty file_infos
  in
  let direct =
    List.fold_left
      (fun (direct : string list Common.SMap.t) (fi : file_info) ->
        Common.SMap.add (Fpath.to_string fi.fi_file)
          (direct_includes ~by_path ~by_basename fi) direct)
      Common.SMap.empty file_infos
  in
  let closure = closures ~direct file_keys in
  let exports =
    List.fold_left
      (fun (exports : (string * Func_info.t) list Common.SMap.t)
           (file : string) ->
        match Hashtbl.find_opt file_funcs_index file with
        | None -> exports
        | Some (funcs : Func_info.t list) ->
          Common.SMap.add file (external_names funcs) exports)
      Common.SMap.empty file_keys
  in
  let declarers =
    List.fold_left
      (fun (declarers : string list Common.SMap.t) (fi : file_info) ->
        let file = Fpath.to_string fi.fi_file in
        List.fold_left
          (fun (declarers : string list Common.SMap.t) (name : string) ->
            Common.SMap.update name
              (fun (earlier : string list option) ->
                match earlier with
                | Some ((first :: _) as files) when String.equal first file ->
                  Some files
                | Some (files : string list) -> Some (file :: files)
                | None -> Some [ file ])
              declarers)
          declarers
          (file_scope_declarations fi.fi_ast
           @ List.map fst
               (Option.value (Common.SMap.find_opt file exports) ~default:[])))
      Common.SMap.empty file_infos
  in
  let declared = declared_at ~closure ~declarers exports in
  { im_visible =
      List.fold_left
        (fun (memo : Func_info.t list Common.SMap.t Common.SMap.t)
             (file : string) ->
          fst
            (visible_from ~direct ~declared ~active:Common.SMap.empty memo file))
        Common.SMap.empty file_keys }

let bindings_of_file (include_map : t) (file : string)
    : Scope_binding.positioned_binding list =
  Common.SMap.fold
    (fun (name : string) (funcs : Func_info.t list)
         (bindings : Scope_binding.positioned_binding list) ->
      Scope_binding.function_binding_of ~pos:None ~parent_path:[] name funcs
      @ bindings)
    (Option.value (Common.SMap.find_opt file include_map.im_visible)
       ~default:Common.SMap.empty)
    []
