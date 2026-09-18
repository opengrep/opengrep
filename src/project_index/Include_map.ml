module G = AST_generic

open Types

type include_form =
  | Quoted_include of string
  | Angle_include of string

type t = {
  im_direct : string list Common.SMap.t;
  im_declared : Func_info.t list Common.SMap.t Common.SMap.t;
}

let empty : t =
  { im_direct = Common.SMap.empty; im_declared = Common.SMap.empty }

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

let reachable_from ~(direct : string list Common.SMap.t) (file : string)
    : unit Common.SMap.t =
  let rec walk (reached : unit Common.SMap.t) (pending : string list)
      : unit Common.SMap.t =
    match pending with
    | [] -> reached
    | (next : string) :: rest ->
      if Common.SMap.mem next reached then walk reached rest
      else
        walk (Common.SMap.add next () reached)
          (Option.value (Common.SMap.find_opt next direct) ~default:[] @ rest)
  in
  walk Common.SMap.empty
    (Option.value (Common.SMap.find_opt file direct) ~default:[])

let external_names (funcs : Func_info.t list) : (string * Func_info.t) list =
  List.filter_map
    (fun (func : Func_info.t) ->
      if Receiver.is_static func.Func_info.entity then None
      else
        Option.map
          (fun (name : IL.name) -> (fst name.IL.ident, func))
          (Func_info.as_free func.Func_info.fn_id))
    funcs

let defined_names (named : (string * Func_info.t) list)
    : (string * Func_info.t) list =
  List.filter
    (fun ((_ : string), (func : Func_info.t)) ->
      match func.Func_info.fdef.G.fbody with
      | G.FBDecl _
      | G.FBNothing -> false
      | G.FBStmt _
      | G.FBExpr _ -> true)
    named

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

let declared_at ~(direct : string list Common.SMap.t)
    ~(declarers : string list Common.SMap.t)
    (exports : (string * Func_info.t) list Common.SMap.t)
    : Func_info.t list Common.SMap.t Common.SMap.t =
  Common.SMap.fold
    (fun (defining : string) (named : (string * Func_info.t) list)
         (declared : Func_info.t list Common.SMap.t Common.SMap.t) ->
      let reached = reachable_from ~direct defining in
      let reaches (declaring : string) : bool =
        String.equal declaring defining || Common.SMap.mem declaring reached
      in
      List.fold_left
        (fun (declared : Func_info.t list Common.SMap.t Common.SMap.t)
             ((name : string), (func : Func_info.t)) ->
          List.fold_left
            (fun (declared : Func_info.t list Common.SMap.t Common.SMap.t)
                 (declaring : string) ->
              if not (reaches declaring) then declared
              else
                Common.SMap.update declaring
                  (fun (bound : Func_info.t list Common.SMap.t option) ->
                    Some
                      (Common.SMap.update name
                         (fun (earlier : Func_info.t list option) ->
                           Some
                             (merge_funcs
                                (Option.value earlier ~default:[]) [ func ]))
                         (Option.value bound ~default:Common.SMap.empty)))
                  declared)
            declared
            (Option.value (Common.SMap.find_opt name declarers) ~default:[]))
        declared named)
    exports Common.SMap.empty

let closure_of_file (include_map : t) (file : string) : unit Common.SMap.t =
  reachable_from ~direct:include_map.im_direct file

let visible_in_file (include_map : t) ~(closure : unit Common.SMap.t)
    (file : string) : Func_info.t list Common.SMap.t =
  let declared_in (reached : string) : Func_info.t list Common.SMap.t =
    Option.value
      (Common.SMap.find_opt reached include_map.im_declared)
      ~default:Common.SMap.empty
  in
  Common.SMap.fold
    (fun (reached : string) ()
         (bound : Func_info.t list Common.SMap.t) ->
      merge_by_name bound (declared_in reached))
    closure (declared_in file)

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
  let has_includers : unit Common.SMap.t =
    Common.SMap.fold
      (fun _ (included : string list) (targets : unit Common.SMap.t) ->
        List.fold_left
          (fun (targets : unit Common.SMap.t) (target : string) ->
            Common.SMap.add target () targets)
          targets included)
      direct Common.SMap.empty
  in
  let declarers =
    List.fold_left
      (fun (declarers : string list Common.SMap.t) (fi : file_info) ->
        let file = Fpath.to_string fi.fi_file in
        if not (Common.SMap.mem file has_includers) then declarers
        else
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
          (List.map fst
             (Option.value (Common.SMap.find_opt file exports) ~default:[])))
      Common.SMap.empty file_infos
  in
  let declared =
    declared_at ~direct ~declarers (Common.SMap.map defined_names exports)
  in
  { im_direct = direct; im_declared = declared }

let bindings_of_file (include_map : t) ~(closure : unit Common.SMap.t)
    (file : string) : Scope_binding.positioned_binding list =
  Common.SMap.fold
    (fun (name : string) (funcs : Func_info.t list)
         (bindings : Scope_binding.positioned_binding list) ->
      Scope_binding.function_binding_of ~pos:None ~parent_path:[] name funcs
      @ bindings)
    (visible_in_file include_map ~closure file)
    []

let files_in_closure (closure : unit Common.SMap.t) : string list =
  Common.SMap.fold
    (fun (reached : string) () (files : string list) -> reached :: files)
    closure []
