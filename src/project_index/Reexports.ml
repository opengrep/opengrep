module G = AST_generic
module FA = Graph_from_AST
module Log = Log_projidx.Log

(* Class and module qns share one dotted-string space; [known_class_qns] is
   tested by re-interpreting the module qn. *)
let chase_reexport
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    ~(is_known : Names.Module_qn.t -> bool)
    (qn : Names.Module_qn.t) : Names.Module_qn.t option =
  let visited : (Names.Module_qn.t, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec walk current =
    if is_known current then Some current
    else if Hashtbl.mem visited current then None
    else begin
      Hashtbl.replace visited current ();
      match Hashtbl.find_opt reexport_map current with
      | None -> None
      | Some next -> walk next
    end
  in
  walk qn

(* Re-export a free function under a different bound name (Python
   [from .m import f as g]): expose it as [alias] at the target's own
   position/sid, so name-based lookup finds [alias] while the graph node
   and signature stamp (via fn_id_to_node / resolved_name_of_fn_id in
   the tainting layer) resolve to the target's real definition.  One
   position carrying two names is the convention that marks an alias.
   [entity = None] keeps the target's original name from also being
   exposed under this module. *)
let expose_free_as ~(alias : string) (func : FA.func_info) : FA.func_info =
  match Func_info.bare_name func.FA.fn_id with
  | None -> func
  | Some (tname : IL.name) ->
    let alias_ii = G.empty_id_info () in
    alias_ii.G.id_resolved :=
      (match !(tname.IL.id_info.G.id_resolved) with
       | Some _ as resolved -> resolved
       | None -> Some (G.Global, tname.IL.sid));
    let alias_name : IL.name =
      { ident = (alias, snd tname.IL.ident);
        sid = tname.IL.sid;
        id_info = alias_ii }
    in
    { func with FA.fn_id = Func_info.free_id alias_name; FA.entity = None }

(* [bound -> target] for every name a file re-exports: importing
   [pkg.local] resolves to the defining module. *)
let reexports_of_file ~(cfg : Index_lang_rules.t) (fi : Types.file_info)
    : Types.import list =
  if not cfg.Index_lang_rules.has_reexports then []
  else
    match cfg.Index_lang_rules.reexport_source with
    | Index_lang_rules.Reexports_from_init_file ->
      if cfg.Index_lang_rules.is_init_file fi.Types.fi_file then
        fi.Types.fi_imports
      else []
    | Index_lang_rules.Reexports_from_public_directives ->
      List.filter
        (fun (imp : Types.import) ->
          match imp.Types.im_role with
          | Types.Role_reexports -> true
          | Types.Role_binds -> false)
        fi.Types.fi_imports

let build_reexport_map ~(cfg : Index_lang_rules.t)
    (file_infos : Types.file_info list)
  : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t =
  (* One entry per re-exported name: bounded by the import count. *)
  let reexport_map = Hashtbl.create (List.length file_infos) in
  if not cfg.Index_lang_rules.has_reexports then reexport_map
  else begin
    List.iter (fun (fi : Types.file_info) ->
      let pkg = fi.fi_module_path in
      List.iter (fun (imp : Types.import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from _ -> ()
        | Imports.Named_binding { local; target } ->
          let bound =
            if Names.Module_qn.is_empty pkg
            then Names.Module_qn.of_string local
            else Names.Module_qn.concat pkg local
          in
          if not (Names.Module_qn.equal bound target) then
            Hashtbl.replace reexport_map bound target
      ) (reexports_of_file ~cfg fi)
    ) file_infos;
    reexport_map
  end

(* Dedup so wildcard re-export doesn't double-add a func an explicit import brought in. *)
let merge_dedup ~cur ~newcomers =
  (* [None] (never deduped) for a func with no def file: two distinct
     fileless synthetics with the same name must not collapse onto one
     ([name, ""]) key and lose one. *)
  let key (func : FA.func_info) =
    match Func_info.as_free func.FA.fn_id, Func_info.def_file_opt func with
    | Some name, Some file -> Some (fst name.IL.ident, Fpath.to_string file)
    | _ -> None
  in
  let seen : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun func ->
    match key func with Some k -> Hashtbl.replace seen k () | None -> ())
    cur;
  let added = List.filter (fun func ->
    match key func with
    | None -> true
    | Some k ->
      if Hashtbl.mem seen k then false
      else begin Hashtbl.replace seen k (); true end
  ) newcomers in
  (added @ cur, List.length added)

module MQMap = Map.Make (struct
  type t = Names.Module_qn.t
  let compare = Names.Module_qn.compare
end)

(* The explicit export list of a module-level [__all__ = ["a", "_b"]]
   (Python), as a name-set, or [None] when the module has no [__all__].
   Only top-level assignments count; a list/tuple of string literals is
   the only recognised form (a computed [__all__] yields [None], i.e.
   fall back to the [_]-prefix rule). *)
let dunder_all_of_ast (ast : G.program) : (string, unit) Hashtbl.t option =
  let names_of_rhs (rhs : G.expr) : string list option =
    match rhs.G.e with
    | G.Container ((G.List | G.Tuple), (_, elts, _)) ->
      Some (List.filter_map (fun (e : G.expr) ->
        match e.G.e with
        | G.L (G.String (_, (s, _), _)) -> Some s
        | _ -> None) elts)
    | _ -> None
  in
  let from_stmt (stmt : G.stmt) : string list option =
    match stmt.G.s with
    | G.ExprStmt
        ({ G.e = G.Assign
             ({ G.e = G.N (G.Id (("__all__", _), _)); _ }, _, rhs); _ }, _) ->
      names_of_rhs rhs
    | G.DefStmt
        ({ G.name = G.EN (G.Id (("__all__", _), _)); _ },
         G.VarDef { G.vinit = Some rhs; _ }) ->
      names_of_rhs rhs
    | _ -> None
  in
  match List.find_map from_stmt ast with
  | None -> None
  | Some names ->
    let tbl = Hashtbl.create (List.length names) in
    List.iter (fun n -> Hashtbl.replace tbl n ()) names;
    Some tbl

let build_dunder_all ~(file_infos : Types.file_info list)
  : (string, unit) Hashtbl.t Common.SMap.t =
  List.fold_left
    (fun (declared : (string, unit) Hashtbl.t Common.SMap.t)
         (fi : Types.file_info) ->
      match dunder_all_of_ast fi.Types.fi_ast with
      | None -> declared
      | Some (names : (string, unit) Hashtbl.t) ->
        Common.SMap.add
          (Names.Module_qn.to_string fi.Types.fi_module_path) names declared)
    Common.SMap.empty file_infos

let star_exported ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    (target : Names.Module_qn.t) (name : string) : bool =
  match
    Common.SMap.find_opt (Names.Module_qn.to_string target) dunder_all
  with
  | Some (names : (string, unit) Hashtbl.t) -> Hashtbl.mem names name
  | None -> String.length name > 0 && not (Char.equal name.[0] '_')

let resolve_into_module_index ~(cfg : Index_lang_rules.t)
    ~(project_funcs_by_module
      : (Names.Module_qn.t, FA.func_info list) Hashtbl.t)
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    (file_infos : Types.file_info list)
    : (Names.Module_qn.t * FA.func_info list) list =
  (* Additions as a Map overlay so chained re-exports see prior additions;
     the base table is only read. *)
  let lookup overlay qn =
    match MQMap.find_opt qn overlay with
    | Some fs -> fs
    | None ->
      Option.value (Hashtbl.find_opt project_funcs_by_module qn) ~default:[]
  in
  (* One left-to-right pass over every re-export. Both branches dedup by
     (name, file) so re-injecting an already-present func adds nothing —
     which makes the pass idempotent and lets the fixpoint below detect
     convergence. *)
  let one_pass overlay =
    List.fold_left (fun acc (fi : Types.file_info) ->
      List.fold_left (fun ((overlay, n_added, n_wildcard) as acc)
                        (imp : Types.import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from (target_qn : Names.Module_qn.t) ->
          (* [from M import *] brings in exactly [M.__all__] when M
             declares one (INCLUDING [_]-prefixed names it lists, and
             EXCLUDING public names it omits); otherwise it falls back to
             "every name not starting with [_]" (Python's default). *)
          let public = List.filter (fun (func : FA.func_info) ->
            match Func_info.as_free func.FA.fn_id with
            | Some name ->
              star_exported ~dunder_all target_qn (fst name.IL.ident)
            | None -> false
          ) (lookup overlay target_qn) in
          if public = [] then acc
          else
            let cur = lookup overlay fi.fi_module_path in
            let merged, n = merge_dedup ~cur ~newcomers:public in
            (MQMap.add fi.fi_module_path merged overlay,
             n_added + n, n_wildcard + n)
        | Imports.Named_binding { local; target = target_qn } ->
        match Names.Module_qn.split_last target_qn with
        | Some (target_mod, target_name)
          when not (Names.Module_qn.is_empty target_mod) ->
          let matches = List.filter (fun (func : FA.func_info) ->
            match Func_info.as_free func.FA.fn_id with
            | Some name -> String.equal (fst name.IL.ident) target_name
            | None -> false
          ) (lookup overlay target_mod) in
          if matches = [] then acc
          else
            (* [from .m import f] injects [f]; [from .m import f as g]
               injects it exposed as [g] at [f]'s identity. *)
            let exposed =
              if String.equal local target_name then matches
              else List.map (expose_free_as ~alias:local) matches
            in
            let cur = lookup overlay fi.fi_module_path in
            let merged, n = merge_dedup ~cur ~newcomers:exposed in
            (MQMap.add fi.fi_module_path merged overlay,
             n_added + n, n_wildcard)
        | _ -> acc
      ) acc (reexports_of_file ~cfg fi)
    ) (overlay, 0, 0) file_infos
  in
  (* Iterate to a fixpoint so a chained re-export resolves regardless of
     the order [file_infos] presents the modules in ([b] re-exporting
     from [a] re-exporting from [c] needs [c]'s funcs to reach [a]'s
     overlay before [b] reads it; a single pass only converges if the
     files happen to arrive in dependency order). Bounded by the chain
     depth: each pass either adds a genuinely new binding or stops. *)
  let rec fixpoint overlay total_added total_wildcard =
    let overlay', added, wildcard = one_pass overlay in
    let total_wildcard = total_wildcard + wildcard in
    if added = 0 then (overlay', total_added, total_wildcard)
    else fixpoint overlay' (total_added + added) total_wildcard
  in
  let overlay, n_added, n_wildcard = fixpoint MQMap.empty 0 0 in
  Log.info (fun m ->
    m "Re-exports: %d funcs added to module index (+%d via wildcard)"
      n_added n_wildcard);
  MQMap.bindings overlay
