(* projidx: language-agnostic project-wide symbol walker. *)

module G = AST_generic

open Types

module Log = Log_projidx.Log

module FA = Graph_from_AST

module Function_id_map = Map.Make (struct
  type t = Function_id.t
  let compare = Function_id.compare
end)

let build_funcs_by_id (all_funcs : FA.func_info list)
    : FA.func_info list Function_id_map.t =
  List.fold_left
    (fun (by_id : FA.func_info list Function_id_map.t) (func : FA.func_info) ->
      match Func_info.bare_name func.FA.fn_id with
      | None -> by_id
      | Some (name : IL.name) ->
        let id = Function_id.of_il_name name in
        Function_id_map.add id
          (func :: Option.value (Function_id_map.find_opt id by_id) ~default:[])
          by_id)
    Function_id_map.empty all_funcs

let build_class_parent_paths ~(entries : entry list)
    ~(funcs_by_id : FA.func_info list Function_id_map.t)
    : (Function_id.t * IL.name option list) list Common.SMap.t =
  List.fold_left
    (fun (paths : (Function_id.t * IL.name option list) list Common.SMap.t)
         (entry : entry) ->
      match (entry.kind, entry.defining_class_id) with
      | (K_function | K_class | K_companion), _
      | K_method, None -> paths
      | K_method, Some (class_id : Function_id.t) -> (
        match Function_id_map.find_opt entry.id funcs_by_id with
        | None
        | Some [] -> paths
        | Some ((func : FA.func_info) :: _) -> (
          match List_.init_and_last_opt func.FA.fn_id with
          | None
          | Some (_, None) -> paths
          | Some ((parent_path : IL.name option list), Some _) ->
            let key = Function_id.show class_id in
            let bound =
              Option.value (Common.SMap.find_opt key paths) ~default:[]
            in
            if
              List.exists
                (fun (((bound_id : Function_id.t), _) :
                        Function_id.t * IL.name option list) ->
                  Function_id.equal bound_id class_id)
                bound
            then paths
            else Common.SMap.add key ((class_id, parent_path) :: bound) paths)))
    Common.SMap.empty entries

let build_definitions_by_qn ~(entries : entry list)
    ~(funcs_by_id : FA.func_info list Function_id_map.t)
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    : definition Common.SMap.t * Func_lookup.companion_index =
  let functions =
    List.fold_left
      (fun (by_qn : definition Common.SMap.t) (entry : entry) ->
        match entry.kind with
        | K_class
        | K_companion -> by_qn
        | K_function
        | K_method -> (
          match Function_id_map.find_opt entry.id funcs_by_id with
          | None -> by_qn
          | Some (funcs : FA.func_info list) ->
            let key = Names.Def_qn.to_string entry.qn in
            let previous =
              match Common.SMap.find_opt key by_qn with
              | Some (Function_definitions earlier) -> earlier
              | Some (Class_definition _)
              | None -> []
            in
            Common.SMap.add key
              (Function_definitions (funcs @ previous)) by_qn))
      Common.SMap.empty entries
  in
  let companions : Func_lookup.companion_index =
    List.fold_left
      (fun (companions : Func_lookup.companion_index) (entry : entry) ->
        match entry.kind with
        | K_function
        | K_method
        | K_class -> companions
        | K_companion -> (
          match Names.Def_qn.split_last entry.qn with
          | None -> companions
          | Some ((parent : Names.Def_qn.t), _) ->
            Func_lookup.Class_qn_map.add
              (Names.Class_qn.of_string
                 (Names.Def_qn.to_string (Names.Def_qn.concat parent entry.name)))
              (Names.Class_qn.of_string (Names.Def_qn.to_string entry.qn))
              companions))
      Func_lookup.Class_qn_map.empty entries
  in
  let class_scope (entry : entry) (role : Class_table.role)
      : Class_table.scope_id option =
    match entry.entity with
    | Some { G.name = G.EN name; _ } ->
      Option.map
        (fun (sid : G.SId.t) ->
          { Class_table.scope_binding = sid; scope_role = role })
        (Class_table.definition_binding name)
    | Some _
    | None -> None
  in
  let with_classes =
    List.fold_left
      (fun (by_qn : definition Common.SMap.t) (entry : entry) ->
        match entry.kind with
        | K_function
        | K_method -> by_qn
        | K_class ->
          let qn = Names.Def_qn.to_string entry.qn in
          let class_qn = Names.Class_qn.of_string qn in
          Common.SMap.add qn
            (Class_definition
               { class_file = entry.file;
                 class_qn;
                 class_name = entry.name;
                 class_companion =
                   Func_lookup.Class_qn_map.find_opt class_qn companions;
                 class_entity = entry.entity;
                 class_scope =
                   class_scope entry (Class_table.Definition entry.id) })
            by_qn
        | K_companion ->
          let qn = Names.Def_qn.to_string entry.qn in
          let definition =
            Class_definition
              { class_file = entry.file;
                class_qn = Names.Class_qn.of_string qn;
                class_name = entry.name;
                class_companion = None;
                class_entity = entry.entity;
                class_scope = class_scope entry Class_table.Singleton_object }
          in
          let by_qn = Common.SMap.add qn definition by_qn in
          match Names.Def_qn.split_last entry.qn with
          | None -> by_qn
          | Some ((parent : Names.Def_qn.t), _) ->
            let class_key =
              Names.Def_qn.to_string (Names.Def_qn.concat parent entry.name)
            in
            if Common.SMap.mem class_key by_qn then by_qn
            else Common.SMap.add class_key definition by_qn)
      functions entries
  in
  let with_reexports =
    Hashtbl.fold
      (fun (bound : Names.Module_qn.t) (_target : Names.Module_qn.t)
           (by_qn : definition Common.SMap.t) ->
        let bound_key = Names.Module_qn.to_string bound in
          if Common.SMap.mem bound_key by_qn then by_qn
          else
            let is_known (qn : Names.Module_qn.t) : bool =
              Common.SMap.mem (Names.Module_qn.to_string qn) by_qn
            in
            match Reexports.chase_reexport ~reexport_map ~is_known bound with
            | None -> by_qn
            | Some (target : Names.Module_qn.t) -> (
              match
                Common.SMap.find_opt (Names.Module_qn.to_string target) by_qn
              with
              | None -> by_qn
              | Some (found : definition) ->
                Common.SMap.add bound_key found by_qn))
      reexport_map with_classes
  in
  (with_reexports, companions)


(* Maximum number of files processed per parallel work unit.  Batching
   amortises Domainslib dispatch overhead over many small per-file tasks
   while keeping [chunksize = 1] — one task per thread — so the
   [Memprof_limits]-based memory limit and timeout stay sound (see the
   warning on [Domainslib_.parmap]). *)
(* Files are sorted by decreasing size before the edge pass, so a batch is
   a run of similar-sized files; small batches let the pool balance the
   heavy head of that order instead of handing one domain the 500 largest
   files (on GitLab that left the pool one third busy). *)
let per_file_batch_size =
  match Sys.getenv_opt "OPENGREP_PROJIDX_BATCH" with
  | Some s -> ( try int_of_string s with _ -> 32)
  | None -> 32

(* Split a list into chunks of at most [n] elements. *)
let rec chunks (n : int) (xs : 'a list) : 'a list list =
  match xs with
  | [] -> []
  | _ ->
    let len = List.length xs in
    let take = min n len in
    let batch = List_.take_safe take xs in
    let rest = List_.drop take xs in
    batch :: chunks n rest

(* Run [fn] on each item, in parallel when [ncores > 1] and there is
   more than one batch, sequentially otherwise.  Each parallel work
   unit is a whole batch; inside a batch, a per-item failure becomes
   an [Error] for the caller to log and skip, while the fatal trio is
   re-raised. *)
let run_per_file (caps : < Cap.fork >) ~(ncores : int)
    (fn : 'a -> 'b) (items : 'a list)
    : ('b, 'a * Exception.t) Result.t list =
  (* [fn] fixes the type: one [Ok]/[Error] per item; an [Error] carries the
     item so the caller can attribute (and surface) the failure. The whole
     build runs under one memory limit scope, so the limit's exception
     must reach that scope. *)
  let run_one item =
    try Ok (fn item)
    with
    | (Out_of_memory | Memory_limit.ExceededMemoryLimit _) as exn ->
      Exception.catch_and_reraise exn
    | exn -> Error (item, Exception.catch exn)
  in
  let batches = chunks per_file_batch_size items in
  let n = List.length batches in
  if ncores <= 1 || n <= 1 then List_.map run_one items
  else
    Domainslib_.parmap caps
      ~num_domains:(min ncores n) ~chunksize:1
      ~exception_handler:(fun _ exn ->
        match Exception.get_exn exn with
        | Out_of_memory | Memory_limit.ExceededMemoryLimit _ ->
          Exception.reraise exn
        | _ -> exn)
      (fun batch -> List_.map run_one batch)
      batches
    |> List.map2 (fun batch -> function
        | Ok batch_results -> batch_results
        (* A batch-level failure (thrown outside [run_one]) loses the
           per-item results; attribute it to every item. *)
        | Error exn -> List_.map (fun item -> Error (item, exn)) batch)
        batches
    |> List.concat

(* Wall-clock time of one phase, at info level under one tag so a log grep
   gives the phase table (see also Interfile_dispatch.timed). *)
let timed (name : string) (f : unit -> 'a) : 'a =
  let res, secs = Common.with_time f in
  Log_interfile_timing.Log.info (fun m ->
      m "[interfile timing] project index: %s: %.2fs" name secs);
  res

type linked_file = {
  lf_info : file_info;
  lf_table : Symbol_table.t;
  lf_lookup : Func_lookup.t;
  lf_package : Names.Module_qn.t;
}

type written_start =
  | Start_class of Class_table.scope_id
  | Start_module of Names.Module_qn.t

let file_key (file : Fpath.t) : string = Fpath.to_string (Fpath.normalize file)

let scope_name (id : Class_table.scope_id) : string =
  let name, _, _, _ = G.SId.to_loc id.Class_table.scope_binding in
  name

let written_text ((head : G.name), (rest : string list)) : string =
  String.concat "." (Class_table.qualified_path head @ rest)

let build_class_table ~(lang : Lang.t) ~(cfg : Index_lang_rules.t)
    ~(build_constraints : Go_build_constraints.t)
    ~(definitions_by_qn : definition Common.SMap.t) ~(entries : entry list)
    ~(admitted_classes : unit Common.SMap.t)
    ~(class_aliases : (Names.Class_qn.t * string * Func_info.t list) list)
    (files : linked_file list) : Pipeline.project_classes =
  let module Scope_tbl = Class_table.Scope_tbl in
  let scope_file : linked_file Scope_tbl.t = Scope_tbl.create 1024 in
  let owner_of : Class_table.scope_id Scope_tbl.t = Scope_tbl.create 1024 in
  let defined : unit Scope_tbl.t = Scope_tbl.create 1024 in
  let collected =
    List.concat_map
      (fun (lf : linked_file) ->
        List.map
          (fun (scope : Class_table.class_scope) ->
            let id = Class_table.scope_id_of scope in
            Scope_tbl.replace scope_file id lf;
            if Symbol_table.defined_here lf.lf_table scope then
              Scope_tbl.replace defined id ();
            Option.iter
              (fun (owner : Class_table.class_scope) ->
                Scope_tbl.replace owner_of id (Class_table.scope_id_of owner))
              (Symbol_table.owner lf.lf_table scope);
            scope)
          (Symbol_table.classes lf.lf_table))
      files
  in
  let files_by_key : (string, linked_file) Hashtbl.t =
    Hashtbl.create (List.length files)
  in
  List.iter
    (fun (lf : linked_file) ->
      Hashtbl.replace files_by_key (file_key lf.lf_info.fi_file) lf)
    files;
  let definitions_of_binding : Class_table.scope_id list Class_table.SId_tbl.t =
    Class_table.SId_tbl.create 1024
  in
  List.iter
    (fun (scope : Class_table.class_scope) ->
      match scope.Class_table.role with
      | Class_table.Definition _ ->
        Class_table.SId_tbl.replace definitions_of_binding
          scope.Class_table.binding
          (Class_table.scope_id_of scope
           :: Option.value
                (Class_table.SId_tbl.find_opt definitions_of_binding
                   scope.Class_table.binding)
                ~default:[])
      | Class_table.Singleton_object
      | Class_table.Trait_impl _ -> ())
    collected;
  let held_scope (id : Class_table.scope_id) : Class_table.scope_id =
    if Scope_tbl.mem scope_file id then id
    else
      match
        Class_table.SId_tbl.find_opt definitions_of_binding
          id.Class_table.scope_binding
      with
      | Some [ (held : Class_table.scope_id) ] -> held
      | Some _
      | None -> id
  in
  let scopes_of_qn : (string, Class_table.scope_id list) Hashtbl.t =
    Hashtbl.create 1024
  in
  let qn_of_scope : string Scope_tbl.t = Scope_tbl.create 1024 in
  List.iter
    (fun (entry : entry) ->
      let role =
        match entry.kind with
        | K_class -> Some (Class_table.Definition entry.id)
        | K_companion -> Some Class_table.Singleton_object
        | K_function
        | K_method -> None
      in
      match (role, entry.entity) with
      | Some (role : Class_table.role), Some { G.name = G.EN (name : G.name); _ }
        -> (
        match Class_table.definition_binding name with
        | Some (sid : G.SId.t) ->
          let id =
            held_scope { Class_table.scope_binding = sid; scope_role = role }
          in
          let qn = Names.Def_qn.to_string entry.qn in
          Scope_tbl.replace qn_of_scope id qn;
          Hashtbl.replace scopes_of_qn qn
            (id :: Option.value (Hashtbl.find_opt scopes_of_qn qn) ~default:[])
        | None -> ())
      | _ -> ())
    entries;
  let reopened_qns : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (scope : Class_table.class_scope) ->
      let id = Class_table.scope_id_of scope in
      if scope.Class_table.reopens && Scope_tbl.mem defined id then
        Option.iter
          (fun (qn : string) -> Hashtbl.replace reopened_qns qn ())
          (Scope_tbl.find_opt qn_of_scope id))
    collected;
  let file_of_scope (id : Class_table.scope_id) : Fpath.t =
    let _, (file : string), _, _ = G.SId.to_loc id.Class_table.scope_binding in
    Fpath.v file
  in
  Hashtbl.iter
    (fun (qn : string) (ids : Class_table.scope_id list) ->
      let files =
        List_.uniq_by Fpath.equal
          (List.map file_of_scope
             (List.filter (fun (id : Class_table.scope_id) -> Scope_tbl.mem defined id)
                ids))
      in
      let rec never_together (files : Fpath.t list) : bool =
        match files with
        | [] -> true
        | file :: others ->
          List.for_all
            (fun (other : Fpath.t) ->
              not
                (Go_build_constraints.files_compiled_together build_constraints
                   [ file; other ]))
            others
          && never_together others
      in
      match files with
      | _ :: _ :: _ when never_together files ->
        Hashtbl.replace reopened_qns qn ()
      | _ -> ())
    scopes_of_qn;
  let group_of : int Scope_tbl.t = Scope_tbl.create 1024 in
  let members_of_group : (int, Class_table.class_scope list) Hashtbl.t =
    Hashtbl.create 1024
  in
  let group_of_qn : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let next_group = ref 0 in
  let add_to_group (group : int) (scope : Class_table.class_scope) : unit =
    Scope_tbl.replace group_of (Class_table.scope_id_of scope) group;
    Hashtbl.replace members_of_group group
      (scope
      :: Option.value (Hashtbl.find_opt members_of_group group) ~default:[])
  in
  let new_group (scope : Class_table.class_scope) : unit =
    let group = !next_group in
    incr next_group;
    add_to_group group scope
  in
  List.iter
    (fun (scope : Class_table.class_scope) ->
      let id = Class_table.scope_id_of scope in
      if Scope_tbl.mem defined id then
        match
          Option.bind (Scope_tbl.find_opt qn_of_scope id) (fun (qn : string) ->
            if Hashtbl.mem reopened_qns qn then Some qn else None)
        with
        | Some (qn : string) -> (
          match Hashtbl.find_opt group_of_qn qn with
          | Some (group : int) -> add_to_group group scope
          | None ->
            new_group scope;
            Hashtbl.replace group_of_qn qn (Scope_tbl.find group_of id))
        | None -> new_group scope)
    collected;
  let nested : (int option * string, Class_table.scope_id list) Hashtbl.t =
    Hashtbl.create 1024
  in
  Scope_tbl.iter
    (fun (id : Class_table.scope_id) (_ : int) ->
      let owner_group =
        Option.bind (Scope_tbl.find_opt owner_of id) (Scope_tbl.find_opt group_of)
      in
      let key = (owner_group, scope_name id) in
      Hashtbl.replace nested key
        (id :: Option.value (Hashtbl.find_opt nested key) ~default:[]))
    group_of;
  let unique_group (candidates : Class_table.scope_id list)
      : Class_table.scope_id option =
    let grouped =
      List.filter (fun (id : Class_table.scope_id) -> Scope_tbl.mem group_of id)
        candidates
    in
    match
      List.sort_uniq Int.compare
        (List.filter_map (Scope_tbl.find_opt group_of) grouped)
    with
    | [ _ ] -> List_.hd_opt grouped
    | _ -> None
  in
  let nested_in (owner : int option) (name : string)
      : Class_table.scope_id option =
    unique_group (Option.value (Hashtbl.find_opt nested (owner, name)) ~default:[])
  in
  let scopes_of (qn : Names.Class_qn.t) : Class_table.scope_id list =
    Option.value
      (Hashtbl.find_opt scopes_of_qn (Names.Class_qn.to_string qn))
      ~default:[]
  in
  let of_qn (qn : Names.Class_qn.t) : Class_table.scope_id option =
    unique_group (scopes_of qn)
  in
  let of_qn_seen_from (lf : linked_file) (qn : Names.Class_qn.t)
      : Class_table.scope_id option =
    unique_group
      (List.filter
         (fun (id : Class_table.scope_id) ->
           Go_build_constraints.file_visible_from build_constraints
             lf.lf_info.fi_file (file_of_scope id))
         (scopes_of qn))
  in
  let class_of_module_path (lf : linked_file) (qn : Names.Module_qn.t)
      : Class_table.scope_id option =
    match
      Pipeline.definition_of_target ~definitions_by_qn ~package:lf.lf_package qn
    with
    | Some (Class_definition { class_scope; _ }) ->
      Option.map held_scope class_scope
    | Some (Function_definitions _)
    | None -> None
  in
  let rec owners (id : Class_table.scope_id) : Class_table.scope_id list =
    match Scope_tbl.find_opt owner_of id with
    | Some (owner : Class_table.scope_id) -> owner :: owners owner
    | None -> []
  in
  let start_of_text (lf : linked_file) ~(position : Class_table.position)
      (text : string) : written_start option =
    let entries =
      Func_lookup.resolve_in_scope lf.lf_lookup ~caller_parent_path:[] text
    in
    let named_class =
      match (position, Func_lookup.companion_of_entries entries) with
      | Class_table.Term_position, (Some _ as companion) -> companion
      | Class_table.Term_position, None
      | Class_table.Type_position, _ ->
        Func_lookup.class_of_entries entries
    in
    match named_class with
    | Some (qn : Names.Class_qn.t) ->
      Option.map (fun (id : Class_table.scope_id) -> Start_class id)
        (of_qn_seen_from lf qn)
    | None ->
      Option.map
        (fun (module_qn : Names.Module_qn.t) -> Start_module module_qn)
        (Func_lookup.resolve_alias lf.lf_lookup text)
  in
  let constant_path ~(owners : Class_table.scope_id list) (path : string list)
      : Class_table.scope_id option =
    List.find_map
      (fun (prefix : string list) ->
        of_qn (Names.Class_qn.of_parts (prefix @ path)))
      (List.filter_map
         (fun (owner : Class_table.scope_id) ->
           Option.map
             (fun (qn : string) -> Names.Class_qn.parts (Names.Class_qn.of_string qn))
             (Scope_tbl.find_opt qn_of_scope owner))
         owners
       @ [ [] ])
  in
  let start_of_head (lf : linked_file) ~(owners : Class_table.scope_id list)
      ~(position : Class_table.position) (head : G.name)
      : (written_start * string list) option =
    let head_text, rest =
      match Class_table.qualified_path head with
      | (first : string) :: (rest : string list) -> (Some first, rest)
      | [] -> (None, [])
    in
    let qualified (path : string list) : (written_start * string list) option =
      match path with
      | _ :: _ :: _ ->
        Option.map
          (fun (id : Class_table.scope_id) -> (Start_class id, []))
          (List.find_map
             (fun (prefix : string list) ->
               of_qn_seen_from lf (Names.Class_qn.of_parts (prefix @ path)))
             (List.map Names.Module_qn.parts
                (Func_lookup.own_modules lf.lf_lookup)
              @ [ [] ]))
      | []
      | [ _ ] -> None
    in
    let by_text () : (written_start * string list) option =
      Option.bind head_text (fun (first : string) ->
        if cfg.Index_lang_rules.class_identity_is_constant_path then
          Option.map
            (fun (id : Class_table.scope_id) -> (Start_class id, []))
            (constant_path ~owners (first :: rest))
        else
          match start_of_text lf ~position first with
          | Some (start : written_start) -> Some (start, rest)
          | None -> qualified (first :: rest))
    in
    let info = Class_table.id_info_of_name head in
    match (Class_table.binding_of_id_info info, !(info.G.id_resolved)) with
    | Some (sid : G.SId.t), _
      when Scope_tbl.mem group_of (Class_table.definition_scope sid) ->
      Some (Start_class (Class_table.definition_scope sid), [])
    | Some sid, _ -> (
      match Imports.import_of_binding lf.lf_info.fi_imports sid with
      | Some (imp : import) -> (
        match (position, imp.im_binds) with
        | Class_table.Term_position, Binds_type -> None
        | Class_table.Term_position,
          (Binds_any | Binds_function | Binds_constant | Binds_module)
        | Class_table.Type_position, _ ->
          Some
            ( (match class_of_module_path lf imp.im_target with
               | Some (id : Class_table.scope_id) -> Start_class id
               | None -> Start_module imp.im_target),
              rest ))
      | None -> (
        match !(info.G.id_resolved) with
        | Some ((G.ImportedEntity _ | G.ImportedModule _ | G.GlobalName _), _) ->
          by_text ()
        | Some _
        | None -> None))
    | None, _ -> by_text ()
  in
  let rec follow (lf : linked_file) (start : written_start)
      (segments : string list) : Class_table.scope_id option =
    match (segments, start) with
    | [], Start_class (id : Class_table.scope_id) -> Some id
    | [], Start_module _ -> None
    | (segment : string) :: (rest : string list), Start_class id ->
      Option.bind
        (Option.bind (Scope_tbl.find_opt group_of id) (fun (group : int) ->
           nested_in (Some group) segment))
        (fun (nested_id : Class_table.scope_id) ->
          follow lf (Start_class nested_id) rest)
    | segment :: rest, Start_module (module_qn : Names.Module_qn.t) -> (
      match class_of_module_path lf (Names.Module_qn.concat module_qn segment) with
      | Some (id : Class_table.scope_id) -> follow lf (Start_class id) rest
      | None -> (
        match Func_lookup.module_attribute lf.lf_lookup module_qn segment with
        | Some (Func_lookup.Attr_class (qn : Names.Class_qn.t))
        | Some (Func_lookup.Attr_class_with_companion (qn, _)) ->
          Option.bind (of_qn_seen_from lf qn) (fun (id : Class_table.scope_id) ->
            follow lf (Start_class id) rest)
        | Some (Func_lookup.Attr_module (submodule : Names.Module_qn.t)) ->
          follow lf (Start_module submodule) rest
        | Some (Func_lookup.Attr_functions _)
        | None -> None))
  in
  let resolve_written (lf : linked_file) ~(owners : Class_table.scope_id list)
      ~(position : Class_table.position)
      ((head : G.name), (rest : string list)) : Class_table.scope_id option =
    Option.bind (start_of_head lf ~owners ~position head)
      (fun ((start : written_start), (more : string list)) ->
        follow lf start (more @ rest))
  in
  let file_of_name (name : G.name) : linked_file option =
    let file =
      match Class_table.binding_of_id_info (Class_table.id_info_of_name name) with
      | Some (sid : G.SId.t) ->
        let _, (file : string), _, _ = G.SId.to_loc sid in
        Some file
      | None -> (
        let (_, (tok : Tok.t)) : G.ident =
          match name with
          | G.Id ((id : G.ident), _) -> id
          | G.IdQualified { G.name_last = ((id : G.ident), _); _ } -> id
        in
        try Some (file_key (Tok.file_of_tok tok))
        with Tok.NoTokenLocation _ -> None)
    in
    Option.bind file (Hashtbl.find_opt files_by_key)
  in
  let target_of_external (scope : Class_table.class_scope)
      : Class_table.scope_id option =
    let id = Class_table.scope_id_of scope in
    match (scope.Class_table.role, Scope_tbl.find_opt scope_file id) with
    | Class_table.Definition _, Some (lf : linked_file) ->
      Option.bind
        (Imports.import_of_binding lf.lf_info.fi_imports scope.Class_table.binding)
        (fun (imp : import) -> class_of_module_path lf imp.im_target)
    | (Class_table.Definition _ | Class_table.Singleton_object
      | Class_table.Trait_impl _), _ -> None
  in
  let attach (lf : linked_file) (written : string)
      (scope : Class_table.class_scope) (target : Class_table.scope_id option)
      : unit =
    match Option.bind target (Scope_tbl.find_opt group_of) with
    | Some (group : int) -> add_to_group group scope
    | None ->
      Log.warn (fun m ->
          m "unresolvable receiver type in %s: %s"
            (Fpath.to_string lf.lf_info.fi_file) written);
      new_group scope
  in
  List.iter
    (fun (scope : Class_table.class_scope) ->
      let id = Class_table.scope_id_of scope in
      match (Scope_tbl.mem group_of id, Scope_tbl.find_opt scope_file id) with
      | false, Some (lf : linked_file) ->
        attach lf (scope_name id) scope (target_of_external scope)
      | false, None -> new_group scope
      | true, _ -> ())
    collected;
  List.iter
    (fun (lf : linked_file) ->
      let by_receiver : Class_table.class_scope Scope_tbl.t =
        Scope_tbl.create 16
      in
      let targets : G.name Scope_tbl.t = Scope_tbl.create 16 in
      List.iter
        (fun ((role : Symbol_table.receiver_role), (receiver : G.name),
              (funcs : Func_info.t list)) ->
          let binding =
            match
              Class_table.binding_of_id_info (Class_table.id_info_of_name receiver)
            with
            | Some (sid : G.SId.t) -> sid
            | None ->
              let (_, (tok : Tok.t)) : G.ident =
                match receiver with
                | G.Id ((id : G.ident), _) -> id
                | G.IdQualified { G.name_last = ((id : G.ident), _); _ } -> id
              in
              G.SId.of_site ~file:(file_key lf.lf_info.fi_file) tok
          in
          let id = Class_table.definition_scope binding in
          let named =
            List.fold_left
              (fun (named : Func_info.t list Common.SMap.t) (func : Func_info.t) ->
                match Symbol_table.member_name func with
                | Some (name : string) ->
                  Common.SMap.update name
                    (fun (held : Func_info.t list option) ->
                      Some (Option.value held ~default:[] @ [ func ]))
                    named
                | None -> named)
              Common.SMap.empty funcs
          in
          let previous =
            match Scope_tbl.find_opt by_receiver id with
            | Some (scope : Class_table.class_scope) -> scope
            | None ->
              { Class_table.binding;
                role = (Class_table.definition_scope binding).Class_table.scope_role;
                members = Common.SMap.empty;
                fields = Class_table.Field_path_map.empty;
                parents = [];
                class_side_parents = [];
                kind = Class_table.Class_kind G.Class;
                singleton_exposure = Class_parents.No_singleton_exposure;
                bound_functions = [];
                object_fields = Class_table.Field_path_map.empty;
                extensions = Common.SMap.empty;
                reopens = false }
          in
          let joined (earlier : Func_info.t list Common.SMap.t) =
            Common.SMap.union
              (fun (_ : string) (left : Func_info.t list)
                   (right : Func_info.t list) -> Some (left @ right))
              earlier named
          in
          Scope_tbl.replace targets id receiver;
          Scope_tbl.replace by_receiver id
            (match role with
             | Symbol_table.Method_of ->
               { previous with Class_table.members = joined previous.Class_table.members }
             | Symbol_table.Extension_of ->
               { previous with
                 Class_table.extensions = joined previous.Class_table.extensions }))
        (Symbol_table.unbound_receivers lf.lf_table);
      Scope_tbl.iter
        (fun (id : Class_table.scope_id) (scope : Class_table.class_scope) ->
          Scope_tbl.replace scope_file id lf;
          let receiver = Scope_tbl.find targets id in
          attach lf (written_text (receiver, [])) scope
            (resolve_written lf ~owners:[] ~position:Class_table.Type_position
               (receiver, [])))
        by_receiver)
    files;
  let link (scope : Class_table.class_scope) (parent : Class_table.parent)
      : Class_table.scope_id option =
    let id = Class_table.scope_id_of scope in
    let linked =
      match parent with
      | Class_table.Impl (site : Function_id.t) ->
        Some { Class_table.scope_binding = scope.Class_table.binding;
               scope_role = Class_table.Trait_impl site }
      | Class_table.Bound (sid : G.SId.t) ->
        let own = Class_table.definition_scope sid in
        if Scope_tbl.mem group_of own then Some own
        else
          Option.bind (Scope_tbl.find_opt scope_file id) (fun (lf : linked_file) ->
            Option.bind (Imports.import_of_binding lf.lf_info.fi_imports sid)
              (fun (imp : import) -> class_of_module_path lf imp.im_target))
      | Class_table.Unbound (ty : G.type_) ->
        Option.bind (Scope_tbl.find_opt scope_file id) (fun (lf : linked_file) ->
          Option.bind (Class_table.path_of_type ty)
            (resolve_written lf ~owners:(owners id)
               ~position:Class_table.Type_position))
    in
    (match (linked, Scope_tbl.find_opt scope_file id) with
     | None, Some (lf : linked_file) ->
       Log.warn (fun m ->
           m "unresolvable parent in %s: class %s, parent %s"
             (Fpath.to_string lf.lf_info.fi_file) (scope_name id)
             (match parent with
              | Class_table.Bound (sid : G.SId.t) ->
                let name, _, _, _ = G.SId.to_loc sid in
                name
              | Class_table.Unbound (ty : G.type_) -> (
                match Class_table.path_of_type ty with
                | Some (path : G.name * string list) -> written_text path
                | None -> "<type>")
              | Class_table.Impl _ -> "<impl>"))
     | Some _, _
     | None, None -> ());
    linked
  in
  let outside (position : Class_table.position)
      (context : Class_table.scope_id option)
      (((name : G.name), (rest : string list)) : G.name * string list)
      : Class_table.scope_id option =
    let owners =
      match (name, context) with
      | G.IdQualified { G.name_top = Some _; _ }, _
      | _, None -> []
      | _, Some (id : Class_table.scope_id) -> id :: owners id
    in
    Option.bind (file_of_name name) (fun (lf : linked_file) ->
      resolve_written lf ~owners ~position (name, rest))
  in
  let may_implement ~(interface : Class_table.cls) (candidate : Class_table.cls)
      : bool =
    let directories (cls : Class_table.cls) : string list =
      List.map
        (fun (scope : Class_table.class_scope) ->
          let _, (file : string), _, _ = G.SId.to_loc scope.Class_table.binding in
          Fpath.to_string (Fpath.parent (Fpath.v file)))
        (Class_table.scopes cls)
    in
    (not cfg.Index_lang_rules.interface_dispatch_uses_export_visibility)
    || Common.SMap.for_all
         (fun (name : string) (_ : Func_info.t list) ->
           cfg.Index_lang_rules.name_is_exported name)
         (Class_table.member_table interface)
    || List.exists
         (fun (directory : string) ->
           List.exists (String.equal directory) (directories candidate))
         (directories interface)
  in
  List.iter
    (fun ((class_qn : Names.Class_qn.t), (name : string),
          (funcs : Func_info.t list)) ->
      Option.iter
        (fun (id : Class_table.scope_id) ->
          add_to_group (Scope_tbl.find group_of id)
            { Class_table.binding = id.Class_table.scope_binding;
              role = id.Class_table.scope_role;
              members = Common.SMap.singleton name funcs;
              fields = Class_table.Field_path_map.empty;
              parents = [];
              class_side_parents = [];
              kind = Class_table.Class_kind G.Class;
              singleton_exposure = Class_parents.No_singleton_exposure;
              bound_functions = [];
              object_fields = Class_table.Field_path_map.empty;
              extensions = Common.SMap.empty;
              reopens = false })
        (of_qn class_qn))
    class_aliases;
  let classes =
    Hashtbl.fold
      (fun (_ : int) (scopes : Class_table.class_scope list)
           (classes : Class_table.class_scope list list) ->
        (* The definitions of one class, each a local precedence sequence,
           are merged in file path order: Ruby merges them in load order,
           which is not known statically. *)
        List.sort Class_table.compare_scope scopes :: classes)
      members_of_group []
    |> List.sort
         (fun (left : Class_table.class_scope list)
              (right : Class_table.class_scope list) ->
           List.compare Class_table.compare_scope left right)
  in
  let class_table =
    Class_table.build ~lang ~classes
      ~compiled_together:(Go_build_constraints.compiled_together build_constraints)
      ~defined:(fun (scope : Class_table.class_scope) ->
        Scope_tbl.mem defined (Class_table.scope_id_of scope))
      ~link ~outside ~may_implement
  in
  { Pipeline.class_table;
    class_of_qn =
      (fun (class_qn : Names.Class_qn.t) ->
        if Common.SMap.mem (Names.Class_qn.to_string class_qn) admitted_classes
        then Option.bind (of_qn class_qn) (Class_table.class_of_scope class_table)
        else None) }

let build_project_call_graph (caps : < Cap.fork >)
    ~(cfg : Index_lang_rules.t) ~(lang : Lang.t)
    ~(ncores : int) ~(entries : entry list)
    ?(reexport_map = Hashtbl.create 0)
    ~(go_packages : Scope_go.package_index)
    (file_infos : file_info list)
    : Call_graph.G.t * (entry * FA.func_info list) list * Core_error.t list
      * Type_state.t =
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
  let indexed_entries =
    List.filter
      (fun (entry : entry) ->
        not (cfg.Index_lang_rules.is_stub_file entry.file))
      entries
  in
  let indexed_files =
    List.filter
      (fun (fi : file_info) ->
        not (cfg.Index_lang_rules.is_stub_file fi.fi_file))
      file_infos
  in
  let graph = Call_graph.G.create () in
  (* Per-file synthetic [<top_level>] node for module-scope calls, so the dump
     keeps the caller's file/line. *)
  let top_level_nodes : (string, Function_id.t) Hashtbl.t =
    Hashtbl.create (List.length file_infos) in
  let top_level_node_for (file : Fpath.t) : Function_id.t =
    Hashtbl.find top_level_nodes (Fpath.to_string file)
  in

  let placeholder_fdef tok : G.function_definition = {
    G.fkind = (G.Method, tok);
    fparams = Tok.unsafe_fake_bracket [];
    frettype = None;
    fcaptures = G.no_captures;
    fbody = G.FBNothing;
  } in
  let synth_func_info_for_class
      (class_il : IL.name) (name, tok) : FA.func_info =
    let m_il = IL.{
      ident = (name, tok);
      sid = G.SId.unsafe_default;
      id_info = G.empty_id_info ();
    } in
    { FA.fn_id = Func_info.method_id ~cls:class_il ~meth:m_il;
      entity = None;
      fdef = placeholder_fdef tok }
  in
  let phase1_per_file (fi : file_info)
      : Fpath.t * FA.func_info list * Symbol_table.t =
    (* Go interface methods come as a [TypeDef] with [TyRecordAnon(Interface)],
       not a [ClassDef], so attribute them to the interface. *)
    let interface_methods =
      if not (Lang.equal lang Lang.Go) then []
      else
        List.fold_left (fun acc obs ->
          match obs with
          | Walker.Observation.Type_def { ent; tdef } ->
            (match tdef with
             | { G.tbody = G.NewType
                   { G.t = G.TyRecordAnon ((G.Interface, _),
                                              (_, fields, _)); _ } } ->
               (match ent.G.name with
                | G.EN ((G.Id _ | G.IdQualified _) as iface_name) ->
                  let iface_il = AST_to_IL.var_of_name iface_name in
                  List.fold_left (fun acc field ->
                    match field with
                    | G.F { G.s = G.DefStmt (m_ent, G.FuncDef m_fdef); _ } ->
                      (match m_ent.G.name with
                       | G.EN ((G.Id _ | G.IdQualified _) as m_name) ->
                         let m_il = AST_to_IL.var_of_name m_name in
                         let fn_id = Func_info.method_id ~cls:iface_il ~meth:m_il in
                         { FA.fn_id;
                           entity = Some m_ent;
                           fdef = m_fdef } :: acc
                       | _ -> acc)
                    | _ -> acc
                  ) acc fields
                | _ -> acc)
             | _ -> acc)
          | _ -> acc
        ) [] fi.fi_observations
    in
    let attributed_to_interface (fn_id : FA.fn_id) : bool =
      match Func_info.bare_name fn_id with
      | None -> false
      | Some (name : IL.name) ->
        List.exists
          (fun (method_ : FA.func_info) ->
            match Func_info.bare_name method_.FA.fn_id with
            | None -> false
            | Some (other : IL.name) -> Function_id.equal_il_name name other)
          interface_methods
    in
    let acc =
      List.fold_left (fun (acc : FA.func_info list) obs ->
        match obs with
        | Walker.Observation.Func_def { opt_ent; parent_path; fdef } ->
          if skip_anon opt_ent then acc
          else
            (match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
             | Some fn_id when not (attributed_to_interface fn_id) ->
               { FA.fn_id; entity = opt_ent; fdef } :: acc
             | Some _
             | None -> acc)
        | _ -> acc
      ) interface_methods fi.fi_observations
    in
    let funcs =
      List.fold_left (fun acc obs ->
        match obs with
        | Walker.Observation.Class_def { ent; cdef } ->
          (match Visit_function_defs.entity_to_il_name ent with
           | Some class_il ->
             List.fold_left (fun acc pair ->
               synth_func_info_for_class class_il pair :: acc
             ) acc (cfg.Index_lang_rules.class_body_synth_methods cdef)
           | None -> acc)
        | _ -> acc
      ) acc fi.fi_observations
    in
    (fi.fi_file, funcs, Symbol_table.create ~lang fi.fi_ast funcs)
  in
  let per_file_funcs =
    timed "call graph: functions per file" @@ fun () ->
    run_per_file caps ~ncores phase1_per_file file_infos
  in
  let all_funcs =
    List.concat_map (function Ok (_, fs, _) -> fs | Error _ -> [])
      per_file_funcs
  in
  let symbol_tables : (string, Symbol_table.t) Hashtbl.t =
    Hashtbl.create (List.length file_infos)
  in
  List.iter
    (function
      | Ok ((file : Fpath.t), _, (table : Symbol_table.t)) ->
        Hashtbl.replace symbol_tables (Fpath.to_string file) table
      | Error _ -> ())
    per_file_funcs;
  (* Returned to the caller: a failed file's functions are MISSING from the
     graph, which silently loses every finding through them unless the
     failure is surfaced as a scan error. *)
  let phase1_failures =
    List.filter_map (function
      | Ok _ -> None
      | Error ((fi : file_info), exn) ->
        Log.warn (fun m -> m "[skip] projidx phase 1 failed on %s: %s"
                    (Fpath.to_string fi.fi_file) (Exception.to_string exn));
        Some (Core_error.exn_to_error ~file:fi.fi_file exn))
      per_file_funcs
  in
  List.iter (fun (func : FA.func_info) ->
    match FA.fn_id_to_node func.FA.fn_id with
    | Some node -> Call_graph.G.add_vertex graph node
    | None -> ()
  ) all_funcs;

  let project_class_names : Object_initialization.class_names =
    List.fold_left
      (fun (acc : Object_initialization.class_names) (fi : file_info) ->
        Object_initialization.add_class_names
          (Object_initialization.collect_class_names fi.fi_ast) acc)
      Object_initialization.no_class_names file_infos
  in
  Log.debug (fun m -> m "Project class names: %d (interfile object_mappings)"
    (Object_initialization.count_class_names project_class_names));

  let funcs_by_name : (string, FA.func_info list) Hashtbl.t =
    Hashtbl.create (List.length all_funcs * 2)
  in
  List.iter (fun (func : FA.func_info) ->
    let add_name name =
      let cur = Option.value (Hashtbl.find_opt funcs_by_name name) ~default:[] in
      Hashtbl.replace funcs_by_name name (func :: cur)
    in
    let bare_name =
      Option.map (fun name -> fst name.IL.ident)
        (Func_info.bare_name func.FA.fn_id)
    in
    Option.iter add_name bare_name;
    (* A named lambda carries the synthetic bare name [_tmp_lambda], so this
       code also indexes the lambda under the name of the variable it is bound
       to, and [handler(...)] then resolves. *)
    (match func.FA.entity with
     | Some ent ->
       (match Index_lang_rules.entity_simple_name ent with
        | Some entity_name when (match bare_name with
                                 | Some fn_bare_name ->
                                   not (String.equal fn_bare_name entity_name)
                                 | None -> true) ->
            add_name entity_name
        | _ -> ())
     | None -> ())
  ) all_funcs;
  let project_funcs_by_name = funcs_by_name in

  let admitted_classes : unit Common.SMap.t =
    let admits_own_entity : bool Common.SMap.t =
      List.fold_left
        (fun (gated : bool Common.SMap.t) (entry : entry) ->
          match entry.kind with
          | K_class
          | K_companion ->
            Common.SMap.add (Names.Def_qn.to_string entry.qn)
              (cfg.Index_lang_rules.project_scope_admits entry.entity) gated
          | K_function
          | K_method -> gated)
        Common.SMap.empty indexed_entries
    in
    let rec owners_admit (class_qn : Names.Class_qn.t) : bool =
      match Names.Class_qn.split_last class_qn with
      | None -> true
      | Some ((owner : Names.Class_qn.t), _) ->
        Names.Class_qn.is_empty owner
        || (match
              Common.SMap.find_opt (Names.Class_qn.to_string owner)
                admits_own_entity
            with
            | None -> true
            | Some (admitted : bool) -> admitted && owners_admit owner)
    in
    Common.SMap.filter
      (fun (class_qn : string) (admitted : bool) ->
        admitted && owners_admit (Names.Class_qn.of_string class_qn))
      admits_own_entity
    |> Common.SMap.map (fun (_ : bool) -> ())
  in
  let classes_by_file : entry list Common.SMap.t =
    List.fold_left
      (fun (by_file : entry list Common.SMap.t) (ci : entry) ->
        match ci.kind with
        | K_class
        | K_companion ->
          let key = Fpath.to_string ci.file in
          Common.SMap.add key
            (ci :: Option.value (Common.SMap.find_opt key by_file) ~default:[])
            by_file
        | K_function
        | K_method -> by_file)
      Common.SMap.empty indexed_entries
  in

  let t_indexes_start = Unix.gettimeofday () in
  let file_funcs_index = Type_augment.build_file_funcs_index all_funcs in
  Log.debug (fun m -> m "File-funcs index: %d files"
    (Hashtbl.length file_funcs_index));

  (* Project-wide free-function indexes.  See [Func_index]. *)
  let project_funcs_by_module =
    Func_index.build_by_module ~cfg ~file_infos all_funcs
  in
  Log.debug (fun m -> m "Per-module func index: %d modules (Per_file only)"
    (Hashtbl.length project_funcs_by_module));

  let dunder_all = Reexports.build_dunder_all ~file_infos in

  (* Re-export pass for the languages whose imports re-export.  See
     [Reexports]. *)
  if cfg.Index_lang_rules.has_reexports then
    Reexports.resolve_into_module_index ~cfg
      ~project_funcs_by_module ~dunder_all file_infos
    |> List.iter (fun (qn, funcs) ->
         Hashtbl.replace project_funcs_by_module qn funcs);

  (* Defining-file -> package module qn; disambiguates same-basename packages
     for resolving methods with the same simple name. *)
  let file_module_qn : (string, Names.Module_qn.t) Hashtbl.t =
    let index = Hashtbl.create (List.length file_infos) in
    List.iter (fun (fi : file_info) ->
      Hashtbl.replace index (Fpath.to_string fi.fi_file) fi.fi_module_path
    ) file_infos;
    index
  in

  (* Per-AST [extract_calls] into an edge list; graph mutated only in the
     merge step. *)
  Log_interfile_timing.Log.info (fun m ->
      m "[interfile timing] project index: call graph: indexes (exports, \
         packages, modules, re-exports, visibility): %.2fs"
        (Unix.gettimeofday () -. t_indexes_start));
  let funcs_by_id = build_funcs_by_id all_funcs in
  let (definitions_by_qn : definition Common.SMap.t),
      (companions : Func_lookup.companion_index) =
    timed "call graph: definitions by qualified name" (fun () ->
      build_definitions_by_qn ~entries:indexed_entries ~funcs_by_id
        ~reexport_map)
  in
  let class_parent_paths =
    timed "call graph: class parent paths" (fun () ->
      build_class_parent_paths ~entries:indexed_entries ~funcs_by_id)
  in
  let value_alias_index = Pipeline.build_value_alias_index file_infos in
  let module_scope =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_module ->
      timed "call graph: module exports" (fun () ->
        Scope_module.build_project_scope ~definitions_by_qn ~value_alias_index
          ~classes_by_file ~class_parent_paths ~file_funcs_index
          ~file_infos:indexed_files)
    | `Per_file
    | `Per_crate
    | `Per_constant_path
    | `Per_directory
    | `Per_go_package
    | `Per_package
    | `Per_namespace
    | `Per_translation_unit
    | `Per_project -> Scope_module.no_project_scope
  in
  let attributes_by_module =
    timed "call graph: attributes by module" (fun () ->
      Func_index.build_attributes_by_module ~cfg ~dunder_all
        ~exported:
          (match cfg.Index_lang_rules.unqualified_scope with
           | `Per_module ->
             Func_index.Only_exported_names
               (Scope_module.exported_names
                  (Scope_module.exports_of module_scope))
           | `Per_project ->
             if cfg.Index_lang_rules.module_is_returned_value then
               Func_index.Only_exported_names Common.SMap.empty
             else Func_index.Every_definition_is_an_attribute
           | `Per_file
           | `Per_crate
           | `Per_constant_path
           | `Per_directory
           | `Per_go_package
           | `Per_package
           | `Per_namespace
           | `Per_translation_unit ->
             Func_index.Every_definition_is_an_attribute)
        ~definitions_by_qn ~file_infos:indexed_files)
  in
  let pipeline_ctx : Pipeline.ctx =
    { Pipeline.lang;
      cfg;
      type_state = Type_state.empty;
      definitions_by_qn;
      companions;
      attributes_by_module;
      namespace_scope_bindings =
        timed "call graph: namespace bindings" (fun () ->
          match cfg.Index_lang_rules.unqualified_scope with
          | `Per_namespace
          | `Per_translation_unit ->
            Scope_binding.build_namespace_scope_bindings ~attributes_by_module
              ~file_infos:indexed_files
          | `Per_file
          | `Per_crate
          | `Per_constant_path
          | `Per_directory
          | `Per_go_package
          | `Per_module
          | `Per_project
          | `Per_package -> Common.SMap.empty);
      php_global_bindings =
        (match cfg.Index_lang_rules.unqualified_scope with
         | `Per_namespace ->
           Scope_php.global_function_bindings ~attributes_by_module
         | `Per_file
         | `Per_crate
         | `Per_constant_path
         | `Per_directory
         | `Per_go_package
         | `Per_module
         | `Per_package
         | `Per_translation_unit
         | `Per_project -> []);
      include_map =
        timed "call graph: include closures" (fun () ->
          match cfg.Index_lang_rules.unqualified_scope with
          | `Per_translation_unit ->
            Include_map.build ~file_infos:indexed_files ~file_funcs_index
          | `Per_file
          | `Per_crate
          | `Per_constant_path
          | `Per_directory
          | `Per_go_package
          | `Per_module
          | `Per_namespace
          | `Per_project
          | `Per_package -> Include_map.empty);
      module_scope;
      go_packages;
      build_constraints =
        (if Lang.equal lang Lang.Go then
           timed "call graph: build constraints" (fun () ->
             Go_build_constraints.of_files
               (List.map (fun (fi : file_info) -> (fi.fi_file, fi.fi_ast))
                  file_infos))
         else Go_build_constraints.empty);
      top_level_scope =
        timed "call graph: top level constants" (fun () ->
          match cfg.Index_lang_rules.unqualified_scope with
          | `Per_constant_path
          | `Per_project ->
            Func_lookup.scope_table_of_map
              (Scope_binding.bindings_of_positioned
                 (Scope_binding.top_level_bindings
                    ~keep:cfg.Index_lang_rules.project_scope_admits
                    ~definitions_by_qn))
          | `Per_file
          | `Per_crate
          | `Per_directory
          | `Per_go_package
          | `Per_module
          | `Per_namespace
          | `Per_package
          | `Per_translation_unit -> Func_lookup.empty_scope_table);
      module_object_by_module =
        List.fold_left
          (fun (by_module : Names.Class_qn.t Common.SMap.t) (fi : file_info) ->
            match fi.fi_module_object with
            | None -> by_module
            | Some (class_qn : Names.Class_qn.t) ->
              Common.SMap.add
                (Names.Module_qn.to_string fi.fi_module_path) class_qn
                by_module)
          Common.SMap.empty indexed_files;
      object_classes =
        (if cfg.Index_lang_rules.object_members_bind_in_namespace then
           List.fold_left
             (fun (objects : unit Common.SMap.t) (entry : entry) ->
               match (entry.kind, entry.entity) with
               | K_class, Some { G.name = G.EN (name : G.name); _ } -> (
                 match
                   ( Class_table.binding_of_id_info
                       (Class_table.id_info_of_name name),
                     Hashtbl.find_opt symbol_tables
                       (Fpath.to_string entry.file) )
                 with
                 | Some (sid : G.SId.t), Some (table : Symbol_table.t) -> (
                   match Symbol_table.class_of_binding table sid with
                   | Some
                       { Symbol_table.kind = Symbol_table.Class_kind G.Object;
                         _ } ->
                     Common.SMap.add (Names.Def_qn.to_string entry.qn) ()
                       objects
                   | Some _
                   | None -> objects)
                 | _ -> objects)
               | _ -> objects)
             Common.SMap.empty indexed_entries
         else Common.SMap.empty);
      dunder_all;
      extensions_by_module =
        timed "call graph: extension methods by module" (fun () ->
          Func_index.build_extensions_by_module ~definitions_by_qn);
      nested_types_by_class =
        timed "call graph: nested types by class" (fun () ->
          Func_index.build_nested_types_by_class ~definitions_by_qn);
      classes_by_file;
      class_parent_paths;
      global_imports =
        List.concat_map
          (fun (fi : file_info) ->
            List.filter (fun (imp : import) -> imp.im_global) fi.fi_imports)
          indexed_files;
      project_funcs_by_name;
      project_funcs_by_module;
      file_module_qn;
      project_class_names;
      file_funcs_index;
      top_level_node_for;
      stamp_var_types =
        (fun ~table ~type_state ~caller ast ->
          Type_augment.stamp_var_types_from_bodies ~table ~type_state ~caller
            ast);
      value_alias_index;
    }
  in
  let per_file_scopes =
    timed "call graph: scope tables" @@ fun () ->
    run_per_file caps ~ncores
      (fun (fi : file_info) -> (fi.fi_file, Pipeline.file_scope_of pipeline_ctx fi))
      file_infos
  in
  let file_scopes : (string, Pipeline.file_scope) Hashtbl.t =
    Hashtbl.create (List.length file_infos)
  in
  let scope_failures =
    List.filter_map
      (function
        | Ok ((file : Fpath.t), (scope : Pipeline.file_scope option)) ->
          Option.iter (Hashtbl.replace file_scopes (Fpath.to_string file)) scope;
          None
        | Error ((fi : file_info), exn) ->
          Log.warn (fun m -> m "[skip] projidx scope table failed on %s: %s"
                      (Fpath.to_string fi.fi_file) (Exception.to_string exn));
          Some (Core_error.exn_to_error ~file:fi.fi_file exn))
      per_file_scopes
  in
  let scope_of_file (fi : file_info) : Pipeline.file_scope option =
    Hashtbl.find_opt file_scopes (Fpath.to_string fi.fi_file)
  in
  let classes =
    timed "call graph: class table" @@ fun () ->
    build_class_table ~lang ~cfg
      ~build_constraints:pipeline_ctx.Pipeline.build_constraints
      ~definitions_by_qn ~entries:indexed_entries
      ~admitted_classes
      ~class_aliases:(Scope_module.class_aliases_of module_scope)
      (List.filter_map
         (fun (fi : file_info) ->
           Option.map
             (fun (table : Symbol_table.t) ->
               { lf_info = fi;
                 lf_table = table;
                 lf_lookup =
                   Pipeline.func_lookup_of pipeline_ctx
                     ~class_of_qn:(fun (_ : Names.Class_qn.t) -> None)
                     (scope_of_file fi) fi;
                 lf_package =
                   Module_paths.enclosing_package ~cfg ~file:fi.fi_file
                     fi.fi_module_path })
             (Hashtbl.find_opt symbol_tables (Fpath.to_string fi.fi_file)))
         indexed_files)
  in
  let project_tables : (string, Symbol_table.t * Func_lookup.t) Hashtbl.t =
    Hashtbl.create (List.length file_infos)
  in
  timed "call graph: project tables" (fun () ->
    List.iter
      (fun (fi : file_info) ->
        let key = Fpath.to_string fi.fi_file in
        Option.iter
          (fun (file_table : Symbol_table.t) ->
            Hashtbl.replace project_tables key
              (Pipeline.project_table pipeline_ctx ~classes ~file_table
                 (scope_of_file fi) fi))
          (Hashtbl.find_opt symbol_tables key))
      file_infos);
  let table_of_file (file : Fpath.t) : Symbol_table.t option =
    Option.map fst (Hashtbl.find_opt project_tables (Fpath.to_string file))
  in
  let type_state =
    timed "call graph: declared types" @@ fun () ->
    Type_augment.build_fields_by_class_index ~cfg ~table_of_file
      (Type_augment.populate_returns_from_decls ~table_of_file Type_state.empty
         all_funcs)
      file_infos
  in
  (* Cross-type inference fixpoint: alternate body-return-types and
     self-assignment field-types until neither adds anything.  Rebuild
     [caller_arg_types] between passes so fresh return types feed the next;
     compare on [Type_state] only (the Hashtbl is derived). *)
  let outer_step (ts, _car) =
    let ts =
      Type_augment.augment_return_types_from_bodies ~table_of_file
        ~type_state:ts all_funcs
    in
    let car =
      Type_augment.build_caller_arg_types ~table_of_file ~type_state:ts
        ~funcs_by_file:file_funcs_index file_infos
    in
    let ts =
      Type_augment.augment_fields_from_self_assignments ~lang
        ~caller_arg_types:car ~cfg ~table_of_file ~type_state:ts all_funcs
    in
    (ts, car)
  in
  let outer_equal (a, _) (b, _) = Type_state.equal a b in
  let (type_state, caller_arg_types), outer_iters =
    timed "call graph: type inference fixpoint" @@ fun () ->
    Fixpoint.run
      ~equal:outer_equal
      ~step:outer_step
      ~max_iterations:Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES
      (type_state, Hashtbl.create 0)
  in
  Log.debug (fun m -> m "Body-inferred type fixpoint: %d outer passes, %d caller-arg-types"
    outer_iters (Hashtbl.length caller_arg_types));
  (* [Fixpoint.run] returns [i = max_iterations] only on the cap branch. *)
  if outer_iters >= Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES then
    Log.warn (fun m ->
        m "Body-inferred type fixpoint hit the %d-pass cap without \
           converging; inferred types may be incomplete"
          Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES);
  let type_state =
    timed "call graph: module singletons and value types" @@ fun () ->
    Type_augment.add_value_type_sites ~lang ~table_of_file
      (Type_augment.build_module_singleton_types ~table_of_file type_state
         file_infos)
      all_funcs
  in
  let pipeline_ctx = { pipeline_ctx with Pipeline.type_state } in
  (* per-file wall time of the edge walk, across domains, for the slowest
     files: the pass is parallel, so a few slow files bound its wall time *)
  let file_secs : (Fpath.t * float) list ref = ref [] in
  let file_secs_mutex = Mutex.create () in
  let edges_for_file fi =
    let res, secs =
      Common.with_time (fun () ->
          match Hashtbl.find_opt project_tables (Fpath.to_string fi.fi_file) with
          | Some ((table : Symbol_table.t), (func_lookup : Func_lookup.t)) ->
            Pipeline.edges_for_file pipeline_ctx ~classes ~table ~func_lookup fi
          | None -> [])
    in
    Mutex.lock file_secs_mutex;
    file_secs := (fi.fi_file, secs) :: !file_secs;
    Mutex.unlock file_secs_mutex;
    res
  in
  (* Pre-populate [<top_level>] nodes BEFORE the parallel phase: the table and
     graph are read-only across domains after this. *)
  List.iter (fun (fi : file_info) ->
    let key = Fpath.to_string fi.fi_file in
    if not (Hashtbl.mem top_level_nodes key) then begin
      let node =
        Function_id.of_il_name (FA.top_level_name_of_ast fi.fi_ast)
      in
      Hashtbl.replace top_level_nodes key node;
      Call_graph.G.add_vertex graph node
    end
  ) file_infos;
  (* Largest-first so megafiles don't stall the tail on one worker; stat once
     (decorate-sort-undecorate) to keep the comparator pure. *)
  let file_size_of fi =
    Nonfatal.catch ~default:0 (fun () ->
      (Unix.stat (Fpath.to_string fi.fi_file)).Unix.st_size)
  in
  let file_infos =
    file_infos
    |> List_.map (fun fi -> (file_size_of fi, fi))
    |> List.sort (fun (a, _) (b, _) -> Int.compare b a)
    |> List_.map snd
  in
  let per_file_edges =
    timed "call graph: edges per file" @@ fun () ->
    run_per_file caps ~ncores edges_for_file file_infos
  in
  (* stage split of the edge pass, and the shape of the name table it
     resolves calls against *)
  Log_interfile_timing.Log.info (fun m ->
      let stages =
        Pipeline.edge_stage_report ()
        |> List.map (fun (name, secs) -> Printf.sprintf "%s %.1fs" name secs)
        |> String.concat ", "
      in
      let n_names = Hashtbl.length project_funcs_by_name in
      let sizes =
        Hashtbl.fold
          (fun name funcs acc -> (name, List.length funcs) :: acc)
          project_funcs_by_name []
        |> List.sort (fun (_, a) (_, b) -> compare b a)
      in
      let total = List.fold_left (fun acc (_, n) -> acc + n) 0 sizes in
      let top =
        List.filteri (fun i _ -> i < 8) sizes
        |> List.map (fun (name, n) -> Printf.sprintf "%s:%d" name n)
        |> String.concat " "
      in
      m "[interfile timing] project index: edge pass stages (CPU sum): %s; \
         name table: %d names, %d funcs, largest buckets %s"
        stages n_names total top);
  Log_interfile_timing.Log.info (fun m ->
      let slowest =
        List.sort (fun (_, a) (_, b) -> compare b a) !file_secs
        |> List.filteri (fun i _ -> i < 8)
        |> List.map (fun (file, secs) ->
               Printf.sprintf "%s %.1fs" (Fpath.to_string file) secs)
        |> String.concat ", "
      in
      let total = List.fold_left (fun acc (_, s) -> acc +. s) 0. !file_secs in
      m "[interfile timing] project index: edge pass in-domain total %.1fs \
         over %d files (compare with the stage sum: the rest is outside the \
         timed stages); slowest files: %s"
        total (List.length !file_secs) slowest);
  (* A failed file's outgoing call edges are MISSING from the graph; the
     failure list is returned so the engine can surface it as a scan error. *)
  let t_merge_start = Unix.gettimeofday () in
  let n_emitted =
    List.fold_left
      (fun n -> function Ok edges -> n + List.length edges | Error _ -> n)
      0 per_file_edges
  in
  let phase2_failures =
    timed (Printf.sprintf "call graph: add call edges (%d emitted)" n_emitted)
    @@ fun () ->
    List.filter_map (function
      | Ok edges ->
        List.iter (fun (src, dst, call_tok) ->
          Call_graph.add_edge graph ~src ~dst ~call_tok)
          edges;
        None
      | Error ((fi : file_info), exn) ->
        Log.warn (fun m -> m "[skip] projidx phase 2 failed on %s: %s"
                    (Fpath.to_string fi.fi_file) (Exception.to_string exn));
        Some (Core_error.exn_to_error ~file:fi.fi_file exn))
      per_file_edges
  in
  (* Interface dispatch edges.  See [Structural_dispatch]. *)
  let n_dispatch =
    timed "call graph: interface dispatch edges" @@ fun () ->
    Structural_dispatch.emit_dispatch_edges
      ~lang ~class_table:classes.Pipeline.class_table ~graph
  in
  if n_dispatch > 0 then
    Log.debug (fun m -> m "Interface dispatch: emitted %d Dispatch edges"
      n_dispatch);
  (* Nominal override dispatch: a subclass method shadowing a body-less
     ancestor decl (abstract method).  Same edge shape as interface
     dispatch (impl -> decl), so the reachability dispatch closure treats
     both alike. *)
  let override_pairs : (FA.func_info * FA.func_info) list =
    let arity (func : FA.func_info) : int =
      Receiver.arity lang ~is_method:(Receiver.is_method func.FA.fdef)
        ~is_static:(Receiver.is_static func.FA.entity)
        (Tok.unbracket func.FA.fdef.G.fparams)
    in
    let compiled_together =
      Go_build_constraints.compiled_together pipeline_ctx.Pipeline.build_constraints
    in
    let declared_only (func : FA.func_info) : bool =
      match func.FA.fdef.G.fbody with
      | G.FBDecl _
      | G.FBNothing -> true
      | G.FBStmt _
      | G.FBExpr _ -> false
    in
    List.concat_map
      (fun (cls : Class_table.cls) ->
        let ancestors =
          match (Class_table.order classes.Pipeline.class_table cls).Linearisation.order with
          | _ :: (rest : Class_table.cls list) -> rest
          | [] -> []
        in
        Common.SMap.fold
          (fun (name : string) (own : FA.func_info list)
               (pairs : (FA.func_info * FA.func_info) list) ->
            List.concat_map
              (fun (ancestor : Class_table.cls) ->
                List.concat_map
                  (fun (declared : FA.func_info) ->
                    if declared_only declared then
                      List.map
                        (fun (overriding : FA.func_info) -> (overriding, declared))
                        (List.fold_left
                           (fun (kept : FA.func_info list)
                                (overriding : FA.func_info) ->
                             if
                               Int.equal (arity overriding) (arity declared)
                               && compiled_together [ overriding; declared ]
                               && not
                                    (List.exists
                                       (fun (earlier : FA.func_info) ->
                                         compiled_together [ earlier; overriding ])
                                       kept)
                             then kept @ [ overriding ]
                             else kept)
                           [] own)
                    else [])
                  (Class_table.own_members ancestor name))
              ancestors
            @ pairs)
          (Class_table.member_table cls) [])
      (Class_table.classes classes.Pipeline.class_table)
  in
  let n_override =
    timed "call graph: override dispatch edges" @@ fun () ->
    List.fold_left
      (fun n ((c_m : FA.func_info), (p_m : FA.func_info)) ->
        match FA.fn_id_to_node c_m.FA.fn_id, FA.fn_id_to_node p_m.FA.fn_id with
        | Some src, Some dst ->
          let call_tok =
            match c_m.FA.fn_id with
            | [_; Some m_il] -> snd m_il.IL.ident
            | _ -> snd c_m.FA.fdef.G.fkind
          in
          Call_graph.add_edge ~kind:Call_graph.Dispatch graph
            ~src ~dst ~call_tok;
          n + 1
        | _ -> n)
      0 override_pairs
  in
  if n_override > 0 then
    Log.debug (fun m -> m "Override dispatch: emitted %d Dispatch edges"
      n_override);
  (* Same arity overloads of one scope form a group; for a language whose
     top level scope is the project, that scope is the whole project. See
     [Structural_dispatch]. *)
  let n_overload =
    timed "call graph: overload dispatch edges" @@ fun () ->
    Structural_dispatch.emit_overload_edges ~lang ~cfg ~graph
      ~class_table:classes.Pipeline.class_table all_funcs
  in
  if n_overload > 0 then
    Log.debug (fun m -> m "Overload dispatch: emitted %d Dispatch edges"
      n_overload);
  let inherited_by_class : (entry * FA.func_info list) list =
    List.filter_map
      (fun (entry : entry) ->
        match (entry.kind, entry.entity) with
        | K_class, Some { G.name = G.EN (name : G.name); _ } ->
          Option.bind
            (Option.bind
               (Class_table.binding_of_id_info (Class_table.id_info_of_name name))
               (Class_table.class_of_binding classes.Pipeline.class_table))
            (fun (cls : Class_table.cls) ->
              let own = Class_table.member_table cls in
              let inherited =
                match
                  (Class_table.order classes.Pipeline.class_table cls)
                    .Linearisation.order
                with
                | _ :: (ancestors : Class_table.cls list) ->
                  Common.SMap.fold
                    (fun (member : string) (funcs : FA.func_info list)
                         (inherited : FA.func_info list) ->
                      if Common.SMap.mem member own then inherited
                      else funcs @ inherited)
                    (Class_table.members_along ancestors)
                    []
                | [] -> []
              in
              match inherited with
              | [] -> None
              | _ :: _ -> Some (entry, inherited))
        | _ -> None)
      indexed_entries
  in
  Log_interfile_timing.Log.info (fun m ->
      m "[interfile timing] project index: call graph: edge merge (add edges, \
         dispatch/override/overload): %.2fs"
        (Unix.gettimeofday () -. t_merge_start));
  (graph, inherited_by_class, phase1_failures @ scope_failures @ phase2_failures, type_state)

let project_root_abs_of (project_root : Fpath.t) : Fpath.t =
  fst (Fpath_.absolutify ~cwd:(Fpath.v (Sys.getcwd ())) project_root)

let run_pipeline (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int * file_info list
    * (Fpath.t * Tok.location list) list * Core_error.t list * Type_state.t =
  let cfg = Index_lang_rules.for_lang lang in
  (* Absolutize paths: interface dispatch's [family_key] needs consistent
     directory prefixes. *)
  let project_root_abs = project_root_abs_of project_root in
  (* One walk of the build configuration; its excludes keep the CLI and the
     embedded engine indexing the same files. *)
  let discovered =
    cfg.Index_lang_rules.discover_project ~project_root:project_root_abs
  in
  let excludes = excludes @ discovered.Index_lang_rules.excludes in
  let files =
    timed "discover files" @@ fun () ->
    Discover.discover_files ~targeting_conf
      ~lang ~project_root ~includes ~excludes
  in
  let n_total = List.length files in
  Log.info (fun m -> m "Discovered %d %s files. Parsing with %d domain(s)..."
    n_total (Lang.to_string lang) ncores);
  let absolutize (file : Fpath.t) : Fpath.t =
    fst (Fpath_.absolutify ~cwd:project_root_abs file)
  in
  (* Go package identity from [go.mod] (go.work workspaces are not
     parsed); empty for non-Go, so [mp]
     falls back to the path-derived default. *)
  let go_modules =
    if Lang.equal lang Lang.Go
    then Go_modules.discover ~project_root:project_root_abs
           (List.map absolutize files)
    else Go_modules.empty
  in
  let rust_crates =
    if Lang.equal lang Lang.Rust
    then Rust_crates.discover ~project_root:project_root_abs
           (List.map absolutize files)
    else Rust_crates.empty
  in
  let resolution =
    if cfg.Index_lang_rules.specifiers_name_files then
      timed "specifier resolution" @@ fun () ->
      Module_paths.specifier_resolution_of_files ~cfg
        ~project_root:project_root_abs
        ~paths:discovered.Index_lang_rules.module_paths
        (List.map absolutize files)
    else Module_paths.Specifier_is_module_name
  in
  let process (file : Fpath.t) =
    let file = absolutize file in
    let { Parsing_result2.ast; skipped_tokens; _ } =
      Parse_target.parse_and_resolve_name lang file
    in
    let mp =
      Module_paths.module_qn_of_file ~cfg ~go_modules ~rust_crates
        ~project_root ~ast:(Some ast) file
    in
    (Symbols.collect_in_ast ~cfg ~lang ~resolution ~module_path:mp ~file ast,
     (file, skipped_tokens))
  in
  let results =
    timed (Printf.sprintf "parse + symbols (%d files)" n_total) @@ fun () ->
    if ncores <= 1 then
      List.map (fun file ->
        try Ok (process file)
        with
        | (Out_of_memory | Memory_limit.ExceededMemoryLimit _) as exn ->
          Exception.catch_and_reraise exn
        | exn -> Error (file, Exception.catch exn)
      ) files
    else
      Domainslib_.parmap caps
        ~num_domains:ncores
        ~chunksize:1
        ~exception_handler:(fun file exc ->
          match Exception.get_exn exc with
          | Out_of_memory | Memory_limit.ExceededMemoryLimit _ ->
            Exception.reraise exc
          | _ -> (file, exc))
        process
        files
  in
  let scanned, skipped, all_entries, all_files, all_skipped_tokens,
      parse_failures =
    List.fold_left (fun (sc, sk, es, fis, sts, fails) -> function
      | Ok ((entries, fi), (file, skipped_tokens)) ->
        let sts =
          match skipped_tokens with
          | [] -> sts
          | _ :: _ -> (file, skipped_tokens) :: sts
        in
        (sc + 1, sk,
         List.rev_append entries es,
         fi :: fis,
         sts,
         fails)
      | Error (file, exn) ->
        (* [sk] counts failures so far; log the first five only. *)
        if sk < 5 then
          Log.warn (fun m -> m "[skip] %s: %s" (Fpath.to_string file)
                      (Exception.to_string exn));
        (sc, sk + 1, es, fis, sts,
         Core_error.exn_to_error ~file exn :: fails)
    ) (0, 0, [], [], [], []) results
  in
  let parse_failures = List.rev parse_failures in
  let go_packages =
    timed "Go package index" @@ fun () ->
    Scope_go.build_package_index ~cfg ~file_infos:all_files
  in
  let all_files =
    timed "Go import local names" @@ fun () ->
    Imports.with_package_clause_locals ~cfg
      ~clause_of_module:(Scope_go.importable_clause go_packages)
      all_files
  in
  let reexport_map =
    timed "re-export map" @@ fun () ->
    Reexports.build_reexport_map ~cfg all_files
  in
  Log.debug (fun m -> m "Re-export map: %d entries (lang has_reexports=%b)"
    (Hashtbl.length reexport_map) cfg.Index_lang_rules.has_reexports);
  let wrappers : (string, dataclass_wrapper) Hashtbl.t =
    Hashtbl.create 64 in
  List.iter (fun fi ->
    List.iter (fun (wrapper : Index_lang_rules.wrapper) ->
      Hashtbl.replace wrappers wrapper.w_simple_name wrapper
    ) fi.fi_dataclass_wrappers
  ) all_files;
  Log.debug (fun m -> m "Wrappers: %d" (Hashtbl.length wrappers));
  let synth_from_wrappers =
    timed "wrapper synthesis" @@ fun () ->
    Symbols.dataclass_wrapper_synth_entries ~cfg ~wrappers all_entries
  in
  Log.debug (fun m -> m "Wrapper synthesis: %d dunders emitted"
    (List.length synth_from_wrappers));
  let entries_pre_mro = all_entries @ synth_from_wrappers in
  let graph, inherited_by_class, worker_failures, type_state =
    timed "call graph (edges + fixpoint)" @@ fun () ->
    build_project_call_graph caps ~cfg ~lang ~ncores ~entries:entries_pre_mro
      ~reexport_map ~go_packages all_files
  in
  (* Inherited-method entry rows, derived from the same C3 linearisation
     callee resolution reads, so the diagnostic dump matches what resolution
     sees.  Only [collect] consumers use these rows; [collect_resolved]
     discards them.  The derivation is one pass over the C3 output; consider
     gating it to the [collect] path if it ever shows up in profiles. *)
  let inherited =
    timed "inherited method entries" @@ fun () ->
    List.concat_map (fun ((class_entry : entry), funcs) ->
      List.filter_map (fun (func : Func_info.t) ->
        Option.map (fun (meth : IL.name) ->
          let method_name = fst meth.IL.ident in
          { id = Symbols.synth_function_id class_entry.id method_name;
            name = method_name;
            qn = Names.Def_qn.concat class_entry.qn method_name;
            kind = K_method;
            file = class_entry.file; range = class_entry.range;
            defining_class_id = Some class_entry.id;
            entity = None })
          (Func_info.bare_name func.Func_info.fn_id))
        funcs)
      inherited_by_class
  in
  Log.debug (fun m -> m "Inheritance: %d inherited method entries (lang walks_inheritance=%b)"
    (List.length inherited) cfg.Index_lang_rules.walks_inheritance);
  let final_entries = entries_pre_mro @ inherited in
  Log.info (fun m -> m "Call graph: %d vertices, %d edges"
    (Call_graph.G.nb_vertex graph) (Call_graph.G.nb_edges graph));
  (final_entries, graph, scanned, skipped, all_files, all_skipped_tokens,
   parse_failures @ worker_failures, type_state)

let collect (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int =
  let (entries, graph, scanned, skipped, _all_files, _skipped_tokens,
       _failures, _type_state) =
    run_pipeline caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes ~excludes ()
  in
  (entries, graph, scanned, skipped)

let collect_resolved (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : Call_graph.G.t * (string, G.program) Hashtbl.t
    * (string, Tok.location list) Hashtbl.t * Core_error.t list
    * Type_state.t =
  let project_root_abs = project_root_abs_of project_root in
  let absnorm (file : Fpath.t) : string =
    fst (Fpath_.absolutify ~cwd:project_root_abs file) |> Fpath.to_string
  in
  let (_entries, graph, _scanned, _skipped, all_files, all_skipped_tokens,
       failures, type_state) =
    run_pipeline caps ~targeting_conf ~lang ~project_root:project_root_abs
      ~ncores ~includes ~excludes ()
  in
  let tbl = Hashtbl.create (List.length all_files) in
  List.iter (fun (fi : file_info) ->
    Hashtbl.replace tbl (absnorm fi.fi_file) fi.fi_ast)
    all_files;
  let skipped_tokens_tbl = Hashtbl.create (List.length all_skipped_tokens) in
  List.iter (fun ((file : Fpath.t), (locs : Tok.location list)) ->
    Hashtbl.replace skipped_tokens_tbl (absnorm file) locs)
    all_skipped_tokens;
  (graph, tbl, skipped_tokens_tbl, failures, type_state)

let resolve_ast_for_file (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(target : Fpath.t) ()
  : G.program option =
  let project_root_abs = project_root_abs_of project_root in
  let target_key =
    fst (Fpath_.absolutify ~cwd:project_root_abs target) |> Fpath.to_string
  in
  let _graph, asts, _skipped_tokens, _failures, _type_state =
    collect_resolved caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes:[] ~excludes:[] ()
  in
  Hashtbl.find_opt asts target_key
