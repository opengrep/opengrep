module FA = Graph_from_AST
module G = AST_generic
open Types

(* Wall-clock time of the stages of [edges_for_file], summed across every
   file and every domain; [Project_index] logs it after the edge pass. *)
let edge_stage_secs : (string, float) Hashtbl.t = Hashtbl.create 8
let edge_stage_mutex = Mutex.create ()

let staged (name : string) (f : unit -> 'a) : 'a =
  let res, secs = Common.with_time f in
  Mutex.lock edge_stage_mutex;
  let prev = Option.value (Hashtbl.find_opt edge_stage_secs name) ~default:0. in
  Hashtbl.replace edge_stage_secs name (prev +. secs);
  Mutex.unlock edge_stage_mutex;
  res

let edge_stage_report () : (string * float) list =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) edge_stage_secs []
  |> List.sort (fun (_, a) (_, b) -> compare b a)


(* Infer var classes from assignments and stamp them onto
   [id_instance_type]. *)
type stamp_var_types =
  type_state:Type_state.t ->
  slice_element_of_field:(string * string, G.name) Hashtbl.t ->
  G.program ->
  unit

type required_files_narrowing = {
  narrowable_classes : Names.Class_name.t list;
  rev_path_segs_by_file : string list Common.SMap.t;
}

type file_scope = {
  scope_table : Func_lookup.scope_table;
  bound_class_files : (Names.Class_name.t * Fpath.t) list;
}

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  required_files_narrowing : required_files_narrowing option;
  definitions_by_qn : definition Common.SMap.t;
  attributes_by_module : Func_lookup.module_attributes;
  dunder_all : (string, unit) Hashtbl.t Common.SMap.t;
  resolution_orders : Func_lookup.resolution_orders;
  class_qn_by_definition : Func_lookup.class_qn_by_definition;
  methods_by_class : Func_lookup.methods_by_class;
  classes_by_file : class_info list Common.SMap.t;
  class_parent_paths : (Function_id.t * IL.name option list) list Common.SMap.t;
  all_funcs : Func_info.t list;
  project_constructors : Func_lookup.constructor_index;
  project_funcs_by_name : (string, Func_info.t list) Hashtbl.t;
  project_funcs_by_module :
    (Names.Module_qn.t, Func_info.t list) Hashtbl.t;
  file_module_qn : (string, Names.Module_qn.t) Hashtbl.t;
  project_funcs_by_package : (string, Func_info.t list) Hashtbl.t;
  project_class_names : G.name list;
  file_funcs_index : (string, Func_info.t list) Hashtbl.t;
  default_export_class : (string, G.name) Hashtbl.t;
  named_export_classes : (string * string, G.name) Hashtbl.t;
  default_export_fn : (string, Func_info.t) Hashtbl.t;
  path_suffix_index : (string, string list) Hashtbl.t option;
  slice_element_of_field : (string * string, G.name) Hashtbl.t;
  top_level_node_for : Fpath.t -> Function_id.t;
  visible_names_for_file : file_info -> (string, unit) Hashtbl.t;
  stamp_var_types : stamp_var_types;
  resolve_ts_specifier :
    path_suffix_index:(string, string list) Hashtbl.t option ->
    current_file:Fpath.t -> string -> string list;
  (* (module qn string, exported name) -> module-level bare-name alias
     value, for import-value svalue stamping. See
     [build_value_alias_index]. *)
  value_alias_index : (string * string, G.expr) Hashtbl.t;
}

(* Module-level bare-name value aliases ([f = sink] / [const f = sink] at
   the top level of a module): (module qn string, exported name) -> the
   aliased value expression. A name assigned more than once, or to
   anything but a bare non-local name, is dropped — the alias must be a
   static fact for the stamp to be sound. Feeds
   [stamp_import_value_aliases] (issue #499, cross-file alias). *)
let build_value_alias_index (file_infos : file_info list)
  : (string * string, G.expr) Hashtbl.t =
  let acceptable (e : G.expr) : G.expr option =
    match e.G.e with
    | G.N (G.Id (_, ainfo)) -> (
        match !(ainfo.G.id_resolved) with
        | Some ((G.LocalVar | G.Parameter | G.EnclosedVar), _) -> None
        | Some _
        | None -> Some e)
    | _ -> None
  in
  (* [None] = conflicting/unacceptable: poisoned, never stamped. *)
  let tbl : (string * string, G.expr option) Hashtbl.t = Hashtbl.create 16 in
  let record key rhs =
    match Hashtbl.find_opt tbl key with
    | Some _ -> Hashtbl.replace tbl key None
    | None -> Hashtbl.replace tbl key (acceptable rhs)
  in
  List.iter (fun (fi : file_info) ->
    let module_str = Names.Module_qn.to_string fi.fi_module_path in
    (* Module level only: the program's top-level statements. *)
    List.iter (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.ExprStmt
          ({ e = G.Assign ({ e = G.N (G.Id ((name, _), _)); _ }, _, rhs);
             _ }, _) ->
        record (module_str, name) rhs
      | G.DefStmt
          ({ G.name = G.EN (G.Id ((name, _), _)); _ },
           G.VarDef { G.vinit = Some rhs; _ }) ->
        record (module_str, name) rhs
      | _ -> ())
      fi.fi_ast)
    file_infos;
  let out = Hashtbl.create 16 in
  Hashtbl.iter (fun k v ->
    match v with
    | Some e -> Hashtbl.replace out k e
    | None -> ()) tbl;
  out

(* Issue #499, cross-file alias: [xlib.py: f = sink] gives [f] a [Sym]
   svalue in xlib itself via naming, but an importing file's [f] is a
   fresh binding naming knows nothing about. Stamp the importing file's
   uses of a from-imported name whose target module exports it as a
   module-level bare-name alias; matching with [symbolic_propagation]
   then sees through the import exactly as through a local alias. Only
   empty svalue slots on [ImportedEntity]-resolved ids are written, so
   shadowing locals are untouched. *)
let stamp_import_value_aliases
    ~(value_alias_index : (string * string, G.expr) Hashtbl.t)
    (fi : file_info) : unit =
  if Hashtbl.length value_alias_index = 0 then ()
  else begin
    let by_local : (string, G.expr) Hashtbl.t = Hashtbl.create 4 in
    List.iter (fun (imp : import) ->
      let local = imp.im_local in
      (* From-imports record the full entity path (module ++ name). *)
      match List.rev (Names.Module_qn.parts imp.im_target) with
      | name :: (_ :: _ as rev_module) -> (
          let module_str =
            Names.Module_qn.of_parts (List.rev rev_module)
            |> Names.Module_qn.to_string
          in
          match Hashtbl.find_opt value_alias_index (module_str, name) with
          | Some value when not (Hashtbl.mem by_local local) ->
            Hashtbl.replace by_local local value
          | _ -> ())
      | _ -> ())
      fi.fi_imports;
    if Hashtbl.length by_local > 0 then begin
      let visitor =
        object
          inherit [_] G.iter as super

          method! visit_expr () e =
            (match e.G.e with
            | G.N (G.Id ((s, _), info)) -> (
                match
                  (Hashtbl.find_opt by_local s,
                   !(info.G.id_resolved),
                   !(info.G.id_svalue))
                with
                | Some value, Some (G.ImportedEntity _, _), None ->
                  info.G.id_svalue := Some (G.Sym value)
                | _ -> ())
            | _ -> ());
            super#visit_expr () e
        end
      in
      visitor#visit_program () fi.fi_ast
    end
  end

(* Detect ctor/import-derived var classes and stamp them onto
   [id_instance_type].
   Side effect on [visible]: extends it with discovered class names so
   [build_funcs_by_name] keeps their methods. *)
let stamp_base_var_types
    ~(lang : Lang.t)
    ~(project_class_names : G.name list)
    ~(default_export_class : (string, G.name) Hashtbl.t)
    ~(named_export_classes : (string * string, G.name) Hashtbl.t)
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    ~(visible : (string, unit) Hashtbl.t)
    (fi : file_info) : unit =
  let import_facts =
    List.fold_left (fun acc (local, specifier, kind) ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
      in
      let cls_opt =
        List.find_map (fun path ->
          match kind with
          | I_default -> Hashtbl.find_opt default_export_class path
          | I_named name ->
            (match Hashtbl.find_opt named_export_classes (path, name) with
             | Some _ as found -> found
             | None -> Hashtbl.find_opt default_export_class path)
          | I_namespace ->
            Hashtbl.find_opt default_export_class path
        ) candidates
      in
      match cls_opt with
      | Some cls ->
        let local_id = G.Id ((local, Tok.unsafe_fake_tok local),
                             G.empty_id_info ()) in
        (local_id, cls) :: acc
      | None -> acc
    ) [] fi.fi_import_specifiers
  in
  let ctor_facts =
    Object_initialization.detect_object_initialization
      ~extra_class_names:project_class_names fi.fi_ast lang
  in
  let facts = import_facts @ ctor_facts in
  List.iter (fun (_var, class_name) ->
    match class_name with
    | G.Id ((name_str, _), _) -> Hashtbl.replace visible name_str ()
    | _ -> ()
  ) facts;
  Object_initialization.stamp_id_types facts fi.fi_ast

let build_alias_to_module_qn
    ~(cfg : Index_lang_rules.t) (fi : file_info)
  : (string, Names.Module_qn.t) Hashtbl.t option =
  match cfg.Index_lang_rules.unqualified_scope with
  | `Per_file | `Per_directory ->
    let tbl : (string, Names.Module_qn.t) Hashtbl.t = Hashtbl.create 16 in
    List.iter (fun (imp : import) ->
      let local = imp.im_local in
      let target_qn = imp.im_target in
      if String.length local > 0
         && not (Names.Module_qn.is_empty target_qn) then begin
        let first_seg =
          match Names.Module_qn.parts target_qn with
          | [] -> Names.Module_qn.to_string target_qn
          | first_part :: _ -> first_part
        in
        let bound_qn =
          if String.equal local first_seg
          then Names.Module_qn.of_string local
          else target_qn
        in
        Hashtbl.replace tbl local bound_qn
      end
    ) fi.fi_imports;
    if Int.equal (Hashtbl.length tbl) 0 then None else Some tbl
  | _ -> None

let build_file_funcs_by_package
    ~(cfg : Index_lang_rules.t)
    ~(project_funcs_by_package : (string, Func_info.t list) Hashtbl.t)
    ~(project_funcs_by_module :
        (Names.Module_qn.t, Func_info.t list) Hashtbl.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    (fi : file_info)
  : Func_lookup.bare_name_index option =
  (* Import aliases in a per-file table; the shared project table is read
     only. *)
  let over_project (alias_extra : (string * Func_info.t list) list) =
    let aliases = Hashtbl.create (List.length alias_extra) in
    List.iter (fun (key, funcs) -> Hashtbl.replace aliases key funcs) alias_extra;
    Func_lookup.bare_name_index_override
      ~front:(Func_lookup.bare_name_index_of_hashtbl aliases)
      ~back:(Func_lookup.bare_name_index_of_hashtbl project_funcs_by_package)
  in
  if cfg.Index_lang_rules.unqualified_scope = `Per_directory then begin
    let alias_extra = List.filter_map (fun (imp : import) ->
      let target_str = Names.Module_qn.to_string imp.im_target in
      let basename = Filename.basename target_str in
      if String.equal imp.im_local basename then None
      else
        match Hashtbl.find_opt project_funcs_by_package basename with
        | Some fs -> Some (imp.im_local, fs)
        | None -> None
    ) fi.fi_imports in
    if alias_extra = [] then
      Some (Func_lookup.bare_name_index_of_hashtbl project_funcs_by_package)
    else Some (over_project alias_extra)
  end
  else begin
    let alias_extra_ts = List.filter_map (fun (local, specifier, _kind) ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
      in
      let funcs =
        List.concat_map (fun path ->
          Option.value (Hashtbl.find_opt file_funcs_index path) ~default:[]
          (* an import sees the file's methods and free functions, not
             its nested ones *)
          |> List.filter (fun (func : Func_info.t) ->
                 Option.is_some (Func_info.as_method func.Func_info.fn_id)
                 || Option.is_some (Func_info.as_free func.Func_info.fn_id))
        ) candidates
      in
      if funcs = [] then None else Some (local, funcs)
    ) fi.fi_import_specifiers in
    let alias_extra_py = List.filter_map (fun (imp : import) ->
      match Hashtbl.find_opt project_funcs_by_module imp.im_target with
      | Some fs -> Some (imp.im_local, fs)
      | None -> None
    ) fi.fi_imports in
    let alias_extra = alias_extra_ts @ alias_extra_py in
    if alias_extra = [] then
      Some (Func_lookup.bare_name_index_of_hashtbl project_funcs_by_package)
    else Some (over_project alias_extra)
  end

let build_import_target_files
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    (fi : file_info)
  : (string, (string, unit) Hashtbl.t) Hashtbl.t =
  let target_files = Hashtbl.create 64 in
  let add_under (key : string) (candidates : string list) =
    let set =
      match Hashtbl.find_opt target_files key with
      | Some file_set -> file_set
      | None ->
        let file_set = Hashtbl.create 4 in
        Hashtbl.replace target_files key file_set; file_set
    in
    List.iter (fun path -> Hashtbl.replace set path ()) candidates
  in
  List.iter (fun (local, specifier, kind) ->
    let candidates =
      resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
    in
    if candidates <> [] then begin
      add_under local candidates;
      (* Also record the EXPORTED name: method groups are keyed by the
         class's own name, so an aliased import must contribute its files
         under that name too — otherwise narrowing sees only the files of
         the unaliased import and drops the aliased class's methods. *)
      match kind with
      | I_named exported when not (String.equal local exported) ->
        add_under exported candidates
      | _ -> ()
    end
  ) fi.fi_import_specifiers;
  target_files

(* Local name -> (exported name, files exporting it) for named imports
   bound under a different local name.  Per file, like every other import
   index: the binding exists only in the file that wrote the import.  The
   alias names its origin exactly, which is what tells two same-named
   imported classes apart at a call site. *)
let build_class_aliases
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    (fi : file_info)
  : (string, string * Func_lookup.name_set) Hashtbl.t =
  let tbl = Hashtbl.create 8 in
  List.iter (fun (local, specifier, kind) ->
    match kind with
    | I_named exported when not (String.equal local exported) ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file
          specifier
      in
      if candidates <> [] then begin
        let file_set = Hashtbl.create 4 in
        List.iter (fun path -> Hashtbl.replace file_set path ()) candidates;
        Hashtbl.replace tbl local
          (exported, Func_lookup.name_set_of_hashtbl file_set)
      end
    | _ -> ())
    fi.fi_import_specifiers;
  tbl

(* Restrict an imported class's colliding methods to the file(s) it was
   imported from (keyed by the import's local name) or the caller's own
   file. *)
let narrow_methods_by_import_files
    ~(import_target_files : (string, (string, unit) Hashtbl.t) Hashtbl.t)
    ~(file_of_func : Func_info.t -> string option)
    ~(caller_file : string)
    (ts : Type_state.t) : Type_state.t =
  let keep_file (cls_name : Names.Class_name.t) (file : string) : bool =
    String.equal file caller_file
    || (match
          Hashtbl.find_opt import_target_files
            (Names.Class_name.to_string cls_name)
        with
       | Some target_set -> Hashtbl.mem target_set file
       | None -> false)
  in
  Type_state.narrow ~keep_file ~file_of_func
    ~classes:
      (Hashtbl.fold (fun cls _ acc -> Names.Class_name.of_string cls :: acc)
         import_target_files [])
    ts

(* Restrict colliding methods to files the caller itself requires (whole-file
   "*" import specifiers — Ruby [require_relative], PHP [require]/[include])
   or the caller's own file.  These languages bind no local name per import,
   so the required-file set applies to every class rather than to one imported
   name.  A spec matches a def file by trailing path segments, extensions
   stripped on the final segment of both sides ("widget_b" and "widget_b.php"
   both match ".../widget_b.rb|php"); leading "."/".." segments of a relative
   spec are dropped rather than resolved.  Callers with no whole-file requires
   (e.g. autoloaded Rails/PSR-4 code) leave every group untouched. *)
let narrow_methods_by_required_files
    ~(required_specs : string list)
    ~(file_of_func : Func_info.t -> string option)
    ~(caller_file : string)
    ~(narrowing : required_files_narrowing)
    (ts : Type_state.t) : Type_state.t =
  let spec_suffixes =
    List.filter_map (fun spec ->
      match
        Path_segs.rev_no_ext spec
        |> List.filter (fun seg ->
             not (String.equal seg "") && not (String.equal seg ".")
             && not (String.equal seg ".."))
      with
      | [] -> None
      | segs -> Some segs)
      required_specs
  in
  if spec_suffixes = [] then ts
  else
    let keep_file (_ : Names.Class_name.t) (file : string) : bool =
      String.equal file caller_file
      || (let rev_file_segs =
            Common.SMap.find file narrowing.rev_path_segs_by_file
          in
          List.exists
            (fun rev_spec -> Path_segs.is_prefix rev_spec rev_file_segs)
            spec_suffixes)
    in
    Type_state.narrow ~classes:narrowing.narrowable_classes ~keep_file
      ~file_of_func ts

let resolves_by_binding (lang : Lang.t) : bool =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 -> true
  | _ -> false

let definition_of_target
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(package : Names.Module_qn.t)
    (target : Names.Module_qn.t) : definition option =
  let stored_at (key : string) : definition option =
    Common.SMap.find_opt key definitions_by_qn
  in
  match stored_at (Names.Module_qn.to_string target) with
  | Some _ as found -> found
  | None ->
    if Names.Module_qn.is_empty package then None
    else
      let within_package =
        Names.Module_qn.of_parts
          (Names.Module_qn.parts package @ Names.Module_qn.parts target)
      in
      stored_at (Names.Module_qn.to_string within_package)

type positioned_binding = {
  pb_pos : Pos.t option;
  pb_name : string;
  pb_parent_path : IL.name option list;
  pb_kinds : Func_lookup.scope_kind list;
}

let position_of_tok (tok : Tok.t) : Pos.t option =
  match Tok.loc_of_tok tok with
  | Ok (loc : Tok.location) -> Some loc.Tok.pos
  | Error _ -> None

let equal_parent_path (first : IL.name option list)
    (second : IL.name option list) : bool =
  List.equal (Option.equal Function_id.equal_il_name) first second

let function_binding_of ~(tok : Tok.t)
    ~(parent_path : IL.name option list) (name : string)
    (funcs : Func_info.t list) : positioned_binding list =
  match funcs with
  | [] -> []
  | _ :: _ ->
    [ { pb_pos = position_of_tok tok; pb_name = name;
        pb_parent_path = parent_path;
        pb_kinds =
          List.map
            (fun (func : Func_info.t) -> Func_lookup.Scope_function func)
            funcs } ]

let class_binding_of ~(tok : Tok.t)
    ~(parent_path : IL.name option list) (name : string)
    (class_qn : Names.Class_qn.t) : positioned_binding =
  { pb_pos = position_of_tok tok; pb_name = name;
    pb_parent_path = parent_path;
    pb_kinds = [ Func_lookup.Scope_class class_qn ] }

type bound_in_scope = {
  bs_pos : Pos.t option;
  bs_parent_path : IL.name option list;
  bs_kinds : Func_lookup.scope_kind list;
}

let bindings_of_positioned (bindings : positioned_binding list)
    : Func_lookup.scope_entry list Common.SMap.t =
  let in_file_order =
    List.stable_sort
      (fun (first : positioned_binding) (second : positioned_binding) ->
        Option.compare Pos.compare first.pb_pos second.pb_pos)
      bindings
  in
  List.fold_left
    (fun (bound : bound_in_scope list Common.SMap.t)
         (binding : positioned_binding) ->
      let in_name =
        Option.value (Common.SMap.find_opt binding.pb_name bound) ~default:[]
      in
      let same_scope, other_scopes =
        List.partition
          (fun (entry : bound_in_scope) ->
            equal_parent_path entry.bs_parent_path binding.pb_parent_path)
          in_name
      in
      let bound_now =
        match same_scope with
        | [ (earlier : bound_in_scope) ]
          when Option.equal Pos.equal earlier.bs_pos binding.pb_pos ->
          { earlier with bs_kinds = earlier.bs_kinds @ binding.pb_kinds }
        | _ ->
          { bs_pos = binding.pb_pos; bs_parent_path = binding.pb_parent_path;
            bs_kinds = binding.pb_kinds }
      in
      Common.SMap.add binding.pb_name (bound_now :: other_scopes) bound)
    Common.SMap.empty in_file_order
  |> Common.SMap.map
       (fun (in_name : bound_in_scope list) ->
         List.concat_map
           (fun (entry : bound_in_scope) ->
             List.map
               (fun (kind : Func_lookup.scope_kind) ->
                 { Func_lookup.kind; parent_path = entry.bs_parent_path })
               entry.bs_kinds)
           in_name)

let enclosing_scope_of_class
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    (ci : class_info) : IL.name option list option =
  let encloses_itself (parent_path : IL.name option list) : bool =
    match List.rev parent_path with
    | Some (innermost : IL.name) :: _ ->
      Function_id.equal_name ci.ci_id innermost
    | None :: _
    | [] -> false
  in
  Option.bind
    (Option.bind
       (Common.SMap.find_opt (Function_id.show ci.ci_id) class_parent_paths)
       (List.find_opt
          (fun (((defining : Function_id.t), _) :
                  Function_id.t * IL.name option list) ->
            Function_id.equal defining ci.ci_id)))
    (fun (((_ : Function_id.t), (parent_path : IL.name option list))) ->
      if encloses_itself parent_path then None else Some parent_path)

let own_class_bindings
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(module_path : Names.Module_qn.t)
    ~(fi_file_str : string) : positioned_binding list =
  Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  |> List.filter_map (fun (ci : class_info) ->
       match Names.Class_qn.split_last ci.ci_qn with
       | None -> None
       | Some ((parent : Names.Class_qn.t), (name : string)) ->
         let bind (parent_path : IL.name option list) : positioned_binding =
           class_binding_of ~tok:(Function_id.tok ci.ci_id) ~parent_path name
             ci.ci_qn
         in
         if
           String.equal (Names.Class_qn.to_string parent)
             (Names.Module_qn.to_string module_path)
         then Some (bind [])
         else
           Option.map bind (enclosing_scope_of_class ~class_parent_paths ci))

let own_definitions_of_file
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(fi_file_str : string) : positioned_binding list =
  let own_funcs =
    Option.value (Hashtbl.find_opt file_funcs_index fi_file_str) ~default:[]
  in
  List.concat_map
      (fun (func : Func_info.t) ->
        let bind (parent_path : IL.name option list) (name : IL.name)
            : positioned_binding list =
          function_binding_of ~tok:(snd name.IL.ident) ~parent_path
            (fst name.IL.ident) [ func ]
        in
        match Func_info.as_method func.Func_info.fn_id with
        | Some _ -> []
        | None -> (
          match Func_info.as_free func.Func_info.fn_id with
          | Some (bare_name : IL.name) -> bind [] bare_name
          | None -> (
            match List_.init_and_last_opt func.Func_info.fn_id with
            | Some ((parent_path : IL.name option list),
                    Some (bare_name : IL.name)) ->
              bind parent_path bare_name
            | _ -> [])))
    own_funcs

let build_scope_table
    ~(lang : Lang.t)
    ~(cfg : Index_lang_rules.t)
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    (fi : file_info) : file_scope option =
  if
    not (resolves_by_binding lang)
    || cfg.Index_lang_rules.is_stub_file fi.fi_file
  then None
  else
    let fi_file_str = Fpath.to_string fi.fi_file in
    let package =
      Module_paths.enclosing_package ~cfg ~file:fi.fi_file fi.fi_module_path
    in
    let own_bindings =
      own_definitions_of_file ~file_funcs_index ~fi_file_str
    in
    let own_classes =
      own_class_bindings ~classes_by_file ~class_parent_paths
        ~module_path:fi.fi_module_path ~fi_file_str
    in
    let imported, bound_class_files =
      List.fold_left
        (fun ((imported : positioned_binding list),
              (bound_class_files : (Names.Class_name.t * Fpath.t) list))
             (imp : import) ->
          let tok = imp.im_tok in
          match Imports.binding_of imp with
          | Imports.Wildcard_from (target : Names.Module_qn.t) ->
            ( Common.SMap.fold
                (fun (name : string)
                     (attribute : Func_lookup.module_attribute)
                     (imported : positioned_binding list) ->
                  if not (Reexports.star_exported ~dunder_all target name) then
                    imported
                  else
                    match attribute with
                    | Func_lookup.Attr_functions (funcs : Func_info.t list) ->
                      function_binding_of ~tok ~parent_path:[] name funcs
                      @ imported
                    | Func_lookup.Attr_class (class_qn : Names.Class_qn.t) ->
                      class_binding_of ~tok ~parent_path:[] name class_qn
                      :: imported
                    | Func_lookup.Attr_module _ -> imported)
                (Func_lookup.attributes_of_module attributes_by_module target)
                imported,
              bound_class_files )
          | Imports.Named_binding { local; target } -> (
            match definition_of_target ~definitions_by_qn ~package target with
            | Some (Function_definitions funcs) ->
              (function_binding_of ~tok ~parent_path:[] local funcs @ imported,
               bound_class_files)
            | Some (Class_definition { class_file; class_qn }) ->
              ( class_binding_of ~tok ~parent_path:[] local class_qn :: imported,
                (Names.Class_name.of_string
                   (Names.Module_qn.bare_name target), class_file)
                :: bound_class_files )
            | None -> (imported, bound_class_files)))
        ([], []) fi.fi_imports
    in
    Some { scope_table =
             Func_lookup.scope_table_of_map
               (bindings_of_positioned
                  (own_bindings @ own_classes @ List.rev imported));
           bound_class_files }

let narrow_methods_by_bound_files
    ~(bound_class_files : (Names.Class_name.t * Fpath.t) list)
    ~(file_of_func : Func_info.t -> string option)
    ~(caller_file : string)
    (ts : Type_state.t) : Type_state.t =
  match bound_class_files with
  | [] -> ts
  | _ :: _ ->
    let keep_file (cls : Names.Class_name.t) (file : string) : bool =
      String.equal file caller_file
      || List.exists
           (fun ((bound_cls : Names.Class_name.t), (bound_file : Fpath.t)) ->
             Names.Class_name.equal bound_cls cls
             && String.equal (Fpath.to_string bound_file) file)
           bound_class_files
    in
    Type_state.narrow ~keep_file ~file_of_func
      ~classes:(List.map fst bound_class_files) ts

let func_file_opt (func : Func_info.t) : string option =
  Option.map Fpath.to_string (Func_info.def_file_opt func)

let build_same_file_funcs_by_name
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(fi_file_str : string)
  : (string, Func_info.t list) Hashtbl.t =
  let same_file_list =
    Option.value (Hashtbl.find_opt file_funcs_index fi_file_str)
      ~default:[]
  in
  let tbl = Hashtbl.create (List.length same_file_list) in
  List.iter (fun (func : Func_info.t) ->
    match List_.init_and_last_opt func.Func_info.fn_id with
    | Some (_, Some bare_name) ->
      let name = fst bare_name.IL.ident in
      let cur = Option.value (Hashtbl.find_opt tbl name) ~default:[] in
      Hashtbl.replace tbl name (func :: cur)
    | _ -> ()
  ) same_file_list;
  tbl

let build_funcs_by_name
    ~(visible : (string, unit) Hashtbl.t)
    ~(project_funcs_by_name : (string, Func_info.t list) Hashtbl.t)
    ~(default_export_fn : (string, Func_info.t) Hashtbl.t)
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    ~(import_target_files :
        (string, (string, unit) Hashtbl.t) Hashtbl.t)
    ~(func_file_opt : Func_info.t -> string option)
    ~(func_in_caller_file : Func_info.t -> bool)
    (fi : file_info)
  : (string, Func_info.t list) Hashtbl.t option =
  let tbl = Hashtbl.create (Hashtbl.length visible) in
  Hashtbl.iter (fun name () ->
    match Hashtbl.find_opt project_funcs_by_name name with
    | None -> ()
    | Some fs ->
      let kept = List.filter (fun (func : Func_info.t) ->
        match Func_info.as_method func.Func_info.fn_id with
        | Some (cls, _) -> Hashtbl.mem visible (fst cls.IL.ident)
        | None -> true
      ) fs in
      let kept =
        match Hashtbl.find_opt import_target_files name with
        | None -> kept
        | Some target_set ->
          let matches = List.filter (fun func ->
            func_in_caller_file func
            || (match func_file_opt func with
                | Some file_str -> Hashtbl.mem target_set file_str
                | None -> false)
          ) kept in
          if matches <> [] then matches else kept
      in
      let same, other = List.partition func_in_caller_file kept in
      let kept = same @ other in
      if kept <> [] then Hashtbl.replace tbl name kept
  ) visible;
  List.iter (fun (local, specifier, kind) ->
    match kind with
    | I_default ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
      in
      List.iter (fun path ->
        match Hashtbl.find_opt default_export_fn path with
        | None -> ()
        | Some target ->
          (* The target is exposed under the importer's local name at the
             target's identity. The synthetic bare name carries [local] at
             the target's position and sid, so a name lookup finds [local]
             while [fn_id_to_node] and [resolved_name_of_fn_id] resolve to the
             target's real vertex, which holds its body and signature.
             [Ts_class_aliases] and Reexports.expose_free_as use the same
             convention of one position with two names. A lambda default
             export (the synthetic [_module_exports_default], with no real
             vertex) stays unresolved. *)
          (match Func_info.bare_name target.Func_info.fn_id with
           | None -> ()
           | Some (tname : IL.name) ->
             let alias_ii = G.empty_id_info () in
             alias_ii.G.id_resolved :=
               (match !(tname.IL.id_info.G.id_resolved) with
                | Some _ as r -> r
                | None -> Some (G.Global, tname.IL.sid));
             let il_name = IL.{
               ident = (local, snd tname.IL.ident);
               sid = tname.IL.sid;
               id_info = alias_ii;
             } in
             let synth : Func_info.t = {
               fn_id = [None; Some il_name];
               entity = target.Func_info.entity;
               fdef = target.Func_info.fdef;
             } in
             let cur = Option.value (Hashtbl.find_opt tbl local) ~default:[] in
             Hashtbl.replace tbl local (synth :: cur))
      ) candidates
    (* [import { f as g }]: the call site writes [g], which names no
       project function.  Expose the target under [local] at the TARGET's
       identity, exactly as the default-import branch above does. *)
    | I_named orig_name when not (String.equal local orig_name) ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
      in
      let target_files = Hashtbl.create (List.length candidates) in
      List.iter (fun path -> Hashtbl.replace target_files path ()) candidates;
      (match Hashtbl.find_opt project_funcs_by_name orig_name with
       | None -> ()
       | Some fs ->
         List.iter (fun (target : Func_info.t) ->
           let from_target_file =
             match func_file_opt target with
             | Some file_str -> Hashtbl.mem target_files file_str
             | None -> false
           in
           if from_target_file then
             match Func_info.bare_name target.Func_info.fn_id with
             | None -> ()
             | Some (tname : IL.name) ->
               let alias_ii = G.empty_id_info () in
               alias_ii.G.id_resolved :=
                 (match !(tname.IL.id_info.G.id_resolved) with
                  | Some _ as r -> r
                  | None -> Some (G.Global, tname.IL.sid));
               let il_name = IL.{
                 ident = (local, snd tname.IL.ident);
                 sid = tname.IL.sid;
                 id_info = alias_ii;
               } in
               let synth : Func_info.t = {
                 fn_id = [None; Some il_name];
                 entity = target.Func_info.entity;
                 fdef = target.Func_info.fdef;
               } in
               let cur =
                 Option.value (Hashtbl.find_opt tbl local) ~default:[]
               in
               Hashtbl.replace tbl local (synth :: cur))
           fs)
    | _ -> ()
  ) fi.fi_import_specifiers;
  Some tbl

(* Imported module singletons: stamp [local]'s occurrences with the
   singleton's class. *)
let stamp_singleton_imports
    ~(type_state : Type_state.t)
    (fi : file_info) : unit =
  let facts =
    List.fold_left (fun acc (imp : import) ->
      let local = imp.im_local in
      if String.length local = 0 then acc
      else
        match Type_state.get_module_singleton type_state imp.im_target with
        | None -> acc
        | Some ty ->
          let v_id =
            G.Id ((local, Tok.unsafe_fake_tok local), G.empty_id_info ())
          in
          (v_id, ty) :: acc
    ) [] fi.fi_imports
  in
  Object_initialization.stamp_id_types facts fi.fi_ast

let edges_for_file (ctx : ctx) (fi : file_info)
  : (Function_id.t * Function_id.t * Tok.t) list =
  let { lang; cfg; type_state; required_files_narrowing; definitions_by_qn;
        attributes_by_module; dunder_all;
        resolution_orders; class_qn_by_definition; methods_by_class;
        classes_by_file; class_parent_paths;
        all_funcs;
        project_constructors;
        project_funcs_by_name; project_funcs_by_module; file_module_qn;
        project_funcs_by_package; project_class_names;
        file_funcs_index;
        default_export_class; named_export_classes; default_export_fn;
        path_suffix_index; slice_element_of_field;
        top_level_node_for; visible_names_for_file;
        stamp_var_types; resolve_ts_specifier; value_alias_index } = ctx in
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
    let emitter =
      Edge_emitter.create ~top_level:(top_level_node_for fi.fi_file)
    in
    let visible = staged "visibility" (fun () -> visible_names_for_file fi) in
    let fi_file_str = Fpath.to_string fi.fi_file in
    let top_level_node = top_level_node_for fi.fi_file in
    let func_in_caller_file (func : FA.func_info) : bool =
      match func_file_opt func with
      | Some file_str -> file_str = fi_file_str
      | None -> false
    in
    (* Must run before [build_funcs_by_name]: augments [visible] with cross-file class targets it filters on. *)
    staged "stamp base var types + import aliases" (fun () ->
      stamp_base_var_types ~lang ~project_class_names
        ~default_export_class ~named_export_classes
        ~path_suffix_index ~resolve_ts_specifier ~visible fi;
      stamp_import_value_aliases ~value_alias_index fi);
    let alias_to_module_qn =
      staged "alias to module map" (fun () -> build_alias_to_module_qn ~cfg fi)
    in
    let funcs_by_module_qn
      : (Names.Module_qn.t, FA.func_info list) Hashtbl.t option =
      match cfg.Index_lang_rules.unqualified_scope with
      | `Per_file | `Per_directory -> Some project_funcs_by_module
      | _ -> None
    in
    let file_funcs_by_package =
      staged "file funcs by package" @@ fun () ->
      build_file_funcs_by_package ~cfg ~project_funcs_by_package
        ~project_funcs_by_module ~file_funcs_index
        ~path_suffix_index ~resolve_ts_specifier fi
    in
    let import_target_files =
      staged "import target files" @@ fun () ->
      build_import_target_files ~path_suffix_index ~resolve_ts_specifier fi
    in
    let file_scope =
      staged "scope table" @@ fun () ->
      build_scope_table ~lang ~cfg ~definitions_by_qn
        ~file_funcs_index ~attributes_by_module ~dunder_all ~classes_by_file
        ~class_parent_paths fi
    in
    let file_type_state =
      staged "narrow methods by imports/required files" @@ fun () ->
      match file_scope with
      | Some (scope : file_scope) ->
        narrow_methods_by_bound_files
          ~bound_class_files:scope.bound_class_files
          ~file_of_func:func_file_opt ~caller_file:fi_file_str type_state
      | None ->
      let base =
        cfg.Index_lang_rules.narrow_methods_by_imports
          ~fi_imports:
            (List.map (fun (imp : import) -> (imp.im_local, imp.im_target))
               fi.fi_imports)
          ~file_of_func:func_file_opt type_state
      in
      if cfg.Index_lang_rules.narrow_methods_by_import_files then
        narrow_methods_by_import_files ~import_target_files
          ~file_of_func:func_file_opt ~caller_file:fi_file_str base
      else
        match required_files_narrowing with
        | Some (narrowing : required_files_narrowing) ->
          let required_specs =
            List.filter_map (fun (local, spec, _kind) ->
              if String.equal local "*" then Some spec else None)
              fi.fi_import_specifiers
          in
          narrow_methods_by_required_files ~required_specs
            ~file_of_func:func_file_opt ~caller_file:fi_file_str ~narrowing
            base
        | None -> base
    in
    let same_file_funcs_by_name =
      staged "same-file funcs table" @@ fun () ->
      build_same_file_funcs_by_name ~file_funcs_index ~fi_file_str
    in
    let funcs_by_name =
      staged "funcs_by_name table" @@ fun () ->
      build_funcs_by_name ~visible ~project_funcs_by_name ~default_export_fn
        ~path_suffix_index ~resolve_ts_specifier ~import_target_files
        ~func_file_opt ~func_in_caller_file fi
    in
    let func_lookup =
      staged "func_lookup create" @@ fun () ->
      Func_lookup.create
        ?funcs_by_name:(Option.map Func_lookup.bare_name_index_of_hashtbl funcs_by_name)
        ~project_funcs_by_name:
          (Func_lookup.bare_name_index_of_hashtbl project_funcs_by_name)
        ?funcs_by_module_qn:
          (Option.map Func_lookup.module_index_of_hashtbl funcs_by_module_qn)
        ?alias_to_module_qn:
          (Option.map Func_lookup.alias_index_of_hashtbl alias_to_module_qn)
        ~same_file_funcs_by_name:
          (Func_lookup.bare_name_index_of_hashtbl same_file_funcs_by_name)
        ~overload_groups:(Lang_config.overloads_by_type lang)
        ?funcs_by_package:file_funcs_by_package
        ~file_module_qn:
          (Func_lookup.file_module_index_of_hashtbl file_module_qn)
        ~class_aliases:
          (Func_lookup.class_alias_index_of_hashtbl
             (build_class_aliases ~path_suffix_index ~resolve_ts_specifier fi))
        ?constructors:
          (Option.bind funcs_by_name
             (Func_lookup.constructor_index_of_hashtbl ~lang))
        ~project_constructors
        ~module_attributes:attributes_by_module
        ~resolution_orders
        ~class_qn_by_definition
        ~methods_by_class
        ~scope_table:
          (match file_scope with
           | Some (scope : file_scope) -> scope.scope_table
           | None -> Func_lookup.empty_scope_table)
        ()
    in
    staged "stamp var types" (fun () ->
      stamp_singleton_imports ~type_state fi;
      (* the file's view: the classes it imports, a method's return type
         from the class it sees *)
      stamp_var_types ~type_state:file_type_state ~slice_element_of_field
        fi.fi_ast);
    let per_fdef_edges =
      staged "extract calls" @@ fun () ->
      Visit_function_defs.fold_with_parent_path ~lang
        (fun edges opt_ent parent_path fdef ->
        (* A skipped anon attributes calls to its enclosing named ancestor, else the file's [<top_level>] node. *)
        let caller_kind =
          if skip_anon opt_ent then begin
            match parent_path with
            | [] | [None] -> `Top_anon
            | _ -> `Named parent_path
          end else
            match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
            | Some fn_id -> `Named fn_id
            | None -> `Skip
        in
        match caller_kind with
        | `Skip -> edges
        | _ ->
          let fn_id_for_extract : FA.fn_id =
            match caller_kind with `Named fn_id -> fn_id | _ -> []
          in
          let caller_node_opt : Call_graph.G.V.t option =
            match caller_kind with
            | `Named fn_id -> FA.fn_id_to_node fn_id
            | `Top_anon -> Some top_level_node
            | `Skip -> None
          in
          let is_toplevel_lambda =
            match (opt_ent, parent_path) with
            | (None, [None]) | (None, []) -> true
            | _ -> false
          in
          let fn_id = fn_id_for_extract in
          let body_stmt = AST_generic_helpers.funcbody_to_stmt fdef.G.fbody in
          let body_program = [body_stmt] in
          (* fdef-scoped facts stamped onto the body only, most specific
             first (fill-on-None): isinstance narrowing, then [self]/[cls],
             then typed params. *)
          let fdef_facts =
            let param_facts =
              Tok.unbracket fdef.G.fparams
              |> List.filter_map (fun param ->
                match param with
                | G.ParamReceiver { G.pname = Some pn; ptype = Some pty; _ }
                | G.Param { G.pname = Some pn; ptype = Some pty; _ } ->
                  (match Ty_bare_name.inner_class_name_of_ty pty with
                   | Some cls -> Some (G.Id (pn, G.empty_id_info ()), cls)
                   | None -> None)
                | _ -> None)
            in
            (* An anon fdef's own [fparams] lacks [self]/[cls]; bind them to the enclosing class. *)
            let self_facts =
              match opt_ent, parent_path with
              | None, (Some (cls : IL.name)) :: _
                when Type_state.has_class file_type_state (fst cls.IL.ident) ->
                let cls_id = G.Id (cls.IL.ident, G.empty_id_info ()) in
                let mk str =
                  G.Id ((str, Tok.unsafe_fake_tok str), G.empty_id_info ())
                in
                List.map (fun name -> (mk name, cls_id)) (Receiver.self_names lang)
              | _ -> []
            in
            (* isinstance narrowing over-applies: [(var, T)] holds for the
               whole function, not just the narrowed scope. *)
            let isinstance_facts =
              if not (Lang.equal lang Lang.Python
                   || Lang.equal lang Lang.Python2
                   || Lang.equal lang Lang.Python3) then []
              else
                Walker.fold_exprs_in_stmt ~skip_nested_fdefs:true
                  (fun acc expr ->
                    match expr.G.e with
                    | G.Call ({ e = G.N (G.Id (("isinstance", _), _)); _ },
                              (_, [G.Arg var_e; G.Arg ty_e], _)) ->
                      (match var_e.G.e, ty_e.G.e with
                       | G.N (G.Id _ as var_n), G.N (G.Id _ as ty_n)
                       | G.N (G.Id _ as var_n), G.N (G.IdQualified _ as ty_n) ->
                         let ty_bare_name = match ty_n with
                           | G.Id _ -> ty_n
                           | G.IdQualified { name_last = ((str, tok), _); _ } ->
                             G.Id ((str, tok), G.empty_id_info ())
                         in
                         (var_n, ty_bare_name) :: acc
                       | _ -> acc)
                    | _ -> acc) [] body_stmt
            in
            isinstance_facts @ self_facts @ param_facts
          in
          Object_initialization.stamp_id_types fdef_facts body_program;
          stamp_var_types ~type_state:file_type_state ~slice_element_of_field
            body_program;
          let { FA.calls = callee_calls; callbacks = callback_calls; _ } =
            FA.extract_calls ~lang
              ~identify_callee:
                (Callee_resolution.identify_callee_interfile ~lang
                   ~type_state:file_type_state)
              ~identify_callback:
                (Callback_extraction.identify_callback_interfile ~lang
                   ~type_state:file_type_state)
              ~all_funcs
              ~func_lookup
              ~caller_parent_path:fn_id fdef
          in
          let edges =
            List.fold_left (fun edges (callee, call_tok) ->
              Edge_emitter.emit_call emitter ~caller_node:caller_node_opt
                ~is_toplevel_lambda ~callee ~call_tok @ edges
            ) edges callee_calls
          in
          let edges =
            List.fold_left (fun edges (callback, call_tok, tmp) ->
              Edge_emitter.emit_callback emitter ~caller_node:caller_node_opt
                ~is_toplevel_lambda ~callback ~call_tok ~tmp @ edges
            ) edges callback_calls
          in
          (match opt_ent with
           | None -> edges
           | Some ent when ent.G.attrs = [] -> edges
           | Some ent ->
             let dec_calls =
               FA.extract_decorator_calls
                 ~identify_callee:
                   (Callee_resolution.identify_callee_interfile ~lang
                   ~type_state:file_type_state)
                 ~func_lookup
                 ~caller_parent_path:fn_id ent.G.attrs
             in
             List.fold_left (fun edges (callee, call_tok) ->
               Edge_emitter.emit_call emitter ~caller_node:caller_node_opt
                 ~is_toplevel_lambda ~callee ~call_tok @ edges
             ) edges dec_calls))
      [] fi.fi_ast
    in
    let toplevel_calls =
      FA.extract_toplevel_calls
        ~identify_callee:
          (Callee_resolution.identify_callee_interfile ~lang
                   ~type_state:file_type_state)
        ~func_lookup
        fi.fi_ast
    in
    let toplevel_call_edges =
      List.fold_left (fun edges (callee, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee ~call_tok @ edges
      ) [] toplevel_calls
    in
    let toplevel_callbacks =
      (* The file's visible functions ahead of the project's, layered
         rather than merged: a merge copied the project table once per
         file. *)
      let project_index =
        Func_lookup.bare_name_index_of_hashtbl project_funcs_by_name
      in
      let merged_funcs_by_name =
        match funcs_by_name with
        | None -> project_index
        | Some pf ->
          Func_lookup.bare_name_index_layered
            ~front:(Func_lookup.bare_name_index_of_hashtbl pf)
            ~back:project_index
      in
      let toplevel_func_lookup =
        Func_lookup.create
          ~funcs_by_name:merged_funcs_by_name
          ~overload_groups:(Lang_config.overloads_by_type lang)
          ~module_attributes:attributes_by_module
          ~resolution_orders
          ~class_qn_by_definition
          ~methods_by_class
          ?alias_to_module_qn:
            (Option.map Func_lookup.alias_index_of_hashtbl alias_to_module_qn)
          ~scope_table:
            (match file_scope with
             | Some (scope : file_scope) -> scope.scope_table
             | None -> Func_lookup.empty_scope_table)
          ()
      in
      FA.extract_toplevel_hof_callbacks ~lang
        ~identify_callback:
          (Callback_extraction.identify_callback_interfile ~lang
             ~type_state:file_type_state)
        ~func_lookup:toplevel_func_lookup fi.fi_ast
    in
    let toplevel_callback_edges =
      List.fold_left (fun edges (callback, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee:callback ~call_tok @ edges
      ) [] toplevel_callbacks
    in
    per_fdef_edges @ toplevel_call_edges @ toplevel_callback_edges
