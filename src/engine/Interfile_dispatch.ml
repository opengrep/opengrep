(* Per-rule interfile taint dispatch: rules run in parallel (no shared
   mutable state), each folding its subgraph in topo order (leaves first)
   with a shared sig db.  Formula_cache.t is per-file, not thread-safe. *)

module Log = Log_tainting.Log
module R = Rule
module G = AST_generic
module PM = Core_match
module Effect = Shape_and_sig.Effect
module Effects = Shape_and_sig.Effects
module FunctionMap = Shape_and_sig.FunctionMap
module Lval_env = Taint_lval_env

module FpathMap = Map.Make (Fpath)
module FpathSet = Set.Make (Fpath)
module FidSet = Set.Make (Function_id)

let parse_file (lang : Lang.t) (file : Fpath.t) : G.program =
  let result = Parse_target.parse_and_resolve_name lang file in
  result.Parsing_result2.ast

type file_env = {
  ast : G.program;
  taint_inst : Taint_rule_inst.t;
  glob_env : Lval_env.t;
  glob_effects : Effects.t;
}

type rule_state = {
  rule : R.taint_rule;
  lang : Lang.t;
  relevant_graph : Call_graph.G.t;
  topo_order : Function_id.t list;  (* leaves first, callers last *)
  info_map : Match_tainting_mode.fun_info FunctionMap.t;
  file_envs : file_env FpathMap.t;
  builtin_signature_db : Shape_and_sig.builtin_signature_database option;
  match_on : [ `Sink | `Source ];
  target_root_map : Fpath.t option FpathMap.t;
}

type interfile_target = {
  abs_path : Fpath.t;
  path_root : Fpath.t option;  (* base for absolutifying token paths; None if abs *)
}

type lang_context = {
  lc_lang : Lang.t;
  lc_rules : R.taint_rule list;
  lc_interfile_graph : Interfile_graph.interfile_graph;
  lc_matching_targets : interfile_target list;
}

let file_of_fid (fid : Function_id.t) : Fpath.t option =
  Option.map Fpath.normalize (Function_id.file_of fid)

(* Interfile via the global flag or the rule's own option. *)
let rule_is_interfile ~(taint_interfile : bool) (rule : R.rule) : bool =
  taint_interfile ||
  (match rule.R.options with
   | Some opts -> opts.taint_interfile
   | None -> false)

let interfile_taint_rules_by_lang
    ~(taint_interfile : bool)
    (valid_rules : R.rule list)
    : (Lang.t * R.taint_rule list) list =
  let tbl : (Lang.t, R.taint_rule list) Hashtbl.t = Hashtbl.create 4 in
  List.iter (fun (rule : R.rule) ->
    match rule.R.mode with
    | `Taint _ as mode ->
      if rule_is_interfile ~taint_interfile rule then begin
        (* [to_langs] not [to_lang]: a JS rule [L (Js,[Ts])] must register
           under Ts too, else TS targets get zero findings. *)
        let taint_rule : R.taint_rule = { rule with mode } in
        List.iter (fun (lang : Lang.t) ->
          let existing =
            match Hashtbl.find_opt tbl lang with
            | Some prev -> prev | None -> []
          in
          Hashtbl.replace tbl lang (taint_rule :: existing))
          (Xlang.to_langs rule.R.target_analyzer)
      end
    | _ -> ()
  ) valid_rules;
  Hashtbl.fold (fun (lang : Lang.t) (rules : R.taint_rule list)
    (acc : (Lang.t * R.taint_rule list) list) ->
    (lang, rules) :: acc) tbl []

let interfile_taint_rule_ids
    ~(taint_interfile : bool)
    (valid_rules : R.rule list)
    : Rule_ID.t list =
  List.filter_map (fun (rule : R.rule) ->
    match rule.R.mode with
    | `Taint _ ->
      if rule_is_interfile ~taint_interfile rule then Some (fst rule.R.id)
      else None
    | _ -> None
  ) valid_rules

let interfile_file_set (graph : Call_graph.G.t) : (Fpath.t, bool) Hashtbl.t =
  (* The vertex count bounds the file count. *)
  let tbl = Hashtbl.create (Call_graph.G.nb_vertex graph) in
  Call_graph.G.iter_vertex (fun (vertex : Function_id.t) ->
    match Function_id.file_of vertex with
    | Some fp -> Hashtbl.replace tbl fp true
    | None -> ()
  ) graph;
  tbl

(* Relative internal paths are absolutified against [cwd], not project_root. *)
let targets_in_interfile_graph
    ~(lang : Lang.t)
    ~(cwd : Fpath.t)
    ~(interfile_files : (Fpath.t, bool) Hashtbl.t)
    (targets : Target.t list)
    : interfile_target list =
  List.filter_map (fun (target : Target.t) ->
    match target with
    | Regular ({ analyzer; path = { internal_path_to_content; _ }; _ }) ->
      (match Xlang.to_lang analyzer with
       | Ok target_lang when Lang.equal target_lang lang ->
         let abs_path, path_root =
           Fpath_.absolutify ~cwd internal_path_to_content
         in
         if Hashtbl.mem interfile_files abs_path then
           Some { abs_path; path_root }
         else begin
           Log.warn (fun m ->
               m "interfile preprocess: target %s (abs: %s) not found in \
                  interfile graph (%d files); interfile taint analysis \
                  will not cover this file"
                 (Fpath.to_string internal_path_to_content)
                 (Fpath.to_string abs_path)
                 (Hashtbl.length interfile_files));
           None
         end
       | _ -> None)
    | Lockfile _ -> None
  ) targets

type rule_specs = {
  rs_rule : R.taint_rule;
  rs_sources : Function_id.t list;
  rs_sinks : Function_id.t list;
}

(* Formula cache is per-file to avoid byte-position collisions. *)
let extract_specs_for_rule
    ~(lang : Lang.t)
    ~(xconf : Match_env.xconfig)
    ~prefilter
    ~(contents : (Fpath.t, string) Hashtbl.t)
    ~(ast_table : (Fpath.t, G.program) Hashtbl.t)
    ~(matching_targets : interfile_target list)
    (rule : R.taint_rule)
    : rule_specs =
  let rule_id = fst rule.R.id in
  (* Rule options can change what counts as a source/sink. *)
  let xconf =
    Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
  in
  (* [prefilter] is the rule's source-OR-sink prefilter (not the stock
     same-file AND), so a source-only file still seeds the subgraph;
     compiled once per rule and [contents] read once per file, both
     shared across every (rule, chunk) item. A file with no cached
     content is kept (conservative). *)
  let file_is_relevant (path : Fpath.t) : bool =
    match prefilter with
    | None -> true
    | Some (_formula, func) -> (
        match Hashtbl.find_opt contents path with
        | Some content -> func content
        | None -> true)
  in
  let sources, sinks =
    List.fold_left
      (fun ((src_acc : Function_id.t list),
            (snk_acc : Function_id.t list))
        (target : interfile_target) ->
        match Hashtbl.find_opt ast_table target.abs_path with
        | None -> (src_acc, snk_acc)
        | Some _ when not (file_is_relevant target.abs_path) ->
            (src_acc, snk_acc)
        | Some ast ->
          let formula_cache =
            Formula_cache.mk_specialized_formula_cache [rule]
          in
          let spec_matches, _expls =
            try
              Match_taint_spec.spec_matches_of_taint_rule
                ~per_file_formula_cache:formula_cache
                xconf (Fpath.to_string target.abs_path) (ast, []) rule
            with
            | (Out_of_memory | Stack_overflow | Time_limit.Timeout _) as exn
              ->
              Log.warn (fun m ->
                  m "interfile spec_extract: fatal %s on %s"
                    (Printexc.to_string exn)
                    (Fpath.to_string target.abs_path));
              Exception.catch_and_reraise exn
          in
          let resolve_ranges (ranges : Range.t list)
              : Function_id.t list =
            if List_.null ranges then []
            else
              let fids =
                Graph_from_AST.find_functions_containing_ranges
                  ~lang ast ranges
              in
              List.map (fun (fid : Function_id.t) ->
                (Interfile_graph.absolutify_fid
                           target.path_root fid)
              ) fids
          in
          let source_ranges =
            spec_matches.Match_taint_spec.sources
            |> List.map (fun (rwm, _src) -> rwm.Range_with_metavars.r)
          in
          let sink_ranges =
            spec_matches.Match_taint_spec.sinks
            |> List.map (fun (rwm, _snk) -> rwm.Range_with_metavars.r)
          in
          let source_fids = resolve_ranges source_ranges in
          let sink_fids = resolve_ranges sink_ranges in
          if not (List_.null source_fids) then
            Log.info (fun m ->
                m "interfile preprocess: %s: %d sources for rule %s"
                  (Fpath.to_string target.abs_path)
                  (List.length source_fids)
                  (Rule_ID.to_string rule_id));
          if not (List_.null sink_fids) then
            Log.info (fun m ->
                m "interfile preprocess: %s: %d sinks for rule %s"
                  (Fpath.to_string target.abs_path)
                  (List.length sink_fids)
                  (Rule_ID.to_string rule_id));
          (List.rev_append source_fids src_acc,
           List.rev_append sink_fids snk_acc))
      ([], []) matching_targets
  in
  { rs_rule = rule;
    rs_sources = sources;
    rs_sinks = sinks }


type file_init_acc = {
  fi_info_map : Match_tainting_mode.fun_info FunctionMap.t;
  fi_file_envs : file_env FpathMap.t;
}

(* [fid_set] filters which functions get IL+CFG construction. *)
let init_file
    ~(lang : Lang.t)
    ~(rule : R.taint_rule)
    ~(xconf : Match_env.xconfig)
    ~(path_root : Fpath.t option)
    ~(fid_set : FidSet.t)
    ~(ast_table : (Fpath.t, G.program) Hashtbl.t)
    ~(file_path : Fpath.t)
    (acc : file_init_acc)
    : file_init_acc =
  let abs_file = Fpath.normalize file_path in
  match Hashtbl.find_opt ast_table file_path with
  | None ->
    Log.err (fun m ->
        m "interfile dispatch: file %s not in ast_table"
          (Fpath.to_string file_path));
    acc
  | Some ast ->
  (* Per-file cache: Range.t is byte-position only and would collide
     across files. *)
  let formula_cache =
    Formula_cache.mk_specialized_formula_cache [rule]
  in
  let xconf' =
    Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
  in
  let taint_inst =
    match
      Match_taint_spec.taint_config_of_rule
        ~per_file_formula_cache:formula_cache
        ~allow_partial:true
        xconf' lang file_path (ast, []) rule
    with
    | Some (ti, _spec_matches, _expls) ->
      { ti with Taint_rule_inst.project_root = path_root }
    | None ->
      let empty_preds : Taint_rule_inst.spec_predicates = {
        is_source = (fun _any -> []);
        is_propagator = (fun _any -> []);
        is_sanitizer = (fun _any -> []);
        is_sink = (fun _any -> []);
      } in
      Taint_rule_inst.{
        lang;
        file = file_path;
        project_root = path_root;
        rule_id = fst rule.R.id;
        options = xconf'.Match_env.config;
        track_control = false;
        preds = empty_preds;
        handle_effects = (fun _fn_name effects -> effects);
        java_props_cache = Hashtbl.create 0;
      }
  in
  let glob_env, glob_effects = Taint_input_env.mk_file_env taint_inst ast in
  let file_env = { ast; taint_inst; glob_env; glob_effects } in
  let fid_filter (fid : Function_id.t) : bool =
    FidSet.mem (Interfile_graph.absolutify_fid path_root fid) fid_set
  in
  let raw_info_map =
    Match_tainting_mode.build_info_map ~lang ~fid_filter ast
  in
  let enriched_map =
    FunctionMap.map
      (fun (info : Match_tainting_mode.fun_info) ->
         { info with
           file_ast = Some ast;
           taint_inst = Some taint_inst;
           name = IL.absolutify_name path_root info.name })
      raw_info_map
  in
  (* Re-key by absolutified fid to match graph vertex ids. *)
  let resolved_map =
    FunctionMap.fold
      (fun (fid : Function_id.t)
        (info : Match_tainting_mode.fun_info)
        (map_acc : Match_tainting_mode.fun_info FunctionMap.t) ->
        let abs_fid = Interfile_graph.absolutify_fid path_root fid in
        FunctionMap.add abs_fid info map_acc)
      enriched_map FunctionMap.empty
  in
  { fi_info_map =
      FunctionMap.union
        (fun _k existing _new -> Some existing)
        acc.fi_info_map resolved_map;
    fi_file_envs =
      FpathMap.add abs_file file_env acc.fi_file_envs;
  }

let fid_set_of_graph (graph : Call_graph.G.t) : FidSet.t =
  Call_graph.G.fold_vertex FidSet.add graph FidSet.empty

(* Base path for absolutifying a target's token paths: cwd if its internal
   path is relative, None if already absolute. *)
let build_target_root_map ~(cwd : Fpath.t) (targets : Target.t list)
    : Fpath.t option FpathMap.t =
  List.fold_left (fun (acc : Fpath.t option FpathMap.t) (target : Target.t) ->
      match target with
      | Regular ({ path = { internal_path_to_content; _ }; _ }) ->
        let abs_path, path_root =
          Fpath_.absolutify ~cwd internal_path_to_content
        in
        FpathMap.add abs_path path_root acc
      | Lockfile _ -> acc)
    FpathMap.empty targets

(* None for non-target subgraph files (already absolute; absolutify is a no-op). *)
let path_root_for_file
    (target_root_map : Fpath.t option FpathMap.t)
    (file_path : Fpath.t) : Fpath.t option =
  let norm = Fpath.normalize file_path in
  match FpathMap.find_opt norm target_root_map with
  | Some pr -> pr
  | None -> None

let is_target_file
    (target_root_map : Fpath.t option FpathMap.t)
    (file_path : Fpath.t) : bool =
  FpathMap.mem (Fpath.normalize file_path) target_root_map

type rule_subgraph = {
  rsg_lang_context : lang_context;
  rsg_specs : rule_specs;
  rsg_xconf : Match_env.xconfig;
  rsg_relevant_graph : Call_graph.G.t;
  rsg_topo_order : Function_id.t list;
  rsg_files : Fpath.t list;
  rsg_fid_set : FidSet.t;
}

(* Break direct impl→interface cycles so impls precede interfaces in the
   topo fold.  Only direct cycles; indirect ones survive as SCCs and are
   iterated to a fixpoint by [Sig_engine]. *)
let prune_impl_interface_cycles (graph : Call_graph.G.t) : int =
  let to_remove =
    Call_graph.G.fold_edges_e
      (fun (dispatch_e : Call_graph.G.E.t) (acc : Call_graph.G.E.t list) ->
         let label = Call_graph.G.E.label dispatch_e in
         match label.Call_graph.kind with
         | Call_graph.Dispatch ->
             let impl = Call_graph.G.E.src dispatch_e in
             let iface = Call_graph.G.E.dst dispatch_e in
             Call_graph.G.fold_succ_e
               (fun (call_e : Call_graph.G.E.t)
                    (inner_acc : Call_graph.G.E.t list) ->
                  let call_label = Call_graph.G.E.label call_e in
                  if Call_graph.equal_edge_kind call_label.Call_graph.kind
                       Call_graph.Call
                     && Function_id.equal (Call_graph.G.E.dst call_e) impl
                  then call_e :: inner_acc
                  else inner_acc)
               graph iface acc
         | Call_graph.Call -> acc)
      graph []
  in
  List.iter (Call_graph.G.remove_edge_e graph) to_remove;
  List.length to_remove

(* None when sources/sinks or the resulting subgraph are empty. *)
let compute_rule_subgraph
    ~(xconf : Match_env.xconfig)
    ~(lc : lang_context)
    ~(specs : rule_specs)
    : rule_subgraph option =
  let rule = specs.rs_rule in
  let sources = specs.rs_sources in
  let sinks = specs.rs_sinks in
  if List_.null sources && List_.null sinks then begin
    Log.info (fun m ->
        m "interfile dispatch: rule %s has no sources or sinks, skipping"
          (Rule_ID.to_string (fst rule.R.id)));
    None
  end else
    let xconf' =
      Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
    in
    let interfile_graph = lc.lc_interfile_graph in
    let interfile_depth =
      Some xconf'.Match_env.config.taint_interfile_depth
    in
    let src_in = List.filter (Call_graph.G.mem_vertex interfile_graph) sources in
    let snk_in = List.filter (Call_graph.G.mem_vertex interfile_graph) sinks in
    Log.info (fun m ->
        m "interfile dispatch: rule %s: %d sources, %d sinks, \
           sources_in_graph=%d sinks_in_graph=%d"
          (Rule_ID.to_string (fst rule.R.id))
          (List.length sources) (List.length sinks)
          (List.length src_in) (List.length snk_in));
    (* An empty side only proves there is no flow when the scan covered every
       file the graph knows about; on a partial scan the counterpart may sit in
       a file that was never a target and so never had its specs extracted. *)
    let scan_is_partial =
      let targets =
        lc.lc_matching_targets
        |> List.map (fun (t : interfile_target) -> Fpath.normalize t.abs_path)
        |> FpathSet.of_list
      in
      Interfile_graph.files_of_graph interfile_graph
      |> List.exists (fun f -> not (FpathSet.mem (Fpath.normalize f) targets))
    in
    let relevant_graph =
      match sources, sinks with
      | [], seeds
      | seeds, [] when scan_is_partial ->
        Graph_reachability.compute_seeded_subgraph ?depth:interfile_depth
          ~g_global:interfile_graph
          (Call_graph.G.create ()) ~seeds
      | _ ->
        Graph_reachability.compute_relevant_subgraph ?depth:interfile_depth
          ~g_global:interfile_graph
          (Call_graph.G.create ()) ~sources ~sinks
    in
    Log.info (fun m ->
        m "interfile dispatch: rule %s: relevant subgraph %d vertices, %d edges"
          (Rule_ID.to_string (fst rule.R.id))
          (Call_graph.G.nb_vertex relevant_graph)
          (Call_graph.G.nb_edges relevant_graph));
    let _n_pruned = prune_impl_interface_cycles relevant_graph in
    let topo_order =
      Call_graph.Topo.fold
        (fun (fn : Function_id.t) (acc : Function_id.t list) -> fn :: acc)
        relevant_graph []
      |> List.rev
    in
    let subgraph_files = Interfile_graph.files_of_graph relevant_graph in
    let graph_fid_set = fid_set_of_graph relevant_graph in
    (* Include target files with source/sink matches but no subgraph
       vertices, so their epilogue still runs. *)
    let source_sink_files =
      List.filter_map Function_id.file_of (sources @ sinks)
      |> List.map Fpath.normalize
      |> List.sort_uniq Fpath.compare
    in
    let files =
      let subgraph_file_set = FpathSet.of_list subgraph_files in
      let extra =
        List.filter (fun fp -> not (FpathSet.mem fp subgraph_file_set))
          source_sink_files
      in
      if extra <> [] then
        Log.info (fun m ->
            m "interfile dispatch: rule %s: including %d target files \
               with source/sink matches but no subgraph vertices"
              (Rule_ID.to_string (fst rule.R.id))
              (List.length extra));
      subgraph_files @ extra
    in
    (* Source/sink functions not in the graph; appended (no edges). *)
    let orphan_fids =
      List.filter
        (fun fid -> not (FidSet.mem fid graph_fid_set))
        (sources @ sinks)
    in
    if orphan_fids <> [] then
      Log.info (fun m ->
          m "interfile dispatch: rule %s: appending %d orphan \
             source/sink fids to topo order"
            (Rule_ID.to_string (fst rule.R.id))
            (List.length orphan_fids));
    let topo_order = topo_order @ orphan_fids in
    let fid_set = FidSet.union graph_fid_set (FidSet.of_list orphan_fids) in
    if List_.null files then None
    else
      Some {
        rsg_lang_context = lc;
        rsg_specs = specs;
        rsg_xconf = xconf';
        rsg_relevant_graph = relevant_graph;
        rsg_topo_order = topo_order;
        rsg_files = files;
        rsg_fid_set = fid_set;
      }

(* Precondition: all [rsg] files are in [ast_table]. *)
let init_rule_state
    ~(ast_table : (Fpath.t, G.program) Hashtbl.t)
    ~(target_root_map : Fpath.t option FpathMap.t)
    (rsg : rule_subgraph)
    : rule_state =
  let lang = rsg.rsg_lang_context.lc_lang in
  let rule = rsg.rsg_specs.rs_rule in
  let init_acc =
    List.fold_left
      (fun (acc : file_init_acc) (file_path : Fpath.t) ->
         let path_root = path_root_for_file target_root_map file_path in
         try
           init_file ~lang ~rule ~xconf:rsg.rsg_xconf ~path_root
             ~fid_set:rsg.rsg_fid_set
             ~ast_table ~file_path acc
         with
         | (Out_of_memory | Stack_overflow | Time_limit.Timeout _) as exn ->
           let bt = Printexc.get_backtrace () in
           Log.warn (fun m ->
               m "interfile dispatch: fatal %s in init_file %s for rule %s\n%s"
                 (Printexc.to_string exn)
                 (Fpath.to_string file_path)
                 (Rule_ID.to_string (fst rule.R.id)) bt);
           Exception.catch_and_reraise exn
         | exn ->
           (* See [parse_file_batch]: [semgrep.tainting] is muted by default,
              and this drops one file's functions from the rule's interfile
              state. *)
           Logs.warn (fun m ->
               m "interfile dispatch: skipping file %s for rule %s, its \
                  functions are excluded from cross-file analysis: %s"
                 (Fpath.to_string file_path)
                 (Rule_ID.to_string (fst rule.R.id))
                 (Printexc.to_string exn));
           acc)
      { fi_info_map = FunctionMap.empty;
        fi_file_envs = FpathMap.empty; }
      rsg.rsg_files
  in
  {
    rule;
    lang;
    relevant_graph = rsg.rsg_relevant_graph;
    topo_order = rsg.rsg_topo_order;
    info_map = init_acc.fi_info_map;
    file_envs = init_acc.fi_file_envs;
    builtin_signature_db =
      Some (Builtin_models.create_all_builtin_models lang);
    match_on = Match_tainting_mode.match_on_of_xconf rsg.rsg_xconf;
    target_root_map;
  }

(* Returns None (not raise) on a miss: a raise plus Core_scan's silent-warn
   handler would drop every finding for the rule. *)
let taint_inst_of_info (fid : Function_id.t)
    (info : Match_tainting_mode.fun_info)
    : (Taint_rule_inst.t * G.program) option =
  match info.Match_tainting_mode.taint_inst,
        info.Match_tainting_mode.file_ast with
  | Some ti, Some ast -> Some (ti, ast)
  | None, _ ->
    Log.warn (fun m ->
        m "interfile: function %s missing taint_inst — skipping (likely \
           init_file bug)"
          (Function_id.show_debug fid));
    None
  | _, None ->
    Log.warn (fun m ->
        m "interfile: function %s missing file_ast — skipping (likely \
           init_file bug)"
          (Function_id.show_debug fid));
    None

let glob_env_of_fid (rs : rule_state) (fid : Function_id.t) : Lval_env.t =
  match file_of_fid fid with
  | Some fp ->
    (match FpathMap.find_opt fp rs.file_envs with
     | Some fe -> fe.glob_env
     | None ->
       Log.warn (fun m ->
           m "interfile dispatch: no file_env for %s (function %s)"
             (Fpath.to_string fp)
             (Function_id.show_debug fid));
       Lval_env.empty)
  | None ->
    Log.warn (fun m ->
        m "interfile dispatch: function %s has no file path"
          (Function_id.show_debug fid));
    Lval_env.empty

let extract_and_check_function
    (rs : rule_state)
    (fid : Function_id.t)
    (info : Match_tainting_mode.fun_info)
    ~(detect_findings : bool)
    (db : Shape_and_sig.signature_database)
    : Shape_and_sig.signature_database * PM.t list =
  match taint_inst_of_info fid info with
  | None ->
    (db, [])
  | Some (fn_taint_inst, fun_ast) ->
    let glob_env = glob_env_of_fid rs fid in
    let updated_db, findings =
      (* No [~call_graph]: interfile callee resolution is sid-only (the
         [id_resolved] def-site sids stamped by projidx). The local
         call-graph fallback is for the intrafile path. *)
      Match_tainting_mode.extract_and_check
        ?builtin_signature_db:rs.builtin_signature_db
        ~glob_env
        ~lang:rs.lang ~db ~match_on:rs.match_on
        ~taint_inst:fn_taint_inst ~ast:fun_ast
        ~detect_findings
        info
    in
    if not (List_.null findings) then
      Log.debug (fun m ->
          m "interfile: rule=%s fn=%s found %d match(es)"
            (Rule_ID.to_string (fst rs.rule.R.id))
            (IL.show_name info.Match_tainting_mode.name)
            (List.length findings));
    (updated_db, findings)

let rule_id_of (rs : rule_state) : Rule_ID.t =
  fst rs.rule.R.id


(* Consumed by tools/opengrep-interfile-graph (not built by [make core]). *)
let relevant_graph_of (rs : rule_state) : Call_graph.G.t =
  rs.relevant_graph

let topo_order_of (rs : rule_state) : Function_id.t list =
  rs.topo_order

(* Prefer id_resolved_alternatives (AST mirror of Dispatch edges), fall
   back to graph dispatch_predecessors; drop self-references. *)
let dispatch_impls (rs : rule_state) (fid : Function_id.t) : Function_id.t list =
  let from_alts =
    match FunctionMap.find_opt fid rs.info_map with
    | None -> []
    | Some info ->
      !(info.Match_tainting_mode.name.IL.id_info.G.id_resolved_alternatives)
      |> List.filter_map (fun ((_, sid) : G.resolved_name) ->
             if G.SId.is_unsafe_default sid then None
             else Some (Function_id.of_sid sid))
  in
  let impls =
    match from_alts with
    | [] -> Call_graph.dispatch_predecessors rs.relevant_graph fid
    | xs -> xs
  in
  List.filter (fun (pred : Function_id.t) ->
      not (Function_id.equal pred fid)) impls

let dispatch_merge_fbdecl (rs : rule_state)
    (fid : Function_id.t) (fid_arity : int)
    (db : Shape_and_sig.signature_database)
    : Shape_and_sig.signature_database =
  let dpreds = dispatch_impls rs fid in
  let impl_sigs =
    dpreds
    |> List.filter_map (fun (pred : Function_id.t) ->
           Shape_and_sig.lookup_signature db pred fid_arity)
  in
  let interface_sig_opt =
    Shape_and_sig.lookup_signature db fid fid_arity
  in
  match interface_sig_opt, impl_sigs with
  | _, [] -> db
  | None, _ ->
      Log.debug (fun m ->
          m "merge_dispatch: interface sig not found for %s, \
             skipping dispatch merge"
            (Function_id.show_debug fid));
      db
  | Some interface_sig, _ ->
      let merged =
        Sig_inst.merge_dispatch_signatures impl_sigs interface_sig
      in
      let ext_sig =
        { Shape_and_sig.sig_ = merged;
          arity =
            Shape_and_sig.Arity_exact
              (List.length merged.Shape_and_sig.Signature.params) }
      in
      Shape_and_sig.replace_signature db fid ext_sig

let initial_sig_db (_rs : rule_state) : Shape_and_sig.signature_database =
  Builtin_models.init_signature_database None

let fid_arity_of (rs : rule_state) (info : Match_tainting_mode.fun_info)
    : int =
  Match_tainting_mode.get_arity
    (Tok.unbracket info.Match_tainting_mode.fdef.AST_generic.fparams)
    info rs.lang

(* SCC-aware signature fixpoint over the relevant call graph.  A plain
   topological fold ([Call_graph.Topo]) visits the members of a cycle in
   arbitrary order, so a caller inside a cycle can be summarised before its
   mutual callee and produce an incomplete signature (mutual recursion, and
   indirect impl<->interface dispatch cycles).  Iterate each cyclic SCC to a
   fixpoint; singleton SCCs without a self-loop run once, as a plain single
   pass would. *)
module Sig_lattice = struct
  type t = Shape_and_sig.SignatureSet.t

  (* Guard-aware: plain [equal] is guard-blind and would declare the SCC
     converged while an effect's guard still refines (Clojure length-atom
     guards exist even with [effect_guards] off). *)
  let equal = Shape_and_sig.SignatureSet.equal_with_guards
end

module Sig_store = struct
  type t = Shape_and_sig.signature_database
  type node = Function_id.t
  type lattice = Shape_and_sig.SignatureSet.t
  let get (n : Function_id.t) (db : Shape_and_sig.signature_database) : lattice =
    match FunctionMap.find_opt n db.Shape_and_sig.signatures with
    | Some s -> s
    | None -> Shape_and_sig.SignatureSet.empty
  let set (n : Function_id.t) (s : lattice)
      (db : Shape_and_sig.signature_database) : t =
    { Shape_and_sig.signatures =
        FunctionMap.add n s db.Shape_and_sig.signatures }
end

module Sig_engine =
  Graph_fixpoint.Make (Call_graph.G) (Sig_lattice) (Sig_store)

let topo_fold ~(detect_findings : bool) (rs : rule_state)
    : Shape_and_sig.signature_database * PM.t list =
  let initial_db = initial_sig_db rs in
  (* Extract a function's own signature(s) and REPLACE its db entry with just
     those, so repeated fixpoint iterations don't accumulate several same-arity
     sigs (which makes [find_by_arity] give up). *)
  let extract_replace (fid : Function_id.t)
      (info : Match_tainting_mode.fun_info)
      (db : Shape_and_sig.signature_database)
      : Shape_and_sig.signature_database =
    match taint_inst_of_info fid info with
    | None -> db
    | Some (fn_taint_inst, fun_ast) ->
      let db', fresh =
        Match_tainting_mode.extract_signatures
          ?builtin_signature_db:rs.builtin_signature_db
          ~lang:rs.lang ~db ~taint_inst:fn_taint_inst ~ast:fun_ast info
      in
      let fresh_set =
        List.fold_left
          (fun acc (xs : Shape_and_sig.extended_sig) ->
            (* Widen stored shapes: a self-recursive tree-builder nests its
               return shape one level deeper per SCC round (no fixpoint in
               the shape domain); truncating at the store point cuts the
               ascending chain where the cost is incurred. *)
            let xs =
              { xs with
                Shape_and_sig.sig_ =
                  Taint_shape.truncate_signature
                    ~max_depth:Limits_semgrep.taint_MAX_SIG_SHAPE_DEPTH
                    xs.Shape_and_sig.sig_ }
            in
            Shape_and_sig.SignatureSet.add xs acc)
          Shape_and_sig.SignatureSet.empty fresh
      in
      Sig_store.set fid fresh_set db'
  in
  (* Phase 1: SCC signature fixpoint, no finding emission. *)
  let analyze (fid : Function_id.t)
      (db : Shape_and_sig.signature_database)
      : Shape_and_sig.signature_database =
    match FunctionMap.find_opt fid rs.info_map with
    | None -> db
    | Some info -> (
        match info.Match_tainting_mode.fdef.G.fbody with
        | G.FBDecl _
        | G.FBNothing ->
          (* Interface/abstract: signature comes from merging concrete impls.
             Don't store an empty sig when no impls exist — unsound (callers
             would see "no effects" instead of conservative propagation).
             [dispatch_merge_fbdecl] replaces the interface entry, so it does
             not accumulate across iterations. *)
          let fid_arity = fid_arity_of rs info in
          let dpreds = dispatch_impls rs fid in
          let has_impls =
            dpreds
            |> List.exists (fun (pred : Function_id.t) ->
                   Option.is_some
                     (Shape_and_sig.lookup_signature db pred fid_arity))
          in
          if not has_impls then db
          else dispatch_merge_fbdecl rs fid fid_arity (extract_replace fid info db)
        | _ -> extract_replace fid info db)
  in
  (* [SCC.scc_list] returns components in an order where callers precede
     callees for our callee->caller edges; reverse for callees-first. *)
  let sccs_callees_first =
    List.rev (Call_graph.SCC.scc_list rs.relevant_graph)
  in
  (* Edge-less SOURCE seeds are outside the SCC list, so nothing else
     computes their signature — yet the epilogue and the [id_resolved]-stamp
     channel both reach one without any graph edge.  Restricted to sources:
     an edge-less sink anchors a finding where it already is and needs no
     signature, and analyzing every orphan means a CFG build plus dataflow
     per seed per rule (minutes on a large corpus). *)
  let converged_db =
    Sig_engine.run ~max_iter:20
      ~on_max_iter:(fun (members : Function_id.t list) ->
        Log.warn (fun m ->
            m "interfile: rule %s: signature SCC of size %d hit max_iter, \
               using current DB"
              (Rule_ID.to_string (fst rs.rule.R.id))
              (List.length members)))
      ~sccs:sccs_callees_first ~graph:rs.relevant_graph ~analyze initial_db
  in
  (* Phase 2: single match-emission pass over the converged DB.  Every
     function's callees already have their final signatures, so order is
     irrelevant and the DB is not threaded. *)
  let emit (matches_acc : PM.t list) (fid : Function_id.t) : PM.t list =
    match FunctionMap.find_opt fid rs.info_map with
    | None -> matches_acc
    | Some info -> (
        match info.Match_tainting_mode.fdef.G.fbody with
        | G.FBDecl _
        | G.FBNothing -> matches_acc
        | _ ->
          let fid_file = file_of_fid fid in
          let fid_is_target =
            match fid_file with
            | Some fp -> is_target_file rs.target_root_map fp
            | None -> false
          in
          (* Detect in companions too, not just targets.  A cross-file finding
             is emitted while analysing the CALLER that instantiates the
             callee's signature, so when only the sink's file was targeted the
             caller is a companion and gating on it drops the finding even
             though the sink itself is in scope.  Companion findings are then
             filtered to target files below, so nothing outside the requested
             scope is reported.  On a whole-project scan every file is a target
             and this costs nothing. *)
          let do_detect = detect_findings && Option.is_some fid_file in
          if not do_detect then matches_acc
          else
            (* The re-extraction against [converged_db] is NOT redundant:
               [check_fundef_with_cfg] depends on the immediately preceding
               extraction against a db that already holds this function's own
               converged signature (and its lambda/nested sub-signatures).
               Checking against [converged_db] without re-extracting collapses
               some sink ranges onto the taint origin, and skipping the
               phase-1 summary for no-successor nodes loses source-variant
               findings.  Both were observed on gitlab; don't optimise this
               away without an A/B on a large corpus. *)
            let _db, findings =
              extract_and_check_function rs fid info ~detect_findings:true
                converged_db
            in
            let findings =
              if fid_is_target then findings
              else
                List.filter
                  (fun (pm : PM.t) ->
                     is_target_file rs.target_root_map
                       pm.PM.path.Target.internal_path_to_content)
                  findings
            in
            List.rev_append findings matches_acc)
  in
  let matches = List.fold_left emit [] rs.topo_order in
  (converged_db, matches)

(* Consumed by tools/opengrep-interfile-graph (not built by [make core]). *)
let extract_signatures (rs : rule_state)
    : Shape_and_sig.signature_database =
  Log.info (fun m ->
      m "interfile dispatch: extracting signatures for rule %s (%d functions)"
        (Rule_ID.to_string (fst rs.rule.R.id))
        (List.length rs.topo_order));
  let final_db, _no_findings =
    topo_fold ~detect_findings:false rs
  in
  final_db

(* Findings must report the path the scan was given, not the normalised
   absolute form used internally for graph identity: the reported path is
   hashed verbatim into the finding's fingerprint (match_based_id), so an
   absolute path ties fingerprints to the checkout directory and breaks
   cross-run comparison (e.g. between CI runners).  [target_root_map]
   records each target's original anchor: [Some root] means the target was
   given relative to [root] (restore that), [None] means it was given
   absolute (keep it, matching what a per-target scan would report).
   Non-target companion files stay absolute; they were never targets and
   only appear inside taint traces. *)
let rebase_file (target_root_map : Fpath.t option FpathMap.t)
    (file : Fpath.t) : Fpath.t option =
  match FpathMap.find_opt (Fpath.normalize file) target_root_map with
  | Some (Some root) -> Fpath.relativize ~root file
  | Some None
  | None ->
      None

let rebase_loc (rebase : Fpath.t -> Fpath.t option) (loc : Tok.location)
    : Tok.location =
  match rebase loc.Tok.pos.Pos.file with
  | Some file -> { loc with Tok.pos = { loc.Tok.pos with Pos.file = file } }
  | None -> loc

let rebase_tok (rebase : Fpath.t -> Fpath.t option) (tok : Tok.t) : Tok.t =
  Tok.fix_location (rebase_loc rebase) tok

let rec rebase_call_trace (rebase : Fpath.t -> Fpath.t option)
    (ct : Taint_trace.call_trace) : Taint_trace.call_trace =
  match ct with
  | Taint_trace.Toks toks ->
      Taint_trace.Toks (List_.map (rebase_tok rebase) toks)
  | Taint_trace.Call { call_toks; intermediate_vars; call_trace } ->
      Taint_trace.Call
        {
          call_toks = List_.map (rebase_tok rebase) call_toks;
          intermediate_vars = List_.map (rebase_tok rebase) intermediate_vars;
          call_trace = rebase_call_trace rebase call_trace;
        }

let rebase_trace (rebase : Fpath.t -> Fpath.t option) (trace : Taint_trace.t)
    : Taint_trace.t =
  List_.map
    (fun (item : Taint_trace.item) ->
      {
        Taint_trace.source_trace = rebase_call_trace rebase item.source_trace;
        tokens = List_.map (rebase_tok rebase) item.Taint_trace.tokens;
        sink_trace = rebase_call_trace rebase item.Taint_trace.sink_trace;
      })
    trace

let rebase_pm (target_root_map : Fpath.t option FpathMap.t) (pm : PM.t)
    : PM.t =
  let rebase = rebase_file target_root_map in
  let path =
    let internal_path_to_content =
      match rebase pm.PM.path.Target.internal_path_to_content with
      | Some p -> p
      | None -> pm.PM.path.Target.internal_path_to_content
    in
    let origin =
      match pm.PM.path.Target.origin with
      | Origin.File f as orig -> (
          match rebase f with
          | Some p -> Origin.File p
          | None -> orig)
      | orig -> orig
    in
    { Target.internal_path_to_content; origin }
  in
  let min_loc, max_loc = pm.PM.range_loc in
  let range_loc = (rebase_loc rebase min_loc, rebase_loc rebase max_loc) in
  let taint_trace =
    Option.map
      (fun (lz : Taint_trace.t Lazy.t) ->
        lazy (rebase_trace rebase (Lazy.force lz)))
      pm.PM.taint_trace
  in
  let tokens =
    lazy (List_.map (rebase_tok rebase) (Lazy.force pm.PM.tokens))
  in
  { pm with PM.path; range_loc; taint_trace; tokens }

let run_rule (rs : rule_state) : PM.t list =
  (* The constructor-instance-vars table is domain-local and keyed only by
     [file:class]; without this reset it would carry a prior rule's
     constructor taint into this rule when both run on the same domain. *)
  Dataflow_tainting.reset_constructor ();
  (* Same task boundary for the guard-atom intern table: it is domain-local
     and cleared per target in the intrafile path, but a run of interfile
     rules on one domain would otherwise let it grow unbounded. *)
  Effect_guard.reset_intern ();
  let effects_to_matches =
    Match_tainting_mode.pms_of_effects ~lang:rs.lang ~match_on:rs.match_on
  in
  (* glob_effects for target files (need no signatures). *)
  let glob_matches =
    FpathMap.fold
      (fun (file_path : Fpath.t) (fe : file_env) (acc : PM.t list) ->
         if is_target_file rs.target_root_map file_path then
           List.rev_append (effects_to_matches fe.glob_effects) acc
         else acc)
      rs.file_envs []
  in
  (* Topo fold: sig_db grows monotonically, callees precede callers. *)
  let final_db, topo_matches =
    topo_fold ~detect_findings:true rs
  in
  (* Per-file epilogue (class-init + top-level) for target files.  Skip a
     file whose [<top_level>] vertex is outside rs.topo_order: its
     non-function code is on no source→sink path and can't produce a ToSink. *)
  let topo_universe = FidSet.of_list rs.topo_order in
  let epilogue_relevant (file_path : Fpath.t) (fe : file_env) : bool =
    let top_fid =
      Function_id.of_il_name
        (Graph_from_AST.top_level_name_of_ast fe.ast)
      |> Interfile_graph.absolutify_fid
           (path_root_for_file rs.target_root_map file_path)
    in
    FidSet.mem top_fid topo_universe
  in
  let epilogue_matches =
    FpathMap.fold
      (fun (file_path : Fpath.t) (fe : file_env) (acc : PM.t list) ->
         if not (is_target_file rs.target_root_map file_path) then acc
         else if not (epilogue_relevant file_path fe) then acc
         else
           (* Built per rule: only epilogue-relevant files reach here, and
              the dataflow check below dominates the cfg build. *)
           let top_cfg =
             Match_tainting_mode.build_top_level_cfg rs.lang fe.ast
           in
           let class_init_cfgs =
             Match_tainting_mode.build_class_init_cfgs rs.lang fe.ast
           in
           let class_init_effects =
             Match_tainting_mode.check_class_inits_prebuilt fe.taint_inst
               class_init_cfgs
               ~signature_db:final_db
               ?builtin_signature_db:rs.builtin_signature_db
               ()
           in
           let top_effects =
             Match_tainting_mode.check_top_level_prebuilt fe.taint_inst
               top_cfg
               ~signature_db:final_db
               ?builtin_signature_db:rs.builtin_signature_db
               ()
           in
           List.rev_append (effects_to_matches class_init_effects)
             (List.rev_append (effects_to_matches top_effects) acc))
      rs.file_envs []
  in
  List.rev_append glob_matches
    (List.rev_append topo_matches epilogue_matches)
  |> List_.map (rebase_pm rs.target_root_map)
  |> PM.uniq
  |> PM.no_submatches

(* Parallel when ncores>1 and >1 item, else serial; failures go to [on_exn],
   which runs inside the worker domain, so it must not touch shared state.
   Workers' results come back as one list per outcome: successes, and the
   values [on_exn] returned for the failed items. *)
let run_parmap (caps : < Cap.fork >) ~(ncores : int) ~on_exn f items =
  let n = List.length items in
  let results =
    if ncores <= 1 || n <= 1 then
      List_.map
        (Domainslib_.wrap_result f ~exception_handler:on_exn)
        items
    else
      Domainslib_.parmap caps
        ~num_domains:(min ncores n)
        ~chunksize:1 ~exception_handler:on_exn
        f items
  in
  List.partition_map
    (function
      | Ok value -> Either.Left value
      | Error failure -> Either.Right failure)
    results

let parse_batch_size = 500

let spec_extract_batch_size = 2000

let chunks (n : int) (xs : 'a list) : 'a list list =
  let rec loop done_chunks cur cur_len = function
    | [] when cur_len = 0 -> List.rev done_chunks
    | [] -> List.rev (List.rev cur :: done_chunks)
    | item :: rest when cur_len < n ->
      loop done_chunks (item :: cur) (cur_len + 1) rest
    | item :: rest ->
      loop (List.rev cur :: done_chunks) [item] 1 rest
  in
  loop [] [] 0 xs

(* Reuse a [resolved] projidx AST when present (it carries cross-file
   id_resolved); otherwise fresh-parse. *)
let parse_file_batch
    ?(resolved : (string, G.program) Hashtbl.t = Hashtbl.create 0)
    (lang : Lang.t) (files : Fpath.t list)
    : (Fpath.t, G.program) Hashtbl.t =
  let tbl = Hashtbl.create (List.length files) in
  List.iter (fun (file : Fpath.t) ->
    let key = Fpath.to_string (Fpath.normalize file) in
    match Hashtbl.find_opt resolved key with
    | Some ast -> Hashtbl.replace tbl file ast
    | None ->
      (match
        (try Some (parse_file lang file)
         with
         | (Out_of_memory | Stack_overflow
           | Time_limit.Timeout _) as exn ->
           Exception.catch_and_reraise exn
         | exn ->
           (* [Logs] rather than [Log]: the [semgrep.tainting] source is
              skipped by default even under [--debug], so routing this there
              drops the file from analysis with no visible trace at all.  Only
              an internal failure reaches here — malformed input is either
              recovered by the parser or excluded upstream — so it means a bug,
              and must not be silent. *)
           Logs.warn (fun m ->
               m "interfile parse: failed to parse %s, excluding it from \
                  cross-file analysis: %s"
                 (Fpath.to_string file)
                 (Printexc.to_string exn));
           None)
      with
      | None -> ()
      | Some ast -> Hashtbl.replace tbl file ast))
    files;
  tbl

let build_ast_lookup
    (batch_results : (Lang.t * (Fpath.t, G.program) Hashtbl.t) list)
    : (Lang.t, (Fpath.t, G.program) Hashtbl.t) Hashtbl.t =
  let tbl = Hashtbl.create 4 in
  List.iter (fun ((lang : Lang.t),
                  (batch_tbl : (Fpath.t, G.program) Hashtbl.t)) ->
    let merged =
      match Hashtbl.find_opt tbl lang with
      | Some existing -> existing
      | None ->
        let fresh = Hashtbl.create 256 in
        Hashtbl.replace tbl lang fresh;
        fresh
    in
    Hashtbl.iter (Hashtbl.replace merged) batch_tbl)
    batch_results;
  tbl

let ast_table_for_lang
    (ast_lookup : (Lang.t, (Fpath.t, G.program) Hashtbl.t) Hashtbl.t)
    (lang : Lang.t)
    : (Fpath.t, G.program) Hashtbl.t =
  match Hashtbl.find_opt ast_lookup lang with
  | Some tbl -> tbl
  | None -> Hashtbl.create 0

(* Companion files: in subgraphs but not among targets. *)
let parse_companion_files
    (caps : < Cap.fork >)
    ~(ncores : int)
    ~(resolved : (string, G.program) Hashtbl.t)
    ~(target_ast_lookup :
        (Lang.t, (Fpath.t, G.program) Hashtbl.t) Hashtbl.t)
    ~(lang_contexts : lang_context list)
    (rule_subgraphs : rule_subgraph list)
    : (Lang.t * (Fpath.t, G.program) Hashtbl.t) list
      * (Fpath.t * string) list =
  let seen = Hashtbl.create 256 in
  let companion_files : (Lang.t * Fpath.t) list =
    List.concat_map
      (fun (rsg : rule_subgraph) ->
         let lang = rsg.rsg_lang_context.lc_lang in
         let ast_tbl = ast_table_for_lang target_ast_lookup lang in
         List_.filter_map
           (fun (file : Fpath.t) ->
              if Hashtbl.mem ast_tbl file || Hashtbl.mem seen file then
                None
              else begin
                Hashtbl.replace seen file true;
                Some (lang, file)
              end)
           rsg.rsg_files)
      rule_subgraphs
  in
  if List_.null companion_files then ([], [])
  else begin
    Log.info (fun m ->
        m "interfile parse: %d companion files to parse"
          (List.length companion_files));
    let by_lang : (Lang.t, Fpath.t list) Hashtbl.t =
      Hashtbl.create 4
    in
    List.iter (fun ((lang : Lang.t), (file : Fpath.t)) ->
      let existing =
        match Hashtbl.find_opt by_lang lang with
        | Some fs -> fs
        | None -> []
      in
      Hashtbl.replace by_lang lang (file :: existing))
      companion_files;
    let companion_batches : (Lang.t * Fpath.t list) list =
      List.concat_map (fun (lc : lang_context) ->
        let lang = lc.lc_lang in
        match Hashtbl.find_opt by_lang lang with
        | None -> []
        | Some files ->
          chunks parse_batch_size files
          |> List_.map (fun (batch : Fpath.t list) -> (lang, batch)))
        lang_contexts
    in
    let parsed, (failed_batches : (Lang.t * Fpath.t list * string) list) =
      run_parmap caps ~ncores
        ~on_exn:(fun ((lang, batch) : Lang.t * Fpath.t list)
                     (exn : Exception.t) ->
          let msg = Printexc.to_string (Exception.get_exn exn) in
          Log.warn (fun m ->
              m "interfile parse: %s companion batch failed: %s"
                (Lang.to_string lang) msg);
          (lang, batch, msg))
        (fun ((lang, batch) : Lang.t * Fpath.t list) ->
          let tbl = parse_file_batch ~resolved lang batch in
          Log.info (fun m ->
              m "interfile parse: %s: parsed %d/%d companion files"
                (Lang.to_string lang)
                (Hashtbl.length tbl)
                (List.length batch));
          (lang, tbl))
        companion_batches
    in
    (* Companion files are not scan targets, so there is no intrafile
       fallback for them; the loss is cross-file recall through their
       functions.  Surface it per file rather than only in the logs. *)
    let failures : (Fpath.t * string) list =
      List.concat_map
        (fun ((lang, batch, msg) : Lang.t * Fpath.t list * string) ->
           List_.map
             (fun (file : Fpath.t) ->
                (file,
                 Printf.sprintf "%s: interfile companion parse failed: %s"
                   (Lang.to_string lang) msg))
             batch)
        failed_batches
    in
    (parsed, failures)
  end

(* Returns rule_states, interfile langs, and per-rule fallback target paths. *)
let build_rule_states
    (caps : < Cap.fork >)
    ~(ncores : int)
    ~(taint_interfile : bool)
    ~(valid_rules : R.rule list)
    ~(targets : Target.t list)
    ~(targeting_conf : Find_targets.conf)
    ~(xconf : Match_env.xconfig)
    : rule_state list * Xlang.t list * (Rule_ID.t * Fpath.t list) list
      * (Fpath.t * string) list =
  (* A rule-local option counts, not just the global flag. *)
  let lang_rules =
    interfile_taint_rules_by_lang ~taint_interfile valid_rules
  in
  match lang_rules with
  | [] -> ([], [], [], [])
  | _ ->
  Log.info (fun m ->
      m "interfile preprocess: %d languages with interfile taint rules"
        (List.length lang_rules));
  let cwd = Fpath.v (Sys.getcwd ()) in
  let target_root_map = build_target_root_map ~cwd targets in
  (* Partial scans point [project_root] at the repo, not the file's parent;
     missing root falls back to cwd. *)
  let project_root_of_target (target : Target.t) : Fpath.t =
    match target with
    | Regular { project_root = Some root; _ } -> Fpath.normalize root
    | Regular _ | Lockfile _ -> cwd
  in
  let targets_by_root : (string, Fpath.t * Target.t list) Hashtbl.t =
    Hashtbl.create 4
  in
  List.iter (fun (target : Target.t) ->
    let root = project_root_of_target target in
    let key = Fpath.to_string root in
    let _, cur =
      match Hashtbl.find_opt targets_by_root key with
      | Some entry -> entry
      | None -> (root, [])
    in
    Hashtbl.replace targets_by_root key (root, target :: cur))
    targets;
  if Hashtbl.length targets_by_root > 1 then
    Log.info (fun m ->
        m "interfile preprocess: targets span %d project roots; \
           building one interfile graph per (lang, root)"
          (Hashtbl.length targets_by_root));
  (* Target abs_paths interfile dispatch won't cover, needing intrafile
     fallback; consumed by [Core_scan]'s per-target gate. *)
  let fallback_target_paths_by_lang_root :
      (string * string, Fpath.t list) Hashtbl.t =
    Hashtbl.create 4
  in
  let record_fallback_paths ~lang ~project_root new_paths =
    let key =
      (Lang.to_lowercase_alnum lang,
       Fpath.to_string (Fpath.normalize project_root))
    in
    let existing =
      Option.value ~default:[]
        (Hashtbl.find_opt fallback_target_paths_by_lang_root key)
    in
    Hashtbl.replace fallback_target_paths_by_lang_root key
      (List.rev_append new_paths existing)
  in
  let record_fallback ~lang ~project_root targets =
    record_fallback_paths ~lang ~project_root
      (List.filter_map (Target.abs_path ~cwd) targets)
  in
  (* Abs-path keys are globally unique, so merging across roots is safe. *)
  let projidx_asts : (string, G.program) Hashtbl.t = Hashtbl.create 1024 in
  (* Per language: the context (when the build is usable) and the build's
     per-file failures — files whose functions/edges are missing from the
     graph.  The failures become scan errors so the recall loss is visible. *)
  let per_lang :
      (lang_context option * (Fpath.t * string) list) list =
    Hashtbl.fold (fun _ (project_root, root_targets) acc ->
      List_.map
        (fun ((lang : Lang.t), (rules : R.taint_rule list)) ->
          (* Fallback recording must stay lang-scoped: a multi-language
             rule [L (Js,[Ts])] has a context per language, so recording
             another language's targets here would double-run them —
             dispatched by their own context AND per-target via the
             fallback gate. *)
          let lang_targets =
            List.filter (fun (target : Target.t) ->
              match target with
              | Regular { analyzer; _ } ->
                (match Xlang.to_lang analyzer with
                 | Ok target_lang -> Lang.equal target_lang lang
                 | Error _ -> false)
              | Lockfile _ -> false)
              root_targets
          in
          let build_opt =
            Interfile_graph.load_interfile_build caps
              ~ncores ~targeting_conf lang project_root
          in
          (match build_opt with
           | Some (_, asts, _) ->
             Hashtbl.iter (Hashtbl.replace projidx_asts) asts
           | None -> ());
          let file_failures =
            match build_opt with
            | None -> []
            | Some (_, _, failures) ->
              List_.map (fun (file, msg) ->
                (file, Printf.sprintf "%s: %s" (Lang.to_string lang) msg))
                failures
          in
          let lc_opt =
          match build_opt with
          | None ->
            Log.warn (fun m ->
                m "interfile preprocess: project_index build failed for \
                   %s under %s; affected taint rules will fall back to \
                   intrafile for that root"
                  (Lang.to_string lang) (Fpath.to_string project_root));
            record_fallback ~lang ~project_root lang_targets;
            None
          | Some (interfile_graph, _, _) ->
            let interfile_files = interfile_file_set interfile_graph in
            let matching_targets =
              targets_in_interfile_graph ~lang ~cwd
                ~interfile_files lang_targets
            in
            let matched_paths = Hashtbl.create (List.length matching_targets) in
            List.iter (fun (it : interfile_target) ->
              Hashtbl.replace matched_paths (Fpath.normalize it.abs_path) ())
              matching_targets;
            let unmatched =
              List.filter (fun target ->
                match Target.abs_path ~cwd target with
                | None -> false
                | Some path -> not (Hashtbl.mem matched_paths path))
                lang_targets
            in
            (match matching_targets with
             | [] ->
               Log.warn (fun m ->
                   m "interfile preprocess: no scan targets present in \
                      the interfile graph for %s under %s; affected \
                      taint rules will fall back to intrafile for those \
                      targets"
                     (Lang.to_string lang)
                     (Fpath.to_string project_root));
               record_fallback ~lang ~project_root lang_targets;
               None
             | _ :: _ ->
               if unmatched <> [] then begin
                 Log.warn (fun m ->
                     m "interfile preprocess: %d scan target(s) absent \
                        from the interfile graph for %s under %s; they \
                        will fall back to intrafile"
                       (List.length unmatched)
                       (Lang.to_string lang)
                       (Fpath.to_string project_root));
                 record_fallback ~lang ~project_root unmatched
               end;
               Some { lc_lang = lang;
                      lc_rules = rules;
                      lc_interfile_graph = interfile_graph;
                      lc_matching_targets = matching_targets })
          in
          (lc_opt, file_failures))
        lang_rules
      @ acc)
      targets_by_root []
  in
  let lang_contexts : lang_context list =
    List.filter_map (fun (lc_opt, _) -> lc_opt) per_lang
  in
  let index_build_failures : (Fpath.t * string) list =
    List.concat_map (fun (_, failures) -> failures) per_lang
  in
  let target_batches : (Lang.t * Fpath.t list) list =
    List.concat_map (fun (lc : lang_context) ->
      let files =
        List_.map (fun (target : interfile_target) -> target.abs_path)
          lc.lc_matching_targets
      in
      chunks parse_batch_size files
      |> List_.map (fun (batch : Fpath.t list) -> (lc.lc_lang, batch)))
      lang_contexts
  in
  let (target_results :
         (Lang.t * (Fpath.t, G.program) Hashtbl.t) list),
      (failed_target_batches : (Lang.t * Fpath.t list * string) list) =
    run_parmap caps ~ncores
      ~on_exn:(fun ((lang, batch) : Lang.t * Fpath.t list)
                   (exn : Exception.t) ->
        let msg = Printexc.to_string (Exception.get_exn exn) in
        Log.warn (fun m ->
            m "interfile parse: %s batch failed: %s"
              (Lang.to_string lang) msg);
        (lang, batch, msg))
      (fun ((lang, batch) : Lang.t * Fpath.t list) ->
        let tbl = parse_file_batch ~resolved:projidx_asts lang batch in
        Log.info (fun m ->
            m "interfile parse: %s: parsed %d/%d files in batch"
              (Lang.to_string lang)
              (Hashtbl.length tbl)
              (List.length batch));
        (lang, tbl))
      target_batches
  in
  let target_ast_lookup = build_ast_lookup target_results in
  (* Spec extraction matches on FRESH Naming-only parses: matching is
     positional (ranges and fids are identical for the same bytes), and
     the projidx-published [id_type]/svalue payloads inside [id_info]
     make every generic AST traversal ~2 orders of magnitude slower —
     on grafana, 188s vs 1s of formula matching for one rule.  The
     stamped ASTs stay in [target_ast_lookup] for dispatch, whose sid
     resolution needs them. *)
  let (extraction_results :
         (Lang.t * (Fpath.t, G.program) Hashtbl.t) list),
      (failed_extraction_batches : (Lang.t * Fpath.t list * string) list) =
    run_parmap caps ~ncores
      ~on_exn:(fun ((lang, batch) : Lang.t * Fpath.t list)
                   (exn : Exception.t) ->
        let msg = Printexc.to_string (Exception.get_exn exn) in
        Log.warn (fun m ->
            m "interfile extraction parse: %s batch failed: %s"
              (Lang.to_string lang) msg);
        (lang, batch, msg))
      (fun ((lang, batch) : Lang.t * Fpath.t list) ->
        (lang, parse_file_batch lang batch))
      target_batches
  in
  let extraction_ast_lookup = build_ast_lookup extraction_results in
  (* A failed parse batch leaves its files with no dispatch AST and/or no
     extraction AST: they can neither be dispatched nor seed the subgraph,
     so without intervention their findings would silently vanish (the
     per-target gate blocks interfile rules on non-fallback paths).  Run
     them per-target intrafile instead, and surface one scan error per
     file.  The fallback key's root component is ignored by its consumer
     (it matches on lang alone), so [cwd] serves as the key root. *)
  let failed_parse_files : (Lang.t * Fpath.t * string) list =
    let seen = Hashtbl.create 16 in
    (failed_target_batches @ failed_extraction_batches)
    |> List.concat_map
         (fun ((lang, batch, msg) : Lang.t * Fpath.t list * string) ->
            List_.filter_map
              (fun (file : Fpath.t) ->
                 if Hashtbl.mem seen file then None
                 else begin
                   Hashtbl.replace seen file true;
                   Some (lang, file, msg)
                 end)
              batch)
  in
  List.iter
    (fun ((lang, file, _msg) : Lang.t * Fpath.t * string) ->
       record_fallback_paths ~lang ~project_root:cwd [ file ])
    failed_parse_files;
  let parse_failures : (Fpath.t * string) list =
    List_.map
      (fun ((lang, file, msg) : Lang.t * Fpath.t * string) ->
         (file,
          Printf.sprintf "%s: interfile parse failed: %s"
            (Lang.to_string lang) msg))
      failed_parse_files
  in
  (* (rule, chunk) pairs in one parmap so an expensive-to-match rule
     spreads across domains. *)
  let spec_pairs : (lang_context * R.taint_rule) array =
    Array.of_list
      (List.concat_map (fun (lc : lang_context) ->
         List.map (fun (rule : R.taint_rule) -> (lc, rule)) lc.lc_rules)
         lang_contexts)
  in
  let spec_chunk_items : (int * interfile_target list) list =
    spec_pairs |> Array.to_list
    |> List.mapi (fun i ((lc : lang_context), _rule) ->
         chunks spec_extract_batch_size lc.lc_matching_targets
         |> List_.map (fun chunk -> (i, chunk)))
    |> List.concat
  in
  (* Compiled once per rule; contents read once per file — both shared
     read-only across every (rule, chunk) item. *)
  let rule_prefilters =
    Array.map (fun ((_lc : lang_context), (rule : R.taint_rule)) ->
        Analyze_rule.regexp_prefilter_of_interfile_taint_rule
          (rule :> R.rule))
      spec_pairs
  in
  let target_contents : (Fpath.t, string) Hashtbl.t =
    Hashtbl.create
      (List.fold_left
         (fun acc (lc : lang_context) ->
            acc + List.length lc.lc_matching_targets)
         0 lang_contexts)
  in
  if Array.exists Option.is_some rule_prefilters then
    List.iter (fun (lc : lang_context) ->
        List.iter (fun (target : interfile_target) ->
            if not (Hashtbl.mem target_contents target.abs_path) then
              match UFile.read_file target.abs_path with
              | content -> Hashtbl.replace target_contents target.abs_path content
              | exception ((Out_of_memory | Time_limit.Timeout _) as exn) ->
                  Exception.catch_and_reraise exn
              | exception _ -> ())
          lc.lc_matching_targets)
      lang_contexts;
  let (spec_partials : (int * rule_specs) list),
      (failed_spec_chunks : (int * string) list) =
    run_parmap caps ~ncores
      ~on_exn:(fun ((i, _chunk) : int * interfile_target list)
                 (exn : Exception.t) ->
        let _lc, rule = spec_pairs.(i) in
        let msg = Exception.to_string exn in
        Log.warn (fun m ->
            m "interfile spec_extract: rule %s failed: %s"
              (Rule_ID.to_string (fst rule.R.id)) msg);
        (i, msg))
      (fun ((i, chunk) : int * interfile_target list) ->
        let lc, rule = spec_pairs.(i) in
        let specs =
          extract_specs_for_rule ~lang:lc.lc_lang ~xconf
            ~prefilter:rule_prefilters.(i)
            ~contents:target_contents
            ~ast_table:(ast_table_for_lang extraction_ast_lookup lc.lc_lang)
            ~matching_targets:chunk rule
        in
        (i, specs))
      spec_chunk_items
  in
  (* A failed chunk leaves rule [i]'s seeds incomplete for this context;
     dispatching a partial seeding would find an unpredictable subset and
     could double-report against the per-target fallback.  Treat the rule
     as failed for the whole context — like a failed [init_rule_state]:
     no dispatch, every matching target runs per-target intrafile. *)
  let failed_spec_rules : (int, unit) Hashtbl.t = Hashtbl.create 4 in
  List.iter
    (fun ((i, _msg) : int * string) ->
       Hashtbl.replace failed_spec_rules i ())
    failed_spec_chunks;
  let all_specs : (lang_context * rule_specs) list =
    let module IntMap = Map.Make (Int) in
    let by_rule =
      List.fold_left
        (fun acc ((i, specs) : int * rule_specs) ->
          let sources, sinks =
            Option.value (IntMap.find_opt i acc) ~default:([], [])
          in
          IntMap.add i
            (List.rev_append specs.rs_sources sources,
             List.rev_append specs.rs_sinks sinks)
            acc)
        IntMap.empty spec_partials
    in
    spec_pairs |> Array.to_list
    |> List.mapi (fun i ((lc : lang_context), (rule : R.taint_rule)) ->
         if Hashtbl.mem failed_spec_rules i then None
         else
           let rs_sources, rs_sinks =
             Option.value (IntMap.find_opt i by_rule) ~default:([], [])
           in
           Some (lc, { rs_rule = rule; rs_sources; rs_sinks }))
    |> List.filter_map Fun.id
  in
  let rule_subgraphs : rule_subgraph list =
    List_.filter_map
      (fun ((lc, specs) : lang_context * rule_specs) ->
         compute_rule_subgraph ~xconf ~lc ~specs)
      all_specs
  in
  let companion_results, companion_failures =
    parse_companion_files caps ~ncores ~resolved:projidx_asts
      ~target_ast_lookup ~lang_contexts rule_subgraphs
  in
  let full_ast_lookup =
    build_ast_lookup (List.rev_append companion_results target_results)
  in
  (* Publish inferred classes onto [id_type] for FRESH-parsed files only:
     projidx already stamped the ASTs it returned (with project-wide type
     facts), and those are reused verbatim here — re-stamping them is a
     redundant whole-AST walk. Only files absent from [projidx_asts] were
     fresh-parsed and still need it. Once per file (the mapping depends
     only on [(ast, lang)]). *)
  Hashtbl.iter (fun (lang : Lang.t)
                 (tbl : (Fpath.t, G.program) Hashtbl.t) ->
      Hashtbl.iter (fun (file : Fpath.t) (ast : G.program) ->
          let key = Fpath.to_string (Fpath.normalize file) in
          if not (Hashtbl.mem projidx_asts key) then
            try
              Object_initialization.(
                stamp_id_types (detect_object_initialization ast lang) ast)
            with
            | (Out_of_memory | Time_limit.Timeout _) as exn ->
                Exception.catch_and_reraise exn
            | exn ->
                Log.warn (fun m ->
                    m "interfile dispatch: id_type stamping failed for %s: %s"
                      (Fpath.to_string file) (Printexc.to_string exn)))
        tbl)
    full_ast_lookup;
  (* Failed-init rules fall back to per-target intrafile (below). *)
  let (rule_states : rule_state list),
      (failed_rsgs : rule_subgraph list) =
    run_parmap caps ~ncores
      ~on_exn:(fun (rsg : rule_subgraph) (exn : Exception.t) ->
        Log.warn (fun m ->
            m "interfile init_rule: rule %s failed: %s"
              (Rule_ID.to_string (fst rsg.rsg_specs.rs_rule.R.id))
              (Exception.to_string exn));
        rsg)
      (fun (rsg : rule_subgraph) ->
        init_rule_state
          ~ast_table:(ast_table_for_lang full_ast_lookup
                        rsg.rsg_lang_context.lc_lang)
          ~target_root_map rsg)
      rule_subgraphs
  in
  let langs =
    List.map (fun (lc : lang_context) ->
      Xlang.L (lc.lc_lang, []))
      lang_contexts
  in
  (* Per rule, target abs paths interfile dispatch won't cover;
     [Core_scan] runs those per-target. *)
  let fallback_rule_target_paths
      : (Rule_ID.t * Fpath.t list) list =
    if Hashtbl.length fallback_target_paths_by_lang_root = 0 then []
    else
      List_.filter_map (fun (rule : R.rule) ->
        match rule.R.mode with
        | `Taint _ ->
          if not (rule_is_interfile ~taint_interfile rule) then None
          else
            let lang_keys =
              Xlang.to_langs rule.R.target_analyzer
              |> List.map Lang.to_lowercase_alnum
            in
            let paths =
              Hashtbl.fold (fun (lang_key, _root_str) ps acc ->
                if List.mem lang_key lang_keys then List.rev_append ps acc
                else acc)
                fallback_target_paths_by_lang_root []
            in
            (match paths with
             | [] -> None
             | _ -> Some (fst rule.R.id, paths))
        | _ -> None)
        valid_rules
  in
  let failed_fallback =
    List.map (fun (rsg : rule_subgraph) ->
      (fst rsg.rsg_specs.rs_rule.R.id,
       List_.map (fun (target : interfile_target) -> target.abs_path)
         rsg.rsg_lang_context.lc_matching_targets))
      failed_rsgs
  in
  (* Rules whose spec extraction failed: no rule_state was built for the
     context (excluded from [all_specs]), so every matching target must
     run per-target.  [Core_scan] unions entries per rule. *)
  let failed_spec_fallback =
    Hashtbl.fold (fun (i : int) () acc ->
        let lc, rule = spec_pairs.(i) in
        (fst rule.R.id,
         List_.map (fun (target : interfile_target) -> target.abs_path)
           lc.lc_matching_targets)
        :: acc)
      failed_spec_rules []
  in
  (rule_states, langs,
   fallback_rule_target_paths @ failed_fallback @ failed_spec_fallback,
   index_build_failures @ parse_failures @ companion_failures)
