(* Diagnostic tool for inspecting interfile taint analysis graph structure. *)

module G = Call_graph.G
module PI = Opengrep_project_index

let load_graph (project_root : string) (lang : Lang.t) : Call_graph.G.t =
  let root = Fpath.v project_root in
  let targeting_conf =
    Opengrep_project_index.Discover.projidx_default_targeting_conf
  in
  (* CLI entry point: this process owns its capabilities. *)
  let caps = (Cap.fork_and_limits_caps_UNSAFE () :> < Cap.fork >) in
  match
    Interfile_graph.load_interfile_graph caps ~targeting_conf lang root
  with
  | Some graph -> graph
  | None ->
    Printf.eprintf "Error: failed to build interfile graph for %s in %s\n"
      (Lang.to_string lang) project_root;
    exit 1

let load_rules (rules_file : string) : Rule.t list =
  let config : Core_scan_config.t =
    { Core_scan_config.default with
      rule_source = Core_scan_config.Rule_file (Fpath.v rules_file) }
  in
  let (rules, invalid) =
    Core_scan.rules_of_config ~filter_by_targets:false config
  in
  if invalid <> [] then
    Printf.eprintf "Warning: %d invalid rules skipped\n" (List.length invalid);
  rules

(* One TSV edge format for every dump:
   src_loc \t dst_loc \t call_site_loc \t kind. *)
let dump_edges_tsv (out : out_channel) (graph : Call_graph.G.t) : unit =
  G.iter_edges_e (fun (edge : G.E.t) ->
    let src = G.E.src edge in
    let dst = G.E.dst edge in
    let label = G.E.label edge in
    let s_f, s_l, s_c = Function_id.to_file_line_col src in
    let d_f, d_l, d_c = Function_id.to_file_line_col dst in
    let call_site = label.Call_graph.call_site in
    let kind = match label.Call_graph.kind with
      | Call_graph.Call -> "call"
      | Call_graph.Dispatch -> "dispatch"
    in
    Printf.fprintf out "%s:%d:%d\t%s:%d:%d\t%s:%d:%d\t%s\n"
      s_f s_l s_c d_f d_l d_c
      (Fpath.to_string call_site.Pos.file) call_site.Pos.line call_site.Pos.column
      kind
  ) graph

let cmd_full_graph (project_root : string) (lang_str : string)
    (verbose : bool) : int =
  let lang = Lang.of_string lang_str in
  let graph = load_graph project_root lang in
  Printf.printf "Interfile call graph for %s\n" (Lang.to_string lang);
  Printf.printf "  Vertices: %d\n" (G.nb_vertex graph);
  Printf.printf "  Edges: %d\n" (G.nb_edges graph);
  let file_counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
  G.iter_vertex (fun (vertex : Function_id.t) ->
    let file = match Function_id.file_of vertex with
      | Some fp -> Fpath.to_string fp
      | None -> "<unknown>"
    in
    let n = match Hashtbl.find_opt file_counts file with
      | Some n -> n | None -> 0
    in
    Hashtbl.replace file_counts file (n + 1)
  ) graph;
  let sorted_files =
    Hashtbl.fold (fun file n acc -> (file, n) :: acc) file_counts []
    |> List.sort (fun (_, a) (_, b) -> Int.compare b a)
  in
  Printf.printf "  Files: %d\n" (List.length sorted_files);
  if verbose then begin
    List.iter (fun (file, count) ->
      Printf.printf "    %4d  %s\n" count file
    ) sorted_files;
    dump_edges_tsv stderr graph
  end;
  0

(* The [index] command: the raw projidx view.  [PI.Project_index.collect] output as
   built (paths may be relative, permissive targeting), BEFORE the engine
   absolutifies it — [full-graph] shows the same graph as the engine
   consumes it. *)

module Log = PI.Log_projidx.Log

let count_by_kind (entries : PI.Types.entry list) : int * int * int =
  List.fold_left (fun (funcs, methods, classes) (entry : PI.Types.entry) ->
    match entry.PI.Types.kind with
    | PI.Types.K_function -> (funcs + 1, methods, classes)
    | PI.Types.K_method -> (funcs, methods + 1, classes)
    | PI.Types.K_class -> (funcs, methods, classes + 1)
  ) (0, 0, 0) entries

let kind_str (kind : PI.Types.def_kind) : string =
  match kind with
  | PI.Types.K_function -> "function"
  | PI.Types.K_method -> "method"
  | PI.Types.K_class -> "class"

let cmd_index (project_root_str : string) (lang_str : string)
    (sample : int) (dump_all : bool) (ncores : int)
    (includes : string list) (excludes : string list)
    (pyrefly_toml : string option) (list_files_only : bool)
    (dump_edges : bool) : int =
  let lang = Lang.of_string lang_str in
  let project_root = Fpath.v project_root_str |> Fpath.normalize in
  let toml_inc, toml_exc = match pyrefly_toml with
    | None -> ([], [])
    | Some path -> PI.Discover.read_pyrefly_includes_excludes path
  in
  let includes = includes @ toml_inc in
  let excludes = excludes @ toml_exc in
  if includes <> [] || excludes <> [] then
    Log.info (fun m -> m "Filter: %d include(s), %d exclude(s)"
      (List.length includes) (List.length excludes));
  Log.info (fun m -> m "Walking %s for %s files..."
    (Fpath.to_string project_root) (Lang.to_string lang));
  if list_files_only then begin
    let files = PI.Discover.discover_files
      ~targeting_conf:PI.Discover.projidx_default_targeting_conf
      ~lang ~project_root ~includes ~excludes in
    List.iter (fun (file : Fpath.t) -> print_endline (Fpath.to_string file))
      files;
    0
  end
  else begin
    let t0 = Unix.gettimeofday () in
    (* CLI entry point: this process owns its capabilities. *)
    let caps = Cap.fork_and_limits_caps_UNSAFE () in
    let (entries, graph, scanned, skipped) =
      PI.Project_index.collect (caps :> < Cap.fork >)
        ~lang ~project_root ~ncores ~includes ~excludes ()
    in
    let t1 = Unix.gettimeofday () in
    let (funcs, methods, classes) = count_by_kind entries in
    let summary_chan = if dump_all || dump_edges then stderr else stdout in
    Printf.fprintf summary_chan "Files scanned:  %d\n" scanned;
    Printf.fprintf summary_chan "Files skipped:  %d\n" skipped;
    Printf.fprintf summary_chan "Total entries:  %d\n" (List.length entries);
    Printf.fprintf summary_chan "  functions:    %d\n" funcs;
    Printf.fprintf summary_chan "  methods:      %d\n" methods;
    Printf.fprintf summary_chan "  classes:      %d\n" classes;
    Printf.fprintf summary_chan "Elapsed:        %.2fs\n" (t1 -. t0);
    let entry_loc (entry : PI.Types.entry) =
      let file, line, col = Function_id.to_file_line_col entry.PI.Types.id in
      Printf.sprintf "%s:%d:%d" file line col
    in
    if dump_edges then
      dump_edges_tsv stdout graph
    else if dump_all then
      List.iter (fun (entry : PI.Types.entry) ->
        Printf.printf "%s\t%s\t%s\n"
          (kind_str entry.PI.Types.kind) entry.PI.Types.name (entry_loc entry)
      ) entries
    else if sample > 0 then begin
      Printf.printf "\nSample (%d):\n" sample;
      List.iter (fun (entry : PI.Types.entry) ->
        Printf.printf "  [%s] %s  (%s)\n"
          (kind_str entry.PI.Types.kind) entry.PI.Types.name (entry_loc entry)
      ) (List_.take_safe sample entries)
    end;
    0
  end

(* Dump a file's generic AST after call-graph callee resolution, so method /
   qualified / cross-file callees show up in each callee's [id_resolved].
   With a project root the projidx pipeline resolves across files; without,
   resolution is single-file. *)
let cmd_dump_interfile_ast (project_root_opt : string option) (lang_str : string)
    (file_str : string) : int =
  let lang = Lang.of_string lang_str in
  let file = Fpath.v file_str in
  let ast_opt =
    match project_root_opt with
    | Some root ->
      let caps = (Cap.fork_and_limits_caps_UNSAFE () :> < Cap.fork >) in
      (match
         PI.Project_index.resolve_ast_for_file caps ~lang
           ~project_root:(Fpath.v root) ~ncores:1 ~target:file ()
       with
       | Some ast -> Some ast
       | None ->
         Printf.eprintf
           "dump-interfile-ast: %s was not discovered/parsed under project root %s\n"
           file_str root;
         None)
    | None ->
      let res = Parse_target.parse_and_resolve_name lang file in
      ignore (Graph_from_AST.build_call_graph ~lang res.ast);
      if Parsing_result2.has_error res then begin
        Printf.eprintf "dump-interfile-ast: failed to fully parse %s\n" file_str;
        None
      end
      else Some res.ast
  in
  match ast_opt with
  | None -> 1
  | Some ast ->
    let v = Meta_AST.vof_any (AST_generic.Pr ast) in
    Format.set_margin 120;
    print_endline (OCaml.string_of_v v);
    0

let cmd_lookup (project_root : string) (lang_str : string)
    (pattern : string) (verbose : bool) : int =
  let lang = Lang.of_string lang_str in
  let graph = load_graph project_root lang in
  let matches = ref [] in
  G.iter_vertex (fun (vertex : Function_id.t) ->
    let name = Function_id.show vertex in
    if Pcre2_.pmatch_noerr ~rex:(Pcre2_.regexp pattern) name then
      matches := vertex :: !matches
  ) graph;
  let sorted =
    !matches
    |> List.sort (fun (a : Function_id.t) (b : Function_id.t) ->
      String.compare (Function_id.show_debug a) (Function_id.show_debug b))
  in
  Printf.printf "Found %d functions matching \"%s\":\n"
    (List.length sorted) pattern;
  List.iter (fun (vertex : Function_id.t) ->
    Printf.printf "  %s\n" (Function_id.show_debug vertex);
    if verbose then begin
      let callers = G.succ graph vertex in
      let callees = G.pred graph vertex in
      Printf.printf "    callers (%d): %s\n"
        (List.length callers)
        (String.concat ", " (List.map Function_id.show callers));
      Printf.printf "    callees (%d): %s\n"
        (List.length callees)
        (String.concat ", " (List.map Function_id.show callees))
    end
  ) sorted;
  0

let edge_kind_matches (filter : string option) (kind : Call_graph.edge_kind)
    : bool =
  match filter with
  | None -> true
  | Some "call" -> (match kind with Call_graph.Call -> true | _ -> false)
  | Some "dispatch" -> (match kind with Call_graph.Dispatch -> true | _ -> false)
  | Some other ->
      Printf.eprintf "Unknown edge kind filter: %s (use 'call' or 'dispatch')\n"
        other;
      false

let show_edge_kind (kind : Call_graph.edge_kind) : string =
  match kind with
  | Call_graph.Call -> "call"
  | Call_graph.Dispatch -> "dispatch"

let cmd_edges (project_root : string) (lang_str : string)
    (pattern : string) (kind_filter : string option) : int =
  let lang = Lang.of_string lang_str in
  let graph = load_graph project_root lang in
  let rex = Pcre2_.regexp pattern in
  G.iter_vertex (fun (vertex : Function_id.t) ->
    let name = Function_id.show vertex in
    if Pcre2_.pmatch_noerr ~rex name then begin
      (* Edge direction: callee -> caller.  G.succ_e = caller edges, G.pred_e = callee edges *)
      let caller_edges =
        G.succ_e graph vertex
        |> List.filter (fun (edge : G.E.t) ->
            edge_kind_matches kind_filter (G.E.label edge).Call_graph.kind)
      in
      let callee_edges =
        G.pred_e graph vertex
        |> List.filter (fun (edge : G.E.t) ->
            edge_kind_matches kind_filter (G.E.label edge).Call_graph.kind)
      in
      if Option.is_none kind_filter
         || List.length caller_edges > 0
         || List.length callee_edges > 0
      then begin
        Printf.printf "%s\n" (Function_id.show_debug vertex);
        Printf.printf "  Callers (%d):\n" (List.length caller_edges);
        List.iter (fun (edge : G.E.t) ->
          let label = G.E.label edge in
          Printf.printf "    <- [%s] %s\n"
            (show_edge_kind label.Call_graph.kind)
            (Function_id.show_debug (G.E.dst edge))
        ) caller_edges;
        Printf.printf "  Callees (%d):\n" (List.length callee_edges);
        List.iter (fun (edge : G.E.t) ->
          let label = G.E.label edge in
          Printf.printf "    -> [%s] %s\n"
            (show_edge_kind label.Call_graph.kind)
            (Function_id.show_debug (G.E.src edge))
        ) callee_edges
      end
    end
  ) graph;
  0

let build_rule_states_from_args ~(rules_file : Fpath.t)
    ~(target_roots : Fpath.t list) ~(ncores : int)
    : Interfile_dispatch.rule_state list =
  let rules = load_rules (Fpath.to_string rules_file) in
  let roots =
    List.map Scanning_root.of_fpath target_roots
  in
  let { Find_targets.selected = fpaths; _ } =
    Find_targets.get_target_fpaths Find_targets.default_conf roots
  in
  let targets = Core_runner.targets_for_files_and_rules fpaths rules in
  let config : Core_scan_config.t =
    { Core_scan_config.default with
      rule_source = Core_scan_config.Rules rules;
      target_source = Core_scan_config.Targets targets;
      taint_interfile = true;
      taint_intrafile = true;
      taint_interfile_depth = 3;
    }
  in
  let equivs = Core_scan.parse_equivalences config.equivalences_file in
  let xconf = Core_scan.interfile_xconfig config ~equivs in
  let caps = Cap.fork_and_limits_caps_UNSAFE () in
  let targeting_conf =
    Opengrep_project_index.Discover.projidx_default_targeting_conf
  in
  let rule_states, _langs, _fallbacks =
    Interfile_dispatch.build_rule_states
      (caps :> < Cap.fork >)
      ~ncores
      ~taint_interfile:true
      ~valid_rules:rules ~targets
      ~targeting_conf
      ~xconf
  in
  rule_states

let cmd_relevant_graph (rules_file : string)
    (target_roots : string list) (ncores : int) (verbose : bool) : int =
  let rule_states =
    build_rule_states_from_args
      ~rules_file:(Fpath.v rules_file)
      ~target_roots:(List.map Fpath.v target_roots)
      ~ncores
  in
  List.iter (fun (rs : Interfile_dispatch.rule_state) ->
    let rule_id = Interfile_dispatch.rule_id_of rs in
    let graph = Interfile_dispatch.relevant_graph_of rs in
    let topo_order = Interfile_dispatch.topo_order_of rs in
    let files = Interfile_graph.files_of_graph graph in
    Printf.printf "\nRule %s:\n" (Rule_ID.to_string rule_id);
    Printf.printf "  Relevant subgraph: %d vertices, %d edges\n"
      (G.nb_vertex graph) (G.nb_edges graph);
    Printf.printf "  Topo order: %d functions\n"
      (List.length topo_order);
    Printf.printf "  Files in subgraph: %d\n" (List.length files);
    if verbose then begin
      Printf.printf "  Topo order:\n";
      List.iter (fun (fid : Function_id.t) ->
        Printf.printf "    %s\n" (Function_id.show_debug fid)
      ) topo_order;
      Printf.printf "  Files:\n";
      List.iter (fun (fp : Fpath.t) ->
        Printf.printf "    %s\n" (Fpath.to_string fp)
      ) files
    end
  ) rule_states;
  0

(* TODO: replace hand-rolled JSON with yojson (add to dune deps). *)
(* TODO: extract signatures in parallel, one rule per task. *)

let show_extended_sigs (sigs : Shape_and_sig.extended_sig list) : string =
  match sigs with
  | [] -> "(none)"
  | _ ->
      List.map
        (fun (es : Shape_and_sig.extended_sig) ->
          Printf.sprintf "[arity=%d] %s" (Shape_and_sig.int_of_sig_arity es.arity)
            (Shape_and_sig.Signature.show es.sig_))
        sigs
      |> String.concat " | "

let cmd_topo_order (rules_file : string)
    (target_roots : string list) (ncores : int)
    (with_sigs : bool) (json_output : bool) : int =
  let rule_states =
    build_rule_states_from_args
      ~rules_file:(Fpath.v rules_file)
      ~target_roots:(List.map Fpath.v target_roots)
      ~ncores
  in
  if json_output then begin
    let buf = Buffer.create 4096 in
    Buffer.add_string buf "[\n";
    List.iteri (fun (ri : int) (rs : Interfile_dispatch.rule_state) ->
      let rule_id = Interfile_dispatch.rule_id_of rs in
      let topo_order = Interfile_dispatch.topo_order_of rs in
      let sig_db =
        if with_sigs then Some (Interfile_dispatch.extract_signatures rs)
        else None
      in
      if ri > 0 then Buffer.add_string buf ",\n";
      Buffer.add_string buf (Printf.sprintf
        "  { \"rule\": %S, \"topo_order\": [\n"
        (Rule_ID.to_string rule_id));
      List.iteri (fun (i : int) (fid : Function_id.t) ->
        let sig_str =
          match sig_db with
          | None -> "null"
          | Some db ->
              let sigs =
                Shape_and_sig.lookup_all_signatures db fid
              in
              Printf.sprintf "%S" (show_extended_sigs sigs)
        in
        if i > 0 then Buffer.add_string buf ",\n";
        Buffer.add_string buf (Printf.sprintf
          "    { \"index\": %d, \"fid\": %S, \"signature\": %s }"
          i (Function_id.show_debug fid) sig_str)
      ) topo_order;
      Buffer.add_string buf "\n  ] }"
    ) rule_states;
    Buffer.add_string buf "\n]\n";
    print_string (Buffer.contents buf)
  end else begin
    List.iter (fun (rs : Interfile_dispatch.rule_state) ->
      let rule_id = Interfile_dispatch.rule_id_of rs in
      let topo_order = Interfile_dispatch.topo_order_of rs in
      let sig_db =
        if with_sigs then Some (Interfile_dispatch.extract_signatures rs)
        else None
      in
      Printf.printf "\nRule %s:\n" (Rule_ID.to_string rule_id);
      Printf.printf "  Topo order (%d functions):\n"
        (List.length topo_order);
      List.iteri (fun (i : int) (fid : Function_id.t) ->
        match sig_db with
        | None ->
            Printf.printf "    %4d  %s\n" i (Function_id.show_debug fid)
        | Some db ->
            let sigs =
              Shape_and_sig.lookup_all_signatures db fid
            in
            Printf.printf "    %4d  %s  sig: %s\n"
              i (Function_id.show_debug fid) (show_extended_sigs sigs)
      ) topo_order
    ) rule_states
  end;
  0

open Cmdliner

let o_project_root : string Term.t =
  let info =
    Arg.info [ "project-root"; "r" ] ~docv:"DIR"
      ~doc:"Project root directory to index"
  in
  Arg.required (Arg.opt (Arg.some Arg.string) None info)

let o_lang : string Term.t =
  let info =
    Arg.info [ "l"; "lang" ] ~docv:"LANG"
      ~doc:"Language (e.g. go, java, python)"
  in
  Arg.required (Arg.opt (Arg.some Arg.string) None info)

let o_rules : string Term.t =
  let info =
    Arg.info [ "f"; "rules" ] ~docv:"FILE"
      ~doc:"Rules file (YAML)"
  in
  Arg.required (Arg.opt (Arg.some Arg.string) None info)

let o_pattern : string Term.t =
  let info =
    Arg.info [ "p"; "pattern" ] ~docv:"REGEX"
      ~doc:"Function name pattern (regex)"
  in
  Arg.required (Arg.opt (Arg.some Arg.string) None info)

let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:"Show detailed output"
  in
  Arg.value (Arg.flag info)

let o_ncores : int Term.t =
  let info =
    Arg.info [ "j"; "ncores" ] ~docv:"N"
      ~doc:"Number of cores for parallel processing (default: all available)"
  in
  Arg.value (Arg.opt Arg.int (Domainslib_.get_cpu_count ()) info)

let o_targets : string list Term.t =
  let info =
    Arg.info [] ~docv:"TARGET"
      ~doc:"Target files or directories"
  in
  Arg.value (Arg.pos_all Arg.string ["."] info)

let full_graph_cmd : int Cmd.t =
  let doc = "Load and display the full interfile call graph" in
  let term =
    Term.(const cmd_full_graph $ o_project_root $ o_lang $ o_verbose)
  in
  Cmd.v (Cmd.info "full-graph" ~doc) term

let lookup_cmd : int Cmd.t =
  let doc = "Find functions by name pattern" in
  let term =
    Term.(const cmd_lookup $ o_project_root $ o_lang $ o_pattern $ o_verbose)
  in
  Cmd.v (Cmd.info "lookup" ~doc) term

let o_kind : string option Term.t =
  let info =
    Arg.info [ "kind"; "k" ] ~docv:"KIND"
      ~doc:"Filter edges by kind: 'call' or 'dispatch'"
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let edges_cmd : int Cmd.t =
  let doc = "Show callers and callees for functions matching a pattern" in
  let term =
    Term.(const cmd_edges $ o_project_root $ o_lang $ o_pattern $ o_kind)
  in
  Cmd.v (Cmd.info "edges" ~doc) term

let o_sample : int Term.t =
  let info =
    Arg.info [ "sample" ] ~docv:"N"
      ~doc:"Print this many sample entries (0 = none)"
  in
  Arg.value (Arg.opt Arg.int 10 info)

let o_dump_all : bool Term.t =
  let info =
    Arg.info [ "dump-all" ]
      ~doc:"Dump every entry as tab-separated kind, qualified name, location"
  in
  Arg.value (Arg.flag info)

let o_dump_edges : bool Term.t =
  let info =
    Arg.info [ "dump-edges" ]
      ~doc:"Dump call graph edges as tab-separated source, destination, \
            call site, kind"
  in
  Arg.value (Arg.flag info)

let o_include : string list Term.t =
  let info =
    Arg.info [ "include" ] ~docv:"PATH"
      ~doc:"Only index files under this path (repeatable)"
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_exclude : string list Term.t =
  let info =
    Arg.info [ "exclude" ] ~docv:"PATH"
      ~doc:"Skip files under this path (repeatable)"
  in
  Arg.value (Arg.opt_all Arg.string [] info)

let o_pyrefly_toml : string option Term.t =
  let info =
    Arg.info [ "pyrefly-toml" ] ~docv:"FILE"
      ~doc:"Read project-includes/project-excludes from this pyrefly.toml"
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let o_list_files : bool Term.t =
  let info =
    Arg.info [ "list-files" ]
      ~doc:"Print the list of files that would be indexed, then exit"
  in
  Arg.value (Arg.flag info)

let index_cmd : int Cmd.t =
  let doc = "Build the raw project index and dump entries/edges/stats" in
  let term =
    Term.(const cmd_index $ o_project_root $ o_lang $ o_sample $ o_dump_all
          $ o_ncores $ o_include $ o_exclude $ o_pyrefly_toml
          $ o_list_files $ o_dump_edges)
  in
  Cmd.v (Cmd.info "index" ~doc) term

let o_project_root_opt : string option Term.t =
  let info =
    Arg.info [ "project-root"; "r" ] ~docv:"DIR"
      ~doc:"Project root for cross-file callee resolution (single-file \
            resolution when absent)"
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let o_target_file : string Term.t =
  let info = Arg.info [] ~docv:"FILE" ~doc:"File whose AST to dump" in
  Arg.required (Arg.pos 0 (Arg.some Arg.string) None info)

let dump_interfile_ast_cmd : int Cmd.t =
  let doc = "Dump a file's AST after call-graph callee resolution" in
  let term =
    Term.(const cmd_dump_interfile_ast $ o_project_root_opt $ o_lang
          $ o_target_file)
  in
  Cmd.v (Cmd.info "dump-interfile-ast" ~doc) term

let relevant_graph_cmd : int Cmd.t =
  let doc = "Compute the relevant subgraph per (rule, target) pair" in
  let term =
    Term.(const cmd_relevant_graph $ o_rules
          $ o_targets $ o_ncores $ o_verbose)
  in
  Cmd.v (Cmd.info "relevant-graph" ~doc) term

let o_signatures : bool Term.t =
  let info =
    Arg.info [ "signatures" ]
      ~doc:"Run taint analysis and include signatures in output"
  in
  Arg.value (Arg.flag info)

let o_json : bool Term.t =
  let info =
    Arg.info [ "json" ]
      ~doc:"Output in JSON format (default: text)"
  in
  Arg.value (Arg.flag info)

let topo_order_cmd : int Cmd.t =
  let doc = "Emit the topo order (optionally with signatures) per rule" in
  let term =
    Term.(const cmd_topo_order $ o_rules
          $ o_targets $ o_ncores $ o_signatures $ o_json)
  in
  Cmd.v (Cmd.info "topo-order" ~doc) term

let main_cmd : int Cmd.t =
  let doc = "Inspect interfile taint analysis graph structure" in
  let man = [
    `S Manpage.s_description;
    `P "Swiss-knife diagnostic tool for debugging interfile taint analysis. \
        Inspect the projidx-built call graph, compute relevant subgraphs, \
        look up functions, and trace edges.";
  ] in
  Cmd.group (Cmd.info "opengrep-interfile-graph" ~version:"0.1.0" ~doc ~man)
    [ index_cmd; full_graph_cmd; dump_interfile_ast_cmd; lookup_cmd;
      edges_cmd; relevant_graph_cmd; topo_order_cmd ]

let () =
  Parsing_init.init ();
  match Cmd.eval_value ~catch:false ~argv:Sys.argv main_cmd with
  | Ok (`Ok code) -> exit code
  | Ok (`Version | `Help) -> exit 0
  | Error _ -> exit 1
