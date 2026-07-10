(* Diagnostic tool for inspecting interfile taint analysis graph structure. *)

module G = Call_graph.G

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
    G.iter_edges_e (fun edge ->
      let src = G.E.src edge in
      let dst = G.E.dst edge in
      let label = G.E.label edge in
      let s_f, s_l, s_c = Function_id.to_file_line_col src in
      let d_f, d_l, d_c = Function_id.to_file_line_col dst in
      let call_site = label.Call_graph.call_site in
      Printf.eprintf "%s:%d:%d\t%s:%d:%d\t%s:%d:%d\n"
        s_f s_l s_c d_f d_l d_c
        (Fpath.to_string call_site.Pos.file) call_site.Pos.line call_site.Pos.column
    ) graph
  end;
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
    [ full_graph_cmd; lookup_cmd; edges_cmd;
      relevant_graph_cmd; topo_order_cmd ]

let () =
  Parsing_init.init ();
  match Cmd.eval_value ~catch:false ~argv:Sys.argv main_cmd with
  | Ok (`Ok code) -> exit code
  | Ok (`Version | `Help) -> exit 0
  | Error _ -> exit 1
