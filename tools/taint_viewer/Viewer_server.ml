(* Building the call graph and serving scan results in the taint viewer,
   reusable from other binaries (e.g. `opengrep scan --server`). *)

let build_call_graph (caps : < Cap.fork >) ~(project_root : Fpath.t)
    (lang : Lang.t) : Call_graph.G.t option =
  let targeting_conf =
    Opengrep_project_index.Discover.projidx_default_targeting_conf
  in
  match
    Interfile_graph.load_interfile_graph caps ~targeting_conf lang project_root
  with
  | Some graph ->
      (* Graph paths may be project-relative; trace paths are absolute. *)
      let abs_root =
        if Fpath.is_abs project_root then project_root
        else Fpath.(v (Sys.getcwd ()) // project_root) |> Fpath.normalize
      in
      Some (Call_graph.make_paths_absolute abs_root graph)
  | None -> None

(* Majority language among result paths; used to pick the call graph
   language when the caller doesn't know it. *)
let infer_lang (json_str : string) : Lang.t option =
  match Yojson.Basic.from_string json_str with
  | exception _ -> None
  | `Assoc fields -> (
      match List.assoc_opt "results" fields with
      | Some (`List results) -> (
          let langs =
            List.concat_map
              (fun r ->
                match r with
                | `Assoc rf -> (
                    match List.assoc_opt "path" rf with
                    | Some (`String p) -> (
                        try Lang.langs_of_filename (Fpath.v p) with
                        | _ -> [])
                    | _ -> [])
                | _ -> [])
              results
          in
          let counts =
            List.fold_left
              (fun acc l ->
                let n = Option.value ~default:0 (List.assoc_opt l acc) in
                (l, n + 1) :: List.remove_assoc l acc)
              [] langs
          in
          match
            List.sort (fun (_, a) (_, b) -> Int.compare b a) counts
          with
          | (l, _) :: _ -> Some l
          | [] -> None)
      | _ -> None)
  | _ -> None

(* Serve [json_str] (opengrep JSON results) on http://localhost:port,
   blocking until interrupted. [call_graph] reuses a graph the caller
   already built (e.g. during the scan); otherwise one is built here,
   inferring the language if needed. *)
let serve (caps : < Cap.fork >) ~(project_root : Fpath.t) ?lang ?call_graph
    ~(json_str : string) ~(port : int) () : unit =
  let lang =
    match lang with
    | Some _ -> lang
    | None -> infer_lang json_str
  in
  let call_graph =
    match call_graph with
    | Some graph ->
        Printf.printf "Reusing the interfile call graph built by the scan\n%!";
        let abs_root =
          if Fpath.is_abs project_root then project_root
          else Fpath.(v (Sys.getcwd ()) // project_root) |> Fpath.normalize
        in
        Some (Call_graph.make_paths_absolute abs_root graph)
    | None ->
    match lang with
    | None ->
        Printf.printf
          "Could not infer a language from the results; serving without a \
           call graph (functions will not be resolved).\n%!";
        None
    | Some lang -> (
        Printf.printf
          "Building interfile call graph for %s in %s (can take a minute on \
           large projects)...\n\
           %!"
          (Lang.to_string lang)
          (Fpath.to_string project_root);
        match build_call_graph caps ~project_root lang with
        | Some g -> Some g
        | None ->
            Printf.printf
              "Failed to build the interfile call graph; serving without \
               it.\n\
               %!";
            None)
  in
  Printf.printf "Starting taint viewer on http://localhost:%d\n%!" port;
  Printf.printf "Press Ctrl+C to stop\n%!";
  Lwt_main.run (Taint_viewer.start_server ~json_str ~call_graph ~port)
