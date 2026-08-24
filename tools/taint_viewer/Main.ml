(* Taint trace viewer - standalone tool
 *
 * Usage:
 *   Direct serve (builds the interfile call graph for the project):
 *     taint_viewer results.json --project-root . --lang python --port 8080
 *
 *   Two-step: process and save, then load and serve
 *     taint_viewer results.json -r . -L python --output findings.json
 *     taint_viewer --load findings.json --port 8080
 *
 * Paths in results.json are resolved against the current directory, so run
 * from the same directory opengrep was run in.
 *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let build_call_graph (project_root : string option) (lang : string option) =
  match project_root, lang with
  | None, _ | _, None -> None
  | Some root, Some lang_str ->
      let lang =
        match Lang.of_string_opt lang_str with
        | Some l -> l
        | None -> failwith ("Unknown language: " ^ lang_str)
      in
      let root = Fpath.v root in
      (* CLI entry point: this process owns its capabilities. *)
      let caps = (Cap.fork_and_limits_caps_UNSAFE () :> < Cap.fork >) in
      Printf.printf "Building interfile call graph for %s in %s (can take a minute on large projects)...\n%!"
        lang_str (Fpath.to_string root);
      match Viewer_server.build_call_graph caps ~project_root:root lang with
      | Some graph -> Some graph
      | None ->
          failwith
            (Printf.sprintf "failed to build interfile graph for %s in %s"
               lang_str (Fpath.to_string root))

let run json_file project_root lang port output_file load_file =
  match load_file with
  | Some load_path ->
      let findings_json = read_file load_path in
      Printf.printf "Loaded pre-computed findings from %s\n" load_path;
      Printf.printf "Starting taint viewer on http://localhost:%d\n" port;
      Printf.printf "Press Ctrl+C to stop\n%!";
      Lwt_main.run (Taint_viewer.start_server_with_findings ~findings_json ~port)
  | None ->
      let json_file = match json_file with
        | Some f -> f
        | None -> failwith "JSON_FILE is required when not using --load"
      in
      let json_str = read_file json_file in
      let call_graph = build_call_graph project_root lang in
      Printf.printf "Loaded call graph: %s\n%!"
        (match call_graph with Some _ -> "yes" | None -> "no (pass --project-root and --lang)");
      match output_file with
      | Some out_path ->
          Printf.printf "Processing findings...\n%!";
          let findings_json = Taint_viewer.get_findings_json ~json_str ~call_graph in
          let oc = open_out out_path in
          output_string oc findings_json;
          close_out oc;
          Printf.printf "Wrote findings JSON to %s\n%!" out_path
      | None ->
          Printf.printf "Starting taint viewer on http://localhost:%d\n" port;
          Printf.printf "Press Ctrl+C to stop\n%!";
          Lwt_main.run (Taint_viewer.start_server ~json_str ~call_graph ~port)

let json_file_arg =
  let doc = "Path to JSON results file from opengrep --json" in
  Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv:"JSON_FILE" ~doc)

let project_root_arg =
  let doc = "Project root used to build the interfile call graph" in
  Cmdliner.Arg.(value & opt (some string) None & info ["project-root"; "r"] ~docv:"DIR" ~doc)

let lang_arg =
  let doc = "Language of the project (e.g. python, go, ruby)" in
  Cmdliner.Arg.(value & opt (some string) None & info ["lang"; "L"] ~docv:"LANG" ~doc)

let port_arg =
  let doc = "Port to serve on" in
  Cmdliner.Arg.(value & opt int 8080 & info ["port"; "p"] ~docv:"PORT" ~doc)

let output_arg =
  let doc = "Output findings JSON to file instead of starting server" in
  Cmdliner.Arg.(value & opt (some string) None & info ["output"; "o"] ~docv:"FILE" ~doc)

let load_arg =
  let doc = "Load pre-computed findings JSON and serve (skips call graph construction)" in
  Cmdliner.Arg.(value & opt (some string) None & info ["load"; "l"] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Visualize taint traces from opengrep JSON output" in
  let info = Cmdliner.Cmd.info "taint_viewer" ~doc in
  Cmdliner.Cmd.v info
    Cmdliner.Term.(const run $ json_file_arg $ project_root_arg $ lang_arg
                   $ port_arg $ output_arg $ load_arg)

let () =
  Parsing_init.init ();
  exit (Cmdliner.Cmd.eval cmd)
