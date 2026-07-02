(* [Function_id] hash/compare/equal ignore [Tok] bytepos, so AST- and graph-derived ids for the same def compare equal directly. *)

module Log = Log_tainting.Log
module R = Rule

type interfile_graph = Call_graph.G.t

(* AST map (abs path -> AST) carrying cross-file callee [id_resolved]. *)
type resolved_asts = (string, AST_generic.program) Hashtbl.t

(* [None] on failure.  Called once per (lang, project_root) per scan —
   [Interfile_dispatch] groups all of a language's rules onto one build —
   so there is nothing to cache. *)
let load_interfile_build (caps : < Cap.fork >)
    ?(ncores : int = 0)
    ~(targeting_conf : Find_targets.conf)
    (lang : Lang.t) (project_root : Fpath.t)
    : (interfile_graph * resolved_asts) option =
  let project_root_abs =
    if Fpath.is_abs project_root then Fpath.normalize project_root
    else Fpath.(v (Sys.getcwd ()) // project_root) |> Fpath.normalize
  in
  let ncores =
    if ncores <= 0 then Domainslib_.get_cpu_count () else ncores
  in
  let cfg = Opengrep_project_index.Index_lang_rules.for_lang lang in
  let excludes = cfg.discover_excludes ~project_root:project_root_abs in
  try
    let (graph, asts) =
      Opengrep_project_index.Main.collect_resolved caps
        ~targeting_conf
        ~lang ~project_root:project_root_abs ~ncores
        ~includes:[] ~excludes ()
    in
    (* [Interfile_dispatch] looks up vertices by absolute path. *)
    Some (Call_graph.make_paths_absolute project_root_abs graph, asts)
  with
  | (Out_of_memory | Stack_overflow | Time_limit.Timeout _) as exn ->
    Exception.catch_and_reraise exn
  | exn ->
    Log.warn (fun m ->
        m "Interfile_graph: build failed for %s under %s: %s"
          (Lang.to_string lang)
          (Fpath.to_string project_root_abs)
          (Printexc.to_string exn));
    None

let load_interfile_graph (caps : < Cap.fork >)
    ?(ncores : int = 0)
    ~(targeting_conf : Find_targets.conf)
    (lang : Lang.t) (project_root : Fpath.t)
    : interfile_graph option =
  Option.map fst
    (load_interfile_build caps ~ncores ~targeting_conf lang project_root)

(* Graph uses absolute paths, so ids touching it must be absolute too. *)
let absolutify_fid (project_root : Fpath.t option) (fid : Function_id.t)
    : Function_id.t =
  match project_root with
  | Some root -> Function_id.make_absolute root fid
  | None -> fid

let files_of_graph (g : Call_graph.G.t) : Fpath.t list =
  let tbl : (string, Fpath.t) Hashtbl.t = Hashtbl.create 64 in
  Call_graph.G.iter_vertex (fun (v : Function_id.t) ->
    match Function_id.file_of v with
    | Some fp -> Hashtbl.replace tbl (Fpath.to_string fp) fp
    | None -> ()
  ) g;
  Hashtbl.fold (fun _k fp acc -> fp :: acc) tbl []
  |> List.sort Fpath.compare
