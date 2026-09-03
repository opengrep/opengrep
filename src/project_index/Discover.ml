let read_pyrefly_includes_excludes (path : string)
  : string list * string list =
  let toml = Otoml.Parser.from_file path in
  let array_at_key key =
    match Otoml.find_opt toml (Otoml.get_array Otoml.get_string) [key] with
    | None -> []
    | Some xs -> xs
  in
  (array_at_key "project-includes",
   array_at_key "project-excludes")

let lang_matches (lang : Lang.t) (file : Fpath.t) : bool =
  Nonfatal.catch ~default:false (fun () ->
    List.exists (Lang.equal lang) (Lang.langs_of_filename file))

(* Matching a compiled [Re.re] mutates its lazily-built DFA cache, so a
   shared pattern must not be matched from parallel domains. Safe here:
   matchers stay local to [apply_include_exclude], and discovery runs
   serially on the coordinator before [run_pipeline] spawns workers. *)
type matcher =
  | Prefix of string
  | Glob of Re.re

let has_glob_meta (pat : string) : bool =
  String.contains pat '*' || String.contains pat '?' || String.contains pat '['

let compile_pattern (pat : string) : matcher =
  if has_glob_meta pat then
    Glob (Re.compile (Re.Glob.glob ~anchored:true ~double_asterisk:true pat))
  else
    Prefix pat

(* Patterns are '/'-separated on every platform (glob convention); [Ppath]
   guarantees the same for the matched path. *)
let path_matches_any (matchers : matcher list) (rel_path : Ppath.t) : bool =
  let rel_str = String.concat "/" (Ppath.relative_segments rel_path) in
  List.exists (fun matcher ->
    match matcher with
    | Prefix pat ->
      String.equal pat rel_str
      || (String.length rel_str > String.length pat
          && String.sub rel_str 0 (String.length pat + 1) = pat ^ "/")
    | Glob re -> Re.execp re rel_str
  ) matchers

let relative_to ~(project_root : Fpath.t) (file : Fpath.t) : Fpath.t =
  match Fpath.relativize ~root:project_root file with
  | Some rel -> rel
  | None -> file

let apply_include_exclude ~(project_root : Fpath.t)
    ~(includes : string list) ~(excludes : string list)
    (files : Fpath.t list) : Fpath.t list =
  let inc = List.map compile_pattern includes in
  let exc = List.map compile_pattern excludes in
  (* [None] when the file has no project-relative form (not under
     [project_root]); the project-relative patterns cannot classify it. *)
  let project_ppath (file : Fpath.t) : Ppath.t option =
    let rel = relative_to ~project_root file in
    if Fpath.is_abs rel then None
    else
      match Ppath.of_relative_fpath rel with
      | ppath -> Some ppath
      | exception Invalid_argument _ -> None
  in
  List.filter (fun file ->
    match project_ppath file with
    | None -> (match includes with [] -> true | _ -> false)
    | Some rel ->
      let included = match includes with
        | [] -> true
        | _ -> path_matches_any inc rel
      in
      included && not (path_matches_any exc rel)
  ) files

let scanning_roots_from_includes ~(project_root : Fpath.t)
    (includes : string list) : Scanning_root.t list * string list =
  let simple, glob =
    List.partition (fun pat -> not (has_glob_meta pat)) includes in
  match simple, glob with
  | [], _ -> ([Scanning_root.of_fpath project_root], glob)
  | _, [] ->
    let roots = List.map (fun pat ->
      Scanning_root.of_fpath
        (Fpath.append project_root (Fpath.v pat) |> Fpath.normalize)
    ) simple in
    (roots, [])
  | _ ->
    ([Scanning_root.of_fpath project_root], includes)

let discover_files ~(targeting_conf : Find_targets.conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t)
    ~(includes : string list) ~(excludes : string list) : Fpath.t list =
  let roots, residual_includes =
    scanning_roots_from_includes ~project_root includes
  in
  let { Find_targets.selected; _ } =
    Find_targets.get_target_fpaths targeting_conf roots
  in
  (* [Find_targets] emits paths relative to the process cwd, but every
     consumer resolves them against [project_root] — module-path
     derivation, the parse workers, [make_paths_absolute].  The two
     conventions only coincide when the scan is launched from the project
     root; from a subdirectory the files fail to read and the index comes
     out empty.  Return absolute paths so no consumer has to guess. *)
  let cwd = Fpath.v (Sys.getcwd ()) in
  let selected =
    List_.map (fun (file : Fpath.t) -> fst (Fpath_.absolutify ~cwd file))
      selected
  in
  let lang_files = List.filter (lang_matches lang) selected in
  apply_include_exclude ~project_root
    ~includes:residual_includes ~excludes lang_files

let projidx_default_targeting_conf : Find_targets.conf =
  { Find_targets.default_conf with
    default_semgrepignore_patterns = Semgrepignore.Empty;
    max_target_bytes = -1 }
