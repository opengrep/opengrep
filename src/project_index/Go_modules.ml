type module_ = {
  root : Fpath.t;
  path : string;
}

type t = {
  (* Sorted by root depth descending, so the first matching module is the nearest. *)
  modules : module_ list;
  replaces : (string * string) list;
}

let empty = { modules = []; replaces = [] }

let find_sub (hay : string) (needle : string) : int option =
  let nl = String.length needle and hl = String.length hay in
  if nl = 0 || nl > hl then None
  else
    let rec go i =
      if i > hl - nl then None
      else if String.equal (String.sub hay i nl) needle then Some i
      else go (i + 1)
    in
    go 0

let strip_version (s : string) : string =
  match String.split_on_char ' ' (String.trim s) with
  | path :: _ -> path
  | [] -> s

let parse_replace (line : string) : (string * string) option =
  match find_sub line "=>" with
  | None -> None
  | Some i ->
    let lhs = strip_version (String.sub line 0 i) in
    let rhs =
      strip_version (String.sub line (i + 2) (String.length line - i - 2))
    in
    (* Skip local-directory targets ([./x]); only module-path rewrites apply. *)
    if lhs = "" || rhs = "" || String.length rhs > 0 && rhs.[0] = '.'
    then None
    else Some (lhs, rhs)

let parse_go_mod (content : string) : string option * (string * string) list =
  let step (module_path, replaces, in_block) raw =
    let line =
      match find_sub raw "//" with
      | Some i -> String.trim (String.sub raw 0 i)
      | None -> String.trim raw
    in
    if in_block then
      if String.equal line ")" then (module_path, replaces, false)
      else
        (match parse_replace line with
         | Some r -> (module_path, r :: replaces, true)
         | None -> (module_path, replaces, true))
    else if String.length line >= 7
            && String.equal (String.sub line 0 7) "module " then
      (Some (String.trim (String.sub line 7 (String.length line - 7))),
       replaces, false)
    else if String.equal line "replace (" then (module_path, replaces, true)
    else if String.length line >= 8
            && String.equal (String.sub line 0 8) "replace " then
      (match parse_replace (String.sub line 8 (String.length line - 8)) with
       | Some r -> (module_path, r :: replaces, false)
       | None -> (module_path, replaces, false))
    else (module_path, replaces, false)
  in
  let module_path, replaces, _in_block =
    List.fold_left step (None, [], false)
      (String.split_on_char '\n' content)
  in
  (module_path, List.rev replaces)

module SSet = Set.Make (String)

let go_mod_dirs ~(project_root : Fpath.t) (go_files : Fpath.t list)
  : Fpath.t list =
  let root = Fpath.normalize project_root in
  let rec walk ((seen, dirs) as st) dir =
    let dir = Fpath.normalize dir in
    let key = Fpath.to_string dir in
    if SSet.mem key seen then st
    else
      let seen = SSet.add key seen in
      let dirs =
        if Sys.file_exists (Fpath.to_string (Fpath.add_seg dir "go.mod"))
        then dir :: dirs
        else dirs
      in
      let parent = Fpath.parent dir in
      if Fpath.is_prefix root dir && not (Fpath.equal parent dir)
      then walk (seen, dirs) parent
      else (seen, dirs)
  in
  let _seen, dirs =
    List.fold_left (fun st f -> walk st (Fpath.parent f))
      (SSet.empty, []) go_files
  in
  dirs

let discover ~(project_root : Fpath.t) (go_files : Fpath.t list) : t =
  let modules, replaces =
    List.fold_left (fun (modules, replaces) dir ->
      let go_mod = Fpath.add_seg dir "go.mod" in
      match Nonfatal.catch ~default:None
              (fun () -> Some (UFile.read_file go_mod)) with
      | None -> (modules, replaces)
      | Some content ->
        let mpath, reps = parse_go_mod content in
        let modules =
          match mpath with
          | Some path -> { root = Fpath.normalize dir; path } :: modules
          | None -> modules
        in
        (modules, reps @ replaces))
      ([], []) (go_mod_dirs ~project_root go_files)
  in
  let modules =
    List.sort (fun a b ->
      compare (String.length (Fpath.to_string b.root))
        (String.length (Fpath.to_string a.root))) modules
  in
  { modules; replaces }

let import_path_of_dir (t : t) (dir : Fpath.t) : string option =
  let dir = Fpath.normalize dir in
  List.find_opt (fun m -> Fpath.is_prefix m.root dir) t.modules
  |> Option.map (fun m ->
    match Fpath.relativize ~root:m.root dir with
    | None -> m.path
    | Some rel ->
      let segs =
        Fpath.segs (Fpath.normalize rel)
        |> List.filter (fun s -> s <> "" && s <> ".")
      in
      if segs = [] then m.path else m.path ^ "/" ^ String.concat "/" segs)

let resolve_import (t : t) (import_path : string) : string =
  let rec apply = function
    | [] -> import_path
    | (old_, new_) :: rest ->
      if String.equal import_path old_ then new_
      else if String.length import_path > String.length old_
           && String.equal (String.sub import_path 0 (String.length old_ + 1))
                (old_ ^ "/")
      then new_ ^ String.sub import_path (String.length old_)
             (String.length import_path - String.length old_)
      else apply rest
  in
  apply t.replaces
