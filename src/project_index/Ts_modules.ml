(* This module reads the exclude globs and the path mappings that the
   project's tsconfig files declare. *)

module Log = Log_projidx.Log

let strip_jsonc (str : string) : string =
  let n = String.length str in
  let buf = Buffer.create n in
  let rec loop i state =
    if i >= n then ()
    else
      let ch = str.[i] in
      match state with
      | `Line_cmt ->
        if ch = '\n' then Buffer.add_char buf ch;
        loop (i + 1) (if ch = '\n' then `Normal else `Line_cmt)
      | `Block_cmt ->
        if ch = '*' && i + 1 < n && str.[i + 1] = '/' then loop (i + 2) `Normal
        else begin
          if ch = '\n' then Buffer.add_char buf ch;
          loop (i + 1) `Block_cmt
        end
      | `In_string escaping ->
        Buffer.add_char buf ch;
        let next_state =
          if escaping then `In_string false
          else if ch = '\\' then `In_string true
          else if ch = '"' then `Normal
          else `In_string false
        in
        loop (i + 1) next_state
      | `Normal ->
        if ch = '"' then begin
          Buffer.add_char buf ch; loop (i + 1) (`In_string false)
        end else if ch = '/' && i + 1 < n then begin
          let next = str.[i + 1] in
          if next = '/' then loop (i + 2) `Line_cmt
          else if next = '*' then loop (i + 2) `Block_cmt
          else begin Buffer.add_char buf ch; loop (i + 1) `Normal end
        end else begin
          Buffer.add_char buf ch; loop (i + 1) `Normal
        end
  in
  loop 0 `Normal;
  Buffer.contents buf

(* String-aware: a "," inside a string literal is never a trailing comma. *)
let strip_trailing_commas (str : string) : string =
  let n = String.length str in
  let buf = Buffer.create n in
  let is_ws ch =
    Char.equal ch ' ' || Char.equal ch '\t'
    || Char.equal ch '\n' || Char.equal ch '\r'
  in
  let rec next_significant i =
    if i >= n then None
    else if is_ws str.[i] then next_significant (i + 1)
    else Some str.[i]
  in
  let rec loop i state =
    if i >= n then ()
    else
      let ch = str.[i] in
      match state with
      | `In_string escaping ->
        Buffer.add_char buf ch;
        let next_state =
          if escaping then `In_string false
          else if ch = '\\' then `In_string true
          else if ch = '"' then `Normal
          else `In_string false
        in
        loop (i + 1) next_state
      | `Normal ->
        if ch = '"' then begin
          Buffer.add_char buf ch; loop (i + 1) (`In_string false)
        end
        else if ch = ','
                && (match next_significant (i + 1) with
                    | Some (']' | '}') -> true
                    | Some _ | None -> false)
        then loop (i + 1) `Normal
        else begin
          Buffer.add_char buf ch; loop (i + 1) `Normal
        end
  in
  loop 0 `Normal;
  Buffer.contents buf

(* Yojson rejects a UTF-8 byte-order mark. *)
let strip_bom (str : string) : string =
  if String.length str >= 3
     && Char.equal str.[0] '\xef'
     && Char.equal str.[1] '\xbb'
     && Char.equal str.[2] '\xbf'
  then String.sub str 3 (String.length str - 3)
  else str

let read_tsconfig_excludes (path : Fpath.t) : string list =
  match
    Nonfatal.catch ~default:None (fun () ->
      Some (UFile.read_file path))
  with
  | None ->
    Log.debug (fun m ->
      m "tsconfig: failed to read %s; no excludes applied"
        (Fpath.to_string path));
    []
  | Some raw ->
  Nonfatal.catch ~default:[] (fun () ->
    let cleaned = raw |> strip_bom |> strip_jsonc |> strip_trailing_commas in
    let json = Yojson.Basic.from_string cleaned in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "exclude" fields with
       | Some (`List items) ->
         List.filter_map (function `String str -> Some str | _ -> None) items
       | _ -> [])
    | _ -> [])

(* Prefer tsconfig.build.json over tsconfig.json when both exist. *)
let find_tsconfigs (project_root : Fpath.t) : Fpath.t list =
  let root_str = Fpath.to_string project_root in
  let skip_dir name =
    name = "node_modules" || name = ".git" || name = ".yarn"
    || name = "dist" || name = "build" || name = ".cache"
  in
  (* Depth cap guards against cyclic directory symlinks. *)
  let max_depth = 64 in
  let rec walk (depth : int) (dir : string) (acc : Fpath.t list) : Fpath.t list =
    if depth > max_depth then acc
    else
    let entries =
      Nonfatal.catch ~default:[] (fun () ->
        Sys.readdir dir |> Array.to_list)
    in
    let build_path = Filename.concat dir "tsconfig.build.json" in
    let plain_path = Filename.concat dir "tsconfig.json" in
    let acc =
      if Sys.file_exists build_path then Fpath.v build_path :: acc
      else if Sys.file_exists plain_path then Fpath.v plain_path :: acc
      else acc
    in
    List.fold_left (fun acc entry ->
      let full = Filename.concat dir entry in
      let is_dir = Nonfatal.catch ~default:false (fun () -> Sys.is_directory full) in
      if is_dir && not (skip_dir entry) then walk (depth + 1) full acc
      else acc
    ) acc entries
  in
  walk 0 root_str []

(* Glob patterns are '/'-separated on every platform; the prefix derived
   from [config_dir] is emitted in that form. *)
let normalize_pattern ~(project_root : Fpath.t) ~(config_dir : Fpath.t)
    (pat : string) : string =
  if String.length pat > 0 && pat.[0] = '/' then pat
  else
    match
      Fpath.relativize ~root:(Fpath.normalize project_root)
        (Fpath.normalize config_dir)
    with
    | None -> pat
    | Some rel ->
      let rel = Fpath.rem_empty_seg rel in
      (match Fpath.segs rel with
       | ["."] | ".." :: _ -> pat  (* config at or outside the root *)
       | segs -> String.concat "/" segs ^ "/" ^ pat)

let excludes_of_configs ~(project_root : Fpath.t) (configs : Fpath.t list)
    : string list =
  List.concat_map (fun cfg ->
    let dir = Fpath.parent cfg in
    let raw = read_tsconfig_excludes cfg in
    List.map (normalize_pattern ~project_root ~config_dir:dir) raw)
    configs

let read_tsconfig_paths (path : Fpath.t) : (string * string list) list =
  let config_dir = Fpath.parent path in
  match
    Nonfatal.catch ~default:None (fun () -> Some (UFile.read_file path))
  with
  | None ->
    Log.debug (fun m ->
      m "tsconfig: failed to read %s; no path mappings applied"
        (Fpath.to_string path));
    []
  | Some raw ->
    Nonfatal.catch ~default:[] (fun () ->
      let cleaned = raw |> strip_bom |> strip_jsonc |> strip_trailing_commas in
      match Yojson.Basic.from_string cleaned with
      | `Assoc fields -> (
        match List.assoc_opt "compilerOptions" fields with
        | Some (`Assoc options) ->
          let base_dir =
            match List.assoc_opt "baseUrl" options with
            | Some (`String (base : string)) ->
              Fpath.append config_dir (Fpath.v base)
            | Some _
            | None -> config_dir
          in
          let absolute (target : string) : string =
            Fpath.append base_dir (Fpath.v target) |> Fpath.normalize
            |> Fpath.rem_empty_seg |> Fpath.to_string
          in
          (match List.assoc_opt "paths" options with
           | Some (`Assoc entries) ->
             List.map
               (fun ((key : string), (targets : Yojson.Basic.t)) ->
                 ( key,
                   match targets with
                   | `List items ->
                     List.filter_map
                       (function
                         | `String target -> Some (absolute target)
                         | _ -> None)
                       items
                   | _ -> [] ))
               entries
           | Some _
           | None -> [])
        | Some _
        | None -> [])
      | _ -> [])

let paths_of_configs ~(project_root : Fpath.t) (configs : Fpath.t list)
    : (string * string list) list =
  match configs with
  | [] -> []
  | _ :: _ ->
    let root_str = Fpath.to_string (Fpath.normalize project_root) in
    let at_root =
      List.filter
        (fun (config : Fpath.t) ->
          String.equal
            (Fpath.to_string
               (Fpath.parent config |> Fpath.normalize |> Fpath.rem_empty_seg))
            (Fpath.to_string
               (Fpath.normalize project_root |> Fpath.rem_empty_seg)))
        configs
    in
    Log.debug (fun m ->
      m "tsconfig paths: %d config(s) at the project root %s"
        (List.length at_root) root_str);
    List.concat_map read_tsconfig_paths at_root

let discover ~(project_root : Fpath.t)
    : string list * (string * string list) list =
  let configs = find_tsconfigs project_root in
  (excludes_of_configs ~project_root configs,
   paths_of_configs ~project_root configs)
