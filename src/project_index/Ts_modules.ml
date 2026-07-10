(* TypeScript/JavaScript module handling: tsconfig-driven discover excludes
   and import-specifier resolution against the project file set. *)

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

(* Prefer tsconfig.build.json over tsconfig.json to match scip-typescript. *)
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

let normalize_pattern ~(project_root : Fpath.t) ~(config_dir : Fpath.t)
    (pat : string) : string =
  if String.length pat > 0 && pat.[0] = '/' then pat
  else
    let cfg_rel =
      Nonfatal.catch ~default:"" (fun () ->
        let pr = Fpath.to_string project_root in
        let cd = Fpath.to_string config_dir in
        if String.length cd >= String.length pr
           && String.sub cd 0 (String.length pr) = pr
        then
          let rel = String.sub cd (String.length pr) (String.length cd - String.length pr) in
          if String.length rel > 0 && rel.[0] = '/' then String.sub rel 1 (String.length rel - 1)
          else rel
        else "")
    in
    if cfg_rel = "" then pat
    else cfg_rel ^ "/" ^ pat

let discover_excludes ~(project_root : Fpath.t) : string list =
  let configs = find_tsconfigs project_root in
  List.concat_map (fun cfg ->
    let dir = Fpath.parent cfg in
    let raw = read_tsconfig_excludes cfg in
    List.map (normalize_pattern ~project_root ~config_dir:dir) raw)
    configs

let build_path_suffix_index (file_paths : string list)
  : (string, string list) Hashtbl.t =
  let index : (string, string list) Hashtbl.t = Hashtbl.create 16384 in
  let strip_ext path =
    if Filename.check_suffix path ".tsx" then Filename.chop_suffix path ".tsx"
    else if Filename.check_suffix path ".ts" then Filename.chop_suffix path ".ts"
    else if Filename.check_suffix path ".jsx" then Filename.chop_suffix path ".jsx"
    else if Filename.check_suffix path ".js" then Filename.chop_suffix path ".js"
    else path
  in
  let strip_index path =
    if Filename.check_suffix path "/index" then
      Filename.chop_suffix path "/index"
    else path
  in
  List.iter (fun path ->
    let stripped = path |> strip_ext |> strip_index in
    let parts = String.split_on_char '/' stripped in
    let n = List.length parts in
    let arr = Array.of_list parts in
    for i = 0 to n - 1 do
      let suffix = String.concat "/"
        (Array.to_list (Array.sub arr i (n - i))) in
      let cur = Option.value (Hashtbl.find_opt index suffix) ~default:[] in
      Hashtbl.replace index suffix (path :: cur)
    done
  ) file_paths;
  index

let resolve_specifier
    ?(path_suffix_index : (string, string list) Hashtbl.t option = None)
    ~(current_file : Fpath.t) (specifier : string) : string list =
  if String.length specifier = 0 then []
  else if specifier.[0] = '.' then begin
    let base =
      Fpath.append (Fpath.parent current_file) (Fpath.v specifier)
      |> Fpath.normalize |> Fpath.rem_empty_seg |> Fpath.to_string
    in
    [ base ^ ".ts"; base ^ ".tsx"; base ^ ".js"; base ^ ".jsx";
      base ^ "/index.ts"; base ^ "/index.tsx";
      base ^ "/index.js"; base ^ "/index.jsx" ]
  end
  else
    match path_suffix_index with
    | None -> []
    | Some idx ->
      (Option.value (Hashtbl.find_opt idx specifier) ~default:[])
