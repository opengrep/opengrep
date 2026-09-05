(*
   Load and cache .gitignore (or .semgrepignore) files
*)
open Gitignore

let create ?(gitignore_filenames = [ Gitignore.default_gitignore_filename ])
    ~project_root () =
  {
    project_root;
    gitignore_filenames;
    cache = Hashtbl.create 100;
    unreadable_reported = Hashtbl.create 4;
  }

let anchor_of_git_path git_path =
  Ppath.segments git_path |> Glob.Pattern.of_path_segments

(* The path of the ignore file of a folder, or the file itself when the
   name given on the command line is an absolute path. *)
let ignore_file_path ~(filename : string) (dir : Fpath.t) : Fpath.t =
  let fname = Fpath.v filename in
  if Fpath.is_abs fname then fname else Fpath.add_seg dir filename

let warn_unreadable (t : gitignores_cache) (path : Fpath.t) (reason : string) :
    unit =
  let key = Fpath.to_string path in
  if not (Hashtbl.mem t.unreadable_reported key) then (
    Hashtbl.replace t.unreadable_reported key ();
    (* nosemgrep: no-logs-in-library *)
    Logs.warn (fun m -> m "Ignoring the file '%s': %s" key reason))

(* The contents of an ignore file, or None. A path that is not there says
   nothing. One that is there and cannot be read as a file, because it is a
   folder or because its permissions deny reading, is left out with a
   warning and the scan goes on. pysemgrep scanned normally when the path
   was a folder (it tested is_file() and fell back to the built-in patterns)
   and died with a PermissionError, exit code 2, when it was a file it could
   not open. *)
let read_ignore_file_opt (t : gitignores_cache) (path : Fpath.t) :
    string option =
  let warn (reason : string) : string option =
    warn_unreadable t path reason;
    None
  in
  if not (Sys.file_exists (Fpath.to_string path)) then None
  else if not (UFile.is_reg ~follow_symlinks:true path) then
    warn "not a regular file"
  else
    try Some (UFile.read_file path) with
    | Sys_error (msg : string) -> warn msg
    | Unix.Unix_error ((err : Unix.error), _, _) ->
        warn (Unix.error_message err)

let read_ignore_file (t : gitignores_cache) ~(anchor : Glob.Pattern.t)
    ~(format : Gitignore.format) ~(source_kind : string) (path : Fpath.t) :
    Gitignore.path_selectors =
  match read_ignore_file_opt t path with
  | None -> []
  | Some contents ->
      Parse_gitignore.from_file_contents ~anchor ~format ~source_kind path
        contents

let load t dir_path =
  let tbl = t.cache in
  let key = Ppath.to_string_fast dir_path in
  match Hashtbl.find_opt tbl key with
  | Some res -> res
  | None ->
      let anchor = anchor_of_git_path dir_path in
      let path = Ppath.to_fpath ~root:t.project_root dir_path in
      let patterns =
        List.fold_left
          (fun acc (file : gitignore_filename) ->
            acc
            @ read_ignore_file t ~format:file.format ~anchor
                ~source_kind:file.source_kind
                (ignore_file_path ~filename:file.filename path))
          [] t.gitignore_filenames
      in
      let res =
        match patterns with
        | [] -> None
        | _ :: _ ->
            Some
              ({
                 level_kind = "in-project gitignore files";
                 source_name = Fpath.to_string path;
                 patterns;
               }
                : Gitignore.level)
      in
      Hashtbl.add tbl key res;
      res
[@@profiling]
