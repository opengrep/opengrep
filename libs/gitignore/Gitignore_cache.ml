(*
   Load and cache .gitignore (or .semgrepignore) files
*)
open Gitignore

let create ?(gitignore_filenames = [ Gitignore.default_gitignore_filename ])
    ~project_root () =
  let cache = Hashtbl.create 100 in
  { project_root; gitignore_filenames; cache }

let anchor_of_git_path git_path =
  Ppath.segments git_path |> Glob.Pattern.of_path_segments

(* The files already warned about, so that the '.semgrepignore' of the
   working directory, which is read both as its own level and by the walk
   that enters that directory, is reported once: the warning is about the
   file, not about each reader of it. [forget_warnings] starts a new run. *)
let warned : (string, unit) Hashtbl.t = Hashtbl.create 4

let forget_warnings () : unit = Hashtbl.reset warned

let warn_unreadable (path : Fpath.t) (reason : string) : unit =
  let key = Fpath.to_string path in
  if not (Hashtbl.mem warned key) then (
    Hashtbl.replace warned key ();
    (* nosemgrep: no-logs-in-library *)
    Logs.warn (fun m -> m "Ignoring the file '%s': %s" key reason))

(* An ignore file that cannot be read as a file, because it is a folder or
   because its permissions deny reading, is left out with a warning and the
   scan goes on. pysemgrep scanned normally when the path was a folder (it
   tested is_file() and fell back to the built-in patterns) and died with a
   PermissionError, exit code 2, when it was a file it could not open. *)
let read_ignore_file ~(anchor : Glob.Pattern.t) ~(format : Gitignore.format)
    ~(source_kind : string) (path : Fpath.t) : Gitignore.path_selectors =
  let warn (reason : string) : Gitignore.path_selectors =
    warn_unreadable path reason;
    []
  in
  if not (UFile.is_reg ~follow_symlinks:true path) then
    warn "not a regular file"
  else
    try Parse_gitignore.from_file ~anchor ~format ~source_kind path with
    | Sys_error (msg : string) -> warn msg
    | Unix.Unix_error ((err : Unix.error), _, _) ->
        warn (Unix.error_message err)

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
            let fname = Fpath.v file.filename in
            let file_path =
              if Fpath.is_abs fname then fname
              else Fpath.add_seg path file.filename
            in
            if Sys.file_exists (Fpath.to_string file_path) then
              acc
              @ read_ignore_file ~format:file.format ~anchor
                  ~source_kind:file.source_kind file_path
            else acc)
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
