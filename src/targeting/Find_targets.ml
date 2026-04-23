(* Martin Jambon, Yoann Padioleau
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_t
module Log = Log_targeting.Log

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Find target file candidates from one or more scanning roots.

   ***************************************************************************

   Definitions:
   - scanning root: a path specified on the command line. It may be a folder,
     a regular file, or a symbolic link that resolves to a folder or a
     regular file.
   - target: a regular file that semgrep will scan.
   - project: a folder containing target files in its subfolders. The notion
     of project allows us to locate and consult project-specific settings
     such as '.semgrepignore' files.
   - physical path: a path '/a/b/c' is a physical path to file 'c' if
     neither '/a/b/c', '/a/b', '/a', or '/' are symlinks.

   ***************************************************************************

   Challenges:
   - symbolic links! Symlinks make it possible and common for multiple paths
     to identify the same file. When the user specifies a path on the command
     line, error messages and semgrep results should use that path as
     a prefix rather than an equivalent path.
   - Semgrep accepts scanning roots that potentially belong to different
     projects (unlike Git).
   - the current folder doesn't necessarily belong to the project (unlike
     with Git).

   ***************************************************************************

   How to produce nice target paths?
   = How to identify project roots correctly and return target paths that
   have the scanning root as prefix?

   1. To guarantee that each target belongs to exactly one project and avoid
      confusion, the project root is determined using the physical path
      to the scanning root.
      -> use 'realpath' to get the physical path to the scanning root and
         consult its parent folders recursively until finding the project root.

   2. To reference the path to a target within the project, we use an
      in-project path that is relative to the project root.
      -> list the regular files under the scanning root and express
         their path relative to the project root.

   3. When returning a path to a target file to a user, we make sure
      that the path has the original scanning root path i.e. not necessarily
      a physical or absolute path, followed by the path from the scanning
      root to the target file.
      -> take the in-project path to a target and express it relative it
         the in-project path to the scanning root.
      -> concatenate the original file system path to the scanning root
         with the target path relative to the scanning root.

   Here's an example:

     scanning root: myproject-v2/src
     'myproject-v2' is a symlink: myproject-v2 -> ../myproject
     physical path to the scanning root: /home/me/myproject/src
     project root (physical path): /home/me/myproject
     physical path to some target file: /home/me/myproject/src/hello/hello.py
     in-project path to the target file: /src/hello/hello.py
     final path to the target file: myproject-v2/src/hello/hello.py

   ***************************************************************************

   Performance: collecting target candidates is a one-time operation
   that can be relatively expensive (O(number of files)).

   Partially translated from target_manager.py

   Original python comments:

     Assumes file system does not change during it's existence to cache
     files for a given language etc. If file system changes
     (i.e. git checkout), create a new TargetManager object

     If respect_gitignore is true then will only consider files that are
     tracked or (untracked but not ignored) by git

     If git_baseline_commit is true then will only consider files that have
     changed since that commit

     If allow_unknown_extensions is set then targets with extensions that are
     not understood by semgrep will always be returned by get_files. Else will
     discard targets with unknown extensions

   TODO:
    - optimize, reduce the number of filesystem lookup? or memoize them?
      there are a few places where we stat for a file
    - add an option to select all git-tracked files regardless of
      gitignore or semgrepignore exclusions (will be needed for Secrets)
      and have the exclusions apply only to the files that aren't tracked.
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

type project_root =
  | Filesystem of Rfpath.t
  (* for Semgrep query console *)
  | Git_remote of git_remote

and git_remote = { url : Uri.t } [@@deriving show]

(* Yet another file path related type ...

   This module is a bit fragile as it assumes that target file paths found in
   the file system have the same form as those passed on the command line.
   It won't work with unnormalized paths such as 'foo/../bar.js' that will
   likely be rewritten into 'bar.js'. See:

     $ git ls-files libs/../README.md
     README.md

   This results in 'README.md' being treated as non-explicit target file.

   TODO: use pairs (project, ppath) instead as keys? If we use a dedicated
   record for targets, we can extract the pair (project, ppath):

     type target = {
       project: Project.t; (* provides normalized project root *)
       path: Fppath.t; (* provides (normalized) ppath *)
     }

   If we go this path, we could also add a field 'is_explicit: bool' to the
   target type.
*)
module Explicit_targets = struct
  type t = {
    tbl : (Fpath.t, unit) Hashtbl.t;
        [@printer fun fmt _tbl -> fprintf fmt "<hashtbl>"]
    (* Elements in their original order *)
    list : Fpath.t list;
  }
  [@@deriving show]

  let empty = { tbl = Hashtbl.create 0; list = [] }

  let of_list paths =
    let tbl = Hashtbl.create (2 * List.length paths) in
    List.iter (fun path -> Hashtbl.replace tbl path ()) paths;
    { tbl; list = paths }

  let to_list x = x.list

  (* Fast O(1) operation *)
  let mem x path = Hashtbl.mem x.tbl path
end

type conf = {
  (* global exclude list, passed via semgrep '--exclude'.
   * TODO? use Glob.Pattern.t instead? same for include_
   *)
  exclude : string list;
  (* !!! '--include' is very different from '--exclude' !!!
      The include filter is applied after after gitignore and
      semgrepignore filters. It doesn't override them.

     This field holds a list of patterns passed via 'semgrep --include'
     [!] include_ = None is the opposite of Some [].
     If a list of include patterns is specified, a path must match
     at least of the patterns to be selected.
     ('--require' might make a better flag name, but both grep and ripgrep
      use the '--exclude' and '--include' names).
  *)
  include_ : string list option;
  (* This can be set to [true] in order for files passed as targets to
   * be filtered according to the exclude / include filters; by default
   * this filtering only happens on files found under directories passed
   * as scan targets. See #264. *)
  apply_includes_excludes_to_file_targets: bool;
  max_target_bytes : int;
  respect_gitignore : bool;
  respect_semgrepignore_files : bool;
  semgrepignore_filename : string option;
  always_select_explicit_targets : bool;
  explicit_targets : Explicit_targets.t;
  (* osemgrep-only: option
     (see Git_project.find_any_project_root and the force_root parameter) *)
  force_project_root : project_root option;
  force_novcs_project : bool;
  (* osemgrep-only option, exclude scanning minified files, default false *)
  exclude_minified_files : bool;
  (* TODO? remove it? This is now done in Diff_scan.ml instead? *)
  baseline_commit : string option;
  diff_depth : int;
}
[@@deriving show]

(*************************************************************************)
(* Defaults *)
(*************************************************************************)

let default_conf : conf =
  {
    force_project_root = None;
    force_novcs_project = false;
    exclude = [];
    include_ = None;
    apply_includes_excludes_to_file_targets = false; (* default behaviour *)
    (* Must be kept in sync w/ pysemgrep.
       coupling: cli/src/semgrep/constants.py DEFAULT_MAX_TARGET_SIZE
    *)
    max_target_bytes = 1000000;
    respect_gitignore = true;
    respect_semgrepignore_files = true;
    semgrepignore_filename = None;
    always_select_explicit_targets = false;
    explicit_targets = Explicit_targets.empty;
    exclude_minified_files = false;
    baseline_commit = None;
    diff_depth = 2;
  }


(*************************************************************************)
(* The actual returned type *)
(*************************************************************************)

(* 'a is either Fpath or Fppath *)
type 'a targets = {
  selected : 'a list;
  skipped : Semgrep_output_v1_t.skipped_target list;
  git_repo : bool
}

(*************************************************************************)
(* Diagnostic *)
(*************************************************************************)

let get_reason_for_exclusion (sel_events : Gitignore.selection_event list) :
    Out.skip_reason =
  let fallback = Out.Semgrepignore_patterns_match in
  match sel_events with
  | Gitignore.Selected loc :: _ -> (
      match loc.source_kind with
      | Some str -> (
          match str with
          | "include" -> Out.Cli_include_flags_do_not_match
          | "exclude" -> Out.Cli_exclude_flags_match
          (* TODO: osemgrep supports the new Gitignore_patterns_match, but for
           * legacy reason we don't generate it for now.
           *)
          | "gitignore"
          | "semgrepignore" ->
              Out.Semgrepignore_patterns_match
          | __ -> (* shouldn't happen *) fallback)
      | None -> (* shouldn't happen *) fallback)
  | Gitignore.Deselected _ :: _
  | [] ->
      (* shouldn't happen *) fallback

(*************************************************************************)
(* Filtering *)
(*************************************************************************)

type filter_result =
  | Keep (* select this target file *)
  | Dir (* the path is a directory to scan recursively *)
  | Skip of Out.skipped_target (* ignore this file and report it *)
  | Ignore_silently (* ignore and don't report this file *)

let ignore_path selection_events fpath =
  Log.debug (fun m ->
      m "Ignoring path %s:\n%s" !!fpath
        (Gitignore.show_selection_events selection_events));
  let reason = get_reason_for_exclusion selection_events in
  Skip
    {
      Out.path = fpath;
      reason;
      details =
        Some "excluded by --include/--exclude, gitignore, or semgrepignore";
      rule_id = None;
    }

let apply_include_filter status selection_events include_filter ppath =
  match status with
  | Gitignore.Ignored -> (status, selection_events)
  | Gitignore.Not_ignored -> (
      match include_filter with
      | None -> (status, selection_events)
      | Some include_filter -> Include_filter.select include_filter ppath)

(* Note that include_filter applies only to the paths of regular files. They're
 * applied last, after the exclude/gitignore/semgrepignore filters.
 *)
let filter_path (ign : Gitignore.filter)
    (include_filter : Include_filter.t option) (fppath : Fppath.t) :
    filter_result =
  let { fpath; ppath } : Fppath.t = fppath in
  let status, selection_events = Gitignore_filter.select ign ppath in
  match status with
  | Ignored -> ignore_path selection_events fpath
  | Not_ignored -> (
      (* TODO: check read permission too? *)
      match (Unix.lstat !!fpath).st_kind with
      (* skipping symlinks *)
      | S_LNK -> Ignore_silently
      | S_REG -> (
          let status, selection_events =
            apply_include_filter status selection_events include_filter ppath
          in
          match status with
          | Ignored -> ignore_path selection_events fpath
          | Not_ignored -> Keep)
      | S_DIR -> Dir
      | S_FIFO
      | S_CHR
      | S_BLK
      | S_SOCK ->
          Ignore_silently
      (* We need to filter those paths ASAP otherwise we can get some exn later
       * when trying to process targets that actually do not exist.
       *)
      | exception Unix.Unix_error (err, _fun, _info) ->
          Log.debug (fun m ->
              m "lstat: system error on file '%s': %s" !!fpath
                (Unix.error_message err));
          Ignore_silently)

(*
   Filter a pre-expanded list of target files, such as a list of files
   obtained with 'git ls-files'.

   We apply gitignore/semgrepignore and include filters because
   a tracked file can match a .gitignore pattern (added after tracking).
   We also filter out symlinks and other non-regular files via lstat,
   since git tracks symlinks as blob entries.
*)
let filter_paths
    ((ign, include_filter) : Gitignore.filter * Include_filter.t option)
    (target_files : Fppath.t list) : Fppath.t list * Out.skipped_target list =
  let (selected_paths : Fppath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in
  let add path = Stack_.push path selected_paths in
  let skip target = Stack_.push target skipped in
  target_files
  |> List.iter (fun (fppath : Fppath.t) ->
         let status, selection_events =
           Gitignore_filter.select ign fppath.ppath
         in
         let status, selection_events =
           apply_include_filter status selection_events include_filter
             fppath.ppath
         in
         match status with
         | Gitignore.Ignored ->
             Log.debug (fun m ->
                 m "Ignoring path %s:\n%s" !!(fppath.fpath)
                   (Gitignore.show_selection_events selection_events));
             skip
               {
                 Out.path = fppath.fpath;
                 reason = get_reason_for_exclusion selection_events;
                 details =
                   Some
                     "excluded by --include/--exclude, gitignore, or \
                      semgrepignore";
                 rule_id = None;
               }
         | Gitignore.Not_ignored -> (
             (* Git tracks symlinks as blob entries, so we must filter them
                out just like the filesystem walker (filter_path) does. *)
             match (Unix.lstat !!(fppath.fpath)).st_kind with
             | S_REG -> (
                 match Skip_target.filter_file_access_permissions fppath.fpath with
                 | Ok _ -> add fppath
                 | Error skipped -> skip skipped)
             | S_LNK | S_DIR | S_FIFO | S_CHR | S_BLK | S_SOCK ->
                 Log.debug (fun m ->
                     m "ignore non-regular file: %s" !!(fppath.fpath))
             | exception Unix.Unix_error _ ->
                 Log.debug (fun m ->
                     m "lstat failed, ignoring: %s" !!(fppath.fpath))));
  (!selected_paths, !skipped)

(* Note: throughout this file we use List.rev_append instead of (@) for
 * concatenating file lists, since it is tail-recursive. The order does not
 * matter because we sort and deduplicate at the end in get_targets. *)
let filter_size_and_minified max_target_bytes exclude_minified_files paths =
  let selected_fppaths, skipped_size =
    Result_.partition
      (fun (fppath : Fppath.t) ->
        Result.map
          (fun _ -> fppath)
          (Skip_target.is_big max_target_bytes fppath.fpath))
      paths
  in
  let selected_fppaths, skipped_minified =
    if exclude_minified_files then
      Result_.partition
        (fun (fppath : Fppath.t) ->
          Result.map (fun _ -> fppath) (Skip_target.is_minified fppath.fpath))
        selected_fppaths
    else (selected_fppaths, [])
  in
  Log.debug (fun m -> m "skipped_size: %d" (List.length skipped_size));
  Log.debug (fun m -> m "skipped_minified: %d" (List.length skipped_minified));
  (selected_fppaths, List.rev_append skipped_size skipped_minified)

(*************************************************************************)
(* Finding by walking *)
(*************************************************************************)

(* We used to call 'git ls-files' when conf.respect_gitignore was true,
 * which could potentially speedup things because git may rely on
 * internal data-structures to answer the question instead of walking
 * the filesystem and read the potentially many .gitignore files.
 * However this was not handling .semgrepignore and especially the new
 * ability in osemgrep to negate gitignore decisions in a .semgrepignore,
 * so I think it's simpler to just walk the filesystem whatever the value of
 * conf.respect_git_ignore is. That's what ripgrep does too.
 *
 * python: was called Target.files_from_filesystem ()
 *
 * pre: the scan_root must be a path to a directory
 *)
let walk_skip_and_collect (ign : Gitignore.filter)
    (include_filter : Include_filter.t option) (scan_root : Fppath.t) :
    Fppath.t list * Out.skipped_target list =
  Log.info (fun m ->
      m "scanning file system starting from root %s" (Fppath.show scan_root));
  (* Imperative style! walk and collect.
     This is for the sake of readability so let's try to make this as
     readable as possible.
  *)
  let (selected_paths : Fppath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in

  (* TODO: factorize code with filter_paths? *)
  let add path = Stack_.push path selected_paths in
  let skip target = Stack_.push target skipped in

  (* mostly a copy-paste of List_files.list_regular_files() *)
  let rec aux (dir : Fppath.t) =
    match Skip_target.filter_dir_access_permissions dir.fpath with
    | Error skipped -> skip skipped
    | Ok _path ->
        Log.debug (fun m ->
            m "listing dir %s (ppath = %s)" !!(dir.fpath)
              (Ppath.to_string_for_tests dir.ppath));
        (* TODO? should we sort them first? *)
        let entries = List_files.read_dir_entries dir.fpath in
        (* TODO: factorize code with filter_paths? *)
        entries
        |> List.iter (fun name ->
               let fpath =
                 (* if scan_root was "." we want to display paths as "foo/bar"
                  * and not "./foo/bar"
                  *)
                 if Fpath.equal dir.fpath (Fpath.v ".") then Fpath.v name
                 else Fpath.add_seg dir.fpath name
               in
               let ppath = Ppath.add_seg dir.ppath name in
               let fppath : Fppath.t = { fpath; ppath } in
               match filter_path ign include_filter fppath with
               | Keep -> (
                   match Skip_target.filter_file_access_permissions fpath with
                   | Ok _path -> add fppath
                   | Error skipped -> skip skipped)
               | Skip skipped -> skip skipped
               | Dir -> aux fppath
               | Ignore_silently -> ())
  in
  aux scan_root;
  (* Let's not worry about file order here until we have to.
     They will be sorted later. *)
  (!selected_paths, !skipped)

(*************************************************************************)
(* Finding by using git *)
(*************************************************************************)

(*
   Get the list of files being tracked by git. Return a list of paths
   relative to the project root in addition to their system path
   so that we can filter them with semgrepignore.

   exclude_standard is the --exclude-standard flag to 'git ls-files'
   and requests filtering based on gitignore rules. We don't want it when
   obtaining the list of tracked files because some files can be tracked
   despite being excluded by gitignore.
*)
let git_list_files ~exclude_standard
    (file_kinds : Git_wrapper.ls_files_kind list)
    (project_roots : Project.roots) : Fppath.t list option =
  Log.debug (fun m ->
      m "Find_targets.git_list_files for project %s"
        (Project.show project_roots.project));
  let project = project_roots.project in
  (* TODO: we should not call git_list_files when the project
   * is not a Git_project. We should assert it and not return
   * an option type but an Fppath.t list instead.
   *)
  match project.kind with
  | Git_project ->
      let cwd = Fpath.v (Sys.getcwd ()) in
      Some
        (project_roots.scanning_roots
        |> List.concat_map (fun (sc_root : Fppath.t) ->
               if UFile.is_reg ~follow_symlinks:true sc_root.fpath then
                 [ sc_root ]
               else if UFile.is_dir ~follow_symlinks:true sc_root.fpath then (
                 Log.info (fun m ->
                     m "List git files for scanning root %s"
                       (Fppath.show sc_root));
                 let project_root = Rfpath.to_rpath project.root in
                 (* The path prefix we want for all the target file paths
                    that we return *)
                 let orig_scanning_root_path = sc_root.fpath in
                 (* Best effort to get a relative scanning root path
                    (will fail in file systems with multiple roots) *)
                 let rel_scanning_root_path_or_absolute =
                   if Fpath.is_rel orig_scanning_root_path then
                     orig_scanning_root_path
                   else
                     match
                       Fpath.relativize ~root:cwd orig_scanning_root_path
                     with
                     | Some rel_scanning_root -> rel_scanning_root
                     | None ->
                         (* absolute, on another volume than cwd *)
                         orig_scanning_root_path
                 in
                 (* We can't just cd into the scanning root to obtain paths
                    relative to it because the scanning root may be a regular
                    file. It could also be the root of the file system, so we
                    also can't cd into its parent.
                    This is why we stay in the same cwd and only later convert
                    the resulting paths to be relative to the scanning root. *)
                 Git_wrapper.ls_files_relative ~exclude_standard
                   ~kinds:file_kinds ~project_root
                   [ orig_scanning_root_path ]
                 |> List_.map (fun target_relative_to_cwd_or_absolute ->
                        (* Invariant: the target path is a descendant of the
                           scanning root path *)
                        (* Obtain a path whose prefix is the original scanning
                           root if possible.
                           If the scanning root is './proj/lib',
                           then we want a result target path to be
                           './proj/lib/../hello.c', not the equivalent
                           'proj/hello.c'.
                           The only exception is if the scanning root is '.',
                           in which case we don't produce './foo' but 'foo'.
                        *)
                        let target_fpath =
                          match
                            (* 'root' must be a folder *)
                            Fpath.relativize
                              ~root:rel_scanning_root_path_or_absolute
                              target_relative_to_cwd_or_absolute
                          with
                          | Some target_relative_to_scan_root ->
                              Fpath_.append_no_dot orig_scanning_root_path
                                target_relative_to_scan_root
                          | None -> target_relative_to_cwd_or_absolute
                        in
                        (* Obtain a path relative to the project root *)
                        let target_ppath =
                          match
                            Fpath.relativize
                              ~root:(Rpath.to_fpath project_root)
                              (cwd // target_relative_to_cwd_or_absolute)
                          with
                          | None ->
                              (* TODO: return an Error instead and let the
                                 caller decide instead of assert false
                              *)
                              (* nosemgrep: no-logs-in-library *)
                              Logs.err (fun m ->
                                  m
                                    "Internal error: cannot obtain path \
                                     relative to project root from \
                                     project_root=%s, cwd=%S, \
                                     path_relative_to_cwd=%S"
                                    !!(Rpath.to_fpath project_root)
                                    !!cwd
                                    !!target_relative_to_cwd_or_absolute);
                              assert false
                          | Some fpath_relative_to_project_root ->
                              Ppath.of_relative_fpath
                                fpath_relative_to_project_root
                        in
                        ({ fpath = target_fpath; ppath = target_ppath }
                          : Fppath.t)))
               else (
                 (* scanning root is neither a file nor a folder *)
                 Log.warn (fun m ->
                     m "invalid scanning root %s" !!(sc_root.fpath));
                 [])))
  | _ -> None

(*
   Get the list of files being tracked by git, return a list of paths
   relative to the project root.

   This doesn't include the "untracked files" reported by 'git status'.
   These untracked files may or may not be desirable. Their fate will be
   determined by the semgrepignore rules separately, along with the gitignored
   files that are not being tracked.

   We could also provide similar functions for other file tracking systems
   (Mercurial/hg, Subversion/svn, ...)
*)
let git_list_tracked_files (project_roots : Project.roots) : Fppath.t list option
    =
  git_list_files ~exclude_standard:false [ Cached ] project_roots

(*
   Get tracked files that match gitignore patterns.  These are files that
   were added to git before the gitignore rule was created.

   We use this to split the tracked set: files NOT in this set can skip
   .gitignore pattern evaluation entirely (fast path), while files IN
   this set must go through the full filter so that .semgrepignore '!'
   negation patterns can override the gitignore exclusion.

   Uses 'git ls-files --cached --ignored --exclude-standard' which
   returns tracked files that WOULD be ignored if they weren't tracked.
*)
let git_list_tracked_gitignored_files (project_roots : Project.roots) :
    Fppath.t list option =
  git_list_files ~exclude_standard:true [ Cached; Ignored ] project_roots

(*
   List all the files that are not being tracked by git except those in
   '.git/'. Return a list of paths relative to the project root.

   This is the complement of git_list_tracked_files (except for '.git/').
*)
let git_list_untracked_files ~respect_gitignore (project_roots : Project.roots)
    : Fppath.t list option =
  git_list_files ~exclude_standard:respect_gitignore [ Others ] project_roots

(*************************************************************************)
(* Grouping *)
(*************************************************************************)

let scanning_root_by_project ~(force_root : Project.t option)
    ~(force_novcs : bool) (scanning_root : Scanning_root.t) :
    Project.t * Fppath.t =
  let scanning_root_fpath = Scanning_root.to_fpath scanning_root in
  let kind, scanning_root_info =
    Project.find_any_project_root ~fallback_root:None ~force_novcs ~force_root
      scanning_root_fpath
  in
  let project : Project.t = { kind; root = scanning_root_info.project_root } in
  let path : Fppath.t =
    { fpath = scanning_root_fpath; ppath = scanning_root_info.inproject_path }
  in
  (project, path)

(*
   Identify the project root for each scanning root and group them
   by project root. If the project_root is specified, then we use that.

   This is important to avoid reading the gitignore and semgrepignore files
   twice when multiple scanning roots that belong to the same project.

   TODO? move in paths/Project.ml?
*)
let group_scanning_roots_by_project (conf : conf)
    (scanning_roots : Scanning_root.t list) : Project.roots list =
  (* Force root relativizes scan roots to project roots.
     I.e. if the project_root is /repo/src/ and the scanning root is /src/foo
     it would make the scanning root /foo. So it doesn't make sense to
     combine this with the git remote unless we wanted to make it so git
     remotes could be further specified (say
     github.com/semgrep/semgrep.git:/src/foo).

     TODO: revise the above. 'force_root' is the project root.
  *)
  Log.debug (fun m ->
      m "group_scanning_roots_by_project %s"
        (Logs_.list Scanning_root.to_string scanning_roots));
  let force_root : Project.t option =
    match conf.force_project_root with
    | Some (Filesystem proj_root) ->
        (* This is when --project-root is specified on the command line.
           It doesn't use 'git ls-files' to list files. This is required
           for some tests to pass within our semgrep repo but it's not clear
           why it's like this.
           TODO: make tests work without requiring --project-root? *)
        Some Project.{ kind = Project.Gitignore_project; root = proj_root }
    | Some (Git_remote _)
    | None ->
        (* Usual case when scanning the local file system *)
        None
  in
  scanning_roots
  |> List.filter (fun sc_root ->
         let fpath = Scanning_root.to_fpath sc_root in
         if UFile.is_dir_or_reg ~follow_symlinks:true fpath then true
         else (
           Log.warn (fun m -> m "invalid scanning root: %s" !!fpath);
           false))
  |> List_.map
       (scanning_root_by_project ~force_novcs:conf.force_novcs_project
          ~force_root)
  (* Using a realpath (physical path) in Project.t ensures we group
     correctly even if the scanning_roots went through different symlink paths.
  *)
  |> Assoc.group_assoc_bykey_eff
  |> List_.map (fun (project, scanning_roots) ->
         Project.{ project; scanning_roots })

(*************************************************************************)
(* Work on a single project *)
(*************************************************************************)
(*
   We allow multiple scanning roots and they may not all belong to the same
   git project. Most of the logic is done at a project level, though.
*)

let setup_path_filters conf (project_roots : Project.roots) :
    Gitignore.filter * Include_filter.t option =
  let Project.{ project = { kind; root = project_root }; scanning_roots = _ } =
    project_roots
  in
  (* filter with .gitignore and .semgrepignore *)
  let exclusion_mechanism : Semgrepignore.exclusion_mechanism =
    match kind with
    | Git_project
    | Gitignore_project ->
        {
          use_gitignore_files = conf.respect_gitignore;
          use_semgrepignore_files = conf.respect_semgrepignore_files;
        }
    | Mercurial_project
    | Subversion_project
    | Darcs_project
    | No_VCS_project ->
        {
          use_gitignore_files = false;
          use_semgrepignore_files = conf.respect_semgrepignore_files;
        }
  in
  (* filter also the --include and --exclude from the CLI args
   * (the paths: exclude: include: in a rule are handled elsewhere, in
   * Run_semgrep.ml by calling Filter_target.filter_paths
   *
   * We currently handle gitignores by creating this
   * ign below that then will internally use some cache and complex
   * logic to select files in walk_skip_and_collect().
   * TODO? we could instead change strategy and accumulate the
   * current set of applicable gitignore as we walk down the FS
   * hierarchy. We would not need then to look at each element
   * in the ppath and look for the present of a .gitignore there;
   * the job would have already been done as we walked!
   * We would still need to intialize at the beginning with
   * the .gitignore of all the parents of the scan_root.
   *)
  let semgrepignore_filter =
    Semgrepignore.create ~cli_patterns:conf.exclude
      ?semgrepignore_filename:conf.semgrepignore_filename
      ~default_semgrepignore_patterns:Semgrep_scan_legacy
      ~exclusion_mechanism
      ~project_root:(Rfpath.to_fpath project_root)
      ()
  in
  let include_filter =
    Option.map
      (Include_filter.create ~project_root:(Rfpath.to_fpath project_root))
      conf.include_
  in
  (semgrepignore_filter, include_filter)

let get_targets_from_filesystem conf (project_roots : Project.roots) =
  let ign, include_filter = setup_path_filters conf project_roots in
  List.fold_left
    (fun (selected, skipped) (scan_root : Fppath.t) ->
      (* better: Note that we use Unix.stat below, not Unix.lstat, so
       * osemgrep accepts symlink paths on the command--line;
       * you can do 'osemgrep -e ... ~/symlink-to-proj' or even
       * 'osemgrep -e ... symlink-to-file.py' whereas pysemgrep
       * exits with '"/home/foo/symlink-to-proj" file not found'
       * Note: This may raise Unix.Unix_error.
       * TODO? improve Unix.Unix_error in Find_targets specific exn?
       *)
      let selected2, skipped2 =
        match (Unix.stat !!(scan_root.fpath)).st_kind with
        (* TOPORT? make sure has right permissions (readable) *)
        | S_REG ->
          if not conf.apply_includes_excludes_to_file_targets then
            ([ scan_root ], [])
          else
            walk_skip_and_collect ign include_filter scan_root
        | S_DIR -> walk_skip_and_collect ign include_filter scan_root
        | S_LNK ->
            (* already dereferenced by Unix.stat *)
            raise Impossible
        (* TODO? use write_pipe_to_disk? *)
        | S_FIFO -> ([], [])
        (* TODO? return an error message or a new skipped_target kind? *)
        | S_CHR
        | S_BLK
        | S_SOCK ->
            ([], [])
      in
      ( List.rev_append selected2 selected,
        List.rev_append skipped2 skipped ))
    ([], []) project_roots.scanning_roots

(*
   Select the scanning roots that are regular files or symlinks to regular
   files regardless of filters (gitignore, semgrepignore, --include,
   --exclude, ...).
   If they already occur in the list of skipped targets, they will be removed.

   Returns a third value: the subset of scanning roots that were force-added
   to 'selected_targets'. The caller is expected to re-apply size/minified
   limits to these, since those limits must not be bypassed even for
   explicit scanning roots.
*)
let force_select_scanning_roots
    ?(apply_includes_excludes_to_files = false)
    (project_roots : Project.roots)
    (selected_targets : Fppath.t list)
    (skipped_targets : Out.skipped_target list) :
    Fppath.t list * Out.skipped_target list * Fppath.t list =
  let regular_files_to_add =
    if not apply_includes_excludes_to_files then
      (* default behaviour: *)
      project_roots.scanning_roots
      |> List.filter (fun (sc_root : Fppath.t) ->
          UFile.is_reg ~follow_symlinks:true sc_root.fpath)
    else []
  in
  let skipped_targets =
    let regular_files_to_add =
      regular_files_to_add
      |> List_.map (fun x -> x.Fppath.fpath)
      |> Set_.of_list
    in
    skipped_targets
    |> List.filter (fun (skipped : Out.skipped_target) ->
           not (Set_.mem skipped.path regular_files_to_add))
  in
  let selected_targets = List.rev_append selected_targets regular_files_to_add in
  (selected_targets, skipped_targets, regular_files_to_add)

(*
   Filter files by size, optionally using committed sizes from
   'git ls-tree' to avoid per-file stat() syscalls.

   When git_sizes is provided (a hashtable from git-relative path to
   committed size in bytes), we look up each file's size in the table
   instead of calling stat().  Files that were modified since the last
   commit (present in the 'dirty' set) or missing from the table are
   handled via the stat()-based fallback (filter_size_and_minified),
   since their on-disk size may differ from the committed size.

   When git_sizes is None, all files go through the stat()-based path.
*)
let filter_size ?(git_sizes : (string, int) Hashtbl.t option)
    ?(dirty : string Set_.t option) max_target_bytes exclude_minified_files
    (files : Fppath.t list) : Fppath.t list * Out.skipped_target list =
  match git_sizes with
  | None -> filter_size_and_minified max_target_bytes exclude_minified_files files
  | Some size_map ->
      let dirty_set = Option.value ~default:Set_.empty dirty in
      (* Ppath uses project-relative paths with a leading '/' (e.g. "/src/foo.ml")
         while git ls-tree uses repo-relative paths without it (e.g. "src/foo.ml").
         Strip the leading '/' to match. *)
      let to_rel (ppath : Ppath.t) =
        let s = Ppath.to_string_fast ppath in
        if String.length s > 0 && Char.equal s.[0] '/' then
          String.sub s 1 (String.length s - 1)
        else s
      in
      (* Partition into files whose committed size we can trust (present in
         ls-tree output and not locally modified) vs files that need stat(). *)
      let git_known, need_stat =
        List.partition
          (fun (fp : Fppath.t) ->
            let rel = to_rel fp.ppath in
            Hashtbl.mem size_map rel && not (Set_.mem rel dirty_set))
          files
      in
      (* Size-check using the committed sizes from the hashtable *)
      let ok, skip =
        if max_target_bytes > 0 then
          List.partition_map
            (fun (fp : Fppath.t) ->
              let size = Hashtbl.find size_map (to_rel fp.ppath) in
              if size > max_target_bytes then
                Right
                  {
                    Out.path = fp.fpath;
                    reason = Too_big;
                    details =
                      Some
                        (spf "target file size exceeds %i bytes at %i bytes"
                           max_target_bytes size);
                    rule_id = None;
                  }
              else Left fp)
            git_known
        else (git_known, [])
      in
      (* Minification is content-based (reads first block), so no git
         acceleration is possible — apply it here to the git_known files
         that survived the size check, otherwise --exclude-minified-files
         would be silently skipped for tracked, unmodified files. *)
      let ok, skip_minified =
        if exclude_minified_files then
          Result_.partition
            (fun (fp : Fppath.t) ->
              Result.map (fun _ -> fp) (Skip_target.is_minified fp.fpath))
            ok
        else (ok, [])
      in
      (* Dirty or unknown files fall back to stat() *)
      let stat_ok, stat_skip =
        filter_size_and_minified max_target_bytes exclude_minified_files
          need_stat
      in
      (ok @ stat_ok, skip @ skip_minified @ stat_skip)

(*
   Two-stage filter for git-listed files:
   1. Apply gitignore/semgrepignore/include patterns (filter_paths)
   2. Apply size limits, using git ls-tree sizes when available (filter_size)

   The caller controls which patterns are active via the 'ign' filter —
   e.g. passing a filter with respect_gitignore=false skips .gitignore
   pattern matching but still evaluates .semgrepignore rules.
*)
let filter_git_files ign ?git_sizes ?dirty ~max_target_bytes
    ~exclude_minified_files (files : Fppath.t list) :
    Fppath.t list * Out.skipped_target list =
  let selected, skipped = filter_paths ign files in
  let ok, skipped_size =
    filter_size max_target_bytes exclude_minified_files ?git_sizes ?dirty
      selected
  in
  (ok, skipped @ skipped_size)

(*
   For git projects, we optimise target discovery by delegating expensive
   work to git:

   1. File enumeration: 'git ls-files' lists tracked and untracked files
      without us walking the filesystem.

   2. Gitignore evaluation: instead of loading every per-directory .gitignore
      file and matching patterns against each path segment (the previous
      bottleneck — ~5s for 40k files), we ask git which tracked files match
      gitignore patterns ('git ls-files --cached --ignored --exclude-standard').
      This lets us split tracked files into two sets:

        - tracked_clean: NOT matched by any .gitignore pattern.
          These go through the fast path: a filter that only loads
          per-directory .semgrepignore files (no .gitignore), since git
          already told us they don't match any .gitignore rule.

        - tracked_gitignored: matched by a .gitignore pattern despite being
          tracked (happens when a file was added before the gitignore rule).
          These go through the full filter (.gitignore + .semgrepignore) so
          that .semgrepignore '!' negation patterns can override the gitignore
          exclusion and re-include these files.

   3. Size checking: 'git ls-tree -r -l HEAD' provides committed file sizes
      in a single invocation. We use these sizes instead of stat() for files
      that haven't been modified since HEAD (not in 'git diff --name-only').

   For non-git projects, we fall back to the filesystem walker.
*)
let get_targets_for_project conf (project_roots : Project.roots) : Fppath.t targets =
  Log.debug (fun m -> m "Find_target.get_targets_for_project");
  let git_tracked = git_list_tracked_files project_roots in
  let git_untracked =
    git_list_untracked_files ~respect_gitignore:conf.respect_gitignore
      project_roots
  in
  let selected_targets, skipped_targets =
    match (git_tracked, git_untracked) with
    | Some tracked, Some untracked ->
        Log.debug (fun m ->
            m "target file candidates from git: tracked: %i, untracked: %i"
              (List.length tracked)
              (List.length untracked));
        (* Full filter: loads both .gitignore and .semgrepignore per directory.
           Used for gitignored tracked files (to honour '!' negation in
           .semgrepignore) and untracked files. *)
        let ign_full = setup_path_filters conf project_roots in
        (* Lightweight filter: only loads .semgrepignore per directory,
           skips .gitignore.  Safe for tracked files that git already
           confirmed don't match any gitignore pattern. *)
        let ign_no_gitignore =
          setup_path_filters
            { conf with respect_gitignore = false }
            project_roots
        in
        let project_root =
          Rpath.to_fpath (Rfpath.to_rpath project_roots.project.root)
        in
        (* Committed file sizes — avoids one stat() per unmodified file *)
        let git_sizes = Git_wrapper.ls_tree_sizes ~cwd:project_root () in
        (* Files modified since HEAD — these need stat() for accurate size *)
        let dirty = Git_wrapper.diff_names ~cwd:project_root () in
        let max_target_bytes = conf.max_target_bytes in
        let exclude_minified_files = conf.exclude_minified_files in
        (* Split tracked files by gitignore status *)
        let tracked_clean, tracked_gitignored =
          match git_list_tracked_gitignored_files project_roots with
          | Some ignored ->
              let ignored_set =
                ignored
                |> List_.map (fun (fp : Fppath.t) -> fp.fpath)
                |> Set_.of_list
              in
              List.partition
                (fun (fp : Fppath.t) -> not (Set_.mem fp.fpath ignored_set))
                tracked
          | None -> (tracked, [])
        in
        (* Fast path: semgrepignore only, no gitignore pattern matching *)
        let clean_ok, clean_skip =
          filter_git_files ign_no_gitignore ~git_sizes ~dirty
            ~max_target_bytes ~exclude_minified_files tracked_clean
        in
        (* Slow path: full filter for tracked-but-gitignored files *)
        let gi_ok, gi_skip =
          filter_git_files ign_full ~git_sizes ~dirty
            ~max_target_bytes ~exclude_minified_files tracked_gitignored
        in
        (* Untracked files: full filter, no git sizes (not in ls-tree) *)
        let untracked_ok, untracked_skip =
          filter_git_files ign_full
            ~max_target_bytes ~exclude_minified_files untracked
        in
        ( clean_ok @ gi_ok @ untracked_ok,
          clean_skip @ gi_skip @ untracked_skip )
    | None, _
    | _, None ->
        (* Non-git project: walk the filesystem and use stat() for sizes *)
        let selected, skipped =
          get_targets_from_filesystem conf project_roots
        in
        let ok, skipped_size =
          filter_size conf.max_target_bytes conf.exclude_minified_files
            selected
        in
        (ok, skipped @ skipped_size)
  in
  let is_git_repo = Option.is_some git_tracked in
  let selected_targets, skipped_targets, forced =
    force_select_scanning_roots
      ~apply_includes_excludes_to_files:conf.apply_includes_excludes_to_file_targets
      project_roots
      selected_targets
      skipped_targets
  in
  (* Re-apply size/minified limits to force-added scanning roots so that
     explicit CLI file arguments still honour --max-target-bytes and
     --exclude-minified-files. Only the forced subset is checked; the rest
     of 'selected_targets' has already been filtered above. *)
  let _, forced_skipped =
    filter_size_and_minified conf.max_target_bytes conf.exclude_minified_files
      forced
  in
  let forced_skipped_paths =
    forced_skipped
    |> List_.map (fun (s : Out.skipped_target) -> s.path)
    |> Set_.of_list
  in
  let selected_targets =
    List.filter
      (fun (fp : Fppath.t) -> not (Set_.mem fp.fpath forced_skipped_paths))
      selected_targets
  in
  let skipped_targets = List.rev_append forced_skipped skipped_targets in
  { selected = selected_targets; skipped = skipped_targets; git_repo = is_git_repo }

(* for semgrep query console *)
let clone_if_remote_project_root conf =
  match conf.force_project_root with
  | Some (Git_remote { url }) ->
      let cwd = Fpath.v (Unix.getcwd ()) in
      Log.info (fun m ->
          m "Sparse cloning %a into CWD: %a" Uri.pp url Fpath.pp cwd);
      (match Git_wrapper.sparse_shallow_filtered_checkout url (Fpath.v ".") with
      | Ok () -> ()
      | Error msg ->
          failwith
            (spf "Error while sparse cloning %s into %s: %s" (Uri.to_string url)
               (Fpath.to_string cwd) msg));
      Git_wrapper.checkout ();
      Log.info (fun m -> m "Sparse cloning done")
  | Some (Filesystem _)
  | None ->
      ()

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* TODO: The 'git_repo' field is needed to print out a warning to the
 * user, because some files in a git repo can be ignored. When multiple
 * roots are specified, we display this warning to the user when
 * at least one root is a git repo. Should we be more precise and
 * display which roots are git repos? Maybe in verbose mode? *)
let get_targets conf scanning_roots : Fppath.t targets =
  clone_if_remote_project_root conf;
  let raw =
    List.fold_left
      (fun acc root ->
        let r = get_targets_for_project conf root in
        { selected = List.rev_append r.selected acc.selected;
          skipped = List.rev_append r.skipped acc.skipped;
          git_repo = r.git_repo || acc.git_repo })
      { selected = []; skipped = []; git_repo = false }
      (group_scanning_roots_by_project conf scanning_roots)
  in
  (* Size/minified filtering is now done per-project in
     get_targets_for_project, so we only need to deduplicate and sort here. *)
  let selected = raw.selected |> List.sort_uniq Fppath.compare in
  let skipped =
    List.sort_uniq
      (fun (a : Out.skipped_target) (b : Out.skipped_target) -> Fpath.compare a.path b.path)
      raw.skipped
  in
  { selected; skipped; git_repo = raw.git_repo }
[@@profiling]

let get_target_fpaths conf scanning_roots =
  let v = get_targets conf scanning_roots in
  { v with selected = List_.map (fun { Fppath.fpath; _ } -> fpath) v.selected }
