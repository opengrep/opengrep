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

type project_root = Filesystem of Rfpath.t [@@deriving show]

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

(* What a differential scan diffs against. The constructors record what the
 * producer knows about the ref:
 * - Merge_base_of: any git rev; Diff_scan first computes
 *   merge-base(HEAD, rev). This is 'opengrep scan --baseline-commit'.
 * - Commit: a resolved commit that already is the wanted base, diffed
 *   against directly. 'opengrep ci' produces these from its CI-provider
 *   merge-base machinery (python: BaselineHandler is_mergebase).
 * - Rev: a symbolic rev used directly as the base, no merge-base
 *   computation. 'opengrep ci --baseline-commit main' is this.
 *)
type baseline_ref =
  | Merge_base_of of string
  | Commit of Digestif.SHA1.t
  | Rev of string
[@@deriving show]

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
  baseline_commit : baseline_ref option;
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

let skipped_of_ignored (selection_events : Gitignore.selection_event list)
    (fpath : Fpath.t) : Out.skipped_target =
  Log.debug (fun m ->
      m "Ignoring path %s:\n%s" !!fpath
        (Gitignore.show_selection_events selection_events));
  let reason = get_reason_for_exclusion selection_events in
  { Out.path = fpath; reason; details = None; rule_id = None }

let ignore_path selection_events fpath =
  Skip (skipped_of_ignored selection_events fpath)

(* the path without its last n segments *)
let rec parent_n (n : int) (path : Fpath.t) : Fpath.t =
  if n <= 0 then Fpath.rem_empty_seg path
  else parent_n (n - 1) (Fpath.parent path)

let apply_include_filter status selection_events include_filter ppath =
  match status with
  | Gitignore.Ignored -> (status, selection_events)
  | Gitignore.Not_ignored -> (
      match include_filter with
      | None -> (status, selection_events)
      | Some include_filter -> Include_filter.select include_filter ppath)

(* Never scanned and never reported, as pysemgrep's PATHS_ALWAYS_SKIPPED
   (target_manager.py), which held '**/.git' and '**/.git/**': the folder
   git keeps its data in, and everything under it, whatever the ignore
   files of the project say. *)
let is_always_skipped (ppath : Ppath.t) : bool =
  Ppath.segments ppath |> List.exists (String.equal ".git")

(* Note that include_filter applies only to the paths of regular files. They're
 * applied last, after the exclude/gitignore/semgrepignore filters.
 *)
let filter_path ?(kind : Unix.file_kind option) (ign : Gitignore.filter)
    (include_filter : Include_filter.t option) (fppath : Fppath.t) :
    filter_result =
  let { fpath; ppath } : Fppath.t = fppath in
  if is_always_skipped ppath then Ignore_silently
  else
  let status, selection_events = Gitignore_filter.select ign ppath in
  match status with
  | Ignored -> ignore_path selection_events fpath
  | Not_ignored -> (
      (* TODO: check read permission too? *)
      (* [kind] is what a caller that has already stat'ed the path knows: a
         scanning root is followed with Unix.stat, so it is filtered as the
         file it leads to rather than skipped as a symlink. *)
      match
        match kind with
        | Some (kind : Unix.file_kind) -> kind
        | None -> (Unix.lstat !!fpath).st_kind
      with
      (* skipping symlinks *)
      | S_LNK -> Ignore_silently
      | S_REG -> (
          let status, selection_events =
            apply_include_filter status selection_events include_filter ppath
          in
          match status with
          | Ignored -> ignore_path selection_events fpath
          | Not_ignored -> Keep)
      | S_DIR -> (
          (* A pattern ending with a slash, like 'sub/', applies to
             directories only and matches a path ending with a slash only.
             Tested with the slash, the directory is skipped and reported
             once; without it, it would be entered and each file reported. *)
          let status, selection_events =
            Gitignore_filter.select ign (Ppath.add_seg ppath "")
          in
          match status with
          | Ignored -> ignore_path selection_events fpath
          | Not_ignored -> Dir)
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
   obtained with 'git ls-files'. A strong postcondition is that the
   paths returned must correspond to existing regular files!
*)
(* An ignored directory is reported once, with the first file met under it
   from a scanning root at or above it. *)
type dir_status =
  | Dir_not_ignored
  | Dir_ignored of Gitignore.selection_event list
  | Dir_reported

(* the ppath of a scanning root without the trailing slash marker of a
   directory, as the ancestors of a file are built *)
let root_ppath (root : Fppath.t) : Ppath.t =
  Ppath.create
    ("" :: List.filter (fun (seg : string) -> not (String.equal seg ""))
             (Ppath.relative_segments root.ppath))

let filter_paths
    ((ign, include_filter) : Gitignore.filter * Include_filter.t option)
    (scanning_roots : Fppath.t list) (target_files : Fppath.t list) :
    Fppath.t list * Out.skipped_target list =
  let (selected_paths : Fppath.t list ref) = ref [] in
  let (skipped : Out.skipped_target list ref) = ref [] in
  let add path = Stack_.push path selected_paths in
  let skip target = Stack_.push target skipped in
  let (dirs : (Ppath.t, dir_status) Hashtbl.t) = Hashtbl.create 1024 in
  let (roots : (Ppath.t, unit) Hashtbl.t) = Hashtbl.create 16 in
  scanning_roots
  |> List.iter (fun (root : Fppath.t) ->
         Hashtbl.replace roots (root_ppath root) ());
  (* The files under an ignored directory are dropped without being tested
     and the directory is reported once, as pysemgrep did.

     The fpath of a file is a prefix, the scanning root as typed or as git
     reports it, followed by its segments below the root, the same as in
     its ppath. Dropping segments below the root from the fpath gives the
     fpath of an ancestor. Nothing is known of the prefix, which may go
     through a symlink, so an ignored ancestor above the root is not
     reported here: the file is, by the filter below, as pysemgrep did. *)
  let under_ignored_dir (fppath : Fppath.t) : bool =
    (* the ancestors from the project root down; [below] is the number of
       segments of the file under the ancestor being looked at *)
    let rec loop (dir_ppath : Ppath.t) (in_root : bool)
        (remaining : string list) (below : int) : bool =
      match remaining with
      | []
      | [ _ ] ->
          false
      | segment :: remaining ->
          let dir_ppath = Ppath.add_seg dir_ppath segment in
          let below = below - 1 in
          let in_root = in_root || Hashtbl.mem roots dir_ppath in
          let status =
            match Hashtbl.find_opt dirs dir_ppath with
            | Some status -> status
            | None ->
                (* the trailing slash makes directory-only patterns apply *)
                let status, selection_events =
                  Gitignore_filter.select ign (Ppath.add_seg dir_ppath "")
                in
                let status =
                  match status with
                  | Gitignore.Ignored -> Dir_ignored selection_events
                  | Gitignore.Not_ignored -> Dir_not_ignored
                in
                Hashtbl.replace dirs dir_ppath status;
                status
          in
          match (status, in_root) with
          | Dir_not_ignored, _ -> loop dir_ppath in_root remaining below
          | (Dir_ignored _ | Dir_reported), false -> false
          | Dir_ignored selection_events, true ->
              skip
                (skipped_of_ignored selection_events
                   (parent_n below fppath.fpath));
              Hashtbl.replace dirs dir_ppath Dir_reported;
              true
          | Dir_reported, true -> true
    in
    let segments = Ppath.relative_segments fppath.ppath in
    loop Ppath.root (Hashtbl.mem roots Ppath.root) segments
      (List.length segments)
  in
  (* A scanning root that is a symlink to a file is the file the user
     named: git lists it like any other, but it is followed here as it is
     when the walk starts from it, instead of being dropped for being a
     symlink. *)
  let kind_of_scanning_root (fppath : Fppath.t) : Unix.file_kind option =
    if
      Hashtbl.mem roots (root_ppath fppath)
      && UFile.is_reg ~follow_symlinks:true fppath.fpath
    then Some Unix.S_REG
    else None
  in
  target_files
  |> List.iter (fun fppath ->
         if under_ignored_dir fppath then ()
         else
         match
           filter_path ?kind:(kind_of_scanning_root fppath) ign include_filter
             fppath
         with
         | Keep -> (
             (* This section is similar to what we have in
                'walk_skip_and_collect' but the rest is sufficiently different
                that sharing code makes things complicated
                (e.g. no dir access filtering for git targets) *)
             match Skip_target.filter_file_access_permissions fppath.fpath with
             | Ok _path -> add fppath
             | Error skipped -> skip skipped)
         (* shouldn't happen if we work on the output of 'git ls-files *)
         | Dir -> ()
         | Skip x -> skip x
         | Ignore_silently ->
             Log.debug (fun m -> m "ignore silently: %s" !!(fppath.fpath)));
  (!selected_paths, !skipped)

(* Note: throughout this file we use List.rev_append instead of (@) for
 * concatenating file lists, since it is tail-recursive. The order does not
 * matter because we sort and deduplicate at the end in get_targets. *)
(* [keep_any_extension] holds for the files the default extension
   exclusions must not apply to, [keep_any_size] for those the size limit
   must not apply to. *)
let filter_extension_size_and_minified ~(keep_any_extension : Fppath.t -> bool)
    ~(keep_any_size : Fppath.t -> bool) max_target_bytes exclude_minified_files
    paths =
  (* by extension first, as it reads nothing *)
  let selected_fppaths, skipped_extension =
    Result_.partition
      (fun (fppath : Fppath.t) ->
        if keep_any_extension fppath then Ok fppath
        else
          Result.map
            (fun _ -> fppath)
            (Skip_target.has_excluded_extension fppath.fpath))
      paths
  in
  let selected_fppaths, skipped_size =
    Result_.partition
      (fun (fppath : Fppath.t) ->
        if keep_any_size fppath then Ok fppath
        else
          Result.map
            (fun _ -> fppath)
            (Skip_target.is_big max_target_bytes fppath.fpath))
      selected_fppaths
  in
  let selected_fppaths, skipped_minified =
    if exclude_minified_files then
      Result_.partition
        (fun (fppath : Fppath.t) ->
          Result.map (fun _ -> fppath) (Skip_target.is_minified fppath.fpath))
        selected_fppaths
    else (selected_fppaths, [])
  in
  Log.debug (fun m ->
      m "skipped_extension: %d" (List.length skipped_extension));
  Log.debug (fun m -> m "skipped_size: %d" (List.length skipped_size));
  Log.debug (fun m -> m "skipped_minified: %d" (List.length skipped_minified));
  ( selected_fppaths,
    List.rev_append skipped_extension
      (List.rev_append skipped_size skipped_minified) )

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
      (* Canonicalise cwd on Windows so its spelling (case, 8.3 short names)
         agrees with git's canonical paths; otherwise relativizing against it
         can emit a '..' walk-up. No-op on case-sensitive filesystems. *)
      let cwd = Rpath.canonical_if_win (Fpath.v (Sys.getcwd ())) in
      Some
        (project_roots.scanning_roots
        |> List.concat_map (fun (sc_root : Fppath.t) ->
               if UFile.is_reg ~follow_symlinks:true sc_root.fpath then
                 (* A single-file scanning root is returned as-is: it has no
                    descendants to relativize, so it never produced the '..'
                    walk-up. Canonicalising it here would only desync its fpath
                    from its ppath and double-report the file. *)
                 [ sc_root ]
               else if UFile.is_dir ~follow_symlinks:true sc_root.fpath then (
                 Log.info (fun m ->
                     m "List git files for scanning root %s"
                       (Fppath.show sc_root));
                 let project_root = Rfpath.to_rpath project.root in
                 (* The path prefix we want for all the target file paths that
                    we return: the scanning root exactly as the user typed it,
                    like pyopengrep. *)
                 let orig_scanning_root_path = sc_root.fpath in
                 (* On Windows the typed root can differ from git's canonical
                    paths in case or via 8.3 short names; relativizing it
                    against git's canonical targets would emit a '..' walk-up.
                    Canonicalise a copy for the relativize and the git lookup
                    below so they stay clean, while still prefixing the typed
                    root onto the result. Canonicalising also resolves the
                    symlinks of the typed root: git rejects a path that leads
                    outside the directory it runs in. *)
                 let canon_scanning_root_path =
                   Rpath.canonical_exn sc_root.fpath
                 in
                 (* We can't just cd into the scanning root to obtain paths
                    relative to it because the scanning root may be a regular
                    file. It could also be the root of the file system, so we
                    also can't cd into its parent.
                    This is why we stay in the same cwd and only later convert
                    the resulting paths to be relative to the scanning root. *)
                 Git_wrapper.ls_files_relative ~exclude_standard
                   ~kinds:file_kinds ~project_root
                   [ canon_scanning_root_path ]
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
                        match
                          (* Both absolute and normalised: relative to
                             cwd, the typed root and the path listed by
                             git can spell one directory two ways, '../sub'
                             and '.', and never match. *)
                          Fpath.relativize ~root:canon_scanning_root_path
                            (Fpath.normalize
                               (cwd // target_relative_to_cwd_or_absolute))
                        with
                        | Some target_relative_to_scan_root ->
                            (* The segments below the root extend both the
                               root as typed and its ppath, so that the
                               file and its root are in one frame even
                               when the root goes through a symlink. *)
                            ({
                               fpath =
                                 Fpath_.append_no_dot orig_scanning_root_path
                                   target_relative_to_scan_root;
                               ppath =
                                 Ppath.append_fpath sc_root.ppath
                                   target_relative_to_scan_root;
                             }
                              : Fppath.t)
                        | None ->
                            (* cannot happen with a correct root: the scan
                               aborts rather than report a wrong path *)
                            failwith
                              (spf
                                 "internal error: git listed a path outside \
                                  the scanning root: root=%s, cwd=%s, \
                                  path=%s"
                                 !!canon_scanning_root_path !!cwd
                                 !!target_relative_to_cwd_or_absolute)))
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
  (* Outside any VCS, the working directory is the project when the root
     is under it, so that its ignore files apply, as the Python wrapper
     reads the .semgrepignore of the working directory. *)
  let fallback_root : Rfpath.t option =
    let cwd = Rpath.getcwd () in
    match Rpath.of_fpath scanning_root_fpath with
    | Ok root when Fpath.is_prefix (Rpath.to_fpath cwd) (Rpath.to_fpath root)
      ->
        Some (Rfpath.of_fpath_exn (Rpath.to_fpath cwd))
    | Ok _
    | Error _ ->
        None
  in
  let kind, scanning_root_info =
    Project.find_any_project_root ~fallback_root ~force_novcs ~force_root
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
        Some Project.{ kind = Project.No_VCS_project; root = proj_root }
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
  (* The ignore file of the working directory applies wherever the scanning
     roots are, as it did for the Python wrapper. When the working
     directory is the project root it is already read as the project's own,
     with its patterns anchored there. *)
  let working_directory : Fpath.t option =
    let cwd = Rpath.to_fpath (Rpath.getcwd ()) in
    if Fpath.equal cwd (Rpath.to_fpath (Rfpath.to_rpath project_root)) then None
    else Some cwd
  in
  let semgrepignore_filter =
    Semgrepignore.create ~cli_patterns:conf.exclude ?working_directory
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

(* Work from a list of target paths obtained with git *)
let filter_targets
    (filters : Gitignore.filter * Include_filter.t option)
    (project_roots : Project.roots) (all_files : Fppath.t list) =
  filter_paths filters project_roots.scanning_roots all_files

let get_targets_from_filesystem
    ((ign, include_filter) : Gitignore.filter * Include_filter.t option)
    conf (project_roots : Project.roots) =
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
          let keep_if_readable () =
            match Skip_target.filter_file_access_permissions scan_root.fpath with
            | Ok _path -> ([ scan_root ], [])
            | Error skipped -> ([], [ skipped ])
          in
          if not conf.apply_includes_excludes_to_file_targets then
            (* a file the user named is taken whatever the filters, but it
               still has to be readable *)
            keep_if_readable ()
          else
            (* '--force-exclude': the filters apply to the file the user
               named as they do to any other file. Walking it is not an
               option: the walk tests the read and execute permissions of
               what it starts from, which a regular file rarely has. The
               kind comes from the Unix.stat above, so a root that is a
               symlink to a file is filtered as that file and not dropped
               for being a symlink. *)
            (match filter_path ~kind:Unix.S_REG ign include_filter scan_root with
            | Keep -> keep_if_readable ()
            | Skip skipped -> ([], [ skipped ])
            | Dir
            | Ignore_silently ->
                ([], []))
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
   A file the scan cannot read is not selected: it is reported as skipped,
   and the caller turns that into an error of the run.
*)
let force_select_scanning_roots
    ?(apply_includes_excludes_to_files = false)
    (project_roots : Project.roots)
    (selected_targets : Fppath.t list)
    (skipped_targets : Out.skipped_target list) :
    Fppath.t list * Out.skipped_target list =
  let regular_files_to_add, unreadable_files =
    if not apply_includes_excludes_to_files then
      (* default behaviour: *)
      project_roots.scanning_roots
      |> List.filter (fun (sc_root : Fppath.t) ->
          UFile.is_reg ~follow_symlinks:true sc_root.fpath)
      |> List.partition_map (fun (sc_root : Fppath.t) ->
             match Skip_target.filter_file_access_permissions sc_root.fpath with
             | Ok _path -> Left sc_root
             | Error skipped -> Right skipped)
    else ([], [])
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
  (selected_targets, List.rev_append unreadable_files skipped_targets)

(*
   Target files are identified by following these steps:

   1. A list of folders or files are specified explicitly on the command line.
      These are referred to as "explicit" targets and they should not
      be filtered out even if they match some exclusion patterns.
      This is the input of the 'get_targets' function.
   2. If the project is a git project, use 'git ls-files' or
      equivalent to expand the scanning roots into a list of files.
      This list may include files that would be excluded by the gitignore
      mechanism but are nonetheless being tracked by git (it happens).
   3. The scanning roots from step (1) are expanded using our own
      semgrepignore mechanism. This allows the inclusion of additional
      files that are not under git control because .semgrepignore
      files allows de-exclusion/re-inclusion patterns such as e.g.
      '!build/'.
      Typically, the sets of files produced by (2) and (3) overlap vastly.
   4. Take the union of (2) and (3).
*)
(* A directory given as a scanning root that the ignore rules exclude is
   not listed at all. The walk tests the entries of a directory, never the
   directory it starts from, and git would list everything under it. *)
let ignored_scanning_root (ign : Gitignore.filter) (root : Fppath.t) :
    Out.skipped_target option =
  if not (UFile.is_dir ~follow_symlinks:true root.fpath) then None
  else
    (* the trailing slash makes directory-only patterns apply *)
    let status, selection_events =
      Gitignore_filter.select ign (Ppath.add_seg root.ppath "")
    in
    match status with
    | Not_ignored -> None
    | Ignored ->
        Log.warn (fun m ->
            m "the scanning root %s is skipped, nothing under it is scanned:\n%s"
              !!(root.fpath)
              (Gitignore.show_selection_events selection_events));
        Some (skipped_of_ignored selection_events root.fpath)

let get_targets_for_project conf (project_roots : Project.roots) : Fppath.t targets =
  Log.debug (fun m -> m "Find_target.get_targets_for_project");
  let ((ign, _) as filters) = setup_path_filters conf project_roots in
  let skipped_roots, scanning_roots =
    project_roots.scanning_roots
    |> List.partition_map (fun (root : Fppath.t) ->
           match ignored_scanning_root ign root with
           | Some skipped -> Left skipped
           | None -> Right root)
  in
  let project_roots = { project_roots with scanning_roots } in
  (* Obtain the list of files from git if possible because it does it
     faster than what we can do by scanning the filesystem: *)
  let git_tracked = git_list_tracked_files project_roots in
  let git_untracked =
    git_list_untracked_files ~respect_gitignore:conf.respect_gitignore
      project_roots
  in
  let selected_targets, skipped_targets =
    match (git_tracked, git_untracked) with
    (* Git only *)
    | Some tracked, Some untracked ->
        Log.debug (fun m ->
            m "target file candidates from git: tracked: %i, untracked: %i"
              (List.length tracked)
              (List.length untracked));
        filter_targets filters project_roots (List.rev_append tracked untracked)
    (* Non-Git projects *)
    | None, _
    | _, None ->
        get_targets_from_filesystem filters conf project_roots
  in
  let skipped_targets = List.rev_append skipped_roots skipped_targets in
  let is_git_repo = Option.is_some git_tracked in
  let selected_targets, skipped_targets =
    force_select_scanning_roots
      ~apply_includes_excludes_to_files:conf.apply_includes_excludes_to_file_targets
      project_roots
      selected_targets
      skipped_targets
  in
  { selected = selected_targets; skipped = skipped_targets; git_repo = is_git_repo }

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* The files the user named on the command line. The default exclusions by
   extension and the size limit apply to what walking a directory root
   turns up, not to these: pysemgrep added them back after filtering
   (bypass_includes_excludes_for_files of target_manager.py). *)
let explicit_file_targets (scanning_roots : Scanning_root.t list) :
    Fpath.t Set_.t =
  scanning_roots
  |> List_.map Scanning_root.to_fpath
  |> List.filter (fun (fpath : Fpath.t) ->
         UFile.is_reg ~follow_symlinks:true fpath)
  |> Set_.of_list

(* TODO: The 'git_repo' field is needed to print out a warning to the
 * user, because some files in a git repo can be ignored. When multiple
 * roots are specified, we display this warning to the user when
 * at least one root is a git repo. Should we be more precise and
 * display which roots are git repos? Maybe in verbose mode? *)
let get_targets conf scanning_roots : Fppath.t targets =
  let explicit_files : Fpath.t Set_.t =
    if conf.apply_includes_excludes_to_file_targets then Set_.empty
    else explicit_file_targets scanning_roots
  in
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
  let is_explicit_file (fppath : Fppath.t) : bool =
    Set_.mem fppath.fpath explicit_files
  in
  let selected, skipped_files =
    raw.selected
    |> List.sort_uniq Fppath.compare
    |> filter_extension_size_and_minified
         ~keep_any_extension:(fun (fppath : Fppath.t) ->
           (* a file kept by a '--include' pattern of the user was asked
              for as much as one named on the command line *)
           Option.is_some conf.include_ || is_explicit_file fppath)
         ~keep_any_size:is_explicit_file conf.max_target_bytes
         conf.exclude_minified_files
  in
  let skipped =
    List.sort_uniq
      (fun (a : Out.skipped_target) (b : Out.skipped_target) -> Fpath.compare a.path b.path)
      (List.rev_append skipped_files raw.skipped)
  in
  { selected; skipped; git_repo = raw.git_repo }
[@@profiling]

let get_target_fpaths conf scanning_roots =
  let v = get_targets conf scanning_roots in
  { v with selected = List_.map (fun { Fppath.fpath; _ } -> fpath) v.selected }
