(*
   Support for file tree filtering using the gitignore specification.
*)
open Gitignore
open Ppath.Operators

let create ?(higher_priority_levels = [])
    ~(gitignore_file_cache : Gitignore.gitignores_cache) () =
  {
    project_root = gitignore_file_cache.project_root;
    higher_priority_levels;
    gitignore_file_cache;
    dir_states = Hashtbl.create 1024;
  }

let is_selected (sel_events : Gitignore.selection_event list) =
  match sel_events with
  | [] -> false
  | Deselected _ :: _ -> false
  | Selected _ :: _ -> true

let result_of_selection_events sel_events =
  let status = if is_selected sel_events then Ignored else Not_ignored in
  (status, sel_events)

(*
   Scan successive precedence levels, stopping as soon as one level
   decides to gitignore (= select) the path.
*)
let rec fold_levels func sel_events levels =
  match levels with
  | [] -> sel_events
  | level :: levels ->
      let sel_events = func sel_events level in
      if is_selected sel_events then
        (* early exit, unlike List.fold_left *)
        sel_events
      else fold_levels func sel_events levels

(* The events of the patterns of [group] that [event_of] gives for a path,
   the last first, before [sel_events]. A group is one precedence level:
   its patterns are read in order, the ignore files from the project root
   down, and the last matching pattern decides. *)
let select_in_group (event_of : path_selector -> selection_event option)
    (sel_events : selection_event list) (group : level list) :
    selection_event list =
  List.fold_left
    (fun (acc : selection_event list) (level : level) ->
      List.fold_left
        (fun (acc : selection_event list) (path_selector : path_selector) ->
          match event_of path_selector with
          | Some (event : selection_event) -> event :: acc
          | None -> acc)
        acc level.patterns)
    sel_events group

(*
   Filter a path, assuming all its parents were deselected
   (= not gitignored).
*)
let select_file (path : Ppath.t) (sel_events : selection_event list)
    (group : level list) : selection_event list =
  select_in_group
    (fun (path_selector : path_selector) -> path_selector.matcher path)
    sel_events group
[@@profiling]

(* Check the directory [dir_path], given without its trailing slash, against
   the patterns of [group] in one pass: a pattern matches the directory with
   or without the trailing slash, which the directory-only patterns need. *)
let select_directory (dir_path : Ppath.t) (sel_events : selection_event list)
    (group : level list) : selection_event list =
  (* add trailing slash to match directory-only patterns *)
  let dir_path_with_slash = dir_path / "" in
  select_in_group
    (fun (path_selector : path_selector) ->
      match path_selector.matcher dir_path with
      | Some (event : selection_event) -> Some event
      | None -> path_selector.matcher dir_path_with_slash)
    sel_events group

(* Add [segment] to [parent_path] and check whether the partial path is
   gitignored. [last] is true for the path's final segment. Returns the
   events and levels to continue from. [priority_levels] are the first
   levels of [levels], the ones given to the filter: a path is decided by
   each of them in order, then by the levels of the ignore files together,
   as one gitignore, and the first of these groups that ignores it
   decides. *)
let enter ~(priority_levels : level list)
    (gitignore_file_cache : Gitignore.gitignores_cache) sel_events levels
    parent_path segment ~last =
  let levels =
    (* load the local ignore file *)
    match Gitignore_cache.load gitignore_file_cache parent_path with
    | Some additional_level -> levels @ [ additional_level ]
    | None -> levels
  in
  let file_path = parent_path / segment in
  let groups =
    List_.map (fun (level : level) -> [ level ]) priority_levels
    @ [ List_.drop (List.length priority_levels) levels ]
  in
  let select_group =
    if last then select_file file_path else select_directory file_path
  in
  (fold_levels select_group sel_events groups, levels)

(* [dir_states] memoises the state after each intermediate
   directory: the events and levels of a directory depend only on the
   directory, and every path below it repeats the same checks otherwise.
   A path with a trailing slash is a directory: its state is the one it has
   as the parent of a path. The last segment of any other path is never
   memoised. *)
let select_path
    ~(dir_states : (string, selection_event list * level list) Hashtbl.t)
    (gitignore_file_cache : Gitignore.gitignores_cache) sel_events levels
    relative_segments =
  let priority_levels = levels in
  (* State after the intermediate directories [dirs], memoised per prefix. *)
  let rec dir_state (dirs : string list) : selection_event list * level list =
    match List.rev dirs with
    | [] -> (sel_events, levels)
    | segment :: rev_parents -> (
        let parents = List.rev rev_parents in
        let parent_path = Ppath.add_segs Ppath.root parents in
        let key = Ppath.to_string_fast (parent_path / segment) in
        match Hashtbl.find_opt dir_states key with
        | Some state -> state
        | None ->
            let sel_events, levels = dir_state parents in
            let state =
              if is_selected sel_events then (sel_events, levels)
              else
                enter ~priority_levels gitignore_file_cache sel_events levels
                  parent_path segment ~last:false
            in
            Hashtbl.replace dir_states key state;
            state)
  in
  let dirs, last_segment =
    match List.rev relative_segments with
    | "" :: rev_dirs -> (List.rev rev_dirs, None)
    | segment :: rev_dirs -> (List.rev rev_dirs, Some segment)
    | [] -> ([], None)
  in
  let sel_events, levels = dir_state dirs in
  match last_segment with
  | Some segment when not (is_selected sel_events) ->
      fst
        (enter ~priority_levels gitignore_file_cache sel_events levels
           (Ppath.add_segs Ppath.root dirs) segment ~last:true)
  | Some _
  | None ->
      sel_events

(*
   Filter a path according to gitignore rules, requiring all the parent paths
   to be deselected (not gitignored).

   For a given path, we check the acceptability of all its ancestor folders.
   Each time we descend into a folder, we read the ignore files in
   that folder which add filters to the existing filters found earlier.
*)
let select t (full_git_path : Ppath.t) =
  let rel_segments = Ppath.relative_segments full_git_path in
  (* the levels given to the filter (built-in and command-line patterns)
     and the levels of the ignore files found along the way *)
  select_path ~dir_states:t.dir_states t.gitignore_file_cache []
    t.higher_priority_levels rel_segments
  |> result_of_selection_events
[@@profiling]
