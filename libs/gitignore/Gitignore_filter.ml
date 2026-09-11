(*
   Support for file tree filtering using the gitignore specification.
*)
open Gitignore
open Ppath.Operators

let create ?(higher_priority_levels = []) ?(lower_priority_levels = [])
    ~(gitignore_file_cache : Gitignore.gitignores_cache) () =
  {
    project_root = gitignore_file_cache.project_root;
    higher_priority_levels;
    gitignore_file_cache;
    lower_priority_levels;
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

(*
   Filter a path, assuming all its parents were deselected
   (= not gitignored).
*)
let select_one acc levels path : Gitignore.selection_event list =
  fold_levels
    (fun acc (level : Gitignore.level) ->
      List.fold_left
        (fun acc (path_selector : Gitignore.path_selector) ->
          match path_selector.matcher path with
          | Some ((Selected _ | Deselected _) as x) -> x :: acc
          | None -> acc)
        acc level.patterns)
    acc levels
[@@profiling]

(* Add [segment] to [parent_path] and check whether the partial path is
   gitignored. [last] is true for the path's final segment, and for a
   directory queried with a trailing slash, whose own directory-only check
   is the empty segment that follows. Returns the events and levels to
   continue from. *)
let enter opt_gitignore_file_cache sel_events levels parent_path segment
    ~last =
  let levels =
    match opt_gitignore_file_cache with
    | Some cache -> (
        (* load local gitignore file *)
        match Gitignore_cache.load cache parent_path with
        | Some additional_level -> levels @ [ additional_level ]
        | None -> levels)
    | None -> levels
  in
  let file_path = parent_path / segment in
  let sel_events = select_one sel_events levels file_path in
  let deselected =
    match sel_events with
    | Deselected _ :: _ -> true
    | _ -> false
  in
  (* stop here, don't go deeper as per gitignore spec *)
  if is_selected sel_events || last then (sel_events, levels)
  (* If a path has been deselected, don't test for dir-only patterns *)
  else if deselected then (sel_events, levels)
  else
    (* add trailing slash to match directory-only patterns *)
    let dir_path = file_path / "" in
    (select_one sel_events levels dir_path, levels)

(* [dir_states], when given, memoises the state after each intermediate
   directory: the events and levels of a directory depend only on the
   directory, and every path below it repeats the same checks otherwise.
   The last segment, and the directory-only check of a trailing slash, are
   never memoised. *)
let select_path ?dir_states opt_gitignore_file_cache sel_events levels
    relative_segments =
  let rec loop sel_events levels parent_path segments =
    match segments with
    | [] -> sel_events
    | segment :: segments ->
        let last =
          match segments with
          | []
          | [ "" ] ->
              true
          | _ :: _ -> false
        in
        let sel_events, levels =
          enter opt_gitignore_file_cache sel_events levels parent_path segment
            ~last
        in
        if is_selected sel_events then sel_events
        else loop sel_events levels (parent_path / segment) segments
  in
  (* State after the intermediate directories [dirs], memoised per prefix. *)
  let rec dir_state (dirs : string list) : selection_event list * level list =
    match List.rev dirs with
    | [] -> (sel_events, levels)
    | segment :: rev_parents -> (
        let parents = List.rev rev_parents in
        let parent_path = Ppath.add_segs Ppath.root parents in
        let key = Ppath.to_string_fast (parent_path / segment) in
        match Option.bind dir_states (fun tbl -> Hashtbl.find_opt tbl key) with
        | Some state -> state
        | None ->
            let sel_events, levels = dir_state parents in
            let state =
              if is_selected sel_events then (sel_events, levels)
              else
                enter opt_gitignore_file_cache sel_events levels parent_path
                  segment ~last:false
            in
            Option.iter (fun tbl -> Hashtbl.replace tbl key state) dir_states;
            state)
  in
  let dirs, tail =
    match List.rev relative_segments with
    | "" :: segment :: rev_dirs -> (List.rev rev_dirs, [ segment; "" ])
    | segment :: rev_dirs -> (List.rev rev_dirs, [ segment ])
    | [] -> ([], [])
  in
  let sel_events, levels = dir_state dirs in
  if is_selected sel_events then sel_events
  else loop sel_events levels (Ppath.add_segs Ppath.root dirs) tail

(*
   Filter a path according to gitignore rules, requiring all the parent paths
   to be deselected (not gitignored).

   For a given path, we check the acceptability of all its ancestor folders.
   Each time we descend into a folder, we read the .gitignore files in
   that folder which add filters to the existing filters found earlier.
*)
let select t (full_git_path : Ppath.t) =
  let sel_events = [] in
  let rel_segments = Ppath.relative_segments full_git_path in
  (* higher levels (command-line)
     and middle levels (gitignore files discovered along the way) *)
  let sel_events =
    select_path ~dir_states:t.dir_states (Some t.gitignore_file_cache)
      sel_events t.higher_priority_levels rel_segments
  in
  if is_selected sel_events then result_of_selection_events sel_events
  else
    (* lower levels (other sources of gitignore patterns) *)
    let sel_events =
      select_path None sel_events t.lower_priority_levels rel_segments
    in
    result_of_selection_events sel_events
[@@profiling]
