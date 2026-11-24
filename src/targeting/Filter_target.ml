(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Filter target candidates.

   Filtering each (rule, target) pair can become problematic since the number
   of such pairs is O(number of targets * number of rules).
   TODO? This is why we should cache the results of this step.
   This allows reducing the number of rules to the number of different
   languages and patterns used by the rules.
   update: there used to be such an opti, but Pad removed it, because
   it was not clear that was the actual bottleneck. Gitignore
   seems currently to be the thing to optimize.

   Partially translated from target_manager.py
*)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

(* Used by Core_runner.split_jobs_by_language() *)
let filter_target_for_xlang (xlang : Xlang.t) (path : Fpath.t) : bool =
  match xlang with
  | L (lang, langs) ->
      (* ok if the file appears to be in one of rule's languages *)
      lang :: langs
      |> List.exists (fun lang -> Guess_lang.inspect_file_p lang path)
  | LRegex
  | LSpacegrep
  | LAliengrep ->
      true

(*
   Tentative plan:

   - write tests specifically to test paths.include and paths.exclude
     in different contexts (absolute target paths, relative target paths,
     git project, non-git project);
   - use the same rules as for gitignore (forget about Python's wcmatch
     that we're not going to reimplement completely):
     * a slash at the beginning or in the middle of the path makes it
       anchored;
     * reuse what we implemented for gitignore and semgrepignore.
   - update the semgrep documentation accordingly i.e. mention that the
     standard is gitignore/semgrepignore, not wcmatch anymore.

   Details:

   Used by [rule_applies].
   See also https://semgrep.dev/docs/writing-rules/rule-syntax/#paths
   TODO: according to the doc,
   "Paths [in include: and exclude: patterns] are relative to the root
   directory of the scanned project" but it seems pysemgrep does not honor
   this thing because adding an exclude such as "metachecking/*" will
   filter code even in subdirs calls metachecking. The glob is not anchored,
   even when it contains a '/'.
   LESS: there used to be a time where semgrep-core was handling the
   include/exclude too. Can we find back this code?
*)
let filter_paths (paths : Rule.paths) (path_str : string) : bool =
  (* A glob matches at any depth: see [Rule.glob_of_pattern]. *)
  let match_glob (glob : Rule.glob) : bool =
    glob.matchers
    |> List.exists (fun (matcher : Glob.Match.compiled_pattern) ->
           Glob.Match.run matcher path_str)
  in

  let { Rule.require; exclude } = paths in
  let is_excluded = exclude |> List.exists match_glob in
  (* from the doc: "when mixing inclusion and exclusion filters,
   * the exclusion ones take precedence."
   *)
  if is_excluded then false
  else
    let is_required =
      match require with
      (* no require patterns means no constraints *)
      | [] -> true
      | _xs -> require |> List.exists match_glob
    in
    is_required
[@@profiling]

(* Note that filtering is applied on the basis of the target's origin, not the
 * target's "file". This is because filtering should apply to the user's
 * perception of the file, not whatever we may transform it to internally.
 *
 * For instance, the "file" of a target may be a tempfile which has no meaning,
 * and is essentially randomly generated. `paths:` filtering shouldn't apply to
 * this!
 *
 * Note also that `paths:` filters are relative to the root of a project [0]:
 * a file found under a directory target is matched by its path relative to
 * the project root, whatever form the directory target was given in; a file
 * given as a target is matched by its origin.
 *
 * [0]: <https://semgrep.dev/docs/writing-rules/rule-syntax/#paths>
 *)
let matched_paths (target_proj_path : Target_and_root.target_proj_path)
    (origin : Origin.t) : string list =
  match target_proj_path with
  | In_project ppath -> [ String.concat "/" (Ppath.relative_segments ppath) ]
  | Not_in_project -> (
      match origin with
      | File path -> [ Fpath.to_string path ]
      | GitBlob { paths = target_paths; _ } ->
          List_.map
            (fun ((_, path_at_commit) : _ * Fpath.t) ->
              Fpath.to_string path_at_commit)
            target_paths)

let rule_applies (paths : Rule.paths option) (matched_paths : string list) :
    bool =
  match paths with
  | Some paths -> List.exists (filter_paths paths) matched_paths
  | None -> true
