open Gitignore
module M = Glob.Match

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let read_lines_from_string =
  (*
     - eliminate trailing spaces
     - support Windows line endings regardless of current platform
  *)
  let sep = Pcre2_.regexp " *\r?\n" in
  fun str ->
    match Pcre2_.split ~rex:sep str with
    | Ok res -> res
    | Error err ->
        (* not sure why it would happen so we let it fail *)
        raise (Pcre2.Error err)

let is_ignored_line =
  let rex = Pcre2_.regexp "^(?:[ \t]$|#.*)$" in
  fun str -> Pcre2_.pmatch_noerr ~rex str

(* semgrep-legacy (deprecated)

   Try to parse a line of input as a ':include' instruction
*)
let parse_maybe_include_line =
  let rex = Pcre2_.regexp {|^[ \t]*:include[ \t]*([^ \t]*)[ \t]*$|} in
  let parse ~orig_semgrepignore_path line : Fpath.t option =
    match Pcre2_.exec ~rex line with
    | Ok (Some res) -> (
        match Pcre2_.get_substring rex res 1 with
        | Ok (Some path) -> (
            match Fpath.of_string path with
            | Ok path ->
                (* nosemgrep: no-logs-in-library *)
                Logs.warn (fun m ->
                    m
                      "Deprecated include directive '%s' in semgrepignore file \
                       '%s'"
                      line
                      (Fpath.to_string orig_semgrepignore_path));
                Some path
            | Error _ -> None)
        | Ok None
        | Error _ ->
            None)
    | Ok None
    | Error _ ->
        None
  in
  parse

let rec contains_nontrailing_slash (pat : Glob.Pattern.t) =
  match pat with
  | Segment [] :: pat -> contains_nontrailing_slash pat
  | [] -> false
  | _nonempty :: (* trailing slash *) [ Segment [] ]
  | [ _nonempty ] ->
      false
  | _nonempty1 :: _nonempty2 :: _ -> true

(* anchored pattern = relative to the work directory only, as opposed to
   being relative to any folder in the subtree. *)
let is_anchored_pattern (pat : Glob.Pattern.t) =
  match pat with
  (* /... *)
  | Segment [] :: _ -> true
  (* **/ *)
  | Any_subpath :: _ -> true
  | pat -> contains_nontrailing_slash pat

(*
   Parse and compile a gitignore pattern.

   The resulting matcher matches a git path, i.e. a file path relative
   to the git project root.

   anchor: path of the gitignore file's directory relative to the git project
   root. For example, if the gitignore path is '/foo/.gitignore',
   then the pattern '/bar' will be expanded into '/foo/bar'.
   However a non-anchored pattern such as '*.c' will be expanded into
   '/foo/**/*.c'.
*)
let parse_pattern ~source ~anchor str : M.compiled_pattern =
  let pat = Glob.Parse.parse_string str in
  let absolute_pattern =
    if is_anchored_pattern pat then Glob.Pattern.append anchor pat
    else Glob.Pattern.append anchor (Any_subpath :: pat)
  in
  M.compile ~source absolute_pattern

(*
   A '--include' or '--exclude' pattern from the command line matches
   anywhere in a path, where a gitignore pattern containing a slash is
   anchored at the folder of its file. pysemgrep expanded such a pattern
   'foo/bar' into the two wcmatch patterns '**/foo/bar' and '**/foo/bar/**'
   (TargetManager.preprocess_path_patterns of target_manager.py); the
   second one is what selects the files under a folder named by the
   pattern, so both are needed.
*)
let parse_cli_pattern ~source ~anchor str : M.compiled_pattern list =
  let pat = Glob.Pattern.remove_leading_slash (Glob.Parse.parse_string str) in
  let anywhere : Glob.Pattern.t = Any_subpath :: pat in
  [ anywhere; Glob.Pattern.append anywhere [ Any_subpath ] ]
  |> List_.map (fun (pat : Glob.Pattern.t) ->
         M.compile ~source (Glob.Pattern.append anchor pat))

(* whether any of the patterns matches, without allocating a closure per
   call: this runs on every path of the scan *)
let rec any_match (path : string) (patterns : M.compiled_pattern list) : bool =
  match patterns with
  | [] -> false
  | pattern :: patterns -> M.run pattern path || any_match path patterns

(* [compile] turns the pattern of a line into the matchers any of which
   selects a path. *)
let parse_line_gen
    ~(compile : source:M.loc -> string -> M.compiled_pattern list) source_name
    source_kind line_number line_contents =
  if is_ignored_line line_contents then None
  else
    let loc : M.loc =
      {
        source_name;
        source_kind = Some source_kind;
        line_number;
        line_contents;
      }
    in
    let is_negated, pattern_str =
      match remove_negator line_contents with
      | None -> (false, line_contents)
      | Some s -> (true, s)
    in
    let patterns = compile ~source:loc pattern_str in
    let matcher (ppath : Ppath.t) =
      match any_match (Ppath.to_string_fast ppath) patterns with
      | true ->
          if is_negated then Some (Deselected loc) else Some (Selected loc)
      | false -> None
    in
    Some { loc; matcher }

let compile_gitignore_pattern ~anchor ~source (str : string) :
    M.compiled_pattern list =
  [ parse_pattern ~source ~anchor str ]

let compile_cli_pattern ~anchor ~source (str : string) :
    M.compiled_pattern list =
  parse_cli_pattern ~source ~anchor str

(* A line whose pattern is not anchored to the folder of its file matches
   at any depth, so it still means something for a path outside that
   folder. Comments and blank lines are kept so that the line numbers of
   the remaining patterns are the ones of the file. *)
let is_unanchored_line (line : string) : bool =
  is_ignored_line line
  ||
  let pattern_str =
    match remove_negator line with
    | None -> line
    | Some s -> s
  in
  not (is_anchored_pattern (Glob.Parse.parse_string pattern_str))

(* semgrep-legacy *)
let get_include_path ~orig_semgrepignore_path relative_include_path =
  let base_dir = Fpath.parent orig_semgrepignore_path in
  (* Preserve the original path components as much as possible to avoid
     possible confusion later *)
  Fpath.(base_dir // relative_include_path)

(*
   semgrep-legacy

   Expand lines like ':include foo/bar' into their contents.

   This is an legacy feature from semgrep that is now deprecated.

   It will not expand includes recursively to avoid cycles and other
   complications.
*)
let rec expand_includes ~orig_semgrepignore_path lines =
  let expand_line line =
    match parse_maybe_include_line ~orig_semgrepignore_path line with
    | Some relative_include_path ->
        let include_path =
          get_include_path ~orig_semgrepignore_path relative_include_path
        in
        if UFile.is_reg ~follow_symlinks:true include_path then
          include_path |> UFile.read_file |> read_lines_from_string
        else
          (* ignore silently
             (why: git also ignores .gitignore files that are broken
             symlinks) *)
          []
    | None -> [ line ]
  in
  List.concat_map expand_line lines

and from_lines ~compile ~allow_include ~name ~source_kind ~source_path lines =
  let lines =
    (* Don't allow ':include' when reading exclusion patterns from the
       command line (or not from a file in general) *)
    match source_path with
    | Some orig_semgrepignore_path when allow_include ->
        if allow_include then expand_includes ~orig_semgrepignore_path lines
        else lines
    | Some _
    | None ->
        lines
  in
  List_.mapi
    (fun i contents ->
      let linenum = i + 1 in
      parse_line_gen ~compile name source_kind linenum contents)
    lines
  |> List_.filter_map (fun x -> x)

and from_string_gen ~compile ~allow_include ~name ~source_path ~source_kind str
    =
  let lines = read_lines_from_string str in
  from_lines ~compile ~allow_include ~name ~source_path ~source_kind lines

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

and from_string ~anchor ~name ~source_kind str =
  from_string_gen
    ~compile:(compile_gitignore_pattern ~anchor)
    ~allow_include:false ~name ~source_path:None ~source_kind str

and cli_patterns_from_string ~anchor ~name ~source_kind str =
  from_string_gen
    ~compile:(compile_cli_pattern ~anchor)
    ~allow_include:false ~name ~source_path:None ~source_kind str

and unanchored_from_string ~anchor ~name ~source_kind ~source_path str =
  (* ':include' is expanded first: the directive is a line of its own and
     would otherwise survive the filter as a literal pattern *)
  let lines =
    let lines = read_lines_from_string str in
    match source_path with
    | Some orig_semgrepignore_path ->
        expand_includes ~orig_semgrepignore_path lines
    | None -> lines
  in
  from_lines
    ~compile:(compile_gitignore_pattern ~anchor)
    ~allow_include:false ~name ~source_path:None ~source_kind
    (lines |> List.filter is_unanchored_line)

and from_file ~anchor ~format ~source_kind path =
  path |> UFile.read_file
  |> from_string_gen
       ~compile:(compile_gitignore_pattern ~anchor)
       ~allow_include:(format = Legacy_semgrepignore)
       ~name:(Fpath.to_string path) ~source_path:(Some path) ~source_kind
[@@profiling]
