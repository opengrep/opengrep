(*
   Parse and interpret '.semgrepignore' files in addition to '.gitignore'
   files.

   The patterns they contain specify file paths to exclude from Semgrep scans.

   See the ml file for compatibility issues.
*)

(*
   We have to support the legacy built-in semgrepignore patterns
   when scanning for source code but we want something different
   or empty when scanning for secrets.

   The 'Empty' case is useful for testing.
*)
open Gitignore

type default_semgrepignore_patterns = Empty | Semgrep_scan_legacy

(*
   TODO: Preprocess a file to expand ':include' directives before parsing it
   using gitignore rules.

   Honor them with a deprecation warning.
*)
type exclusion_mechanism = {
  use_gitignore_files : bool;
  use_semgrepignore_files : bool;
}

(*
   The default semgrepignore used when no .semgrepignore exists
   at the project root (osemgrep) or in the current folder (legacy pysemgrep).

   It was copied from templates/.semgrepignore in the Python source.

   Coupling:
   If you modify this file, also modify:
   OSS/cli/src/semgrep/templates/.semgrepignore
*)
let default_semgrepignore_for_semgrep_scan =
  {|
# Git administrative folder or file
.git

# Common large paths
node_modules/
build/
dist/
vendor/
.env/
.venv/
.tox/
*.min.js
.npm/
.yarn/

# Common test paths
test/
tests/
testsuite/
*_test.go

# Semgrep rules folder
.semgrep

# Semgrep-action log folder
.semgrep_logs/
|}

let gitignore_files = Gitignore.default_gitignore_filename

let semgrepignore_files ?(custom_filename=".semgrepignore") () : Gitignore.gitignore_filename =
  {
    source_kind = "semgrepignore";
    filename = custom_filename;
    format = Gitignore.Legacy_semgrepignore;
  }

let contents_of_builtin_semgrepignore = function
  | Empty -> ""
  | Semgrep_scan_legacy -> default_semgrepignore_for_semgrep_scan

let create ?(cli_patterns = []) ~default_semgrepignore_patterns
    ~exclusion_mechanism ~project_root ?(custom_semgrepignore_filename=None) () =
  let root_anchor = Glob.Pattern.root_pattern in
  let default_patterns =
    Parse_gitignore.from_string ~name:"default semgrepignore patterns"
      ~source_kind:"default" ~anchor:root_anchor
      (contents_of_builtin_semgrepignore default_semgrepignore_patterns)
  in
  let cli_patterns =
    List.concat_map
      (Parse_gitignore.from_string ~name:"exclude pattern from command line"
         ~source_kind:"exclude" ~anchor:root_anchor)
      cli_patterns
  in
  let default_semgrepignore_file_level : Gitignore.level =
    {
      level_kind = "default semgrepignore patterns";
      source_name = "<built-in>";
      patterns = default_patterns;
    }
  in
  let cli_level : Gitignore.level =
    {
      level_kind = "command-line includes/excludes";
      source_name = "<command line>";
      patterns = cli_patterns;
    }
  in

  (* Process the custom semgrepignore file parameter *)
  let (custom_filename, custom_semgrepignore_path, is_custom_path) =
    match custom_semgrepignore_filename with
    | Some path when Sys.file_exists path ->
        let filename = Filename.basename path in
        (filename, path, true)
    | Some name -> (name, "", false)
    | None -> (".semgrepignore", "", false)
  in

  let semgrepignore_files = semgrepignore_files ~custom_filename () in
  let kinds_of_ignore_files_to_consult =
    (* order matters: first gitignore then semgrepignore *)
    (if exclusion_mechanism.use_gitignore_files then [ gitignore_files ] else [])
    @
    (if exclusion_mechanism.use_semgrepignore_files then [ semgrepignore_files ]
     else [])
  in

  (* Check if custom path exists directly *)
  let custom_path_exists =
    is_custom_path && Sys.file_exists custom_semgrepignore_path
  in

  (* Check if there is a top-level '.semgrepignore'. If not, use builtins. *)
  let root_semgrepignore_exists =
    if custom_path_exists then
      true
    else
      let root_dir = Ppath.to_fpath ~root:project_root Ppath.root in
      let semgrepignore_path = Fpath.add_seg root_dir custom_filename in
      Sys.file_exists (Fpath.to_string semgrepignore_path)
  in

  (* This condition determines whether the default semgrepignore rules should apply. *)
  let use_default_semgrepignore =
    exclusion_mechanism.use_semgrepignore_files
    && (not root_semgrepignore_exists)
    && (not custom_path_exists)
  in

  (* If custom path exists, load patterns from it *)
  let custom_patterns =
    if custom_path_exists then
      let content =
        try
          UChan.with_open_in custom_semgrepignore_path (fun ic ->
            really_input_string ic (in_channel_length ic))
        with
          _ -> ""
      in
      Parse_gitignore.from_string
        ~name:"custom semgrepignore file"
        ~source_kind:"semgrepignore"
        ~anchor:root_anchor
        content
    else []
  in

  (* Add custom patterns as a higher priority level if they exist *)
  let custom_level =
    if custom_path_exists then
      Some {
        level_kind = "custom semgrepignore file";
        source_name = custom_semgrepignore_path;
        patterns = custom_patterns;
      }
    else
      None
  in

  let higher_priority_levels =
    if use_default_semgrepignore then
      (* use the built-in semgrepignore rules in the absence of a root
         '.semgrepignore' file *)
      [ default_semgrepignore_file_level; cli_level ]
    else
      match custom_level with
      | Some level -> [ level; cli_level ]
      | None -> [ cli_level ]
  in

  let gitignore_filter =
    Gitignore_filter.create
      ~higher_priority_levels
      ~gitignore_filenames:(
        if custom_path_exists
        then []
        else kinds_of_ignore_files_to_consult)
      ~project_root ()
  in
  gitignore_filter
