(*
   Parse and interpret '.semgrepignore' files.

   Original implementation: ignores.py

   The legacy semgrepignore behavior is irregular and we're trying to move
   away from it and align closely with gitignore behavior.

   Old behavior:
   Here are the differences with gitignore listed in the legacy documentation:
   - '!' pattern negations aren't supported
   - character range patterns aren't supported
   - ':include' directives are a semgrepignore addition
   - '.semgrepignore' files placed anywhere in the tree are ignored (?)

   New behavior:
   - support '!' and character ranges to conform to gitignore syntax
   - don't support ':include' to conform to gitignore syntax
   - automatically use any '.gitignore' and use '.semgrepignore' additionally
     as if the latter was appended to the former.
   Support for negated patterns ('!') allows a .semgrepignore to
   undo exclusions made in a .gitignore.

   Migration plan:
   - print a deprecation notice if an ':include' directive is found in the
     root .semgrepignore.
   - stay silent otherwise: this is problematic only in the case of a
     .semgrepignore that doesn't include the .gitignore explicitly and
     contains fewer exclusions that the .gitignore. The new behavior will
     exclude more files than before.

   Questions:
   - Do some people really run semgrep on purpose on files that are excluded
     from source control?
     (we need to answer this to figure out the consequences of the migration
     plan)
*)
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
# Administrative folder or file used by popular version control systems
.git
.svn
.hg
_darcs
CVS

# Paths to files and folders that are typically large and ignorable
build/
vendor/
dist/
*.min.js
.env/
.tox/

# Package managers
node_modules/
.npm/
.yarn/
.venv/
_opam/
_build/
_cargo/
# Note that PHP composer uses vendor/ and C++ conan uses build/
# .venv is used both by Go and Python.

# Common test paths
test/
tests/
testsuite/
*_test.go
|}

let gitignore_files = Gitignore.default_gitignore_filename

let semgrepignore_files : Gitignore.gitignore_filename =
  {
    source_kind = "semgrepignore";
    filename = ".semgrepignore";
    format = Gitignore.Legacy_semgrepignore;
  }

let contents_of_builtin_semgrepignore = function
  | Empty -> ""
  | Semgrep_scan_legacy -> default_semgrepignore_for_semgrep_scan

let create ?(cli_patterns = []) ~default_semgrepignore_patterns
    ~exclusion_mechanism ~project_root () =
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

  (* Check for SEMGREP_R2C_INTERNAL_EXPLICIT_SEMGREPIGNORE environment variable *)
  let explicit_semgrepignore_level = 
    match Sys.getenv_opt "SEMGREP_R2C_INTERNAL_EXPLICIT_SEMGREPIGNORE" with
    | None -> None
    | Some explicit_path -> (
        match Fpath.of_string explicit_path with
        | Error _ ->
            (* nosemgrep: no-logs-in-library *)
            Logs.warn (fun m -> m "Invalid path in SEMGREP_R2C_INTERNAL_EXPLICIT_SEMGREPIGNORE: %s" explicit_path);
            None
        | Ok explicit_fpath -> (
            try
              if Sys.file_exists explicit_path then (
                (* nosemgrep: no-logs-in-library *)
                Logs.info (fun m -> m "Using explicit semgrepignore file from environment variable: %s" explicit_path);
                let content = UFile.read_file explicit_fpath in
                let patterns = Parse_gitignore.from_string 
                  ~name:("explicit semgrepignore: " ^ explicit_path)
                  ~source_kind:"semgrepignore" 
                  ~anchor:root_anchor 
                  content in
                Some ({
                  level_kind = "explicit semgrepignore file";
                  source_name = explicit_path;
                  patterns = patterns;
                } : Gitignore.level)
              ) else (
                (* nosemgrep: no-logs-in-library *)
                Logs.warn (fun m -> m "Explicit semgrepignore file not found: %s" explicit_path);
                None
              )
            with
            | exn ->
                (* nosemgrep: no-logs-in-library *)
                Logs.warn (fun m -> m "Error reading explicit semgrepignore file %s: %s" explicit_path (Printexc.to_string exn));
                None
        )
    )
  in

  let kinds_of_ignore_files_to_consult =
    (* order matters: first gitignore then semgrepignore *)
    (if exclusion_mechanism.use_gitignore_files then [ gitignore_files ] else [])
    @
    (* Only look for .semgrepignore files if no explicit file was provided *)
    if exclusion_mechanism.use_semgrepignore_files && explicit_semgrepignore_level = None then [ semgrepignore_files ]
    else []
  in
  (*
     Check if there is a top-level '.semgrepignore'. If not, use builtins.

     We don't check for '.semgrepignore' down the tree, so if a user needs
     to override the default semgrepignore rules, they need at least an
     empty root '.semgrepignore' file.
     
     If an explicit semgrepignore file is provided, we don't use the default patterns.
  *)
  let root_semgrepignore_exists =
    if explicit_semgrepignore_level <> None then true (* explicit file overrides defaults *)
    else
      let root_dir = Ppath.to_fpath ~root:project_root Ppath.root in
      let semgrepignore_path = Fpath.add_seg root_dir ".semgrepignore" in
      Sys.file_exists (Fpath.to_string semgrepignore_path)
  in

  (*
     This condition determines whether the default semgrepignore rules
     should apply.
  *)
  let use_default_semgrepignore =
    exclusion_mechanism.use_semgrepignore_files && not root_semgrepignore_exists
  in

  let higher_priority_levels =
    let base_levels = 
      if use_default_semgrepignore then
        (* use the built-in semgrepignore rules in the absence of a root
           '.semgrepignore' file *)
        [ default_semgrepignore_file_level; cli_level ]
      else [ cli_level ]
    in
    (* Add explicit semgrepignore level with highest priority if provided *)
    match explicit_semgrepignore_level with
    | None -> base_levels
    | Some level -> level :: base_levels
  in
  let gitignore_filter =
    Gitignore_filter.create ~higher_priority_levels
      ~gitignore_filenames:kinds_of_ignore_files_to_consult ~project_root ()
  in
  gitignore_filter
