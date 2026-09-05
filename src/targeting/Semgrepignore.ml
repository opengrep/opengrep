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

let default_semgrepignore_filename = ".semgrepignore"

let gitignore_files = Gitignore.default_gitignore_filename

let semgrepignore_files ~filename : Gitignore.gitignore_filename =
  {
    source_kind = "semgrepignore";
    filename;
    format = Gitignore.Legacy_semgrepignore;
  }

let contents_of_builtin_semgrepignore = function
  | Empty -> ""
  | Semgrep_scan_legacy -> default_semgrepignore_for_semgrep_scan

let create ?(cli_patterns = []) ?(working_directory : Fpath.t option)
    ?(semgrepignore_filename = default_semgrepignore_filename) ~default_semgrepignore_patterns
    ~exclusion_mechanism ~project_root () =
  let root_anchor = Glob.Pattern.root_pattern in
  let default_patterns =
    Parse_gitignore.from_string ~name:"default semgrepignore patterns"
      ~source_kind:"default" ~anchor:root_anchor
      (contents_of_builtin_semgrepignore default_semgrepignore_patterns)
  in
  let cli_patterns =
    List.concat_map
      (Parse_gitignore.cli_patterns_from_string
         ~name:"exclude pattern from command line" ~source_kind:"exclude"
         ~anchor:root_anchor)
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
  let semgrepignore_files = semgrepignore_files ~filename:semgrepignore_filename
  in
  let kinds_of_ignore_files_to_consult =
    (* order matters: first gitignore then semgrepignore *)
    (if exclusion_mechanism.use_gitignore_files then [ gitignore_files ] else [])
    @
    if exclusion_mechanism.use_semgrepignore_files then [ semgrepignore_files ]
    else []
  in
  (* the ignore files of this filter are read through this cache, which
     reports an unreadable one once *)
  let gitignore_file_cache =
    Gitignore_cache.create ~gitignore_filenames:kinds_of_ignore_files_to_consult
      ~project_root ()
  in
  let read_ignore_file_opt (dir : Fpath.t) : string option =
    Gitignore_cache.read_ignore_file_opt gitignore_file_cache
      (Gitignore_cache.ignore_file_path ~filename:semgrepignore_filename dir)
  in
  (*
     Check if there is a top-level '.semgrepignore'. If not, use builtins.

     We don't check for '.semgrepignore' down the tree, so if a user needs
     to override the default semgrepignore rules, they need at least an
     empty root '.semgrepignore' file.
  *)
  let root_semgrepignore_exists =
    (* a file the scan cannot read holds no patterns, so it does not
       replace the built-in ones either *)
    Option.is_some
      (read_ignore_file_opt (Ppath.to_fpath ~root:project_root Ppath.root))
  in

  (*
     This condition determines whether the default semgrepignore rules
     should apply.
  *)
  let use_default_semgrepignore =
    exclusion_mechanism.use_semgrepignore_files && not root_semgrepignore_exists
  in

  (*
     pysemgrep read the '.semgrepignore' of the working directory whatever
     the scanning root was, and applied to a path outside that directory
     only the patterns that are not anchored to it, the ones that match at
     any depth (the _survives method of ignores.py). When the working
     directory is the project root, its file is read as the project's own
     and its patterns are anchored there, so nothing is added here.
  *)
  let working_directory_levels : Gitignore.level list =
    match working_directory with
    | Some dir when exclusion_mechanism.use_semgrepignore_files -> (
        let path =
          Gitignore_cache.ignore_file_path ~filename:semgrepignore_filename dir
        in
        match read_ignore_file_opt dir with
        | None -> []
        | Some (contents : string) ->
            [
              {
                Gitignore.level_kind = "semgrepignore of the working directory";
                source_name = Fpath.to_string path;
                patterns =
                  Parse_gitignore.unanchored_from_string
                    ~name:(Fpath.to_string path) ~source_kind:"semgrepignore"
                    ~anchor:root_anchor ~source_path:(Some path) contents;
              };
            ])
    | _ -> []
  in
  let higher_priority_levels =
    (* the working directory's file comes last: target_manager.py applied
       the command-line includes and excludes before the semgrepignore
       filter, so a line there cannot bring back a path '--exclude' or
       '--include' left out *)
    (if use_default_semgrepignore then
       (* use the built-in semgrepignore rules in the absence of a root
          '.semgrepignore' file *)
       [ default_semgrepignore_file_level; cli_level ]
     else [ cli_level ])
    @ working_directory_levels
  in
  Gitignore_filter.create ~higher_priority_levels ~gitignore_file_cache ()
