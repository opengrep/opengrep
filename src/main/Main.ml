(*
 * The author disclaims copyright to this source file.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A SEMantic GREP.
 * See https://semgrep.dev/ for more information.
 *
 * This is the entry point of the semgrep-core program used internally
 * by pysemgrep, the entry point of osemgrep, and currently also the entry point
 * of semgrep for windows. See also the ../../cli/bin/semgrep python wrapper
 * script which is currently the real entry point of semgrep.
 * LATER: when osemgrep is fully done we can just get rid of semgrep-core,
 * osemgrep, the wrapper script, and have a single binary called 'semgrep'.
 *
 * Related work using code patterns (from oldest to newest):
 *  - Structural Search and Replace (SSR) in Jetbrains IDEs
 *    http://www.jetbrains.com/idea/documentation/ssr.html
 *    http://tv.jetbrains.net/videocontent/intellij-idea-static-analysis-custom-rules-with-structural-search-replace
 *  - Coccinelle (the precursor of Semgrep) for C
 *    https://coccinelle.gitlabpages.inria.fr/website/
 *  - Sgrep (Syntactical GREP, another precursor of Semgrep) for PHP
 *    https://github.com/facebook/pfff/wiki/Sgrep
 *  - gogrep and ruleguard for Go
 *    https://github.com/mvdan/gogrep/
 *    https://github.com/quasilyte/go-ruleguard
 *  - phpgrep for PHP
 *    https://github.com/quasilyte/phpgrep
 *    https://speakerdeck.com/quasilyte/phpgrep-syntax-aware-code-search
 *    https://github.com/VKCOM/noverify/blob/master/docs/dynamic-rules.md
 *  - cgrep for C
 *    http://awgn.github.io/cgrep/
 *  - Comby for many languages
 *    https://comby.dev/
 *  - Weggli for C/C++ (inspired by Semgrep)
 *    https://github.com/weggli-rs/weggli
 *  - ASTgrep (inspired by Semgrep)
 *    https://ast-grep.github.io/
 *
 * related AST search tools:
 *  - "ASTLOG: A Language for Examining Abstract Syntax Trees"
 *     https://www.usenix.org/legacy/publications/library/proceedings/dsl97/full_papers/crew/crew.pdf
 *  - rubocop pattern
 *    https://docs.rubocop.org/rubocop-ast/node_pattern.html
 *  - astpath, using XPATH on ASTs
 *    https://github.com/hchasestevens/astpath
 *
 * related code search and indexing tools:
 *  - "Tutorial on the C Information Abstraction System"
 *     https://www2.eecs.berkeley.edu/Pubs/TechRpts/1987/CSD-87-327.pdf
 *  - "JQuery: Finding your way through thangled code"
 *     https://www.cs.ubc.ca/labs/spl/projects/jquery/papers.htm
 *  - Codequery (from Pfff too)
 *    https://github.com/facebookarchive/pfff/wiki/CodeQuery
 *  - CodeQL (known previously as Semmle and before CodeQuest)
 *    https://codeql.github.com/
 *  - Kythe (sucessor of Grok by Steve Yegge at Google)
 *    https://kythe.io/
 *  - LSP the Language Server protocol
 *    https://langserver.org/
 *  - SCIP and LSIF by sourcegraph
 *    https://github.com/sourcegraph/scip
 *  - Glean
 *    https://glean.software/
 *  - many more (e.g., PQL)
 *
 * related grep-like tools:
 *  - ack
 *    http://beyondgrep.com/
 *  - ripgrep
 *    https://github.com/BurntSushi/ripgrep
 *  - hound https://codeascraft.com/2015/01/27/announcing-hound-a-lightning-fast-code-search-tool/
 *  - many grep-based linters (in Zulip, autodesk, bento, etc.)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let first_hyphen_in_argv argv =
  Array.find_index
    (fun arg -> String.starts_with ~prefix:"-" arg)
    argv

let position_for_experimental_flag argv =
  match first_hyphen_in_argv argv with
  | None -> Array.length argv - 1
  | Some i -> i

(* TODO[Issue #131]: Add some expectation tests for such functions. *)
let with_experimental_flag argv =
  let len = position_for_experimental_flag argv in
  Array.concat [
    Array.sub argv 0 len;
    [| "--experimental" |];
    Array.sub argv len (Array.length argv - len);
  ]

(* let _ = assert (with_experimental_flag [| "opengrep"; "scan"; "--help" |]
                   = [| "opengrep"; "scan"; "--experimental"; "--help" |])
   let _ = assert (with_experimental_flag [| "opengrep"; "-c"; "rules"; "libs" |]
                   = [| "opengrep"; "--experimental"; "-c"; "rules"; "libs" |]) *)

let flags_that_require_experimental : string list =
  [ "--output-enclosing-context"; "--semgrepignore-filename" ]

let experimental_flags_error_msg : string =
  "The --experimental option required for the following flags: "

let check_experimental_flags (argv : string array) : unit =
  if Array.mem "--experimental" argv
  then ()
  else
    match
      List.filter (Fun.flip Array.mem argv) flags_that_require_experimental
    with
    | [] -> ()
    | xs -> ignore (Error.abort (experimental_flags_error_msg ^ String.concat ", " xs))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* We currently use the same binary for semgrep-core and osemgrep (and now
 * also for semgrep for windows). See 'make core' and './dune' install section.
 * We use the argv[0] trick below to decide whether the user wants the
 * semgrep-core or osemgrep (or semgrep) behavior.
 *)
let () =
  Cap.main (fun (caps : Cap.all_caps) ->
      let argv = CapSys.argv caps#argv in
      let argv0 =
        (* remove the possible ".exe" extension for Windows and ".bc" *)
        Fpath.v argv.(0) |> Fpath.base |> Fpath.rem_ext |> Fpath.to_string
      in
      let experimental =
        Array.mem "--experimental" argv
      in
      match argv0, experimental with
      (* TODO[Issue #125]: Why does invoking [opengrep-cli] has argv0 = 'opengrep-core'?
       * This happens if the experimental flag is not passed. *)
      (* opengrep-cli a.k.a. osemgrep *)
      | "opengrep-cli", _
      (* In the long term (and in the short term on windows) we want to ship
       * opengrep-cli as the default "opengrep" binary, without any
       * wrapper script such as cli/bin/semgrep around it.
       *)
      | "opengrep", _
      | _, true ->
          let exit_code =
            match argv0 with
            | "opengrep" ->
                (* adding --experimental so we don't default back to pysemgrep *)
                CLI.main
                  (caps :> CLI.caps)
                  (* XXX: Should be after "scan" or similar.
                   * See line 161 in: src/osemgrep/cli/CLI.ml. *)
                  (if experimental then argv else with_experimental_flag argv)
            | _else_ ->
                check_experimental_flags argv;
                CLI.main (caps :> CLI.caps) argv
          in
          if not (Exit_code.Equal.ok exit_code) then
            Logs.info (fun m ->
                m "Error: %s\nExiting with error status %i: %s\n%!"
                  exit_code.description exit_code.code
                  (String.concat " " (Array.to_list argv)));
          CapStdlib.exit caps#exit exit_code.code
      (* legacy opengrep-core a.k.a. semgrep-core *)
      | _else_ -> Core_CLI.main caps argv)
