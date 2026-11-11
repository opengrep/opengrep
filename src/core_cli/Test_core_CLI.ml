open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end (e2e) testing of the low-level engine CLI, i.e.
 * 'opengrep --core'
 *
 * See also tests for the 'opengrep --core -generate_ast_json' in
 * in semgrep-interfaces/tests/test-ast run from make core-test-e2 run
 * itself from .github/workflow/test.yml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let t = Testo.create

(* Mostly a copy-paste of Test_pro_core_CLI.ml *)
type exn_res = ExnExit of int

(* the opengrep executable of the checkout at [root] *)
let executable_in (root : Fpath.t) : string =
  let extension = if Sys.win32 then ".exe" else "" in
  Fpath.(root / "bin" / ("opengrep" ^ extension)) |> Fpath.to_string

let run_main ~(executable : string) (caps : Cap.all_caps) (cmd : string) :
    (unit, exn_res) result =
  let args = String_.split ~sep:"[ \t]+" cmd in
  (* we run main_exn() below in a child process because it modifies many globals
   * and we don't want to write code to reset those globals between two
   * tests; simpler to just fork.
   * NOTE: Cannot fork once domains are spawned, so moving to create_process. 
   *)
  print_string (spf "executing: opengrep --core %s\n" cmd);
  let input, output = Unix.pipe () in
  let pid = CapUnix.create_process caps#exec
      executable (Array.of_list ("opengrep" :: "--core" :: args))
      Unix.stdin
      output
      (* Unix.stdout *)
      Unix.stderr in
  let _ = Unix.close output in
  (* Relay the output of the child process to stdout, else it's not
   * captured by Testo. *)
  let buffer = Bytes.create 1024 in

  let rec safe_read fd buffer offset len =
    try Unix.read fd buffer offset len
    with Unix.Unix_error (Unix.EINTR, "read", _) ->
      (* Read was interrupted, retry *)
      safe_read fd buffer offset len
  in

  let rec relay_output () =
    match safe_read input buffer 0 (Bytes.length buffer) with
    | 0 -> ()  (* EOF: Child process closed stdout *)
    | n ->
        Stdlib.output stdout buffer 0 n;
        flush stdout;
        relay_output ()
  in

  relay_output ();
  Unix.close input;

  let rec wait_for_process pid =
  try
    match CapUnix.waitpid caps#exec [Unix.WUNTRACED; Unix.WNOHANG] pid with
    | 0, _ ->
        Unix.sleepf 0.01;
        wait_for_process pid
    | _, Unix.WEXITED 0 -> Ok ()
    | _, Unix.WEXITED n -> Error (ExnExit n)
    | _, _ -> Error (ExnExit 1)
  with
  | Unix.Unix_error (Unix.ECHILD, "waitpid", _) ->
      (* No child process found, assume it has already exited *)
      Ok ()
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      (* Retry on EINTR (interrupted system call) *)
      wait_for_process pid
  | _exn ->
      (* Unexpected error *)
      Error (ExnExit 1)

  in
  wait_for_process pid

(* run_main with the executable of the checkout the tests run in *)
let run_main_in_checkout : Cap.all_caps -> string -> (unit, exn_res) result =
  run_main ~executable:(executable_in (Fpath.v "."))

let assert_Ok res =
  match res with
  | Ok () -> print_string "OK"
  | _ -> failwith "Not OK"

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let normalize =
  [
    Testo.mask_pcre_pattern {|\{"version":"([^"]+)","results":\[|}
  ]

module F = Testutil_files

let interfile_rule_content =
  {|
rules:
  - id: interfile-taint
    mode: taint
    options:
      interfile: true
    pattern-sources:
      - pattern: source()
    pattern-sinks:
      - pattern: sink(...)
    message: "cross-file taint"
    languages: [python]
    severity: ERROR
|}

let interfile_targets_content =
  {|[
  ["CodeTarget", {"path": "main.py", "analyzer": "python", "products": ["sast"]}],
  ["CodeTarget", {"path": "helper.py", "analyzer": "python", "products": ["sast"]}],
  ["CodeTarget", {"path": "local.py", "analyzer": "python", "products": ["sast"]}]
]|}

(* main.py reaches the sink through helper.py; local.py has the source and
   the sink in one function, so that its finding shows that the rule ran *)
let interfile_files : F.t list =
  [
    F.File ("rules.yaml", interfile_rule_content);
    F.File ("targets.json", interfile_targets_content);
    F.File ("helper.py", "def wrap():\n    return source()\n");
    F.File
      ("main.py", "from helper import wrap\n\ndef go():\n    sink(wrap())\n");
    F.File ("local.py", "def go():\n    sink(source())\n");
  ]

(* The files of the findings of a scan of [interfile_files] by an engine CLI
   run with [flags]. The run is in a directory of its own, so that the
   interfile graph covers these files only. *)
let interfile_finding_paths (caps : Cap.all_caps) (flags : string) :
    string list =
  let executable = executable_in (Fpath.v (Sys.getcwd ())) in
  F.with_tempfiles ~chdir:true interfile_files (fun (_root : Fpath.t) ->
      let res, stdout_output =
        Testo.with_capture stdout (fun () ->
            run_main ~executable caps
              ("-rules rules.yaml -targets targets.json -json" ^ flags))
      in
      (match res with
      | Ok () -> ()
      | Error (ExnExit code) -> failwith (spf "exit code %d" code));
      (* the JSON output is the last line, after the line that run_main
         prints and the progress dots *)
      let json =
        match
          String.split_on_char '\n' stdout_output
          |> List.filter (fun (line : string) -> not (String.equal line ""))
          |> List.rev
        with
        | last :: _ -> last
        | [] -> failwith "no output"
      in
      (Semgrep_output_v1_j.core_output_of_string json).results
      |> List_.map (fun (m : Semgrep_output_v1_t.core_match) ->
             Fpath.to_string m.path)
      |> List.sort_uniq String.compare)

let test_enable_interfile (caps : Cap.all_caps) () =
  Alcotest.(check (list string))
    "without -enable_interfile the rule runs per target" [ "local.py" ]
    (interfile_finding_paths caps "");
  Alcotest.(check (list string))
    "with -enable_interfile the rule runs interfile"
    [ "local.py"; "main.py" ]
    (interfile_finding_paths caps " -enable_interfile");
  Alcotest.(check (list string))
    "with -enable_interfile and -disable_intrafile the rule runs per target"
    [ "local.py" ]
    (interfile_finding_paths caps " -enable_interfile -disable_intrafile")

let semgrep_core_tests (caps : Cap.all_caps) : Testo.t list =
  Testo.categorize "semgrep-core CLI (e2e)"
    [
      (* TODO: This works on OSX but not on Linux, we should fix it
       * later. *)
      (* t "--help" (fun () ->
             match run_main caps "handle --help" with
             (\* old: exception (Common.UnixExit 0) -> *\)
             | Ok () (\* Error (ExnExit 0) *\) -> print_string "OK"
             | _ -> failwith "Not OK"); *)
      t "handle -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/rules_v2/new_syntax.yaml -l python \
             tests/rules_v2/new_syntax.py -debug"
          in
          run_main_in_checkout caps cmd |> assert_Ok);
      t ~checked_output:(Testo.stdout ())
        "output of -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml -l python \
             tests/semgrep-core-e2e/targets/basic.py -debug"
          in
          run_main_in_checkout caps cmd |> assert_Ok);
      (* we could also assert that the output is actually equal to the
       * previous one
       *)
      t ~checked_output:(Testo.stdout ()) "handle -targets" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml  -targets \
             tests/semgrep-core-e2e/targets.json -debug"
          in
          run_main_in_checkout caps cmd |> assert_Ok);
      t ~checked_output:(Testo.stdout ()) ~normalize "the -output_enclosing_context flag"
        (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml  -targets \
             tests/semgrep-core-e2e/targets.json -debug -json -output_enclosing_context"
          in
          run_main_in_checkout caps cmd |> assert_Ok);
      t "an interfile rule runs interfile only under -enable_interfile, and \
         not under -disable_intrafile"
        (test_enable_interfile caps);
    ]

let tests (caps : Cap.all_caps) : Testo.t list = semgrep_core_tests caps
