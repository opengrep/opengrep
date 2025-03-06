open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end (e2e) testing of the semgrep-core CLI program
 *
 * See also tests for the semgrep-core -generate_ast_json in
 * in semgrep-interfaces/tests/test-ast run from make core-test-e2 run
 * itself from .github/workflow/test.yml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let t = Testo.create

(* Mostly a copy-paste of Test_pro_core_CLI.ml *)
type exn_res = ExnExit of int

let run_main (caps : Cap.all_caps) (cmd : string) : (unit, exn_res) result =
  let args = String_.split ~sep:"[ \t]+" cmd in
  (* we run main_exn() below in a child process because it modifies many globals
   * and we don't want to write code to reset those globals between two
   * tests; simpler to just fork.
   * NOTE: Cannot fork once domains are spawned, so moving to create_process. 
   *)
  print_string (spf "executing: opengrep-core %s\n" cmd);
  let extension = if Sys.win32 then ".exe" else "" in
  let executable = Fpath.(v "bin" / ("opengrep-core" ^ extension))
                   |> Fpath.to_string in
  let input, output = Unix.pipe () in
  let pid = CapUnix.create_process caps#exec
      executable (Array.of_list ("opengrep-core" :: args))
      Unix.stdin
      output
      (* Unix.stdout *)
      Unix.stderr in
  let _ = Unix.close output in
  (* Relay the output of the child process to stdout, else it's not
   * captured by Testo. *)
  let buffer = Bytes.create 1024 in
  let rec relay_output () =
    match Unix.read input buffer 0 (Bytes.length buffer) with
    | 0 -> ()  (* EOF: Child process closed stdout *)
    | n ->
      Stdlib.output stdout buffer 0 n;
      flush stdout;
      relay_output ()
  in
  relay_output ();
  Unix.close input;
  let _pid, process_status =
    CapUnix.waitpid caps#exec [Unix.WUNTRACED; Unix.WNOHANG] pid
  in
  match process_status with
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED n -> Error (ExnExit n)
  | _ -> Error (ExnExit 1)

let assert_Ok res =
  match res with
  | Ok () -> print_string "OK"
  | _ -> failwith "Not OK"

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let semgrep_core_tests (caps : Cap.all_caps) : Testo.t list =
  Testo.categorize "semgrep-core CLI (e2e)"
    [
      t "--help" (fun () ->
          match run_main caps "handle --help" with
          (* old: exception (Common.UnixExit 0) -> *)
          | Ok () (* Error (ExnExit 0) *) -> print_string "OK"
          | _ -> failwith "Not OK");
      t "handle -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/rules_v2/new_syntax.yaml -l python \
             tests/rules_v2/new_syntax.py -debug"
          in
          run_main caps cmd |> assert_Ok);
      t ~checked_output:(Testo.stdout ())
        "output of -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml -l python \
             tests/semgrep-core-e2e/targets/basic.py -debug"
          in
          run_main caps cmd |> assert_Ok);
      (* we could also assert that the output is actually equal to the
       * previous one
       *)
      t ~checked_output:(Testo.stdout ()) "handle -targets" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml  -targets \
             tests/semgrep-core-e2e/targets.json -debug"
          in
          run_main caps cmd |> assert_Ok);
    ]

let tests (caps : Cap.all_caps) : Testo.t list = semgrep_core_tests caps
