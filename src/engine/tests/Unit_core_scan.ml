(* Tests of what the engine reports to its caller, rather than of what it
   matches. *)

open Common
open Fpath_.Operators

let t = Testo.create

module F = Testutil_files

(* A search rule, so that each target is a work item, and an interfile
   rule, which is a work item of its own. *)
let rules =
  {|
rules:
  - id: eqeq-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
  - id: interfile-taint
    mode: taint
    options:
      taint_interfile: true
    pattern-sources:
      - pattern: source()
    pattern-sinks:
      - pattern: sink(...)
    message: "tainted data reaches a sink"
    languages: [python]
    severity: ERROR
|}

let a_py = {|
from b import g

def f(a):
    g(source())
    return a == a
|}

let b_py = {|
def g(x):
    sink(x)
    return x == x
|}

let test_progress_adds_up (caps : Core_scan.caps) ~(ncores : int) () =
  let files =
    [
      F.File ("rules.yaml", rules); F.File ("a.py", a_py); F.File ("b.py", b_py);
    ]
  in
  Testutil_git.with_git_repo files (fun (cwd : Fpath.t) ->
      let rule_file = Fpath.(cwd / "rules.yaml") in
      let parsed_rules =
        match Parse_rule.parse_and_filter_invalid_rules rule_file with
        | Ok (rules, _invalid) -> rules
        | Error e ->
            failwith
              (spf "failed to parse %s: %s" !!rule_file (Rule_error.show e))
      in
      let xlang = Test_engine.first_xlang_of_rules parsed_rules in
      let { Find_targets.selected = all_fpaths; _ } =
        Find_targets.get_target_fpaths Find_targets.default_conf
          [ Scanning_root.of_fpath cwd ]
      in
      let targets =
        all_fpaths
        |> List.filter (Filter_target.filter_target_for_xlang xlang)
        |> List_.map (fun (fpath : Fpath.t) ->
               Target.mk_target ~project_root:cwd xlang fpath)
      in
      let started : (int * int) option Atomic.t = Atomic.make None in
      let completed : int Atomic.t = Atomic.make 0 in
      let progress_hook (progress : Core_scan_config.progress) : unit =
        match progress with
        | Core_scan_config.Scanning_started { targets; interfile_rules } ->
            Atomic.set started (Some (targets, interfile_rules))
        | Core_scan_config.Target_done
        | Core_scan_config.Interfile_rule_done ->
            Atomic.incr completed
        | Core_scan_config.Analyzing_targets
        | Core_scan_config.Building_interfile_graph ->
            ()
      in
      let config =
        Core_scan_config.
          {
            default with
            rule_source = Rule_file rule_file;
            target_source = Targets targets;
            output_format = NoOutput;
            ncores;
            progress_hook = Some progress_hook;
          }
      in
      Common.protect
        ~finally:(fun () -> Globals.reset ())
        (fun () ->
          (match Core_scan.scan caps config with
          | Ok (_ : Core_result.t) -> ()
          | Error e -> Exception.reraise e);
          match Atomic.get started with
          | None -> Alcotest.fail "Scanning_started was not reported"
          | Some ((targets : int), (interfile_rules : int)) ->
              Alcotest.(check int) "two targets" 2 targets;
              Alcotest.(check int) "one interfile rule" 1 interfile_rules;
              Alcotest.(check int)
                "one completion per work item"
                (targets + interfile_rules)
                (Atomic.get completed)))

let tests (caps : Core_scan.caps) : Testo.t list =
  Testo.categorize "Core_scan"
    [
      t "the progress events add up to the Scanning_started total, 1 core"
        (test_progress_adds_up caps ~ncores:1);
      t "the progress events add up to the Scanning_started total, 4 cores"
        (test_progress_adds_up caps ~ncores:4);
    ]
