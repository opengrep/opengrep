(* Tests of what the engine promises its caller, rather than of what it
   matches. *)

open Common
open Fpath_.Operators

let t = Testo.create

module F = Testutil_files
module RP = Core_result

let eqeq_rule =
  {|
rules:
  - id: eqeq-bad
    pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

(* two matches, so that the count below says something *)
let two_findings_py = {|
def f(a):
    return a == a

def g(x):
    return x == x
|}

(* A progress hook belongs to the caller, and the engine calls it from the
   [finally] of a work item, where an exception becomes [Finally_raised]:
   it would fail the unit and report a fault in the progress display as a
   fault in the scan. The engine contains it instead, so a hook that raises
   costs its own counting and nothing else.
   coupling: Core_scan.report_progress *)
let test_raising_progress_hook (caps : Core_scan.caps) () =
  let files =
    [ F.File ("rule.yaml", eqeq_rule); F.File ("a.py", two_findings_py) ]
  in
  Testutil_git.with_git_repo files (fun (cwd : Fpath.t) ->
      let rule_file = Fpath.(cwd / "rule.yaml") in
      let rules =
        match Parse_rule.parse_and_filter_invalid_rules rule_file with
        | Ok (rules, _invalid) -> rules
        | Error e ->
            failwith
              (spf "failed to parse %s: %s" !!rule_file (Rule_error.show e))
      in
      let xlang = Test_engine.first_xlang_of_rules rules in
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
      (* counted before raising, so that a hook that is never called cannot
         pass this test by never failing either *)
      let calls = Atomic.make 0 in
      let config =
        Core_scan_config.
          {
            default with
            rule_source = Rule_file rule_file;
            target_source = Targets targets;
            output_format = NoOutput;
            progress_hook =
              Some
                (fun (_ : Core_scan_config.progress) ->
                  Atomic.incr calls;
                  failwith "a progress hook that fails");
          }
      in
      Common.protect
        ~finally:(fun () -> Globals.reset ())
        (fun () ->
          let result =
            match Core_scan.scan caps config with
            | Ok r -> r
            | Error e -> Exception.reraise e
          in
          Alcotest.(check bool)
            "the hook was reached" true
            (Atomic.get calls > 0);
          Alcotest.(check int)
            "the findings are still reported" 2
            (List.length result.RP.processed_matches);
          Alcotest.(check int)
            "and the scan reports no error of its own" 0
            (List.length result.RP.errors)))

let tests (caps : Core_scan.caps) : Testo.t list =
  Testo.categorize "Core_scan"
    [
      t "a progress hook that raises does not fail the scan"
        (test_raising_progress_hook caps);
    ]
