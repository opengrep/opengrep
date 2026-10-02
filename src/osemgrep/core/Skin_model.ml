(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The data that a skin renders.
 *
 * The records are independent of the engine's types: plain data, already
 * counted, grouped and ordered by the builders (Scan_plan.ml for the plan,
 * Summary_report.ml for the summary). A skin with a different structure
 * reorders these records without counting again.
 *)

(*****************************************************************************)
(* Phrases *)
(*****************************************************************************)

(* A count printed in words, for example "3 files matching --exclude
 * patterns", or "2 files and 1 directories matching .semgrepignore
 * patterns" when a group counts two kinds of entry.
 *)
type phrase = {
  (* at least one entry, each with a non-zero count *)
  counts : (int * string) list;
  (* the subject of the counts; "" when the nouns are enough *)
  suffix : string;
}

(* The sum of the counts of a phrase, for a skin that words the phrase
   itself. *)
let total_of_phrase (x : phrase) : int =
  x.counts
  |> List.fold_left (fun (acc : int) ((n : int), (_ : string)) -> acc + n) 0

(* the default text of a phrase *)
let string_of_phrase (x : phrase) : string =
  let counts =
    x.counts
    |> List_.map (fun ((n : int), (noun : string)) ->
           Printf.sprintf "%d %s" n noun)
    |> String.concat " and "
  in
  match x.suffix with
  | "" -> counts
  | suffix -> counts ^ " " ^ suffix

(*****************************************************************************)
(* The start: what the scan is, before it has read anything *)
(*****************************************************************************)

module Start = struct
  type feature = { name : string; description : string; enabled : bool }

  (* the environment of an 'opengrep ci' run *)
  type ci_env = {
    version : string;
    ocaml_version : string;
    environment : string;
    event_name : string;
  }

  type rule_source =
    | Registry
    | Git
    | Local
    (* -e/--pattern, which is not a config *)
    | Pattern

  type t = {
    (* false when stdout is not a terminal, and for the pattern mode, so
     * that a log of a scan does not contain the banner *)
    banner : bool;
    features : feature list;
    rule_source : rule_source;
    (* None for a plain scan; 'opengrep ci' prints it with or without a
     * banner *)
    ci : ci_env option;
  }
end

(*****************************************************************************)
(* The plan: what the scan is about to do *)
(*****************************************************************************)

module Plan = struct
  type lang_row = { language : string; rules : int; files : int }
  type origin_row = { origin : string; rules : int }

  (* The run of a differential scan that a plan describes. A
     --baseline-commit scan prints two plans: one for the working tree, then
     one for the same paths at the baseline commit, the baseline scan that
     determines which findings are new. The baseline scan often has no
     targets, so without a label its plan would appear to contradict the
     first. *)
  type run =
    | Current
    | Baseline

  type t = {
    (* the files targeting found, and every rule that was loaded *)
    num_targets : int;
    tracked_by_git : bool;
    num_rules : int;
    (* the files paired with at least one rule, and the rules paired with at
       least one file *)
    num_files_with_a_rule : int;
    num_rules_with_a_target : int;
    (* the languages of the jobs, deduplicated, in job order *)
    languages : string list;
    (* sorted by files desc, then rules desc, then language *)
    lang_rows : lang_row list;
    (* sorted by rules desc, then origin *)
    origin_rows : origin_row list;
    run : run;
  }
end

(*****************************************************************************)
(* The files no scan analysed, or analysed only in part *)
(*****************************************************************************)

module Summary = struct
  type t = {
    (* what narrowed the scan: a baseline commit, or the git listing *)
    limited : string option;
    skipped : phrase list;
    partially_analyzed : phrase option;
    (* warnings about the scan rather than about a file, such as the targets
     * the interfile graph leaves out; their text is printed with --verbose *)
    unplaced_warnings : int;
  }

  let empty : t =
    {
      limited = None;
      skipped = [];
      partially_analyzed = None;
      unplaced_warnings = 0;
    }

  (* true when there is nothing to report *)
  let is_empty (x : t) : bool =
    Option.is_none x.limited
    && List_.null x.skipped
    && Option.is_none x.partially_analyzed
    && Int.equal x.unplaced_warnings 0
end

(*****************************************************************************)
(* The end of the scan *)
(*****************************************************************************)

module Result = struct
  (* The counts that end a scan. files_scanned counts every file scanned;
   * files_with_findings counts those with at least one finding, the N of
   * "in N files". *)
  type tally = {
    rules_ran : int;
    (* the distinct rules with at least one finding, the N of "from N
     * rules"; not rules_ran *)
    rules_with_findings : int;
    files_scanned : int;
    files_with_findings : int;
    findings : int;
  }

  type t = {
    summary : Summary.t;
    (* None for 'opengrep ci', which prints its own final lines *)
    tally : tally option;
  }
end
