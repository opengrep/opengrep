(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The data a skin renders.
 *
 * A skin decides what the report looks like, so it must be free of the
 * engine's types: everything here is plain data, already counted, grouped
 * and ordered by the builders (Scan_plan.ml for the plan, Summary_report.ml
 * for the rest). A skin that wants a different structure reorders these
 * records; it never has to recount anything.
 *)

(*****************************************************************************)
(* Phrases *)
(*****************************************************************************)

(* A count the report states in words, e.g. "3 files matching --exclude
 * patterns", or "2 files and 1 directories matching .semgrepignore
 * patterns" when a group counts two kinds of entry.
 *)
type phrase = {
  (* at least one entry, each with a non-zero count *)
  counts : (int * string) list;
  (* what the counts are about; "" when the nouns say it already *)
  suffix : string;
}

(* the plain wording of a phrase, which a skin is free not to use *)
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

  type rule_source =
    | Registry
    | Git
    | Local
    (* -e/--pattern, which is not a config at all *)
    | Pattern

  type t = {
    (* false off a terminal, and for the pattern mode, so that a log of a
     * scan does not gain the banner *)
    banner : bool;
    features : feature list;
    rule_source : rule_source;
  }
end

(*****************************************************************************)
(* The plan: what the scan is about to do *)
(*****************************************************************************)

module Plan = struct
  type lang_row = { language : string; rules : int; files : int }
  type origin_row = { origin : string; rules : int }

  type t = {
    (* the files targeting found, and every rule that was loaded *)
    num_targets : int;
    tracked_by_git : bool;
    num_rules : int;
    (* what the jobs pair up: a file some rule scans, a rule with a file *)
    num_files_with_a_rule : int;
    num_rules_with_a_target : int;
    (* the languages of the jobs, deduplicated, in job order *)
    languages : string list;
    (* sorted by files desc, then rules desc, then language *)
    lang_rows : lang_row list;
    (* sorted by rules desc, then origin *)
    origin_rows : origin_row list;
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
     * the interfile graph leaves out; their text prints with --verbose *)
    unplaced_warnings : int;
  }

  (* a scan that left nothing out says nothing *)
  let is_empty (x : t) : bool =
    Option.is_none x.limited
    && List_.null x.skipped
    && Option.is_none x.partially_analyzed
    && Int.equal x.unplaced_warnings 0
end

(*****************************************************************************)
(* The rules that ran out of time *)
(*****************************************************************************)

module Timeouts = struct
  type file = { path : string; rule_ids : string list }

  type t = {
    (* one entry per file, sorted by path; the ids of each are sorted too *)
    files : file list;
    (* the --timeout-threshold in force, which the report reads back *)
    threshold : int;
  }
end

(*****************************************************************************)
(* The end of the scan *)
(*****************************************************************************)

module Result = struct
  (* the one-line count a scan signs off with *)
  type tally = { rules_ran : int; files_scanned : int; findings : int }

  type t = {
    summary : Summary.t;
    (* absent for 'opengrep ci', which signs off with its own lines *)
    tally : tally option;
  }
end
