(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Module to represent and parse the test annotations of a rule's test file.
 * See https://semgrep.dev/docs/writing-rules/testing-rules/ for more info.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type kind =
  (* The good one, should be reported (TP) *)
  | Ruleid
  (* Should be reported but are not because of current engine limitations (FN) *)
  | Todoruleid
  (* Are reported but should not (FP) *)
  | Todook
  (* Those should *not* be reported (TN)
   * The 'ok:' is not that useful (it's mostly a comment) and actually
   * complicates some code during parsing (see the _no_ok regexps below).
   *)
  | Ok
[@@deriving show]

(* ex: "#ruleid: lang.ocaml.do-not-use-lisp-map" *)
type t = {
  kind : kind;
  (* alt: ids: Rule_ID.t list; (instead we return a list of annots) *)
  id : Rule_ID.t;
}
[@@deriving show]

(* starts at 1 *)
type linenb = int
type annotations = (t * linenb) list

let annotation_keywords = "\\(ruleid\\|ok\\|todoruleid\\|todook\\):"
let prefilter_annotation_regexp = ".*" ^ annotation_keywords ^ ".*"

(* removing ok as it could be valid code (as in `ok: foo` in JS)
 * alt: choose an annotation for ok: that would be less ambiguous
 * alt: get rid of ok:, just care about prook: and deepok:
 *)
let prefilter_annotation_regexp_no_ok =
  ".*\\(ruleid\\|todoruleid\\|todook\\):.*"

let annotation_regexp = "^" ^ annotation_keywords ^ "\\(.*\\)"

let (comment_syntaxes : (string * string option) list) =
  [
    ("#", None);
    ("//", None);
    ("<!--", Some "-->");
    ("(*", Some "*)");
    ("/*", Some "*/");
    (* in jsx as in
     *       <div>
     *         {/* ruleid: href-semgrep-app */}
     * where you can't use comment
     *)
    ("{/*", Some "*/}");
    ("'", None); (* vb.net *)
    (";;", None); (* (";", None) *) (* clojure *)
  ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let kind_of_string (str : string) : kind =
  match str with
  | "ruleid" -> Ruleid
  | "ok" -> Ok
  | "todoruleid" -> Todoruleid
  | "todook" -> Todook
  | s -> failwith (spf "not a valid annotation: %s" s)

(* "ruleid: foo, bar" -> (Ruleid, "foo, bar") *)
let parse_kind_opt (s : string) : (kind * string) option =
  if s =~ annotation_regexp then
    let kind_str, s = Common.matched2 s in
    Some (kind_of_string kind_str, String.trim s)
  else None

(* matches a comment opener followed by an annotation keyword, anywhere in the
 * line *)
let annotated_comment_regexp : Str.regexp =
  let openers =
    comment_syntaxes
    |> List_.map (fun ((prefix, _) : string * string option) -> Str.quote prefix)
    |> String.concat "\\|"
  in
  Str.regexp ("\\(" ^ openers ^ "\\)[ \t]*" ^ annotation_keywords)

(* The text of the comment. The annotation may follow code on the same line
 * ('x = 1  # ruleid: foo'), as in pysemgrep, which looked for a comment
 * opener followed by the keyword anywhere in the line. A comment without an
 * annotation must open the line.
 *)
let remove_enclosing_comment_opt (str : string) : string option =
  let text_after ((prefix, suffixopt) : string * string option) (pos : int) :
      string =
    let text = Str.string_after str (pos + String.length prefix) in
    match suffixopt with
    | Some suffix when String.ends_with ~suffix text ->
        Str.string_before text (String.length text - String.length suffix)
    | Some suffix ->
        Logs.warn (fun m -> m "could not find end comment %s in %s" suffix text);
        text
    | None -> text
  in
  let syntax_of (prefix : string) : string * string option =
    comment_syntaxes
    |> List.find (fun ((p, _) : string * string option) -> String.equal p prefix)
  in
  match Str.search_forward annotated_comment_regexp str 0 with
  | pos -> Some (text_after (syntax_of (Str.matched_group 1 str)) pos)
  | exception Not_found ->
      comment_syntaxes
      |> List.find_opt (fun ((prefix, _) : string * string option) ->
             String.starts_with ~prefix str)
      |> Option.map (fun (syntax : string * string option) -> text_after syntax 0)

let () =
  Testo.test "Test_subcommand.remove_enclosing_comment_opt" (fun () ->
      let test_remove (str : string) (expected : string option) =
        let res = remove_enclosing_comment_opt str in
        if not (res =*= expected) then
          failwith
            (spf "didn't match, got %s, expected %s" (Dumper.dump res)
               (Dumper.dump expected))
      in
      test_remove "# foobar" (Some " foobar");
      test_remove "// foobar" (Some " foobar");
      test_remove "<!-- foobar -->" (Some " foobar ");
      ())

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* This does a few things:
 *  - check comments: #, //, ( *, <--
 *  - support multiple ruleids separated by commas
 * alt: use parser combinators instead of those regexps/trims/Str.string_xxx
 *)
let annotations_of_string (orig_str : string) (file : Fpath.t) (idx : linenb) :
    annotations =
  let s = orig_str in
  let error_context = spf "in %s line %d" !!file idx in
  if s =~ prefilter_annotation_regexp then
    (* " <!-- ruleid: foo.bar --> " *)
    let s = String.trim s in
    (* "<!-- ruleid: foo.bar -->" *)
    let res = remove_enclosing_comment_opt s in
    match res with
    | None ->
        (* some Javascript code has valid code such as { ok: true } that is not
         * a semgrep annotation hence the use of a no_ok prefilter below
         *)
        if s =~ prefilter_annotation_regexp_no_ok then
          Logs.err (fun m ->
              m "annotation without leading comment: %s" orig_str)
        else
          Logs.debug (fun m ->
              m "skipping %s, actually not an annotation" orig_str);
        []
    | Some s -> (
        (* " ruleid: foo.bar " *)
        let s = String.trim s in
        (* "ruleid: foo.bar" *)
        match parse_kind_opt s with
        | Some (kind, s) ->
            let xs =
              Str.split_delim (Str.regexp "[ \t]*,[ \t]*") s
              |> List_.map String.trim
            in
            xs
            |> List_.filter_map (fun id_str ->
                   match Rule_ID.of_string_opt id_str with
                   | Some id -> Some ({ kind; id }, idx)
                   | None ->
                       Logs.warn (fun m ->
                           m
                             "malformed rule ID '%s' (%s) skipping this \
                              annotation"
                             id_str error_context);
                       None)
        | None ->
            Logs.warn (fun m ->
                m "could not parse annotation: %s (%s)" orig_str error_context);
            [])
  else []

(* Note that this returns the line of the annotation itself. In practice,
 * you must then add +1 to it if you want to compare it to where semgrep
 * report matches.
 *
 * alt: use Core_error.expected_error_lines_of_files but it does not
 * allow to extract the ruleID after the annotation_kind
 *)
let annotations (file : Fpath.t) : annotations =
  UFile.cat file |> List_.index_list_1
  |> List.concat_map (fun (s, idx) -> annotations_of_string s file idx)

let () =
  Testo.test "Test_subcommand.annotations" (fun () ->
      let test (str : string) (expected : t list) =
        let xs =
          annotations_of_string str (Fpath.v "foo") 0
          |> List_.map (fun (annot, _idx) -> annot)
        in
        if not (xs =*= expected) then
          failwith
            (spf "Annotations didn't match, got %s, expected %s"
               (Dumper.dump xs) (Dumper.dump expected))
      in
      let rule_id s = Rule_ID.of_string_exn s in
      test "// ruleid: foo" [ { kind = Ruleid; id = rule_id "foo" } ];
      test "// ruleid: foo, bar"
        [
          { kind = Ruleid; id = rule_id "foo" };
          { kind = Ruleid; id = rule_id "bar" };
        ];
      test "<!-- ruleid: foo-bar -->" [ { kind = Ruleid; id = rule_id "foo-bar" } ];
      (* the ok: does not mean it's an annot; it's regular (JS) code *)
      test "return res.send({ok: true})" [];
      (* the annotation may follow code on the same line *)
      test "x = 1 # todook: foo" [ { kind = Todook; id = rule_id "foo" } ];
      ())

(*****************************************************************************)
(* Annotations grouping and filtering *)
(*****************************************************************************)

(* group them by rule id, and adjust the linenb + 1 so it can be used to
 * compare actual matches.
 *)
let group_by_rule_id (annots : annotations) : (Rule_ID.t, linenb list) Assoc.t =
  annots
  |> Assoc.group_by (fun ({ id; _ }, _) -> id)
  |> List_.map (fun (id, xs) ->
         ( id,
           xs
           |> List_.map (fun (_, line) -> line + 1)
           (* should not be needed given how annotations work but safer *)
           |> List.sort_uniq Int.compare ))

let filter_todo (annots : annotations) (xs : linenb list) : linenb list =
  let (todos : linenb Set_.t) =
    annots
    |> List_.filter_map (fun ({ kind; _ }, line) ->
           match kind with
           (* + 1 because the expected/reported is the line after the annotation *)
           | Todook
           | Todoruleid ->
               Some (line + 1)
           | Ruleid
           | Ok ->
               None)
    |> Set_.of_list
  in
  xs |> List_.exclude (fun line -> Set_.mem line todos)
