type kind =
  (* The good one, should be reported (TP) *)
  | Ruleid
  (* Should be reported but are not because of current engine limitations (FN) *)
  | Todoruleid
  (* Are reported but should not (FP) *)
  | Todook
  (* Those should *not* be reported (TN) *)
  | Ok
[@@deriving show]

(* ex: "#ruleid: lang.ocaml.do-not-use-lisp-map" *)
type t = { kind : kind; id : Rule_ID.t } [@@deriving show]

(* starts at 1 *)
type linenb = int
type annotations = (t * linenb) list

val annotations : Fpath.t -> annotations
val group_by_rule_id : annotations -> (Rule_ID.t, linenb list) Assoc.t
(* Drop the lines carrying a 'todook:' or a 'todoruleid:' annotation.
 * python: test.py subtracts todo_ok_lines and todo_ruleid_lines from both the
 * expected and the reported lines before comparing them, which is what makes
 * those annotations mean "do not judge this line". *)
val filter_todo : annotations -> linenb list -> linenb list
