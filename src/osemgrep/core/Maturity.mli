(* This module selects how mature a behaviour the user asks for.
 *)

type t =
  (* the plain behaviour, see CLI.ml *)
  | Default
  (* for enabling the features still marked experimental *)
  | Experimental
  (* Leaving on the edge, using osemgrep with osemgrep-only features enabled *)
  | Develop
[@@deriving show]

(* --experimental/--develop CLI processing *)
val o_maturity : t Cmdliner.Term.t
