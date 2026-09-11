(* This module selects how mature a behaviour the user asks for.
 *)

type t =
  (* the plain behaviour, see CLI.ml *)
  | Default
  (* what --experimental selects; no behaviour depends on it any more, the
     flag is accepted for compatibility *)
  | Experimental
  (* Leaving on the edge, using osemgrep with osemgrep-only features enabled *)
  | Develop
[@@deriving show]

(* --experimental/--develop CLI processing *)
val o_maturity : t Cmdliner.Term.t
