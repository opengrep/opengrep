module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This module selects how mature a behaviour the user asks for.
 *)

(*************************************************************************)
(* Types *)
(*************************************************************************)

type t =
  (* alt: we could use an option type also *)
  | Default
  (* Leaving on the edge! This is used to specify whether to get rid of
   * pysemgrep behavior/limitations/errors or to keep how things were done
   * before (even if they were bad, but just to remain backward compatible).
   *)
  | Develop
[@@deriving show]

(*************************************************************************)
(* Maturity Cmdliner *)
(*************************************************************************)

(* We keep these as explicit flags so they show up in the man pages
 * (e.g., in 'opengrep scan --help').
 *)

let o_develop : bool Term.t =
  let info =
    (* alt: get rid  of the pysemgrep behaviors/limitations/errors *)
    Arg.info [ "develop" ] ~doc:{|Living on the edge.|}
  in
  Arg.value (Arg.flag info)

let o_maturity : t Term.t =
  let combine (develop : bool) = if develop then Develop else Default in
  Term.(const combine $ o_develop)
