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
  (* What --experimental selects. It used to enable the features still
   * marked experimental; those gates are gone, so nothing behaves
   * differently under it and the flag is only accepted for compatibility.
   *)
  | Experimental
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

let o_experimental : bool Term.t =
  let info =
    Arg.info [ "experimental" ]
      ~doc:
        {|Accepted for compatibility; the OCaml implementation is the only one.|}
  in
  Arg.value (Arg.flag info)

let o_develop : bool Term.t =
  let info =
    (* alt: get rid  of the pysemgrep behaviors/limitations/errors *)
    Arg.info [ "develop" ] ~doc:{|Living on the edge.|}
  in
  Arg.value (Arg.flag info)

let o_maturity : t Term.t =
  let combine experimental develop =
    match (experimental, develop) with
    | false, false -> Default
    | true, false -> Experimental
    | false, true -> Develop
    | _else_ ->
        Error.abort "mutually exclusive options --experimental/--develop"
  in
  Term.(const combine $ o_experimental $ o_develop)
