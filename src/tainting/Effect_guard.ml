(* Engine-emitted guards attached to taint effects.
 *
 * A guard is a constraint on the caller-side arguments at a call site; it
 * represents a condition the dataflow engine inferred (e.g. from a Switch
 * case pattern) that an effect's applicability depends on. At signature
 * instantiation time, each guard is evaluated against the caller's actual
 * arguments; if any guard of an effect is definitely-false, the effect is
 * dropped before it can contribute to findings.
 *
 * Separate from 'Rule.precondition', which is author-specified boolean
 * logic over taint LABELS, evaluated at finding time against the taint
 * tokens reaching a sink. Effect guards are engine-emitted, target-shape
 * constraints, evaluated earlier, against the call site's argument values.
 *
 * Semantics: an effect's [guards] list is a conjunction — all must hold.
 * An empty list means "no constraint", as is the case for today's effects. *)

module T = Taint

type t =
  | LenEq of T.arg * int
      (** [LenEq (arg, n)] holds iff the caller's argument bound to [arg]
          denotes a sequence (CList/CTuple/CArray/CSet) of length exactly
          [n]. Emitted for list-shaped Switch cases with exact arity. *)
  | LenGe of T.arg * int
      (** [LenGe (arg, k)] holds iff the caller's argument bound to [arg]
          denotes a sequence of length at least [k]. Emitted for list-
          shaped Switch cases with a trailing variadic slot
          ([p1; ...; pK; & rest]). *)
[@@deriving eq, ord]

let show = function
  | LenEq (arg, n) -> Printf.sprintf "#%s == %d" (T.show_arg arg) n
  | LenGe (arg, k) -> Printf.sprintf "#%s >= %d" (T.show_arg arg) k

(* Sets of guards — a conjunction, order-independent. A set is preferred over
 * a list so that accumulating the same guard twice does not make two
 * otherwise-equal effects compare as distinct members of [Effects.t]. *)
module Set = Stdlib.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

let show_set gs =
  if Set.is_empty gs then ""
  else
    "["
    ^ String.concat " && " (gs |> Set.elements |> List.map show)
    ^ "]"
