(* Iago Abal, Nat Mote
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Taint traces to help understand semgrep findings (especially useful
 * when in interfile mode).
 *
 * Note that those "core" taint traces are translated at some point in
 * Semgrep_output_v1.match_dataflow_trace
 *
 * See also Matching_explanation.ml for a complementary way to explain
 * findings related to formula composition. The taint traces, as
 * the name suggest, are to explain the flow of taints through different
 * variable assignments and function calls.
 *
 * Do not confuse those traces with our telemetry traces in Trace_data.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* The path of a taint: the variables it propagates through, and the calls
   to functions that it enters by a parameter and leaves by the return or by
   an l-value. A path is a list of its own, so that a variable costs one
   cell, as in a list of tokens.

   While the analysis of a function runs, a path is newest first, and so is
   the path in an effect of its signature; the instantiation of a call
   reverses it once, into the call. The path that reaches a sink is put in
   execution order (Dataflow_tainting.effects_of_tainted_sink), and so is
   the path inside a call, which starts at the parameter by which the taint
   enters the function and ends at the return by which it leaves it, when
   it enters or leaves by one. *)
type tainted_tokens =
  | Nil
  | Variable of Tok.t * tainted_tokens
  | Parameter of Tok.t * tainted_tokens
  | Return of Tok.t * tainted_tokens
  | Through_call of through_call * tainted_tokens

(* A call that the taint goes through: the name of the function called, when
   it has one, and the path inside the function. [size] is the number of
   tokens of the call, as the flat path carries them: its name and the path
   inside, without the return. *)
and through_call = {
  callee : Tok.t option;
  inside : tainted_tokens;
  size : int;
}
[@@deriving show, eq, ord]

let rec rev_append (path : tainted_tokens) (acc : tainted_tokens) :
    tainted_tokens =
  match path with
  | Nil -> acc
  | Variable (tok, rest) -> rev_append rest (Variable (tok, acc))
  | Parameter (tok, rest) -> rev_append rest (Parameter (tok, acc))
  | Return (tok, rest) -> rev_append rest (Return (tok, acc))
  | Through_call (call, rest) -> rev_append rest (Through_call (call, acc))

(* the path in reverse, at its outer level; the paths inside its calls keep
   their order *)
let rev (path : tainted_tokens) : tainted_tokens = rev_append path Nil

(* the number of tokens of the first element of a path, as the flat path
   carries them, and the rest of the path *)
let head_size (path : tainted_tokens) : int =
  match path with
  | Nil
  | Return _ ->
      0
  | Variable _
  | Parameter _ ->
      1
  | Through_call (call, _) -> call.size

let tail (path : tainted_tokens) : tainted_tokens =
  match path with
  | Nil -> Nil
  | Variable (_, rest)
  | Parameter (_, rest)
  | Return (_, rest)
  | Through_call (_, rest) ->
      rest

(* the number of tokens of a path, as many as its flat form has *)
let size (path : tainted_tokens) : int =
  let rec count (acc : int) (path : tainted_tokens) : int =
    match path with
    | Nil -> acc
    | _ -> count (acc + head_size path) (tail path)
  in
  count 0 path

(* Whether [a] has fewer tokens than [b]. Like List.compare_lengths, it walks
   the two paths no further than it needs to. *)
let fewer_tokens (a : tainted_tokens) (b : tainted_tokens) : bool =
  (* whether [path] has more than [n] tokens *)
  let rec more_than (n : int) (path : tainted_tokens) : bool =
    n < 0
    ||
    match path with
    | Nil -> false
    | _ -> more_than (n - head_size path) (tail path)
  in
  (* [diff] is the tokens of [a] walked so far minus those of [b] *)
  let rec walk (diff : int) (a : tainted_tokens) (b : tainted_tokens) : bool =
    match (a, b) with
    | Nil, _ -> more_than diff b
    | _, Nil -> not (more_than (-diff - 1) a)
    | _ -> walk (diff + head_size a - head_size b) (tail a) (tail b)
  in
  walk 0 a b

let through_call ~(callee : Tok.t option) ~(inside : tainted_tokens) :
    through_call =
  {
    callee;
    inside;
    size =
      Option.fold ~none:0 ~some:(fun (_ : Tok.t) -> 1) callee + size inside;
  }

let rec map_toks (f : Tok.t -> Tok.t) (path : tainted_tokens) : tainted_tokens
    =
  match path with
  | Nil -> Nil
  | Variable (tok, rest) -> Variable (f tok, map_toks f rest)
  | Parameter (tok, rest) -> Parameter (f tok, map_toks f rest)
  | Return (tok, rest) -> Return (f tok, map_toks f rest)
  | Through_call (call, rest) ->
      Through_call
        ( {
            call with
            callee = Option.map f call.callee;
            inside = map_toks f call.inside;
          },
          map_toks f rest )

(* The tokens associated with a single pattern match involved in a taint trace *)
type pattern_match_tokens = Tok.t list [@@deriving show, eq, ord]

(* Simplified version of Taint.source_to_sink meant for finding reporting *)
type call_trace =
  (* A direct match *)
  | Toks of pattern_match_tokens
  (* An indirect match through a function call *)
  | Call of {
      call_toks : pattern_match_tokens;
      intermediate_vars : tainted_tokens;
      call_trace : call_trace;
    }
[@@deriving show, eq, ord]

(* The trace of a single source of taint, to the sink.
   There may be many of these, taking different paths. For a single
   sink, the fact that it produces a finding might be the product of
   many taints, due to labels.
   These taints may also take their own paths, because they might arrive
   via different variables.
*)
type item = {
  source_trace : call_trace;
      (** This is the path that the taint takes, from the source, to get to
        the current function in which the taint finding is reported. *)
  tokens : tainted_tokens;
      (** This is the path taken within the current function, to link the
        taint source obtained earlier with a sink. Both of these might
        be done through a chain of function calls. *)
  sink_trace : call_trace;
      (** This is the path that the taint takes, from the function context,
        to get to the sink. *)
}
[@@deriving show, eq, ord]

type t = item list [@@deriving show, eq, ord]
