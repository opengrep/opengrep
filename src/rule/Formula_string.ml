(* Opengrep authors
 *
 * Copyright (C) 2026 Opengrep authors
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

module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The strings under the pattern keys of a rule, ordered as pysemgrep's
 * rule.py formula_string() orders them. This is the rule part of the
 * match-based id (the "fingerprint" of a finding), which must stay equal to
 * the one the Python wrapper computed.
 *
 * pysemgrep walks the loaded YAML: a string counts, a mapping contributes
 * its non-null values, a sequence its elements, and any other scalar
 * (boolean, number, null in a sequence) raises ValueError, which makes the
 * whole formula string empty. Each level joins its parts sorted, with one
 * space.
 *)

(*****************************************************************************)
(* Implementation *)
(*****************************************************************************)

(* rule_lang.py RuleValidation.PATTERN_KEYS *)
let pattern_keys : string list =
  [
    "join";
    "match";
    "pattern";
    "pattern-either";
    "pattern-regex";
    "pattern-sinks";
    "pattern-sources";
    "patterns";
    "postprocessor-patterns";
    "request";
    "response";
    "taint";
  ]

exception Not_a_string

let join (xs : string list) : string =
  xs |> List.sort String.compare |> String.concat " "

(* the fields of a YAML mapping, without the null-valued ones *)
let fields_of_dict (fields : G.expr list) : (string * G.expr) list =
  fields
  |> List.filter_map (fun (field : G.expr) ->
         match field.G.e with
         | G.Container
             (G.Tuple, (_, [ { e = G.L (G.String (_, (key, _), _)); _ }; value ], _))
           -> (
             match value.G.e with
             | G.L (G.Null _) -> None
             | _ -> Some (key, value))
         | _ -> raise Not_a_string)

(* rule.py get_subrules(). An unquoted metavariable name is an identifier
 * in the generic AST of a rule file; pysemgrep sees it as a string. *)
let rec subrules (e : G.expr) : string =
  match e.G.e with
  | G.L (G.String (_, (s, _), _))
  | G.N (G.Id ((s, _), _)) ->
      s
  | G.Container (G.Dict, (_, fields, _)) ->
      fields_of_dict fields |> List.map (fun (_, value) -> subrules value) |> join
  | G.Container (G.Array, (_, xs, _)) -> xs |> List.map subrules |> join
  | G.Alias (_, e) -> subrules e
  | _ -> raise Not_a_string

let of_rule (rule : G.expr) : string =
  match rule.G.e with
  | G.Container (G.Dict, (_, fields, _)) -> (
      try
        fields_of_dict fields
        |> List.filter_map (fun (key, value) ->
               if List.mem key pattern_keys then Some (subrules value) else None)
        |> join
      with
      | Not_a_string -> "")
  | _ -> ""
