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

type atom_facts

type hcond = private {
  node : IL.exp;
  hash : int;
  id : int;
  mutable facts : atom_facts option;
}

type literal = private {
  atom : hcond;
  negated : bool;
}

type clause = literal list
type cond = private clause list

type t = {
  cond : cond;
  param_refs : (IL.name * int) list;
}

type atoms

val create_atoms : unit -> atoms
val is_length_atom : IL.exp -> bool
val compare_cond : cond -> cond -> int
val cond_is_top : cond -> bool
val cond_is_bot : cond -> bool
val of_exp : lang:Lang.t -> atoms -> IL.exp -> cond
val drop_frozen_literals : IL.param list -> cond -> cond
val atoms_of_cond : cond -> IL.exp list
val map_atoms :
  lang:Lang.t -> atoms -> IL.name list -> (IL.exp -> IL.exp) -> cond -> cond
val eval : cond -> bool option
val raw_clauses : cond -> (IL.exp * bool) list list
val literals_consistent : (IL.exp * bool) list -> bool
val top : t
val is_top : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int
val show : ?truncate_guards:bool -> t -> string
val cond_vars : t -> IL.name list
val show_in_brackets : ?truncate_guards:bool -> t -> string

module Set : Stdlib.Set.S with type elt = t

val show_set : ?truncate_guards:bool -> Set.t -> string

val merge_param_refs :
  (IL.name * int) list -> (IL.name * int) list -> (IL.name * int) list

val compose_and : t -> t -> t
val compose_or : t -> t -> t
val conjoin : t list -> t
val of_branch_cond :
  lang:Lang.t -> atoms -> negated:bool -> IL.param list -> IL.exp -> t list
