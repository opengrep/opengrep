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

type t = {
  guard_atoms : Effect_guard.atoms;
  constructor_envs : (string, Taint_lval_env.t) Hashtbl.t;
}

let create (guard_atoms : Effect_guard.atoms) : t =
  { guard_atoms; constructor_envs = Hashtbl.create 16 }
