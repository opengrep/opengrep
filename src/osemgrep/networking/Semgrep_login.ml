(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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
(* Types *)
(*****************************************************************************)

type shared_secret = Uuidm.t
type login_session = shared_secret * Uri.t

(*****************************************************************************)
(* Weak logged in check *)
(*****************************************************************************)

(* LATER: this does not really check the user is logged in; it just checks
 * whether a token is defined in ~/.semgrep/settings.yml.
 * In theory, we should actually authenticate this token and communicate with
 * the backend to double check (which could slow down the program startup).
 * In fact, some of us generated fake tokens in order to be able to
 * use --pro, especially in CI jobs that was suddenly breaking.
 * A bit like Microsoft back in the days, it is maybe better to not put too
 * strong "piracy" verification and allow users to cheat.
 * Note that Semgrep_settings can also get the token from the environment.
 * coupling: auth.is_logged_in_weak() in pysemgrep
 * TODO: should take Cap.network at least, so when we're ready to move
 * to an actual network (possibly cached) call, we are ready.
 *)
let is_logged_in_weak () =
  true

