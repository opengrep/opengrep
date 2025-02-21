(* Yoann Padioleau, Martin Jambon
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module Log = Log_process_limits.Log
module M = Memprof_limits

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* A timeout exception with accompanying debug information:
   - a descriptive name
   - the time limit
     The mli interface makes this type private to help prevent unsafe uses of
     the exception. The type is actually defined in the commons compilation
     unit to allow logging to not treat it a an error.
*)
type timeout_info = Exception.timeout_info

exception Timeout = Exception.Timeout

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let string_of_timeout_info { Exception.name; max_duration } =
  spf "%s:%g" name max_duration

(* could be in Control section *)

(* TODO: Limit by allocations. *)
(* XXX: The sleeping threads delays the whole program! Hence the incremental sleeping,
 * and the approach of stopping the sleeping thread when the work is done. Ideally,
 * the granularity parameter should be set also by CLI flag. *)
let set_timeout (_caps : < Cap.time_limit >) ?(granularity_float_s=0.1) ~name max_duration f =
  let token = M.Token.create () in
  (* TODO:
   * - Is there a more efficient way do this?
   * - Should we calculate actual elapsed time? *)
  let finished_work = Atomic.make false in
  let rec timeout duration =
    if Atomic.get finished_work then ()
    else if duration <= 0.0 then M.Token.set token 
    else (
      Thread.delay granularity_float_s;
      timeout (duration -. granularity_float_s)
    )
  in
  ignore (Thread.create timeout max_duration);
  match M.limit_with_token ~token
          (fun () -> Fun.protect ~finally:(fun () -> Atomic.set finished_work true) f) with
  | Ok res -> Some res
  | Error _exn -> 
      Log.warn (fun m -> m "%S timeout at %g s (we abort)" name max_duration);
      None
  | exception exn ->
      let e = Exception.catch exn in
      Log.err (fun m -> m "exn while in set_timeout");
      Exception.reraise e

let set_timeout_opt ?(granularity_float_s=0.1) ~name time_limit f =
  match time_limit with
  | None -> Some (f ())
  | Some (x, caps) -> set_timeout caps ~granularity_float_s ~name x f
