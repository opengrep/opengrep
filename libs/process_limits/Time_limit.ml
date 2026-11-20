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
module Log = Log_process_limits.Log
module M = Memprof_limits

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* could be in Control section *)

(* Seconds on the monotonic clock: a step of the system clock does not move a
   deadline. *)
let now () : float = Int64.to_float (Mtime_clock.now_ns ()) *. 1e-9

(* TODO: Limit by allocations. *)
(* XXX: The sleeping threads delays the whole program! Hence the incremental sleeping,
 * and the approach of stopping the sleeping thread when the work is done. Ideally,
 * the granularity parameter should be set also by CLI flag. *)
let set_timeout (_caps : < Cap.time_limit >) ?(granularity_float_s=0.1) ~name max_duration f =
  let token = M.Token.create () in
  (* - Is there a more efficient way do this? *)
  let finished_work = Atomic.make false in
  let deadline = now () +. max_duration in
  let rec timeout () =
    if Atomic.get finished_work then ()
    else
      let left = deadline -. now () in
      if left <= 0.0 then M.Token.set token
      else (
        Thread.delay (Float.min granularity_float_s left);
        timeout ())
  in
  (* TODO: Investigate why too many threads are created, to the point of failing
   * to create more. *)
  ignore (Thread.create timeout ());
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

(* The tasks of a rule now running, the time their current busy period
   began, and the length of the busy periods that ended. *)
type usage = { running : int; busy_since : float; used : float }

type shared_limit = { limit : float; usage : usage Atomic.t }

let create_shared_limit (limit : float) : shared_limit =
  { limit; usage = Atomic.make { running = 0; busy_since = 0.; used = 0. } }

let rec update (cell : 'a Atomic.t) (f : 'a -> 'a * 'b) : 'b =
  let old = Atomic.get cell in
  let next, result = f old in
  if Atomic.compare_and_set cell old next then result else update cell f

(* The length of the union of the running intervals up to [now]. *)
let used_at (now : float) (u : usage) : float =
  if u.running > 0 then u.used +. (now -. u.busy_since) else u.used

(* A task started at [s] with [r = limit - used s] stops by [s + r]: while
   tasks run, [used] grows at wall-clock rate, so the union of the running
   intervals stays within [limit], up to the delay of the timer's wake-up
   that crosses the deadline and the time to the task's next sampled
   allocation. *)
let with_shared_limit (caps : < Cap.time_limit >) ~(name : string)
    (shared : shared_limit) (f : unit -> 'a) : 'a option =
  let remaining =
    update shared.usage (fun (u : usage) ->
        let now = now () in
        let remaining = shared.limit -. used_at now u in
        if remaining <= 0. then (u, remaining)
        else
          ( { u with
              running = u.running + 1;
              busy_since = (if Int.equal u.running 0 then now else u.busy_since);
            },
            remaining ))
  in
  if remaining <= 0. then None
  else
    Fun.protect
      ~finally:(fun () ->
        update shared.usage (fun (u : usage) ->
            let now = now () in
            let running = u.running - 1 in
            ( (if Int.equal running 0 then
                 { running; busy_since = 0.; used = u.used +. (now -. u.busy_since) }
               else { u with running }),
              () )))
      (fun () ->
        (* The task may have waited since it was admitted: its timer gets the
           time left now. *)
        let remaining =
          shared.limit -. used_at (now ()) (Atomic.get shared.usage)
        in
        if remaining <= 0. then None else set_timeout caps ~name remaining f)
