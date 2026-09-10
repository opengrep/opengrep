(* Martin Jambon
 *
 * Copyright (C) 2021-2023 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Avoid segfaults when the process runs out of memory.

   See also https://gitlab.com/gadmm/memprof-limits/.

  NOTE: Be careful when adding logging, tracing, or other kinds of
  opentelmetry!!! This module uses a GC alarm to do its work, and our telemetry
  library can and will deadlock if it is called from the gc alarm. See the
  comment above the GC alarm function `limit_memory` for more info.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

exception ExceededMemoryLimit of string

module M = Memprof_limits

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*
   Fail gracefully if memory becomes insufficient.

   It raises Out_of_memory if we're over the memory limit at the end of a
   major GC cycle.

   See https://discuss.ocaml.org/t/todays-trick-memory-limits-with-gc-alarms/4431
   for detailed explanations.

   NOTE: caveat, from the above link:
   This is not reliable with multi-threaded programs, because we do not know in
   which thread the exception must be raised.

   So maybe try to adapt so it uses memprof-limits instead.

*)

(* We should not pass the limit as parameter because it's global. *)
let set_global_memory_limit_mb mem_limit_mb =
  (* NOTE: We get 0 from somewhere... memprof-limits takes -1 to mean 'no limit.' *)
  let mem_limit_mb = if Int.equal mem_limit_mb 0 then -1 else mem_limit_mb in
  M.set_global_memory_limit (mem_limit_mb * 1024) 

(* TODO[Issue #132]: The [mem_limit_mb] is global, and should not be a parameter,
 * but the whole thing is a mess and it requires too many changes right now.
 * In any case, the value does not change per invocation. *)
let run_with_global_memory_limit _caps ?get_context ~mem_limit_mb f =
  match mem_limit_mb with
  | 0 -> f ()
  | _ ->
    (* NOTE: Known race condition... see comment above. *)
    set_global_memory_limit_mb mem_limit_mb;
    let context () =
      match get_context with
      | None -> ""
      | Some get_context ->
          let context_str = get_context () in
          spf "[%s] " context_str
    in
    match M.limit_global_memory f with
    | Ok res -> res
    | Error _exn ->
        Logs.err (fun m ->
            m "%sexceeded heap memory limit of %d MiB" (context ()) mem_limit_mb);
        Gc.compact ();
        raise (ExceededMemoryLimit (spf "Exceeded memory limit of %d MiB" mem_limit_mb))
    | exception (Out_of_memory as exn) ->
        let e = Exception.catch exn in
        Logs.err (fun m -> m "exn while in run_with_global_memory_limit");
        (* Try to free up some space. Expensive operation. *)
        Gc.compact ();
        Exception.reraise e
    | exception exn ->
        let e = Exception.catch exn in
        Logs.err (fun m -> m "exn while in run_with_global_memory_limit");
        (* Any other exception raised by [f], such as a rule timeout, is
         * re-raised without compacting. *)
        Exception.reraise e
