(* Yoann Padioleau
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
open Fpath_.Operators

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*
   Sort targets by decreasing size. This is meant for optimizing
   CPU usage when processing targets in parallel on a fixed number of cores.
*)

let sort_targets_by_decreasing_size (targets : Target.t list) : Target.t list =
  targets
  |> List_.sort_by_key
       (fun target -> UFile.filesize (Target.internal_path target))
       (* Flip the comparison so we get descending,
        * instead of ascending, order *)
       (Fun.flip Int.compare)

let core_error_of_path_exc (internal_path : Fpath.t) (e : Exception.t) :
    Core_error.t =
  let exn = Exception.get_exn e in
  Logs.err (fun m ->
      m "exception on %s (%s)" !!internal_path (Printexc.to_string exn));
  Core_error.exn_to_error ~file:internal_path e

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Run jobs in parallel, using number of cores specified with -j *)
let map_targets caps (ncores : int)
    (f : Target.t -> 'a) (targets : Target.t list) :
    ('a, Target.t * Core_error.t) result list =
  (*
     Sorting the targets by decreasing size is based on the assumption
     that larger targets will take more time to process. Starting with
     the longer jobs allows parmap to feed the workers with shorter and
     shorter jobs, as a way of maximizing CPU usage.
     This is a kind of greedy algorithm, which is in general not optimal
     but hopefully good enough in practice.

     This is needed only when ncores > 1, but to reduce discrepancy between
     the two modes, we always sort the target queue in the same way.
  *)
  let targets = sort_targets_by_decreasing_size targets in

  (* Default to core_error and the target here since that's what's most
     usefule in Core_scan. Maybe we should instead pass this as a parameter? *)
  let exception_handler (x : Target.t) (e : Exception.t) :
      Target.t * Core_error.t =
    let internal_path = Target.internal_path x in
    (x, core_error_of_path_exc internal_path e)
  in

  (* old:
   *    if ncores <= 1 then List_.map (fun x -> Ok (f x)) targets else ( ... )
   * But this was wrong because 'f' can throw exns and so we would
   * get a different semantic when ncores > 1 where we capture exns hence
   * the use of wrap_result below.
   *)
  if ncores <= 1 then
    targets |> List_.map (fun x -> Domainslib_.wrap_result f ~exception_handler x)
  else (
    Logs.debug (fun m ->
        m "running in parallel with %d cores on %d targets" ncores
          (List.length targets));
    Domainslib_.parmap caps
      ~num_domains:ncores
      ~chunksize:1
      ~exception_handler
      f
      targets)
