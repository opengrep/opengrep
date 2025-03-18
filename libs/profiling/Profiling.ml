(* Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pad's poor's man profiler. See pfff's Main.ml for example of use
 * and the -profile command-line flag.
 *
 * You should probably rely on ocamlprof, perf, memprof, and the
 * many other OCaml profiling tools.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type prof = ProfAll | ProfNone | ProfSome of string list

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let profile = ref ProfNone
let show_trace_profile = ref false

let check_profile category =
  match !profile with
  | ProfAll -> true
  | ProfNone -> false
  | ProfSome l -> List.mem category l

let _profile_table = Kcas_data.Hashtbl.create () (* 100 *)

let adjust_profile_entry category difftime =
  let open Kcas in
  let open Kcas_data in
  Xt.commit {tx = (fun ~xt ->
    match Hashtbl.Xt.find_opt ~xt _profile_table category with
    | Some (xtime, xcount) ->
        Xt.modify ~xt xtime (fun xtime -> xtime +. difftime);
        Xt.incr ~xt xcount
    | None ->
        let xtime = Loc.make 0.0 in
        let xcount = Loc.make 0 in
        Hashtbl.Xt.add ~xt _profile_table category (xtime, xcount))}

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* subtle: don't forget to give all argumens to f, otherwise partial app
 * and will profile nothing.
 *
 * todo: try also detect when complexity augment each time, so can
 * detect the situation for a function gets worse and worse ?
 *)
let profile_code category f =
  if not (check_profile category) then f ()
  else (
    if !show_trace_profile then Logs.debug (fun m -> m "> %s" category);
    let t = Unix.gettimeofday () in
    let res, prefix =
      try (Ok (f ()), "") with
      (*TODO: Timeout _ as*)
      | exn ->
          let e = Exception.catch exn in
          (Error e, "*")
    in
    let category = prefix ^ category in
    (* add a '*' to indicate timeout func *)
    let t' = Unix.gettimeofday () in

    if !show_trace_profile then Logs.debug (fun m -> m "< %s" category);

    adjust_profile_entry category (t' -. t);
    match res with
    | Ok res -> res
    | Error e -> Exception.reraise e)

(*****************************************************************************)
(* Diagnostic *)
(*****************************************************************************)

(* todo: also put  % ? also add % to see if coherent numbers *)
let profile_diagnostic () : string =
  if !profile =*= ProfNone then ""
  else
    let xs =
      Kcas_data.Hashtbl.fold
        (fun k (t, n) acc ->
           (k, ((Kcas.Loc.get t), (Kcas.Loc.get n))) :: acc) _profile_table []
      |> List.sort (fun (_k1, (t1, _n1)) (_k2, (t2, _n2)) -> compare t2 t1)
    in
    let max_key_len = List.fold_left (fun acc (k, _) -> max acc (String.length k)) 0 xs in
    Buffer_.with_buffer_to_string (fun buf ->
        let prf fmt = Printf.bprintf buf fmt in
        prf "\n";
        prf "---------------------\n";
        prf "profiling result\n";
        prf "---------------------\n";
        xs
        |> List.iter (fun (k, (t, n)) ->
               prf "%-*s : %10.3f sec %10d count\n" max_key_len k t n))

let warn_if_take_time timethreshold s f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t' = Unix.gettimeofday () in
  if t' -. t > float_of_int timethreshold then
    Logs.warn (fun m -> m "processing took %7.1fs: %s" (t' -. t) s);
  res

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let profile_code2 category f =
  profile_code category (fun () ->
      if !profile =*= ProfAll then
        Logs.info (fun m -> m "starting: %s" category);
      let t = Unix.gettimeofday () in
      let res = f () in
      let t' = Unix.gettimeofday () in
      if !profile =*= ProfAll then
        Logs.info (fun m -> m "ending: %s, %fs" category (t' -. t));
      res)

(*****************************************************************************)
(* Init *)
(*****************************************************************************)
let flags () =
  [
    ( "-profile",
      Arg.Unit (fun () -> profile := ProfAll),
      " output profiling information" );
    ("-show_trace_profile", Arg.Set show_trace_profile, " show trace");
  ]

let log_diagnostics_and_gc_stats () =
  Logs.warn (fun m -> m "%s" (profile_diagnostic ()));
  Gc.print_stat stderr

(* ugly *)
let _ =
  UCommon.before_exit :=
    (fun () -> if !profile <> ProfNone then log_diagnostics_and_gc_stats ())
    :: !UCommon.before_exit
