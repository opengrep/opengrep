(* Yoann Padioleau
 *
 * Copyright (C) 1998-2024 Yoann Padioleau
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
module Log = Log_commons.Log

(*###########################################################################*)
(* Prelude *)
(*###########################################################################*)

(* if set, certain functions like unwind_protect will not
 * do a try and finalize and instead just call the function, which
 * helps in ocamldebug and also in getting better backtraces.
 * This is also useful to set in a js_of_ocaml (jsoo) context to
 * again get better backtraces.
 *)
let debugger = ref false

(*****************************************************************************)
(* Circular dependencies *)
(*****************************************************************************)

(* The following functions should be in their respective sections but
 * because some functions in some sections use functions in other
 * sections, and because I don't want to take care of the order of
 * those sections (of those dependencies), I put the functions causing
 * dependency problem here. C is better than OCaml on this with the
 * ability to declare prototypes, enabling some form of forward
 * references.
 *)

let spf = Printf.sprintf

(* alt: We tried using 'Unix.sigprocmask' to temporarily block 'SIGALRM' but
 * somehow, in rare cases (e.g.run p/default on repos/brotli/js/decode.js) we
 * end up calling 'Time_limit.set_timeout' while 'SIGALRM' is *blocked*.
 * Unclear why, are we perhaps calling 'set_timeout' from within a
 * 'finally'? Or is 'Unix.sigprocmask' failing to restore the signal mask?
 * It works if we block/unblock (rather than block/restore) but this does not
 * play well with calls to 'protect' nested inside 'finally' blocks.
 *)
let protect ~finally work =
  (* nosemgrep: no-fun-protect *)
  try Fun.protect ~finally work with
  | Fun.Finally_raised exn1 as exn ->
      (* Just re-raise whatever exception was raised during a 'finally',
       * drop 'Finally_raised'.
       *)
      Log.err (fun m ->
          m "protect: %s" (exn |> Exception.catch |> Exception.to_string));
      Exception.catch_and_reraise exn1

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

include Eq.Operators

let phys_equal = Eq.phys_equal
let phys_not_equal = Eq.phys_not_equal

(* Used to give choice whether id_info fields should be checked in semgrep *)
let equal_ref_option equal_f a b =
  match (!a, !b) with
  | None, None -> true
  | Some a, Some b -> equal_f a b
  | Some _, None
  | None, Some _ ->
      false

(*****************************************************************************)
(* Disable physical equality/inequality operators *)
(*****************************************************************************)
(* now in Eq.ml *)

(*****************************************************************************)
(* Comparison *)
(*****************************************************************************)
(* now in Ord.ml *)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)
(* to be used in pipe operations *)
let before_return f v =
  f v;
  v

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)
(* see also profiling/Profiling.ml now *)

(* Report the time a function takes. *)
let with_time f =
  let t1 = UUnix.gettimeofday () in
  let res = f () in
  let t2 = UUnix.gettimeofday () in
  (res, t2 -. t1)

(*****************************************************************************)
(* Exn *)
(*****************************************************************************)

(* See also Common.protect earlier *)

(* use Common.protect instead? *)
let unwind_protect f cleanup =
  if !debugger then f ()
  else
    try f () with
    | exn ->
        let e = Exception.catch exn in
        cleanup e;
        Exception.reraise e

(* TODO: remove and use 'protect' instead? but then it will not have the
 * !debugger goodies *)
let finalize f cleanup =
  (* Does this debugger mode changes the semantic of the program too much?
   * Is it ok in a debugging context to not call certain cleanup()
   * and let the exn bubble up?
   * TODO: maybe I should not use save_excursion/finalize so much? maybe
   *  -debugger can help to see code that I should refactor?
   *)
  if !debugger then (
    let res = f () in
    cleanup ();
    res)
  else protect f ~finally:cleanup

(* This is everywhere, and it's not thread-safe when the ref is non-local.
 * It's best to force this to use DLS to be safe. We already don't interleave tasks
 * on each domain at any time. But sometimes it's used for local variables and we
 * should not pay the cost in these cases (see Visit_rule), so we need 2 versions
 * of this function, and we must decide on each call site which one to use. *)
let save_excursion reference newv f =
  let module DLS = Domain.DLS in
  let old = DLS.get reference in
  DLS.set reference newv;
  finalize f (fun _ -> DLS.set reference old)

(* The original non-thread-safe version. *)
let save_excursion_unsafe reference newv f =
  let old = !reference in
  reference := newv;
  finalize f (fun _ -> reference := old)

let memoized ?(use_cache = true) h k f =
  if not use_cache then f ()
  else
    (* NOTE: In a parallel scenario, we can of course run [f ()] more than once,
     * so it's better to make sure that any [f] passed has only tolerable side
     * effects (like reading a file we know should be there) and always returns
     * the same result or raises the same exception. We could propably use a
     * [Kcas] transaction if we wished to ensure that replacement of existing
     * values does not happen, but I don't see the benefit for our purposes. *)
    match Kcas_data.Hashtbl.find_opt h k with
    | Some v -> v
    | None ->
        let v = f () in
        (* XXX: Thread Sanitizer still reports innocent (?) races with [Kcas]. *)
        Kcas_data.Hashtbl.replace h k v;
        v

(* Because memoize does not create the hashtable itself, but uses an existing one,
 * when we have a domain-local hashtable, it will be safe to use in parallel. *)
let memoized_not_thread_safe ?(use_cache = true) h k f =
  if not use_cache then f ()
  else
    match Hashtbl.find_opt h k with
    | Some v -> v
    | None ->
        let v = f () in
        Hashtbl.replace h k v;
        v

exception Todo
exception Impossible
exception Multi_found (* to be consistent with Not_found *)
exception UnixExit of int
exception ErrorOnFile of string * Fpath.t

let exn_to_s exn = Printexc.to_string exn

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

let compose f g x = f (g x)
(* does not work :( let ( rond_utf_symbol ) f g x = f(g(x)) *)

(* trick to have something similar to the   1 `max` 4   haskell infix notation.
   by Keisuke Nakano on the caml mailing list.
   >    let ( /* ) x y = y x
   >    and ( */ ) x y = x y
   or
   let ( <| ) x y = y x
   and ( |> ) x y = x y

   > Then we can make an infix operator <| f |> for a binary function f.
*)

let flip f a b = f b a
let curry f x y = f (x, y)
let uncurry f (a, b) = f a b
let const x _y = x
let do_nothing () = ()
let rec applyn n f o = if n =|= 0 then o else applyn (n - 1) f (f o)

(* I think Brandon added that, not sure where it comes from *)
let on g f x y = g (f x) (f y)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(*****************************************************************************)
(* Arguments/options and command line *)
(*****************************************************************************)
(* use Cmdliner, or Arg_helpers if you really have to *)

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)
(* See Cmdliner now *)

(*****************************************************************************)
(* Test *)
(*****************************************************************************)
(* See Testo. and Testutil_* modules *)

(*###########################################################################*)
(* Basic types *)
(*###########################################################################*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

(*****************************************************************************)
(* Option *)
(*****************************************************************************)

let ( let* ) = Option.bind

(*
 (*http://roscidus.com/blog/blog/2013/10/13/ocaml-tips/#handling-option-types*)
  let (|?) maybe default =
    match maybe with
    | Some v -> v
    | None -> Lazy.force default
*)

(* not sure why but can't use let (?:) a b = ... then at use time ocaml yells *)
let ( ||| ) a b =
  match a with
  | Some x -> x
  | None -> b

(*****************************************************************************)
(* Result *)
(*****************************************************************************)
let ( let/ ) = Result.bind

(*****************************************************************************)
(* Either *)
(*****************************************************************************)
(* now in Either_.ml *)

(*****************************************************************************)
(* Regexp, can also use PCRE *)
(*****************************************************************************)

(* NOTE: This is used from visitors concurrently I think, is [Str.matched_group]
 * thread-safe? No, but it is domain-safe and we use 1 thread per domain. *)
let (matched : int -> string -> string) = fun i s -> Str.matched_group i s
let matched1 s = matched 1 s
let matched2 s = (matched 1 s, matched 2 s)
let matched3 s = (matched 1 s, matched 2 s, matched 3 s)
let matched4 s = (matched 1 s, matched 2 s, matched 3 s, matched 4 s)

let matched5 s =
  (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)

let matched6 s =
  (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)

let matched7 s =
  ( matched 1 s,
    matched 2 s,
    matched 3 s,
    matched 4 s,
    matched 5 s,
    matched 6 s,
    matched 7 s )

module DLS = Domain.DLS

let _memo_compiled_regexp = DLS.new_key (fun () -> Hashtbl.create 101)

let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    (* NOTE: The internal state of matches is in DLS, but we are sharing
     * here between domains also. Maybe make key domain local? [re ^ domain-id]. *)
    memoized_not_thread_safe (DLS.get _memo_compiled_regexp)
      re
      (* (re ^ "-" ^ string_of_int ((Domain.self ()) :> int)) *)
      (fun () -> Str.regexp re)
  in
  (* Is that ok in a parallel scenario? If we use more than one Thread per domain,
   * or if we use effects (for example nested parallelism using domainslib) then
   * this can corrupt the [last_search_result_key] of another task, leading to
   * unpleasant surprises... But now we use a simple parallel-map style function
   * that invokes domainslib, so it's ok. *)
  Str.string_match compile_re s 0

let match_func s re = candidate_match_func s re
let ( =~ ) s re = match_func s re

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

(* ruby *)
let i_to_s = string_of_int
let s_to_i = int_of_string

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

(*
   Input a line of text from a file in a way that works for Windows files
   or Unix files in any mode on any platform. Line terminators are eaten
   up.

   Known bug: CR characters preceding a CRLF sequence are eaten up anyway
   in text mode on Windows. Workaround: open the file in binary mode.

   Unix file on Unix:    "a\nb"   -> "a"
   Windows file on Unix: "a\r\nb" -> "a"  (* input_line returns "a\r" *)
   Unix file on Windows text mode:      "a\nb" -> "a"
   Unix file on Windows binary mode:    "a\nb" -> "a"
   Windows file on Windows text mode:   "a\r\nb" -> "a"
   Windows file on Windows binary mode: "a\r\nb" -> "a"

   What you need to know about Windows vs. Unix line endings
   =========================================================

   CR designates the byte '\r', LF designates '\n'.
   CRLF designates the sequence "\r\n".

   On Windows, there are two modes for opening a file for reading or writing:
   - binary mode, which is the same as on Unix.
   - text mode, which is special.

   A file opened in text mode on Windows causes the reads and the writes
   to translate line endings:
   - read from text file: CRLF -> LF
   - write to text file: LF -> CRLF

   To avoid these translations when running on Windows, the file must be
   opened in binary mode.

   Q: When must a file be opened in text mode?
   A: Never. Only stdin, stdout, and stderr connected to a console should
      be in text mode but the OS takes care of this for us.

   Q: Really never?
   A: Never for reading. For writing, maybe some Windows applications behave
      so badly that they can't recognize single LFs, in which case local logs
      and such might have to be opened in text mode. See next question.

   Q: Do all text processing applications on Windows support LF line endings
      in input files?
   A: Hopefully. They really should. The only way they don't support single LFs
      is if they open files in binary mode and then search for CRLF
      sequences as line terminators exclusively, which would be strange.

   Q: Do the parsers in pfff support all line endings?
   A: Yes unless there's a bug. All parsers for real programming languages
      should use the pattern '\r'?'\n' to match line endings.
*)
let input_text_line ic =
  let s = input_line ic in
  String_.trim_cr s

(*###########################################################################*)
(* Containers *)
(*###########################################################################*)

(*****************************************************************************)
(* List *)
(*****************************************************************************)
(* Now in List_.ml *)

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
(* Now in Assoc.ml *)

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

(*****************************************************************************)
(* Matrix *)
(*****************************************************************************)

(*****************************************************************************)
(* Set *)
(*****************************************************************************)
(* Now in Set_.ml *)

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)
(* Now in Hashtbl_.ml *)

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)
(* Now in Stack_.ml *)

(*****************************************************************************)
(* Tree *)
(*****************************************************************************)

(*****************************************************************************)
(* Maps *)
(*****************************************************************************)

module SMap = Map.Make (String)

type 'a smap = 'a SMap.t

(*****************************************************************************)
(* Graph *)
(*****************************************************************************)
(* Now in Ograph_*.ml *)

(*###########################################################################*)
(* Postlude *)
(*###########################################################################*)

(*****************************************************************************)
(* Operators *)
(*****************************************************************************)

module Operators = struct
  let ( =~ ) = ( =~ )
  let ( = ) = ( = ) (* already shadowed *)
  let ( =|= ) = ( =|= )
  let ( =$= ) = ( =$= )
  let ( =:= ) = ( =:= )
  let ( =*= ) = ( =*= )
  let ( == ) = ( == ) (* already shadowed *)
  let ( != ) = ( != ) (* already shadowed *)
end
