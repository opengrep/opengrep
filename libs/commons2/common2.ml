(* Yoann Padioleau
 *
 * Copyright (C) 1998-2009 Yoann Padioleau
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

(*###########################################################################*)
(* Prelude *)
(*###########################################################################*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The following functions should be in their respective sections but
 * because some functions in some sections use functions in other
 * sections, and because I don't want to take care of the order of
 * those sections, of those dependencies, I put the functions causing
 * dependency problem here. C is better than caml on this with the
 * ability to declare prototype, enabling some form of forward
 * reference.
 *)

let rec (do_n : int -> (unit -> unit) -> unit) =
 fun i f ->
  if i =|= 0 then ()
  else (
    f ();
    do_n (i - 1) f)

let rec (foldn : ('a -> int -> 'a) -> 'a -> int -> 'a) =
 fun f acc i -> if i =|= 0 then acc else foldn f (f acc i) (i - 1)

let sum_int = List.fold_left ( + ) 0

(* could really call it 'for' :) *)
let fold_left_with_index f acc =
  let rec fold_lwi_aux acc n = function
    | [] -> acc
    | x :: xs -> fold_lwi_aux (f acc x n) (n + 1) xs
  in
  fold_lwi_aux acc 0

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let rec drop n xs =
  match (n, xs) with
  | 0, _ -> xs
  | _, [] -> failwith "drop: not enough"
  | n, _x :: xs -> drop (n - 1) xs

let exclude p xs = List.filter (fun x -> not (p x)) xs

(*let last l = List_.hd_exn "unexpected empty list" (last_n 1 l) *)
let rec list_last = function
  | [] -> raise Not_found
  | [ x ] -> x
  | _x :: y :: xs -> list_last (y :: xs)

let (list_of_string : string -> char list) = function
  | "" -> []
  | s -> List_.enum 0 (String.length s - 1) |> List_.map (String.get s)

let (lines : string -> string list) =
 fun s ->
  let rec lines_aux = function
    | [] -> []
    | [ x ] -> if x = "" then [] else [ x ]
    | x :: xs -> x :: lines_aux xs
  in
  Str.split_delim (Str.regexp "\r\n\\|\n") s |> lines_aux

let (matched : int -> string -> string) = fun i s -> Str.matched_group i s

let foldl1 p xs =
  match xs with
  | x :: xs -> List.fold_left p x xs
  | [] -> failwith "foldl1: empty list"

let repeat e n =
  let rec repeat_aux acc = function
    | 0 -> acc
    | n when n < 0 -> failwith "repeat"
    | n -> repeat_aux (e :: acc) (n - 1)
  in
  repeat_aux [] n

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

(* I used this in coccinelle where the huge logging of stuff ask for
 * a more organized solution that use more visual indentation hints.
 *
 * todo? could maybe use log4j instead ? or use Format module more
 * consistently ?
 *)

let _tab_level_print = ref 0
let _prefix_pr = ref ""

let _chan_pr2 = ref (None : out_channel option)

let out_chan_pr2 ?(newline = true) s =
  match !_chan_pr2 with
  | None -> ()
  | Some chan ->
      output_string chan (s ^ if newline then "\n" else "");
      flush chan

let pr2 s =
  UStdlib.prerr_string !_prefix_pr;
  do_n !_tab_level_print (fun () -> UStdlib.prerr_string " ");
  UStdlib.prerr_string s;
  UStdlib.prerr_string "\n";
  flush UStdlib.stderr;
  out_chan_pr2 s;
  ()

(* old:
 * let pr s = (print_string s; print_string "\n"; flush stdout)
 * let pr2 s = (prerr_string s; prerr_string "\n"; flush stderr)
 *)

(* ---------------------------------------------------------------------- *)

(* I can not use the _xxx ref tech that I use for common_extra.ml here because
 * ocaml don't like the polymorphism of Dumper mixed with refs.
 *
 * let (_dump_func : ('a -> string) ref) = ref
 * (fun x -> failwith "no dump yet, have you included common_extra.cmo?")
 * let (dump : 'a -> string) = fun x ->
 * !_dump_func x
 *
 * So I have included directly dumper.ml in common.ml. It's more practical
 * when want to give script that use my common.ml, I just have to give
 * this file.
 *)

(* ---------------------------------------------------------------------- *)
let pr2_gen x = pr2 (Dumper.dump x)

(* ---------------------------------------------------------------------- *)
let xxx_once f s =
  match () with
  | _ when !UCommon.disable_pr2_once ->
      (* nosemgrep: no-pr2 *)
      UCommon.pr2 s
  | _ when Saturn.Htbl.try_add UCommon._already_printed s true ->
      f ("(ONCE) " ^ s)
  | _else_ -> ()

let pr2_once s = xxx_once pr2 s

(* ---------------------------------------------------------------------- *)
let mk_pr2_wrappers aref =
  let fpr2 s =
    if (Domain.DLS.get aref) then pr2 s else (* just to the log file *)
                          out_chan_pr2 s
  in
  let fpr2_once s = if (Domain.DLS.get aref) then pr2_once s else xxx_once out_chan_pr2 s in
  (fpr2, fpr2_once)

(* ---------------------------------------------------------------------- *)
(* could also be in File section *)

(* ---------------------------------------------------------------------- *)

(* old: include Printf, include are evil and graph_code_cmt does not like them*)
(* cf common.mli, fprintf, printf, eprintf, sprintf.
 * also what is this ?
 *  val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
 *  val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
 *)

(* ex of printf:
 *  printf "%02d" i
 * for padding
 *)

let spf = Printf.sprintf

(* ---------------------------------------------------------------------- *)

let _chan = UStdlib.stderr

let dolog s =
  output_string _chan (s ^ "\n");
  flush _chan

let verbose_level = ref 1
let log s = if !verbose_level >= 1 then dolog s

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

(* now near cmd_to_list: let get_mem() = *)

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

(* See also OUnit *)

(* commented because does not play well with js_of_ocaml
*)
(* let example b =
     if b then () else failwith ("ASSERT FAILURE: " ^ Printexc.get_backtrace ()) *)

(* let _ex1 = assert (List_.enum 1 4 =*= [ 1; 2; 3; 4 ]) *)

let assert_equal a b =
  if not (a =*= b) then
    failwith
      ("assert_equal: those 2 values are not equal:\n\t" ^ Dumper.dump a
     ^ "\n\t" ^ Dumper.dump b ^ "\n")

(* let (example2 : string -> bool -> unit) =
    fun s b ->
     try assert b with
     | _x -> failwith s *)

(*-------------------------------------------------------------------*)
let _list_bool = ref []

let (example3 : string -> bool -> unit) =
 fun s b -> _list_bool := (s, b) :: !_list_bool

(* could introduce a fun () otherwise the calculus is made at compile time
 * and this can be long. This would require to redefine test_all.
 *   let (example3: string -> (unit -> bool) -> unit) = fun s func ->
 *   _list_bool := (s,func):: (!_list_bool)
 *
 * I would like to do as a func that take 2 terms, and make an = over it
 * avoid to add this ugly fun (), but pb of type, cant do that :(
 *)

let (test_all : unit -> unit) =
 fun () ->
  List.iter
    (fun (s, b) ->
      UPrintf.printf "%s: %s\n" s (if b then "passed" else "failed"))
    !_list_bool

let ( ++ ) a b = a @ b
let _ex = example3 "++" ([ 1; 2 ] @ [ 3; 4; 5 ] =*= [ 1; 2; 3; 4; 5 ])

(*-------------------------------------------------------------------*)
(* Regression testing *)
(*-------------------------------------------------------------------*)

(* cf end of file. It uses too many other common functions so I
 * have put the code at the end of this file.
 *)

(* todo? take code from julien signoles in calendar-2.0.2/tests *)
(*

(* Generic functions used in the tests. *)

val reset : unit -> unit
val nb_ok : unit -> int
val nb_bug : unit -> int
val test : bool -> string -> unit
val test_exn : 'a Lazy.t -> string -> unit


let ok_ref = ref 0
let ok () = incr ok_ref
let nb_ok () = !ok_ref

let bug_ref = ref 0
let bug () = incr bug_ref
let nb_bug () = !bug_ref

let reset () =
  ok_ref := 0;
  bug_ref := 0

let test x s =
  if x then ok () else begin UPrintf.printf "%s\n" s; bug () end;;

let test_exn x s =
  try
    ignore (Lazy.force x);
    UPrintf.printf "%s\n" s;
    bug ()
  with _ ->
    ok ();;
*)

(*****************************************************************************)
(* Quickcheck like (sfl) *)
(*****************************************************************************)

(* related work:
 *  - http://cedeela.fr/quickcheck-for-ocaml.html
 *)

(*---------------------------------------------------------------------------*)
(* generators *)
(*---------------------------------------------------------------------------*)
type 'a gen = unit -> 'a

let (ig : int gen) = fun () -> URandom.int 10

let (lg : 'a gen -> 'a list gen) =
 fun gen () -> foldn (fun acc _i -> gen () :: acc) [] (URandom.int 10)

let (pg : 'a gen -> 'b gen -> ('a * 'b) gen) =
 fun gen1 gen2 () -> (gen1 (), gen2 ())

let (oneofl : 'a list -> 'a gen) =
 fun xs () -> List.nth xs (URandom.int (List.length xs))
(* let oneofl l = oneof (List.map always l) *)

let (oneof : 'a gen list -> 'a gen) =
 fun xs -> List.nth xs (URandom.int (List.length xs))

let (always : 'a -> 'a gen) = fun e () -> e

let (frequency : (int * 'a gen) list -> 'a gen) =
 fun xs ->
  let sums = sum_int (List_.map fst xs) in
  let i = URandom.int sums in
  let rec freq_aux acc = function
    | (x, g) :: xs -> if i < acc + x then g else freq_aux (acc + x) xs
    | _ -> failwith "frequency"
  in
  freq_aux 0 xs

let frequencyl l = frequency (List_.map (fun (i, e) -> (i, always e)) l)

(*
let b = oneof [always true; always false] ()
let b = frequency [3, always true; 2, always false] ()
*)

(* cant do this:
 *    let rec (lg: ('a gen) -> ('a list) gen) = fun gen -> oneofl [[]; lg gen ()]
 * nor
 *    let rec (lg: ('a gen) -> ('a list) gen) = fun gen -> oneof [always []; lg gen]
 *
 * because caml is not as lazy as haskell :( fix the pb by introducing a size
 * limit. take the bounds/size as parameter. morover this is needed for
 * more complex type.
 *
 * how make a bintreeg ?? we need recursion
 *
 * let rec (bintreeg: ('a gen) -> ('a bintree) gen) = fun gen () ->
 * let rec aux n =
 * if n = 0 then (Leaf (gen ()))
 * else frequencyl [1, Leaf (gen ()); 4, Branch ((aux (n / 2)), aux (n / 2))]
 * ()
 * in aux 20
 *
 *)

(*---------------------------------------------------------------------------*)
(* property *)
(*---------------------------------------------------------------------------*)

(* todo: a test_all_laws, better syntax (done already a little with ig in
 * place of intg. En cas d'erreur, print the arg that not respect
 *
 * todo: with monitoring, as in haskell, laws = laws2, no need for 2 func,
 * but hard i found
 *
 * todo classify, collect, forall
 *)

(* return None when good, and Just the_problematic_case when bad *)
let (laws : string -> ('a -> bool) -> 'a gen -> 'a option) =
 fun _s func gen ->
  let res =
    foldn
      (fun acc _i ->
        let n = gen () in
        (n, func n) :: acc)
      [] 1000
  in
  let res = List.filter (fun (_x, b) -> not b) res in
  if res =*= [] then None
  else Some (fst (List_.hd_exn "unexpected empty list" res))

let rec (statistic_number : 'a list -> (int * 'a) list) = function
  | [] -> []
  | x :: xs ->
      let splitg, splitd = List.partition (fun y -> y =*= x) xs in
      (1 + List.length splitg, x) :: statistic_number splitd

(* in pourcentage *)
let (statistic : 'a list -> (int * 'a) list) =
 fun xs ->
  let stat_num = statistic_number xs in
  let totals = sum_int (List_.map fst stat_num) in
  List_.map (fun (i, v) -> (i * 100 / totals, v)) stat_num

let (laws2 :
      string -> ('a -> bool * 'b) -> 'a gen -> 'a option * (int * 'b) list) =
 fun _s func gen ->
  let res =
    foldn
      (fun acc _i ->
        let n = gen () in
        (n, func n) :: acc)
      [] 1000
  in
  let stat = statistic (List_.map (fun (_x, (_b, v)) -> v) res) in
  let res = List.filter (fun (_x, (b, _v)) -> not b) res in
  if res =*= [] then (None, stat)
  else (Some (fst (List_.hd_exn "unexpected empty list" res)), stat)

(* todo, do with coarbitrary ?? idea is that given a 'a, generate a 'b
 * depending of 'a and gen 'b, that is modify gen 'b, what is important is
 * that each time given the same 'a, we must get the same 'b !!!
 *)

(*
let (fg: ('a gen) -> ('b gen) -> ('a -> 'b) gen) = fun gen1 gen2 () ->
let b = laws "funs" (fun (f,g,h) -> x <= y ==> (max x y  = y)       )(pg ig ig)
 *)

(*
let one_of xs = List.nth xs (URandom.int (List.length xs))
let take_one xs =
  if empty xs then failwith "Take_one: empty list"
  else
    let i = URandom.int (List.length xs) in
    List.nth xs i, filter_index (fun j _ -> i <> j) xs
*)

type timestamp = int

(*****************************************************************************)
(* String_of *)
(*****************************************************************************)
(* To work with the macro system autogenerated string_of and print_ function
   (kind of deriving a la haskell) *)

(* int, bool, char, float, ref ?, string *)

let string_of_list f xs = "[" ^ (xs |> List_.map f |> String.concat ";") ^ "]"

let string_of_option f = function
  | None -> "None "
  | Some x -> "Some " ^ f x

(* specialised
   let (string_of_list: char list -> string) =
   List.fold_left (fun acc x -> acc^(Char.escaped x)) ""
*)

(* julia: convert something printed using format to print into a string *)
(* now at bottom of file
   let format_to_string f =
   ...
*)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

let ( $ ) f g x = g (f x)

(* TODO: Check non thread-safe use of references below. *)

(* finalize, cf prelude *)

(*****************************************************************************)
(* Concurrency *)
(*****************************************************************************)

(* from http://en.wikipedia.org/wiki/File_locking
 *
 * "When using file locks, care must be taken to ensure that operations
 * are atomic. When creating the lock, the process must verify that it
 * does not exist and then create it, but without allowing another
 * process the opportunity to create it in the meantime. Various
 * schemes are used to implement this, such as taking advantage of
 * system calls designed for this purpose (but such system calls are
 * not usually available to shell scripts) or by creating the lock file
 * under a temporary name and then attempting to move it into place."
 *
 * => can't use 'if(not (file_exist xxx)) then create_file xxx' because
 * file_exist/create_file are not in atomic section (classic problem).
 *
 * from man open:
 *
 * "O_EXCL When used with O_CREAT, if the file already exists it
 * is an error and the open() will fail. In this context, a
 * symbolic link exists, regardless of where it points to.
 * O_EXCL is broken on NFS file systems; programs which
 * rely on it for performing locking tasks will contain a
 * race condition. The solution for performing atomic file
 * locking using a lockfile is to create a unique file on
 * the same file system (e.g., incorporating host- name and
 * pid), use link(2) to make a link to the lockfile. If
 * link(2) returns 0, the lock is successful. Otherwise,
 * use stat(2) on the unique file to check if its link
 * count has increased to 2, in which case the lock is also
 * successful."

 *)

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)

exception Here
exception ReturnExn
exception WrongFormat of string

(* old: let _TODO () = failwith "TODO",  now via fix_caml with raise Todo *)

let internal_error s = failwith ("internal error: " ^ s)
let error_cant_have x = internal_error ("cant have this case" ^ Dumper.dump x)

(* before warning I was forced to do stuff like this:
 *
 * let (fixed_int_to_posmap: fixed_int -> posmap) = fun fixed ->
 * let v = ((fix_to_i fixed) / (power 2 16)) in
 * let _ = UPrintf.printf "coord xy = %d\n" v in
 * v
 *
 * The need for printf make me force to name stuff :(
 * How avoid ? use 'it' special keyword ?
 * In fact dont have to name it, use +> (fun v -> ...)  so when want
 * erase debug just have to erase one line.
 *)
let warning s v =
  pr2 ("Warning: " ^ s ^ "; value = " ^ Dumper.dump v);
  v

(* want or of merd, but cant cos cant put die ... in b (strict call) *)
let ( ||| ) a b =
  try a with
  | _ -> b

(* emacs/lisp inspiration, (vouillon does that too in unison I think) *)

(* now in Prelude:
 * let unwind_protect f cleanup = ...
 * let finalize f cleanup =  ...
 *)

type error = Error of string

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* let _check_stack = ref true
   
   let check_stack_size limit =
     if !_check_stack then (
       pr2 "checking stack size (do ulimit -s 40000 if problem)";
       let rec aux i = if i =|= limit then 0 else 1 + aux (i + 1) in
       assert (aux 0 =|= limit);
       ())
   
   let test_check_stack_size limit =
     (\* bytecode: 100000000 *\)
     (\* native:   10000000 *\)
     check_stack_size (int_of_string limit) *)

(* only relevant in bytecode, in native the stacklimit is the os stacklimit
 * (adjustable by ulimit -s)
 *)
let _init_gc_stack = ()
(* commented because cause pbs with js_of_ocaml
   Gc.set {(Gc.get ()) with Gc.stack_limit = 100 * 1024 * 1024}
*)

(* if process a big set of files then dont want get overflow in the middle
 * so for this we are ready to spend some extra time at the beginning that
 * could save far more later.
 *
 * On Centos 5.2 with ulimit -s 40000 I can only go up to 2000000 in
 * native mode (and it crash with ulimit -s 10000, which is what we want).
 *)
(* let check_stack_nbfiles nbfiles = if nbfiles > 200 then check_stack_size 2000000 *)

(*###########################################################################*)
(* Basic types *)
(*###########################################################################*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)
let ( ==> ) b1 b2 = if b1 then b2 else true (* could use too => *)

(* superseded by another <=> below
   let (<=>) a b = if a =*= b then 0 else if a < b then -1 else 1
*)

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

let string_of_char c = String.make 1 c
let string_of_chars cs = cs |> List_.map (String.make 1) |> String.concat ""

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

(* since 3.08, div by 0 raise Div_by_rezo, and not anymore a hardware trap :)*)
let ( /! ) x y =
  if y =|= 0 then (
    log "common.ml: div by 0";
    0)
  else x / y

(* now in prelude
 * let rec (do_n: int -> (unit -> unit) -> unit) = fun i f ->
 * if i = 0 then () else (f (); do_n (i-1) f)
 *)

let times f n = do_n n f

(* now in prelude
 * let rec (foldn: ('a -> int -> 'a) -> 'a -> int -> 'a) = fun f acc i ->
 * if i = 0 then acc else foldn f (f acc i) (i-1)
 *)

(* in prelude: let sum_int   = List.fold_left (+) 0 *)

let pi = 3.14159265358979323846
let rec power x n = if n =|= 0 then 1 else x * power x (n - 1)
let between i min max = i > min && i < max
let sum xs = List.fold_left ( + ) 0 xs

type compare = Equal | Inf | Sup

let ( <=> ) a b = if a =*= b then Equal else if a < b then Inf else Sup
let ( <==> ) a b = if a =*= b then 0 else if a < b then -1 else 1

type uint = int

let int_of_base s base =
  fold_left_with_index
    (fun acc e i ->
      let j = Char.code e - Char.code '0' in
      if j >= base then failwith "not in good base" else acc + (j * power base i))
    0
    (List.rev (list_of_string s))

(* let int_of_hex s = int_of_base s 16, NONONONO cos 'A' - '0' does not give 10 !! *)

let int64_of_string_opt s =
  try Some (Int64.of_string s) with
  | Failure _ -> None

let int64_of_string_c_octal_opt s =
  let open Common in
  if s =~ "^0\\([0-7]+\\)$" then
    let s = Common.matched1 s in
    int64_of_string_opt ("0o" ^ s)
  else int64_of_string_opt s

let float_of_string_opt s =
  match int64_of_string_c_octal_opt s with
  | Some i -> Some (Int64.to_float i)
  | None -> float_of_string_opt s

let ( += ) ref v = ref := !ref + v
let ( -= ) ref v = ref := !ref - v

(*****************************************************************************)
(* Numeric/overloading *)
(*****************************************************************************)

type 'a numdict =
  | NumDict of
      (('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a))

let add (NumDict (a, _m, _d, _n)) = a
let div (NumDict (_a, _m, d, _n)) = d

module ArithFloatInfix = struct
  let ( +.. ) = ( + )
  let ( -.. ) = ( - )
  let ( /.. ) = ( / )
  let ( *.. ) = ( * )
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( / ) = ( /. )
  let ( * ) = ( *. )
  let ( += ) ref v = ref := !ref + v
end

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

type 'a pair = 'a * 'a
type 'a triple = 'a * 'a * 'a

let fst3 (x, _, _) = x
let thd3 (_, _, z) = z
let pair f (x, y) = (f x, f y)

(* for my ocamlbeautify script *)
(*
let snd = snd
let fst = fst
*)

let double a = (a, a)

(*****************************************************************************)
(* Maybe *)
(*****************************************************************************)

let just = function
  | Some x -> x
  | _ -> failwith "just: pb"

let some = just

let optionise f =
  try Some (f ()) with
  | Not_found -> None

let option_to_list = function
  | None -> []
  | Some x -> [ x ]

(* same
   let map_find f xs =
   xs +> List.map f +> List.find (function Some x -> true | None -> false)
    +> (function Some x -> x | None -> raise Impossible)
*)

(* perl idiom *)
let ( ||= ) aref vf =
  match !aref with
  | None -> aref := Some (vf ())
  | Some _ -> ()

let ( >>= ) m1 m2 =
  match m1 with
  | None -> None
  | Some x -> m2 x

(* http://roscidus.com/blog/blog/2013/10/13/ocaml-tips/#handling-option-types*)
let ( |? ) maybe default =
  match maybe with
  | Some v -> v
  | None -> Lazy.force default

(*****************************************************************************)
(* TriBool *)
(*****************************************************************************)

type bool3 = True3 | False3 | TrueFalsePb3 of string

(*****************************************************************************)
(* Regexp, can also use PCRE *)
(*****************************************************************************)

(* put before String section because String section use some =~ *)

let ( ==~ ) s re = Str.string_match re s 0

let string_match_substring re s =
  try
    let _i = Str.search_forward re s 0 in
    true
  with
  | Not_found -> false

(*
let _ =
  example(string_match_substring (Str.regexp "foo") "a foo b")
let _ =
  example(string_match_substring (Str.regexp "\\bfoo\\b") "a foo b")
let _ =
  example(string_match_substring (Str.regexp "\\bfoo\\b") "a\n\nfoo b")
let _ =
  example(string_match_substring (Str.regexp "\\bfoo_bar\\b") "a\n\nfoo_bar b")
*)
(* does not work :(
   let _ =
   example(string_match_substring (Str.regexp "\\bfoo_bar2\\b") "a\n\nfoo_bar2 b")
*)

(* beurk, side effect code, but hey, it is convenient *)
(* now in prelude
 * let (matched: int -> string -> string) = fun i s ->
 *    Str.matched_group i s
 *
 * let matched1 = fun s -> matched 1 s
 * let matched2 = fun s -> (matched 1 s, matched 2 s)
 * let matched3 = fun s -> (matched 1 s, matched 2 s, matched 3 s)
 * let matched4 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s)
 * let matched5 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)
 * let matched6 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)
 *)

let split sep s = Str.split (Str.regexp sep) s

(*
let _ = example (split "/" "" =*= [])
let _ = example (split ":" ":a:b" =*= ["a";"b"])
*)
let join sep xs = String.concat sep xs
(*
let _ = example (join "/" ["toto"; "titi"; "tata"] =$= "toto/titi/tata")
*)

(*
let rec join str = function
  | [] -> ""
  | [x] -> x
  | x::xs -> x ^ str ^ (join str xs)
*)

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

let chop = function
  | "" -> ""
  | s -> String.sub s 0 (String.length s - 1)

let ( <!!> ) s (i, j) =
  String.sub s i (if j < 0 then String.length s - i + j + 1 else j - i)
(* let _ = example  ( "tototati"<!!>(3,-2) = "otat" ) *)

let ( <!> ) s i = String.get s i

let strip c s =
  let rec remove_prefix s =
    match s with
    | [] -> []
    | c' :: cs -> if c =*= c' then remove_prefix cs else c' :: cs
  in
  list_of_string s |> remove_prefix |> List.rev |> remove_prefix |> List.rev
  |> string_of_chars

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

type filename = string (* TODO could check that exist :) type sux *)

(* with sexp *)
type dirname = string

(* TODO could check that exist :) type sux *)
(* with sexp *)

(* file or dir *)
type path = string

(* realpath: see end of file *)

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

type float_time = float

let floattime_to_unixtime sec = Unix.localtime sec

(* src: ferre in logfun/.../date.ml *)

let day_secs : float = 86400.
let today : unit -> float = fun () -> UUnix.time ()
let month_before : float_time -> float_time = fun d -> d -. (30.0 *. day_secs)

let timestamp () =
  let now = UUnix.time () in
  let tm = floattime_to_unixtime now in

  let d = tm.tm_mday in
  let h = tm.tm_hour in
  let min = tm.tm_min in
  let s = tm.tm_sec in
  (* old: string_of_unix_time tm *)
  spf "%02d %02d:%02d:%02d" d h min s

(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

(* now in prelude:
 * let (list_of_string: string -> char list) = fun s ->
 * (enum 0 ((String.length s) - 1) +> List.map (String.get s))
 *)

let _ = assert (list_of_string "abcd" =*= [ 'a'; 'b'; 'c'; 'd' ])

(*
let rec (list_of_stream: ('a Stream.t) -> 'a list) =
parser
  | [< 'c ; stream >]  -> c :: list_of_stream stream
  | [<>]               -> []

let (list_of_string: string -> char list) =
  Stream.of_string $ list_of_stream
*)

(* now in prelude:
 * let (lines: string -> string list) = fun s -> ...
 *)

let (words : string -> string list) =
 fun s -> Str.split (Str.regexp "[ \t()\";]+") s

let n_space n = repeat " " n |> join ""

(* old: fork sucks.
 * (* note: on MacOS wc outputs some spaces before the number of lines *)
 *)

(* from https://gist.github.com/jaspervdj/1162402 *)
(* Fold over a file in chunks *)
let fold_file f x file_name =
  let buffer = Bytes.create 1024 in
  let file = UStdlib.open_in file_name in
  let rec go a =
    let length = input file buffer 0 (Bytes.length buffer) in
    let a' = f a (Bytes.sub buffer 0 length) in
    if length > 0 then go a' else a'
  in
  let r = go x in
  close_in file;
  r

(* Count the number of newlines in a buffer *)
let count_newlines s =
  let rec go n i =
    try
      let i' = Bytes.index_from s i '\n' in
      go (n + 1) (i' + 1)
    with
    | Not_found -> n
  in
  go 0 0

(* Compose the previous two functions to count the lines in a file *)
let nblines_eff file = fold_file (fun x s -> x + count_newlines s) 0 file

(* old: this could generate some Sys_error "Out of memory" in stressful
 * conditions because of the repeated calls to input_line which on
 * huge files will allocate each time new memory. The GC will reclaim
 * it, but it may be too late and we reach the physical memory limit.
 *)
(*
let nblines_eff2 file =
  let res = ref 0 in
  let finished = ref false in
  let ch = open_in_bin file in
  while not !finished do
    try
      let _ = input_line ch in
      incr res
    with End_of_file -> finished := true
  done;
  close_in ch;
  !res
*)

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)

let _batch_mode = ref false

let y_or_no msg =
  pr2 (msg ^ " [y/n] ?");
  if !_batch_mode then true
  else
    let rec aux () =
      match UStdlib.read_line () with
      | "y"
      | "yes"
      | "Y" ->
          true
      | "n"
      | "no"
      | "N" ->
          false
      | _ ->
          pr2 "answer by 'y' or 'n'";
          aux ()
    in
    aux ()

let (readdir_to_kind_list : string -> Unix.file_kind -> string list) =
 fun path kind ->
  USys.readdir path |> Array.to_list
  |> List.filter (fun s ->
         try
           let stat = UUnix.lstat (path ^ "/" ^ s) in
           stat.st_kind =*= kind
         with
         | UUnix.Unix_error _ ->
             pr2 ("EXN pb stating file: " ^ s);
             false)

let (readdir_to_file_list : string -> string list) =
 fun path -> readdir_to_kind_list path Unix.S_REG

let (readdir_to_link_list : string -> string list) =
 fun path -> readdir_to_kind_list path Unix.S_LNK

(* This regex matches the directory part a glob pattern
   used below. This way we are only trying to match
   files contained in the dir specified by the pattern or subdirs,
   instead of caluclating the contents of the entire
   working directory. I.e. tests/**/*.extension would
   result in tests/ *)
let dir_regex = Str.regexp "^[^\\*]*"

let glob pattern =
  Str.search_forward dir_regex pattern 0 |> ignore;
  let dir = Str.matched_string pattern in
  let regex = pattern |> Re.Glob.glob ~anchored:true |> Re.compile in
  let files = UFile.Legacy.dir_contents dir in
  files |> List.filter (fun s -> Re.execp regex s)

(*###########################################################################*)
(* Collection-like types *)
(*###########################################################################*)

(*****************************************************************************)
(* Nonempty List *)
(*****************************************************************************)

(* A type for nonempty lists *)
type 'a nonempty = Nonempty of 'a * 'a list

(*x: common.ml *)
(*****************************************************************************)
(* List *)
(*****************************************************************************)

(* in prelude
   let push l v =
   l := v :: !l
*)

let rec zip xs ys =
  match (xs, ys) with
  | [], [] -> []
  | [], _ -> failwith "zip: not same length"
  | _, [] -> failwith "zip: not same length"
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let unzip zs =
  List_.fold_right (fun e (xs, ys) -> (fst e :: xs, snd e :: ys)) zs ([], [])

(* now in prelude
 * let rec take n xs =
 * match (n,xs) with
 * | (0,_) -> []
 * | (_,[]) -> failwith "take: not enough"
 * | (n,x::xs) -> x::take (n-1) xs
 *)

let rec take_until p = function
  | [] -> []
  | x :: xs -> if p x then [] else x :: take_until p xs

let take_while p = take_until (p $ not)

(* now in prelude: let rec drop n xs = ... *)
let _ = assert (drop 3 [ 1; 2; 3; 4 ] =*= [ 4 ])

let rec drop_while p = function
  | [] -> []
  | x :: xs -> if p x then drop_while p xs else x :: xs

let drop_until p xs = drop_while (fun x -> not (p x)) xs
let _ = assert (drop_until (fun x -> x =|= 3) [ 1; 2; 3; 4; 5 ] =*= [ 3; 4; 5 ])
let _span p xs = (take_while p xs, drop_while p xs)

let rec (span : ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p -> function
  | [] -> ([], [])
  | x :: xs ->
      if p x then
        let l1, l2 = span p xs in
        (x :: l1, l2)
      else ([], x :: xs)

let _ =
  assert (
    span (fun x -> x <= 3) [ 1; 2; 3; 4; 1; 2 ] =*= ([ 1; 2; 3 ], [ 4; 1; 2 ]))

let (span_tail_call : ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p xs ->
  let rec aux acc xs =
    match xs with
    | [] -> (List.rev acc, [])
    | x :: xs -> if p x then aux (x :: acc) xs else (List.rev acc, x :: xs)
  in
  aux [] xs

let _ =
  assert (
    span_tail_call (fun x -> x <= 3) [ 1; 2; 3; 4; 1; 2 ]
    =*= ([ 1; 2; 3 ], [ 4; 1; 2 ]))

let rec (split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list) =
 fun p -> function
  | [] -> raise Not_found
  | x :: xs ->
      if p x then ([], x, xs)
      else
        let l1, a, l2 = split_when p xs in
        (x :: l1, a, l2)

let _ =
  assert (
    split_when (fun x -> x =|= 3) [ 1; 2; 3; 4; 1; 2 ]
    =*= ([ 1; 2 ], 3, [ 4; 1; 2 ]))

(* not so easy to come up with ... used in aComment for split_paragraph *)
let rec split_gen_when_aux f acc xs =
  match xs with
  | [] -> if List_.null acc then [] else [ List.rev acc ]
  | x :: xs -> (
      match f (x :: xs) with
      | None -> split_gen_when_aux f (x :: acc) xs
      | Some rest ->
          let before = List.rev acc in
          if List_.null before then split_gen_when_aux f [] rest
          else before :: split_gen_when_aux f [] rest)

(* could avoid introduce extra aux function by using ?(acc = []) *)
let split_gen_when f xs = split_gen_when_aux f [] xs

let _ =
  assert (
    split_gen_when
      (function
        | 42 :: xs -> Some xs
        | _ -> None)
      [ 1; 2; 42; 4; 5; 6; 42; 7 ]
    =*= [ [ 1; 2 ]; [ 4; 5; 6 ]; [ 7 ] ])

(* now in prelude:
 * let rec enum x n = ...
 *)

let head_middle_tail xs =
  match xs with
  | x :: y :: xs ->
      let head = x in
      let reversed = List.rev (y :: xs) in
      let tail = List_.hd_exn "unexpected empty list" reversed in
      let middle = List.rev (List_.tl_exn "unexpected empty list" reversed) in
      (head, middle, tail)
  | _ -> failwith "head_middle_tail, too small list"

let _ = assert_equal (head_middle_tail [ 1; 2; 3 ]) (1, [ 2 ], 3)
let _ = assert_equal (head_middle_tail [ 1; 3 ]) (1, [], 3)

(* now in prelude
 * let (++) = (@)
 *)

(* let (++) = (@), could do that, but if load many times the common, then pb *)
(* let (++) l1 l2 = List_.fold_right (fun x acc -> x::acc) l1 l2 *)

let remove x xs =
  let newxs = List.filter (fun y -> y <> x) xs in
  assert (List.length newxs =|= List.length xs - 1);
  newxs

(* now in prelude
   let exclude p xs =
   List.filter (fun x -> not (p x)) xs
*)
(* now in prelude
*)

(* now in prelude:
 * let rec list_last = function
 * | [] -> raise Not_found
 * | [x] -> x
 * | x::y::xs -> list_last (y::xs)
 *)

(* pixel *)
(* now in prelude
 *   let last_n n l = List.rev (take n (List.rev l))
 *   let last l = List_.hd_exn "unexpected empty list" (last_n 1 l)
 *)

(* todo: foldl, foldr (a more consistent foldr) *)

(* now in prelude:
 * let fold_left_with_index f acc = ...
 *)

let rec collect_accu f accu = function
  | [] -> accu
  | e :: l -> collect_accu f (List.rev_append (f e) accu) l

let collect f l = List.rev (collect_accu f [] l)

(* cf also List.partition *)

let fpartition p l =
  let rec part yes no = function
    | [] -> (List.rev yes, List.rev no)
    | x :: l -> (
        match p x with
        | None -> part yes (x :: no) l
        | Some v -> part (v :: yes) no l)
  in
  part [] [] l

(* end pixel *)

let rec inits = function
  | [] -> [ [] ]
  | e :: l -> [] :: List_.map (fun l -> e :: l) (inits l)

let rev = List.rev
let fold_left = List.fold_left

let maximum l = foldl1 max l
let minimum l = foldl1 min l

(* do a map tail recursive, and result is reversed, it is a tail recursive map => efficient *)
let map_eff_rev f l =
  let rec map_eff_aux acc = function
    | [] -> acc
    | x :: xs -> map_eff_aux (f x :: acc) xs
  in
  map_eff_aux [] l

let acc_map f l =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs -> loop (f x :: acc) xs
  in
  loop [] l

let rec (generate : int -> 'a -> 'a list) =
 fun i el -> if i =|= 0 then [] else el :: generate (i - 1) el

let and_list = List.fold_left ( && ) true

let iter_with_previous_opt f = function
  | [] -> ()
  | e :: l ->
      f None e;
      let rec iter_with_previous_ previous = function
        | [] -> ()
        | e :: l ->
            f (Some previous) e;
            iter_with_previous_ e l
      in
      iter_with_previous_ e l

(* pixel *)
let map_flatten f l =
  let rec map_flatten_aux accu = function
    | [] -> accu
    | e :: l -> map_flatten_aux (List.rev (f e) @ accu) l
  in
  List.rev (map_flatten_aux [] l)

(* now in prelude: let rec repeat e n *)

type order = HighFirst | LowFirst

(*****************************************************************************)
(* Set. Have a look too at set*.mli  *)
(*****************************************************************************)
type 'a set = 'a list
(* with sexp *)

let (empty_set : 'a set) = []

let (insert_set : 'a -> 'a set -> 'a set) =
 fun x xs ->
  if List.mem x xs then
    (* let _ = print_string "warning insert: already exist" in *)
    xs
  else x :: xs

let (set : 'a list -> 'a set) =
 fun xs -> xs |> List.fold_left (flip insert_set) empty_set |> List.sort compare

let (forall_set : ('a -> bool) -> 'a set -> bool) = List.for_all
let (filter_set : ('a -> bool) -> 'a set -> 'a set) = List.filter
let (fold_set : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a) = List.fold_left
let (member_set : 'a -> 'a set -> bool) = List.mem

let (inter_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 ->
  s1
  |> fold_set
       (fun acc x -> if member_set x s2 then insert_set x acc else acc)
       empty_set

let (union_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 ->
  s2
  |> fold_set
       (fun acc x -> if member_set x s1 then acc else insert_set x acc)
       s1

let (minus_set : 'a set -> 'a set -> 'a set) =
 fun s1 s2 -> s1 |> filter_set (fun x -> not (member_set x s2))

let (card_set : 'a set -> int) = List.length

let (include_set : 'a set -> 'a set -> bool) =
 fun s1 s2 -> s1 |> forall_set (fun p -> member_set p s2)

let equal_set s1 s2 = include_set s1 s2 && include_set s2 s1

let (include_set_strict : 'a set -> 'a set -> bool) =
 fun s1 s2 -> card_set s1 < card_set s2 && include_set s1 s2

let ( $*$ ) = inter_set
let ( $+$ ) = union_set
let ( $-$ ) = minus_set
let ( $?$ ) a b = member_set a b
let ( $<$ ) = include_set_strict
let ( $<=$ ) = include_set
let ( $=$ ) = equal_set

(* as $+$ but do not check for memberness, allow to have set of func *)
let ( $@$ ) a b = a @ b

(*****************************************************************************)
(* Set as normal list *)
(*****************************************************************************)
(*
let (union: 'a list -> 'a list -> 'a list) = fun l1 l2 ->
  List.fold_left (fun acc x -> if List.mem x l1 then acc else x::acc) l1 l2

let insert_normal x xs = union xs [x]

(* retourne lis1 - lis2 *)
let minus l1 l2 = List.filter    (fun x -> not (List.mem x l2)) l1

let inter l1 l2 = List.fold_left (fun acc x -> if List.mem x l2 then x::acc else acc) [] l1

let union_list =  List.fold_left union []

let uniq lis =
  List.fold_left (function acc -> function el -> union [el] acc) [] lis

(* pixel *)
let rec non_uniq = function
  | [] -> []
  | e::l -> if mem e l then e :: non_uniq l else non_uniq l

let rec inclu lis1 lis2 =
  List.for_all (function el -> List.mem el lis2) lis1

let equivalent lis1 lis2 =
  (inclu lis1 lis2) && (inclu lis2 lis1)

*)

(*****************************************************************************)
(* Set as sorted list *)
(*****************************************************************************)
(* liste trie, cos we need to do intersection, and insertion (it is a set
   cos when introduce has, if we create a new has => must do a recurse_rep
   and another categ can have to this has => must do an union
*)
(*
let rec insert x = function
  | [] -> [x]
  | y::ys ->
      if x = y then y::ys
      else (if x < y then x::y::ys else y::(insert x ys))

(* same, suppose sorted list *)
let rec intersect x y =
  match(x,y) with
  | [], y -> []
  | x,  [] -> []
  | x::xs, y::ys ->
      if x = y then x::(intersect xs ys)
      else
  (if x < y then intersect xs (y::ys)
  else intersect (x::xs) ys
 )
(* intersect [1;3;7] [2;3;4;7;8];;   *)
*)

(*****************************************************************************)
(* Sets specialized *)
(*****************************************************************************)

(* people often do that *)
module StringSetOrig = Set.Make (struct
  type t = string

  let compare = String.compare
end)

module StringSet = struct
  include StringSetOrig

  let of_list xs =
    xs
    |> List.fold_left (fun acc e -> StringSetOrig.add e acc) StringSetOrig.empty

  let to_list t = StringSetOrig.elements t
end

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
type ('a, 'b) assoc = ('a * 'b) list
(* with sexp *)

let assoc = List.assoc
let assoc_opt k l = optionise (fun () -> List.assoc k l)

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] |> List.sort compare

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  (* replace or add? depends the semantic of hashtbl you want *)
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h

(*
let _  =
  let h = Hashtbl.create 101 in
  Hashtbl.add h "toto" 1;
  Hashtbl.add h "toto" 1;
  assert(hash_to_list h =*= ["toto",1; "toto",1])
*)

let hfind_default key value_if_not_found h =
  try Hashtbl.find h key with
  | Not_found ->
      Hashtbl.add h key (value_if_not_found ());
      Hashtbl.find h key

(* not as easy as Perl  $h->{key}++; but still possible *)
let hupdate_default key ~update:op ~default:value_if_not_found h =
  let old = hfind_default key value_if_not_found h in
  Hashtbl.replace h key (op old)

(* see below: let hkeys h = ... *)

(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

let hashset_to_list h = hash_to_list h |> List_.map fst
let hashset_of_list xs = xs |> List_.map (fun x -> (x, true)) |> hash_of_list

let hkeys h =
  let hkey = Hashtbl.create 101 in
  h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
  hashset_to_list hkey

let group_assoc_bykey_eff xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl_.push h k v);
  let keys = hkeys h in
  keys |> List_.map (fun k -> (k, Hashtbl_.get_stack h k))

let _test_group_assoc () =
  let xs = List_.enum 0 10000 |> List_.map (fun i -> (i_to_s i, i)) in
  let xs = ("0", 2) :: xs in
  (*    let _ys = xs +> Common.groupBy (fun (a,resa) (b,resb) -> a = b)  *)
  let ys = xs |> group_assoc_bykey_eff in
  pr2_gen ys

let diff_set_eff xs1 xs2 =
  let h1 = hashset_of_list xs1 in
  let h2 = hashset_of_list xs2 in

  let hcommon = Hashtbl.create 101 in
  let honly_in_h1 = Hashtbl.create 101 in
  let honly_in_h2 = Hashtbl.create 101 in

  h1
  |> Hashtbl.iter (fun k _ ->
         if Hashtbl.mem h2 k then Hashtbl.replace hcommon k true
         else Hashtbl.add honly_in_h1 k true);
  h2
  |> Hashtbl.iter (fun k _ ->
         if Hashtbl.mem h1 k then Hashtbl.replace hcommon k true
         else Hashtbl.add honly_in_h2 k true);
  ( hashset_to_list hcommon,
    hashset_to_list honly_in_h1,
    hashset_to_list honly_in_h2 )

(*****************************************************************************)
(* N-ary tree *)
(*****************************************************************************)

(* no empty tree, must have one root at list *)
type 'a tree2 = Tree of 'a * 'a tree2 list

let rec (tree2_iter : ('a -> unit) -> 'a tree2 -> unit) =
 fun f tree ->
  match tree with
  | Tree (node, xs) ->
      f node;
      xs |> List.iter (tree2_iter f)

type ('a, 'b) tree = Node of 'a * ('a, 'b) tree list | Leaf of 'b
(* with tarzan *)

let rec map_tree ~fnode ~fleaf tree =
  match tree with
  | Leaf x -> Leaf (fleaf x)
  | Node (x, xs) -> Node (fnode x, xs |> List_.map (map_tree ~fnode ~fleaf))

(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)
(* overloading *)

let map = List_.map (* note: really really slow, use rev_map if possible *)
let filter = List.filter
let iter = List.iter
let find = List.find
let exists = List.exists
let sort xs = List.sort compare xs
let length = List.length
(*x: common.ml *)
(*****************************************************************************)
(* Regression testing bis (cocci) *)
(*****************************************************************************)

(* todo: keep also size of file, compute md5sum ? cos maybe the file
 * has changed!.
 *
 * todo: could also compute the date, or some version info of the program,
 * can record the first date when was found a OK, the last date where
 * was ok, and then first date when found fail. So the
 * Common.Ok would have more information that would be passed
 * to the Common.Pb of date * date * date * string   peut etre.
 *
 * todo? maybe use plain text file instead of marshalling.
 *)

type score_result = Ok | Pb of string

(* with sexp *)
type score = (string (* usually a filename *), score_result) Hashtbl.t

(* with sexp *)
type score_list = (string (* usually a filename *) * score_result) list
(* with sexp *)

let empty_score () : score = Hashtbl.create 101

let regression_testing_vs newscore bestscore =
  let newbestscore = empty_score () in

  let allres =
    hash_to_list newscore |> List_.map fst
    $+$ (hash_to_list bestscore |> List_.map fst)
  in
  allres
  |> List.iter (fun res ->
         match
           ( optionise (fun () -> Hashtbl.find newscore res),
             optionise (fun () -> Hashtbl.find bestscore res) )
         with
         | None, None -> raise Common.Impossible
         | Some x, None ->
             UPrintf.printf "new test file appeared: %s\n" res;
             Hashtbl.add newbestscore res x
         | None, Some _x -> UPrintf.printf "old test file disappeared: %s\n" res
         | Some newone, Some bestone -> (
             match (newone, bestone) with
             | Ok, Ok -> Hashtbl.add newbestscore res Ok
             | Pb x, Ok ->
                 UPrintf.printf
                   "PBBBBBBBB: a test file does not work anymore!!! : %s\n" res;
                 UPrintf.printf "Error : %s\n" x;
                 Hashtbl.add newbestscore res Ok
             | Ok, Pb _x ->
                 UPrintf.printf "Great: a test file now works: %s\n" res;
                 Hashtbl.add newbestscore res Ok
             | Pb x, Pb y ->
                 Hashtbl.add newbestscore res (Pb x);
                 if not (x = y) then (
                   UPrintf.printf
                     "Semipb: still error but not same error : %s\n" res;
                   UPrintf.printf "%s\n" (chop ("Old error: " ^ y));
                   UPrintf.printf "New error: %s\n" x)));
  flush UStdlib.stdout;
  flush UStdlib.stderr;
  newbestscore

let get_value filename =
  let chan = UStdlib.open_in_bin filename in
  let x = UStdlib.input_value chan in
  (* <=> Marshal.from_channel  *)
  close_in chan;
  x

let write_value valu filename =
  let chan = UStdlib.open_out_bin filename in
  UStdlib.output_value chan valu;
  (* <=> Marshal.to_channel *)
  (* Marshal.to_channel chan valu [Marshal.Closures]; *)
  close_out chan

let regression_testing newscore best_score_file =
  pr2 ("regression file: " ^ best_score_file);
  let (bestscore : score) =
    if not (USys.file_exists best_score_file) then
      write_value (empty_score ()) best_score_file;
    get_value best_score_file
  in
  let newbestscore = regression_testing_vs newscore bestscore in
  write_value newbestscore (best_score_file ^ ".old");
  write_value newbestscore best_score_file;
  ()

let total_scores score =
  let total = hash_to_list score |> List.length in
  let good =
    hash_to_list score |> List.filter (fun (_s, v) -> v =*= Ok) |> List.length
  in
  (good, total)

let print_total_score score =
  pr2 "--------------------------------";
  pr2 "total score";
  pr2 "--------------------------------";
  let good, total = total_scores score in
  pr2 (Printf.sprintf "good = %d/%d" good total)

(*x: common.ml *)
(*###########################################################################*)
(* Postlude *)
(*###########################################################################*)

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)

(*s: common.ml cmdline *)

(* I put it inside a func as it can help to give a chance to
 * change the globals before getting the options as some
 * options sometimes may want to show the default value.
 *)
let cmdline_flags_devel () =
  [
    ( "-debugger",
      Arg.Set Common.debugger,
      " option to set if launched inside ocamldebug" );
  ]

let cmdline_flags_verbose () =
  [
    ("-verbose_level", Arg.Set_int verbose_level, " <int> guess what");
    ( "-disable_pr2_once",
      Arg.Set UCommon.disable_pr2_once,
      " to print more messages" );
  ]

let cmdline_flags_other () =
  [
    ("-batch_mode", Arg.Set _batch_mode, " no interactivity");
  ]

(* potentially other common options but not yet integrated:

   "-timeout",        Arg.Set_int timeout,
   "  <sec> interrupt LFS or buggy external plugins";

   (* can't be factorized because of the $ cvs stuff, we want the date
   * of the main.ml file, not common.ml
   *)
   "-version",   Arg.Unit (fun () ->
    pr2 "version: _dollar_Date: 2008/06/14 00:54:22 _dollar_";
    raise (Common.UnixExit 0)
    ),
   "   guess what";

   "-shorthelp", Arg.Unit (fun () ->
    !short_usage_func();
    raise (Common.UnixExit 0)
   ),
   "    see short list of options";
   "-longhelp", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
    ),
   "-help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
   ),
   " ";
   "--help", Arg.Unit (fun () ->
    !long_usage_func();
    raise (Common.UnixExit 0)
   ),
   " ";
*)

(* let cmdline_actions () =
     [
       ( "-test_check_stack",
         "  <limit>",
         Arg_.mk_action_1_arg test_check_stack_size );
     ] *)

(*e: common.ml cmdline *)

(*x: common.ml *)
(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)
(* stuff put here cos of of forward definition limitation of ocaml *)

(*---------------------------------------------------------------------------*)
(* Directories part 2 *)
(*---------------------------------------------------------------------------*)

let dirs_and_base_of_file file =
  let dir, base = Filename_.db_of_filename file in
  let dirs = split "/" dir in
  let dirs =
    match dirs with
    | [ "." ] -> []
    | _ -> dirs
  in
  (dirs, base)
