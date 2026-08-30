(*###########################################################################*)
(* Globals *)
(*###########################################################################*)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
(* see the corresponding section for the use of those flags. See also
 * the "Flags and actions" section at the end of this file.
 *)

val verbose_level : int ref

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)
(* cf poslude *)

(*****************************************************************************)
(* Module side effect *)
(*****************************************************************************)
(*
 * I define a few unit tests via some let _ = example (... = ...).
 * I also initialize the random seed, cf _init_random .
 * I also set Gc.stack_size, cf _init_gc_stack .
 *)

(*****************************************************************************)
(* Semi globals *)
(*****************************************************************************)
(* cf the _xxx variables in this file *)

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Pervasive types and operators *)
(*****************************************************************************)

type filename = string
type dirname = string

(* file or dir *)
type path = string

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

(* use Dumper.dump *)
val mk_pr2_wrappers : bool Domain.DLS.key -> (string -> unit) * (string -> unit)

(*****************************************************************************)
(* Test. But have a look at ounit.mli *)
(*****************************************************************************)

(*old: val example : bool -> unit, PB with js_of_ocaml? *)
(* val example : bool -> unit *)

(* generate failwith <string> when pb *)
(* val example2 : string -> bool -> unit *)

(* use Dumper to report when pb *)
val assert_equal : 'a -> 'a -> unit
val _list_bool : (string * bool) list ref
val example3 : string -> bool -> unit
val test_all : unit -> unit

(* regression testing *)
type score_result = Ok | Pb of string
type score = (string (* usually a filename *), score_result) Hashtbl.t
type score_list = (string (* usually a filename *) * score_result) list

val empty_score : unit -> score

val regression_testing :
  score -> filename (* old score file on disk (usually in /tmp) *) -> unit

val regression_testing_vs : score -> score -> score
val total_scores : score -> int (* good *) * int (* total *)
val print_total_score : score -> unit

(* quickcheck spirit *)
type 'a gen = unit -> 'a

(* quickcheck random generators *)
val ig : int gen
val lg : 'a gen -> 'a list gen
val pg : 'a gen -> 'b gen -> ('a * 'b) gen
val oneofl : 'a list -> 'a gen
val oneof : 'a gen list -> 'a gen
val always : 'a -> 'a gen
val frequency : (int * 'a gen) list -> 'a gen
val frequencyl : (int * 'a) list -> 'a gen
val laws : string -> ('a -> bool) -> 'a gen -> 'a option

(* example of use:
 * let b = laws "unit" (fun x -> reverse [x] = [x])    ig
 *)

val statistic_number : 'a list -> (int * 'a) list
val statistic : 'a list -> (int * 'a) list
val laws2 : string -> ('a -> bool * 'b) -> 'a gen -> 'a option * (int * 'b) list

type timestamp = int

(*****************************************************************************)
(* String_of and (pretty) printing *)
(*****************************************************************************)

val string_of_list : ('a -> string) -> 'a list -> string
val string_of_option : ('a -> string) -> 'a option -> string

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

val ( $ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(* cf also the timeout function below that are control related too *)

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)
exception Here
exception ReturnExn
exception WrongFormat of string

val warning : string -> 'a -> 'a
val error_cant_have : 'a -> 'b

type error = Error of string

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* val _check_stack : bool ref
   val check_stack_size : int -> unit
   val check_stack_nbfiles : int -> unit *)

(* internally common.ml set Gc. parameters *)
val _init_gc_stack : unit

(*###########################################################################*)
(* Basic types *)
(*###########################################################################*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)

val ( ||| ) : 'a -> 'a -> 'a
val ( ==> ) : bool -> bool -> bool

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

val string_of_char : char -> string

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

val ( /! ) : int -> int -> int
val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a

(* alias for flip do_n, ruby style *)
val times : (unit -> unit) -> int -> unit
val pi : float
val between : 'a -> 'a -> 'a -> bool
val sum : int list -> int

type compare = Equal | Inf | Sup

val ( <=> ) : 'a -> 'a -> compare
val ( <==> ) : 'a -> 'a -> int

type uint = int

val int_of_base : string -> int -> int
val int64_of_string_opt : string -> int64 option

(* like int_of_string_opt, but also converts C octals like 0400 in
 * the right value. *)
val int64_of_string_c_octal_opt : string -> int64 option

(* like float_of_string_opt, but also converts C octals like 0400 in
 * the right value. *)
val float_of_string_opt : string -> float option

(* useful but sometimes when want grep for all places where do modif,
 * easier to have just code using ':=' and '<-' to do some modifications.
 * In the same way avoid using {contents = xxx} to build some ref.
 *)
val ( += ) : int ref -> int -> unit
val ( -= ) : int ref -> int -> unit

(*****************************************************************************)
(* Numeric/overloading *)
(*****************************************************************************)

type 'a numdict =
  | NumDict of
      (('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a))

val add : 'a numdict -> 'a -> 'a -> 'a
val div : 'a numdict -> 'a -> 'a -> 'a

module ArithFloatInfix : sig
  val ( + ) : float -> float -> float
  val ( - ) : float -> float -> float
  val ( / ) : float -> float -> float
  val ( * ) : float -> float -> float
  val ( +.. ) : int -> int -> int
  val ( -.. ) : int -> int -> int
  val ( /.. ) : int -> int -> int
  val ( *.. ) : int -> int -> int
  val ( += ) : float ref -> float -> unit
end

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

type 'a pair = 'a * 'a
type 'a triple = 'a * 'a * 'a

val fst3 : 'a * 'b * 'c -> 'a
val thd3 : 'a * 'b * 'c -> 'c
val pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val double : 'a -> 'a * 'a

(*****************************************************************************)
(* Maybe *)
(*****************************************************************************)

val just : 'a option -> 'a
val some : 'a option -> 'a (* alias *)
val optionise : (unit -> 'a) -> 'a option
val option_to_list : 'a option -> 'a list
val ( ||= ) : 'a option ref -> (unit -> 'a) -> unit
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( |? ) : 'a option -> 'a Lazy.t -> 'a

(*****************************************************************************)
(* TriBool *)
(*****************************************************************************)
type bool3 = True3 | False3 | TrueFalsePb3 of string

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

val chop : string -> string
val ( <!!> ) : string -> int * int -> string
val ( <!> ) : string -> int -> char

(* strip c s removes all contiguous prefixes of [c] from [s]
   e.g. strip 'a' "abc"   = "bc"
        strip 'b' "abc"   = "abc"
        strip 'c' "cabcc" = "ab"
*)
val strip : char -> string -> string

(*****************************************************************************)
(* Regexp *)
(*****************************************************************************)

val ( ==~ ) : string -> Str.regexp -> bool
val matched : int -> string -> string
val string_match_substring : Str.regexp -> string -> bool

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(* from Unix *)
type float_time = float

val today : unit -> float_time
val month_before : float_time -> float_time

(* useful to put in logs as prefix *)
val timestamp : unit -> string

(*****************************************************************************)
(* Lines/Words/Strings *)
(*****************************************************************************)

val lines : string -> string list
val words : string -> string list
val nblines_eff : filename -> int
val unix_diff : filename -> filename -> string list
val n_space : int -> string

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)
(*
   TODO: migrate file operations to the File module.
   TODO: alternatively, use the bos library; this would be a bigger migration.
*)

val _batch_mode : bool ref
val y_or_no : string -> bool
val readdir_to_file_list : string -> filename list
val readdir_to_link_list : string -> string list

val glob : string -> filename list
(** [glob pattern] takes in a pattern containing a wildcard
  * i.e. ["dir/**/*.extension"] will match any file in the dir directory
  * or subdirectories ending in .extension. This function is equivalent
  * to "ls pattern" in the shell.
  *)

(*###########################################################################*)
(* Collection-like types *)
(*###########################################################################*)

(*****************************************************************************)
(* Nonempty List *)
(*****************************************************************************)

type 'a nonempty = Nonempty of 'a * 'a list

(*****************************************************************************)
(* List *)
(*****************************************************************************)

val hd_opt : 'a list -> 'a option

(* tail recursive efficient map (but that also reverse the element!) *)
val map_eff_rev : ('a -> 'b) -> 'a list -> 'b list

(* tail recursive efficient map, use accumulator  *)
val acc_map : ('a -> 'b) -> 'a list -> 'b list
val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
val span_tail_call : ('a -> bool) -> 'a list -> 'a list * 'a list

(* cf also List.partition *)
val fpartition : ('a -> 'b option) -> 'a list -> 'b list * 'a list
val split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list
val split_gen_when : ('a list -> 'a list option) -> 'a list -> 'a list list
val repeat : 'a -> int -> 'a list
val generate : int -> 'a -> 'a list
val head_middle_tail : 'a list -> 'a * 'a list * 'a
val list_last : 'a list -> 'a
val inits : 'a list -> 'a list list
val ( ++ ) : 'a list -> 'a list -> 'a list
val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val collect : ('a -> 'b list) -> 'a list -> 'b list
val remove : 'a -> 'a list -> 'a list
val exclude : ('a -> bool) -> 'a list -> 'a list
val rev : 'a list -> 'a list (* alias *)
val map_flatten : ('a -> 'b list) -> 'a list -> 'b list
val maximum : 'a list -> 'a
val minimum : 'a list -> 'a
val and_list : bool list -> bool
val sum_int : int list -> int
val iter_with_previous_opt : ('a option -> 'a -> unit) -> 'a list -> unit

(*****************************************************************************)
(* Set. But have a look too at set*.mli; it's better. Or use Hashtbl. *)
(*****************************************************************************)

type 'a set = 'a list

val set : 'a list -> 'a set
val minus_set : 'a set -> 'a set -> 'a set

(* could put them in Common.Infix *)
val ( $*$ ) : 'a set -> 'a set -> 'a set
val ( $+$ ) : 'a set -> 'a set -> 'a set
val ( $-$ ) : 'a set -> 'a set -> 'a set
val ( $?$ ) : 'a -> 'a set -> bool
val ( $<$ ) : 'a set -> 'a set -> bool
val ( $<=$ ) : 'a set -> 'a set -> bool
val ( $=$ ) : 'a set -> 'a set -> bool
val ( $@$ ) : 'a list -> 'a list -> 'a list

(* use internally a hash and return
 * - the common part,
 * - part only in a,
 * - part only in b
 *)
val diff_set_eff : 'a list -> 'a list -> 'a list * 'a list * 'a list

(*****************************************************************************)
(* Set as normal list *)
(*****************************************************************************)

(* cf above *)

(*****************************************************************************)
(* Set as sorted list *)
(*****************************************************************************)

(*****************************************************************************)
(* Sets specialized *)
(*****************************************************************************)

module StringSet : sig
  type elt = string
  type t

  val empty : t
  val add : string -> t -> t
  val remove : string -> t -> t
  val singleton : string -> t
  val of_list : string list -> t
  val to_list : t -> string list
  val is_empty : t -> bool
  val mem : string -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val iter : (string -> unit) -> t -> unit
  val fold : (string -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (string -> bool) -> t -> bool
  val exists : (string -> bool) -> t -> bool
  val filter : (string -> bool) -> t -> t
  val partition : (string -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> string list
  (*
        val min_string : t -> string
        val max_string : t -> string
      *)

  val choose : t -> string
  val split : string -> t -> t * bool * t
end

(*****************************************************************************)
(* Assoc. But have a look too at Mapb.mli; it's better. Or use Hashtbl. *)
(*****************************************************************************)

type ('a, 'b) assoc = ('a * 'b) list

val assoc : 'a -> ('a * 'b) list -> 'b
val assoc_opt : 'a -> ('a, 'b) assoc -> 'b option

type order = HighFirst | LowFirst

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

(* Note that Hashtbl keep old binding to a key so if want a hash
 * of a list, then can use the Hashtbl as is. Use Hashtbl_.get_stack then
 * to get the list of bindings
 *
 * Note that Hashtbl module use different convention :( the object is
 * the first argument, not last as for List or Map.
 *)

val hupdate_default :
  'a -> update:('b -> 'b) -> default:(unit -> 'b) -> ('a, 'b) Hashtbl.t -> unit

val hkeys : ('a, 'b) Hashtbl.t -> 'a list

(*****************************************************************************)
(* N-ary tree *)
(*****************************************************************************)

(* no empty tree, must have one root at least *)
type 'a tree2 = Tree of 'a * 'a tree2 list

val tree2_iter : ('a -> unit) -> 'a tree2 -> unit

type ('a, 'b) tree = Node of 'a * ('a, 'b) tree list | Leaf of 'b

val map_tree :
  fnode:('a -> 'abis) ->
  fleaf:('b -> 'bbis) ->
  ('a, 'b) tree ->
  ('abis, 'bbis) tree

val dirs_and_base_of_file : path -> string list * string

(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)

(* mostly alias to functions in List *)

val map : ('a -> 'b) -> 'a list -> 'b list
val filter : ('a -> bool) -> 'a list -> 'a list
val iter : ('a -> unit) -> 'a list -> unit
val find : ('a -> bool) -> 'a list -> 'a
val exists : ('a -> bool) -> 'a list -> bool

(* generic sort using Pervasives.compare *)
val sort : 'a list -> 'a list
val length : 'a list -> int

(*###########################################################################*)
(* Postlude *)
(*###########################################################################*)

val cmdline_flags_devel : unit -> Arg_.cmdline_options
val cmdline_flags_verbose : unit -> Arg_.cmdline_options
val cmdline_flags_other : unit -> Arg_.cmdline_options
