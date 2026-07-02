(* Typed names for the projidx pipeline.  Each kind is a distinct abstract
   type from a *generative* functor application ([... ()]) — mutually
   incompatible despite a shared string rep, so wrong-key lookups fail to
   compile. *)

module type DOTTED = sig
  type t
  val empty : t
  val of_string : string -> t
  val to_string : t -> string
  val of_parts : string list -> t
  val parts : t -> string list
  val leaf : t -> string
  val is_empty : t -> bool
  (* [(parent, leaf)]; [Some ("", leaf)] for single-segment, [None] for empty. *)
  val split_last : t -> (t * string) option
  val concat : t -> string -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module type SIMPLE = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make_dotted : functor () -> DOTTED
module Make_simple : functor () -> SIMPLE

module Class_qn : DOTTED

(* Dotted module qn; Ruby's [::] is normalised to [.] internally. *)
module Module_qn : DOTTED

module Class_name : SIMPLE
module Method_name : SIMPLE
module Field_name : SIMPLE
