(* See [Names.mli].  Each [Make_*] application is generative, so the
   abstract types are mutually incompatible despite the shared body. *)

module type DOTTED = sig
  type t
  val empty : t
  val of_string : string -> t
  val to_string : t -> string
  val of_parts : string list -> t
  val parts : t -> string list
  val leaf : t -> string
  val is_empty : t -> bool
  val split_last : t -> (t * string) option
  val concat : t -> string -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make_dotted () : DOTTED = struct
  type t = string
  let empty = ""
  let of_string s = s
  let to_string s = s
  let of_parts = String.concat "."
  let parts = String.split_on_char '.'
  let leaf t =
    match List.rev (parts t) with
    | x :: _ -> x
    | [] -> ""
  let is_empty t = String.length t = 0
  let split_last t =
    if String.length t = 0 then None
    else
      match String.rindex_opt t '.' with
      | None -> Some ("", t)
      | Some i ->
        let parent = String.sub t 0 i in
        let leaf = String.sub t (i + 1) (String.length t - i - 1) in
        Some (parent, leaf)
  let concat t s =
    if String.length t = 0 then s
    else if String.length s = 0 then t
    else t ^ "." ^ s
  let equal = String.equal
  let compare = String.compare
end

module type SIMPLE = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make_simple () : SIMPLE = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let is_empty t = String.length t = 0
  let equal = String.equal
  let compare = String.compare
end

module Class_qn = Make_dotted ()
module Module_qn = Make_dotted ()
module Class_name = Make_simple ()
module Method_name = Make_simple ()
module Field_name = Make_simple ()
