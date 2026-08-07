type t

val hash : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val show : t -> string

val show_debug : t -> string

val of_il_name : IL.name -> t

val tok : t -> Tok.t

val of_string_and_tok : string -> Tok.t -> t

val of_sid : AST_generic.sid -> t
(** Compares equal to [of_il_name] of the def the sid points to. *)

val key : t -> string * string * int * int

val to_file_line_col : t -> string * int * int

val file_of : t -> Fpath.t option
(** [None] for synthetic nodes with fake tokens. *)

val make_absolute : Fpath.t -> t -> t
(** Absolutize the id's file path; unchanged if already absolute or fake. *)
