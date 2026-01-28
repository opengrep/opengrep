type t

val hash : t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val show : t -> string

val show_debug : t -> string

val of_il_name : IL.name -> t

val to_file_line_col : t -> string * int * int
