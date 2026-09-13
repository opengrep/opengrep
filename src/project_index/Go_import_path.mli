type t

val of_segments : string list -> t

val of_string : string -> t

val to_string : t -> string

val segments : t -> string list
