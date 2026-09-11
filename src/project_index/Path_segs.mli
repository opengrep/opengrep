(* Path segments as an import's trailing segments are matched against a
   file: from the basename up, the basename's extension dropped. *)

val rev_no_ext : string -> string list
(** [rev_no_ext "pkg/mod.py"] is [["mod"; "pkg"]]; [[]] for a string that is
    no path. *)

val is_prefix : string list -> string list -> bool
(** [is_prefix pre l]: [l] starts with [pre]. *)
