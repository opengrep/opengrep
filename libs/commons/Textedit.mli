type t = {
  path : Fpath.t;
  (* 0-based byte index, inclusive *)
  start : int;
  (* 0-based byte index, exclusive *)
  end_ : int;
  replacement_text : string;
}
[@@deriving show]

type edit_application_result =
  | Success of string
  | Overlap of {
      partial_result : string;
      (* nonempty *)
      conflicting_edits : t list;
    }

type application = {
  modified_files : Fpath.t list;
  (* files that are not regular files with one directory entry, or that
     cannot be examined; their edits are not applied *)
  skipped_files : Fpath.t list;
  (* edits that were not applied because they overlapped with others *)
  conflicting_edits : t list;
}

(* Apply a list of edits, modifying the files in place. If dryrun, do everything
 * but write to the files. A file is examined by one lstat, without following
 * a symbolic link, and is written only as the entry examined. *)
val apply_edits : dryrun:bool -> t list -> application

(* Applies the edits to the given text and returns the result. Pure function. *)
val apply_edits_to_text : Fpath.t -> string -> t list -> edit_application_result

(* Applies the edit to the given text and returns the resulting string. Pure
 * function. *)
val apply_edit_to_text : string -> t -> string
