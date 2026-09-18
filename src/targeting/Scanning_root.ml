(*
   A dedicated type for scanning roots so as to clarify the code.
*)

type t = Fpath.t [@@deriving show]

type directory = { as_written : Fpath.t; canonical : Fpath.t } [@@deriving show]

type under_root = { relative_to_root : Fpath.t; listed : Fpath.t }
[@@deriving show]

let of_fpath x = x
let to_fpath x = x
let of_string = Fpath.v
let to_string = Fpath.to_string

let directory (root : t) : directory =
  let directory =
    if UFile.is_reg ~follow_symlinks:true root then
      Fpath.rem_empty_seg (Fpath.parent root)
    else root
  in
  { as_written = directory; canonical = Rpath.canonical_exn directory }

let path_under_root (root : directory) (canonical_file : Fpath.t)
    : under_root option =
  match Fpath.relativize ~root:root.canonical canonical_file with
  | Some relative_to_root
    when not (Fpath.is_parent_dir ~prefix:true relative_to_root) ->
      Some
        { relative_to_root;
          listed = Fpath_.append_no_dot root.as_written relative_to_root }
  | _ -> None
