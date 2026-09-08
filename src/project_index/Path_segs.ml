(* See the mli. *)

let rev_no_ext (path : string) : string list =
  match Fpath.of_string path with
  | Error _ -> []
  | Ok path -> (
      match List.rev (Fpath.segs path) with
      | [] -> []
      | last :: rev_init -> Filename.remove_extension last :: rev_init)

let rec is_prefix (pre : string list) (l : string list) : bool =
  match (pre, l) with
  | [], _ -> true
  | p :: ps, x :: xs -> String.equal p x && is_prefix ps xs
  | _ :: _, [] -> false
