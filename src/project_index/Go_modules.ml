type module_ = {
  root : Fpath.t;
  path : Go_import_path.t;
}

type t = {
  (* Sorted by root depth descending, so the first matching module is the nearest. *)
  modules : module_ list;
}

let empty = { modules = [] }

let go_mod_name : string = "go.mod"

let find_sub (hay : string) (needle : string) : int option =
  let nl = String.length needle and hl = String.length hay in
  if nl = 0 || nl > hl then None
  else
    let rec go i =
      if i > hl - nl then None
      else if String.equal (String.sub hay i nl) needle then Some i
      else go (i + 1)
    in
    go 0

(* Module path from the [module X] line (comments stripped). [replace]
   directives are intentionally not modelled: they redirect dependency
   module paths, but projidx resolves cross-file references by file
   location, not by fetched dependency, so a rewrite would not change any
   in-tree package's own identity. (Go also honours [replace] only in the
   main module, which projidx cannot single out without a go.work.) *)
let parse_go_mod (content : string) : Go_import_path.t option =
  String.split_on_char '\n' content
  |> List.find_map (fun raw ->
    let line =
      match find_sub raw "//" with
      | Some i -> String.trim (String.sub raw 0 i)
      | None -> String.trim raw
    in
    if String.length line >= 7
       && String.equal (String.sub line 0 7) "module " then
      Some (Go_import_path.of_string
              (String.trim (String.sub line 7 (String.length line - 7))))
    else None)

let discover ~(project_root : Fpath.t) (go_files : Fpath.t list) : t =
  let modules =
    List.fold_left (fun modules dir ->
      let go_mod = Fpath.add_seg dir go_mod_name in
      match Nonfatal.catch ~default:None
              (fun () -> Some (UFile.read_file go_mod)) with
      | None -> modules
      | Some content ->
        (match parse_go_mod content with
         | Some path -> { root = Fpath.normalize dir; path } :: modules
         | None -> modules))
      [] (Discover.manifest_dirs ~project_root ~manifest:go_mod_name go_files)
  in
  (* Nearest module first: the deeper root (more path segments) wins.
     Segment count, not string length — a longer module directory name at
     a shallower depth must not outrank a genuinely deeper nested module. *)
  let depth (m : module_) = List.length (Fpath.segs m.root) in
  { modules = List.sort (fun l r -> compare (depth r) (depth l)) modules }

let import_path_of_dir (t : t) (dir : Fpath.t) : Go_import_path.t option =
  let dir = Fpath.normalize dir in
  List.find_opt (fun module_ -> Fpath.is_prefix module_.root dir) t.modules
  |> Option.map (fun module_ ->
    match Fpath.relativize ~root:module_.root dir with
    | None -> module_.path
    | Some rel ->
      Go_import_path.of_segments
        (Go_import_path.segments module_.path
         @ Fpath.segs (Fpath.normalize rel)))
