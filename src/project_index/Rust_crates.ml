type crate = {
  root : Fpath.t;
  name : string;
}

type t = {
  crates : crate list;
}

let empty : t = { crates = [] }

let manifest_name : string = "Cargo.toml"

let source_dir_name : string = "src"

let crate_root_stems : string list = [ "lib"; "main" ]

let module_file_stem : string = "mod"

let crate_name_of_package (package : string) : string =
  String.map (fun (char : char) -> if Char.equal char '-' then '_' else char)
    package

let package_name (manifest : Fpath.t) : string option =
  Nonfatal.catch ~on:manifest ~default:None (fun () ->
    Otoml.find_opt
      (Otoml.Parser.from_file (Fpath.to_string manifest))
      Otoml.get_string [ "package"; "name" ])

let discover ~(project_root : Fpath.t) (rust_files : Fpath.t list) : t =
  let crates =
    List.fold_left
      (fun (crates : crate list) (dir : Fpath.t) ->
        match package_name (Fpath.add_seg dir manifest_name) with
        | None -> crates
        | Some (package : string) ->
          { root = Fpath.normalize dir;
            name = crate_name_of_package package }
          :: crates)
      []
      (Discover.manifest_dirs ~project_root ~manifest:manifest_name rust_files)
  in
  let depth (crate : crate) : int = List.length (Fpath.segs crate.root) in
  { crates =
      List.sort
        (fun (left : crate) (right : crate) ->
          Int.compare (depth right) (depth left))
        crates }

let crate_of_file (t : t) (file : Fpath.t) : crate option =
  List.find_opt
    (fun (crate : crate) -> Fpath.is_prefix crate.root (Fpath.normalize file))
    t.crates

let module_segments (rel : Fpath.t) : string list =
  let segments = Fpath.segs (Fpath.normalize (Fpath.rem_ext rel)) in
  let segments =
    match segments with
    | first :: (rest : string list) when String.equal first source_dir_name ->
      rest
    | _ -> segments
  in
  match segments with
  | [ (only : string) ] when List.exists (String.equal only) crate_root_stems ->
    []
  | _ -> (
    match List.rev segments with
    | last :: (before : string list)
      when String.equal last module_file_stem -> List.rev before
    | _ -> segments)

let module_qn_of_file (t : t) (file : Fpath.t) : Names.Module_qn.t option =
  Option.map
    (fun (crate : crate) ->
      let segments =
        match Fpath.relativize ~root:crate.root (Fpath.normalize file) with
        | None -> []
        | Some (rel : Fpath.t) -> module_segments rel
      in
      Names.Module_qn.of_parts (crate.name :: segments))
    (crate_of_file t file)
