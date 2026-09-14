module G = AST_generic

type path_pattern =
  | Pattern_exact of string
  | Pattern_wildcard of { before : string; after : string }

type path_entry = {
  pe_pattern : path_pattern;
  pe_targets : path_pattern list;
}

type module_files = {
  mf_modules : Names.Module_qn.t Common.SMap.t;
  mf_paths : path_entry list;
}

type specifier_resolution =
  | Specifier_is_module_name
  | Specifier_names_file of module_files

let pattern_of_string (str : string) : path_pattern =
  match String.index_opt str '*' with
  | None -> Pattern_exact str
  | Some (star : int) ->
    Pattern_wildcard
      { before = String.sub str 0 star;
        after = String.sub str (star + 1) (String.length str - star - 1) }

let has_affixes ~(before : string) ~(after : string) (str : string) : bool =
  let n = String.length str in
  n >= String.length before + String.length after
  && String.equal (String.sub str 0 (String.length before)) before
  && String.equal
       (String.sub str (n - String.length after) (String.length after))
       after

let captured ~(before : string) ~(after : string) (str : string)
    : string option =
  if not (has_affixes ~before ~after str) then None
  else
    let start = String.length before in
    Some
      (String.sub str start (String.length str - start - String.length after))

let matched (pattern : path_pattern) (specifier : string) : string option =
  match pattern with
  | Pattern_exact (exact : string) ->
    if String.equal exact specifier then Some "" else None
  | Pattern_wildcard { before; after } -> captured ~before ~after specifier

let substituted (pattern : path_pattern) (capture : string) : string =
  match pattern with
  | Pattern_exact (exact : string) -> exact
  | Pattern_wildcard { before; after } -> before ^ capture ^ after

(* The extensions a specifier leaves out: the source extensions
   [candidates_of_base] probes. *)
let source_exts : string list =
  [ ".ts"; ".tsx"; ".mts"; ".cts"; ".js"; ".jsx"; ".mjs"; ".cjs" ]

let candidates_of_base (base_path : Fpath.t) : string list =
  let base = Fpath.to_string base_path in
  let index_under (ext : string) : string =
    Fpath.append base_path (Fpath.v ("index" ^ ext)) |> Fpath.to_string
  in
  (* Extensioned specifiers: mandatory under NodeNext resolution, where
     './utils.js' refers to utils.ts on disk (and plain CJS requires
     name the real file).  Try the literal path and the source-extension
     swaps first; appending to an already-extensioned base can only
     produce names like [utils.js.ts], which never exist. *)
  let extensioned =
    let chop = Fpath.to_string (Fpath.rem_ext base_path) in
    match Fpath.get_ext base_path with
    | ".js" -> [ base; chop ^ ".ts"; chop ^ ".tsx" ]
    | ".jsx" -> [ base; chop ^ ".tsx" ]
    | ".mjs" -> [ base; chop ^ ".mts"; chop ^ ".ts" ]
    | ".cjs" -> [ base; chop ^ ".cts"; chop ^ ".ts" ]
    | ".ts" | ".tsx" | ".mts" | ".cts" -> [ base ]
    | "" -> []
    | _ -> [ base ]
  in
  extensioned
  @ List.map (fun (ext : string) -> base ^ ext) source_exts
  @ List.map index_under source_exts

let module_of_candidates (files : module_files) (candidates : string list)
    : Names.Module_qn.t option =
  List.find_map
    (fun (candidate : string) -> Common.SMap.find_opt candidate files.mf_modules)
    candidates

let module_of_relative (files : module_files) ~(current_file : Fpath.t)
    (specifier : string) : Names.Module_qn.t option =
  module_of_candidates files
    (candidates_of_base
       (Fpath.append (Fpath.parent current_file) (Fpath.v specifier)
        |> Fpath.normalize |> Fpath.rem_empty_seg))

let module_of_bare (files : module_files) (specifier : string)
    : Names.Module_qn.t option =
  List.find_map
    (fun (entry : path_entry) ->
      match matched entry.pe_pattern specifier with
      | None -> None
      | Some (capture : string) ->
        List.find_map
          (fun (target : path_pattern) ->
            module_of_candidates files
              (candidates_of_base
                 (Fpath.v (substituted target capture) |> Fpath.normalize
                  |> Fpath.rem_empty_seg)))
          entry.pe_targets)
    files.mf_paths

let module_of_specifier (files : module_files) ~(uri : bool)
    ~(current_file : Fpath.t) (specifier : string)
    : Names.Module_qn.t option =
  if String.length specifier = 0 then None
  else if Char.equal specifier.[0] '.' then
    module_of_relative files ~current_file specifier
  else if uri && not (String.contains specifier ':') then
    module_of_relative files ~current_file specifier
  else module_of_bare files specifier

let module_qn_of_file ~(cfg : Index_lang_rules.t)
    ~(go_modules : Go_modules.t) ~(rust_crates : Rust_crates.t)
    ~(project_root : Fpath.t)
    ~(ast : G.program option) (file : Fpath.t) : Names.Module_qn.t =
  match Go_modules.import_path_of_dir go_modules (Fpath.parent file) with
  | Some (import_path : Go_import_path.t) ->
    Names.Module_qn.of_string (Go_import_path.to_string import_path)
  | None -> (
    match Rust_crates.module_qn_of_file rust_crates file with
    | Some (module_qn : Names.Module_qn.t) -> module_qn
    | None -> (
      match cfg.Index_lang_rules.unqualified_scope with
      | `Per_go_package ->
        Names.Module_qn.of_string
          (Go_import_path.to_string
             (Go_import_path.of_segments
                (Fpath.segs
                   (Fpath.normalize
                      (Discover.relative_to ~project_root
                         (Fpath.parent file))))))
      | `Per_file
      | `Per_constant_path
      | `Per_crate
      | `Per_directory
      | `Per_module
      | `Per_namespace
      | `Per_translation_unit
      | `Per_project
      | `Per_package -> (
        match Option.bind ast cfg.Index_lang_rules.module_path_from_ast with
        | Some module_str -> Names.Module_qn.of_string module_str
        | None ->
          let rel = Discover.relative_to ~project_root file in
          let path_str =
            Fpath.rem_ext rel |> Fpath.normalize |> Fpath.to_string
          in
          let path_str = cfg.Index_lang_rules.rewrite_module_path path_str in
          Names.Module_qn.of_string
            (String.concat "." (Fpath.segs (Fpath.v path_str))))))

let specifier_resolution_of_files ~(cfg : Index_lang_rules.t)
    ~(project_root : Fpath.t) ~(paths : (string * string list) list)
    (files : Fpath.t list) : specifier_resolution =
  Specifier_names_file
    { mf_modules =
        List.fold_left
          (fun (modules : Names.Module_qn.t Common.SMap.t) (file : Fpath.t) ->
            Common.SMap.add (Fpath.to_string file)
              (module_qn_of_file ~cfg ~go_modules:Go_modules.empty
                 ~rust_crates:Rust_crates.empty ~project_root ~ast:None file)
              modules)
          Common.SMap.empty files;
      mf_paths =
        List.map
          (fun ((key : string), (targets : string list)) ->
            { pe_pattern = pattern_of_string key;
              pe_targets = List.map pattern_of_string targets })
          paths }

let relative_head ~(cfg : Index_lang_rules.t) (segment : string)
    : Index_lang_rules.relative_module option =
  Option.map snd
    (List.find_opt
       (fun ((name : string), _) -> String.equal name segment)
       cfg.Index_lang_rules.relative_module_names)

let qn_parts (module_qn : Names.Module_qn.t) : string list =
  if Names.Module_qn.is_empty module_qn then []
  else Names.Module_qn.parts module_qn

let relative_module_qn ~(current : Names.Module_qn.t)
    (relative : Index_lang_rules.relative_module) : Names.Module_qn.t =
  match relative with
  | Index_lang_rules.Own_module -> current
  | Index_lang_rules.Root_module -> (
    match qn_parts current with
    | [] -> Names.Module_qn.empty
    | root :: _ -> Names.Module_qn.of_parts [ root ])
  | Index_lang_rules.Parent_module -> (
    match Names.Module_qn.split_last current with
    | Some ((parent : Names.Module_qn.t), _) -> parent
    | None -> Names.Module_qn.empty)

let module_name_string ~(cfg : Index_lang_rules.t)
    ~(resolution : specifier_resolution)
    ~(current_file : Fpath.t)
    ~(current_module_path : Names.Module_qn.t)
    ~(own_module_names : unit Common.SMap.t)
    ~(is_init_file : bool)
    (mn : G.module_name) : Names.Module_qn.t option =
  let under_own_module (parts : G.ident list) : Names.Module_qn.t =
    Names.Module_qn.of_parts
      (qn_parts current_module_path @ List.map fst parts)
  in
  let of_dotted (parts : G.ident list) : Names.Module_qn.t option =
    let prefix_segs, real_parts =
      let rec split acc = function
        | ((part_str, _) as seg) :: rest
          when String.equal part_str "." || String.equal part_str ".." ->
          split (seg :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      split [] parts
    in
    let real_strs = List.map fst real_parts in
    if List.length prefix_segs = 0 then
      Some (Names.Module_qn.of_parts real_strs)
    else begin
      (* For a relative import, an [__init__.py] file is itself the package,
         so the last segment is not dropped. Each extra [.] removes one
         further level. *)
      let init_offset = if is_init_file then 0 else 1 in
      let extra_dotdots =
        List.fold_left (fun acc (part_str, _) ->
          if String.equal part_str ".." then acc + 1 else acc
        ) 0 prefix_segs
      in
      let drops = init_offset + extra_dotdots in
      let pkg_parts = qn_parts current_module_path in
      let n_keep = max 0 (List.length pkg_parts - drops) in
      let kept = List.filteri (fun i _ -> Int.compare i n_keep < 0) pkg_parts in
      Some (Names.Module_qn.of_parts (kept @ real_strs))
    end
  in
  match mn with
  | G.FileName (spec, _) -> (
    match resolution with
    | Specifier_is_module_name ->
      Some
        (Names.Module_qn.of_string
           (cfg.Index_lang_rules.normalize_import_specifier spec))
    | Specifier_names_file (files : module_files) ->
      module_of_specifier files
        ~uri:cfg.Index_lang_rules.specifiers_are_uris ~current_file spec)
  | G.DottedName (((head : string), _) :: (rest : G.ident list) as parts) -> (
    match relative_head ~cfg head with
    | Some (relative : Index_lang_rules.relative_module) ->
      Some
        (Names.Module_qn.of_parts
           (qn_parts (relative_module_qn ~current:current_module_path relative)
            @ List.map fst rest))
    | None ->
      if Common.SMap.mem head own_module_names then
        Some (under_own_module parts)
      else
        match (cfg.Index_lang_rules.specifiers_are_uris, resolution) with
        | true, Specifier_names_file (files : module_files) ->
          let uri =
            match rest with
            | [] -> head
            | _ :: _ ->
              head ^ ":" ^ String.concat "/" (List.map fst rest)
          in
          module_of_specifier files ~uri:true ~current_file uri
        | true, Specifier_is_module_name
        | false, _ -> of_dotted parts)
  | G.DottedName ([] as parts) -> of_dotted parts

let enclosing_package ~(cfg : Index_lang_rules.t) ~(file : Fpath.t)
    (module_qn : Names.Module_qn.t) : Names.Module_qn.t =
  if cfg.Index_lang_rules.is_init_file file then module_qn
  else
    match Names.Module_qn.split_last module_qn with
    | Some (parent, _) -> parent
    | None -> Names.Module_qn.empty
