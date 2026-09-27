module G = AST_generic
module SMap = Common.SMap

type expr =
  | Const of bool
  | Tag of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr

let known_os : string list =
  [
    "aix";
    "android";
    "darwin";
    "dragonfly";
    "freebsd";
    "hurd";
    "illumos";
    "ios";
    "js";
    "linux";
    "nacl";
    "netbsd";
    "openbsd";
    "plan9";
    "solaris";
    "wasip1";
    "windows";
    "zos";
  ]

let unix_os : string list =
  [
    "aix";
    "android";
    "darwin";
    "dragonfly";
    "freebsd";
    "hurd";
    "illumos";
    "ios";
    "linux";
    "netbsd";
    "openbsd";
    "solaris";
  ]

let known_arch : string list =
  [
    "386";
    "amd64";
    "amd64p32";
    "arm";
    "armbe";
    "arm64";
    "arm64be";
    "loong64";
    "mips";
    "mipsle";
    "mips64";
    "mips64le";
    "mips64p32";
    "mips64p32le";
    "ppc";
    "ppc64";
    "ppc64le";
    "riscv";
    "riscv64";
    "s390";
    "s390x";
    "sparc";
    "sparc64";
    "wasm";
  ]

let is_known (names : string list) (name : string) : bool =
  List.exists (String.equal name) names

let os_satisfies ~(goos : string) (tag : string) : bool =
  String.equal tag goos
  || (String.equal tag "linux" && String.equal goos "android")
  || (String.equal tag "solaris" && String.equal goos "illumos")
  || (String.equal tag "darwin" && String.equal goos "ios")

let value_in_build_context ~(goos : string) ~(goarch : string) (tag : string) :
    bool option =
  if is_known known_os tag then Some (os_satisfies ~goos tag)
  else if is_known known_arch tag then Some (String.equal tag goarch)
  else if String.equal tag "unix" then Some (is_known unix_os goos)
  else None

let rec equal_expr (left : expr) (right : expr) : bool =
  match (left, right) with
  | Const left, Const right -> Bool.equal left right
  | Tag left, Tag right -> String.equal left right
  | Not left, Not right -> equal_expr left right
  | And (left_first, left_second), And (right_first, right_second)
  | Or (left_first, left_second), Or (right_first, right_second) ->
      equal_expr left_first right_first && equal_expr left_second right_second
  | (Const _ | Tag _ | Not _ | And _ | Or _), _ -> false

let rec simplify (e : expr) : expr =
  match e with
  | Const _
  | Tag _ ->
      e
  | Not inner -> (
      match simplify inner with
      | Const holds -> Const (not holds)
      | inner -> Not inner)
  | And (left, right) -> (
      match (simplify left, simplify right) with
      | Const false, _
      | _, Const false ->
          Const false
      | Const true, other
      | other, Const true ->
          other
      | left, right -> And (left, right))
  | Or (left, right) -> (
      match (simplify left, simplify right) with
      | Const true, _
      | _, Const true ->
          Const true
      | Const false, other
      | other, Const false ->
          other
      | left, right -> Or (left, right))

let rec assign (value : string -> bool option) (e : expr) : expr =
  match e with
  | Const _ -> e
  | Tag tag -> (
      match value tag with
      | Some holds -> Const holds
      | None -> e)
  | Not inner -> Not (assign value inner)
  | And (left, right) -> And (assign value left, assign value right)
  | Or (left, right) -> Or (assign value left, assign value right)

let rec holds (true_tags : string list) (e : expr) : bool =
  match e with
  | Const holds -> holds
  | Tag tag -> is_known true_tags tag
  | Not inner -> not (holds true_tags inner)
  | And (left, right) -> holds true_tags left && holds true_tags right
  | Or (left, right) -> holds true_tags left || holds true_tags right

let rec tags_of (e : expr) : string list =
  match e with
  | Const _ -> []
  | Tag tag -> [ tag ]
  | Not inner -> tags_of inner
  | And (left, right)
  | Or (left, right) ->
      tags_of left @ tags_of right

let satisfiable (e : expr) : bool =
  let rec search (true_tags : string list) (free : string list) : bool =
    match free with
    | [] -> holds true_tags e
    | tag :: rest -> search (tag :: true_tags) rest || search true_tags rest
  in
  search [] (List_.uniq_by String.equal (tags_of e))

let reduce (combine : expr -> expr -> expr) (parts : expr list) : expr option =
  match parts with
  | [] -> None
  | first :: rest -> Some (List.fold_left combine first rest)

let file_name_constraint (name : string) : expr option =
  let stem =
    match String.index_opt name '.' with
    | Some dot -> String.sub name 0 dot
    | None -> name
  in
  match String.index_opt stem '_' with
  | None -> None
  | Some underscore -> (
      let parts =
        String.split_on_char '_'
          (String.sub stem underscore (String.length stem - underscore))
      in
      let parts =
        match List.rev parts with
        | "test" :: rest -> List.rev rest
        | _ -> parts
      in
      match List.rev parts with
      | goarch :: goos :: _ when is_known known_os goos && is_known known_arch goarch
        ->
          Some (And (Tag goos, Tag goarch))
      | last :: _ when is_known known_os last || is_known known_arch last ->
          Some (Tag last)
      | _ -> None)

let is_test_file_name (name : string) : bool =
  String.ends_with ~suffix:"_test.go" name

type cofactors = (expr * int array) list

let all_arches : int = (1 lsl List.length known_arch) - 1

let cofactors_of (e : expr) : cofactors =
  let per_os =
    List.map
      (fun (goos : string) ->
        List.filter_map
          (fun ((arch_index : int), (goarch : string)) ->
            match simplify (assign (value_in_build_context ~goos ~goarch) e) with
            | Const false -> None
            | residual -> Some (residual, arch_index))
          (List.mapi (fun (index : int) (goarch : string) -> (index, goarch)) known_arch))
      known_os
  in
  List_.uniq_by equal_expr (List.concat_map (List.map fst) per_os)
  |> List.map (fun (residual : expr) ->
         ( residual,
           Array.of_list
             (List.map
                (List.fold_left
                   (fun (arches : int) ((other : expr), (arch_index : int)) ->
                     if equal_expr other residual then arches lor (1 lsl arch_index)
                     else arches)
                   0)
                per_os) ))

let conjoin (left : cofactors) (right : cofactors) : cofactors =
  List.concat_map
    (fun ((left_residual : expr), (left_contexts : int array)) ->
      List.filter_map
        (fun ((right_residual : expr), (right_contexts : int array)) ->
          let contexts = Array.map2 ( land ) left_contexts right_contexts in
          if Array.for_all (Int.equal 0) contexts then None
          else
            match simplify (And (left_residual, right_residual)) with
            | Const false -> None
            | residual -> Some (residual, contexts))
        right)
    left

type file =
  | Unconstrained
  | Constrained of {
      cofactors : cofactors;
      test_directory : string option;
    }

let equal_cofactors (left : cofactors) (right : cofactors) : bool =
  List.equal
    (fun ((left_residual : expr), (left_contexts : int array))
         ((right_residual : expr), (right_contexts : int array)) ->
      equal_expr left_residual right_residual
      && Int.equal (Array.length left_contexts) (Array.length right_contexts)
      && Array.for_all2 Int.equal left_contexts right_contexts)
    left right

let equal_file (left : file) (right : file) : bool =
  match (left, right) with
  | Unconstrained, Unconstrained -> true
  | Constrained left, Constrained right ->
      Option.equal String.equal left.test_directory right.test_directory
      && equal_cofactors left.cofactors right.cofactors
  | (Unconstrained | Constrained _), _ -> false

module File_tbl = Hashtbl.Make (struct
  type t = file

  let equal = equal_file
  let hash (file : t) : int = Hashtbl.hash file
end)

type t = {
  configurations : file array;
  configuration_of_file : int SMap.t;
}

let empty : t =
  { configurations = [| Unconstrained |]; configuration_of_file = SMap.empty }

let rec expr_of (condition : G.build_constraint) : expr =
  match condition with
  | G.BuildTag (tag, _) -> Tag tag
  | G.BuildNot inner -> Not (expr_of inner)
  | G.BuildAnd (left, right) -> And (expr_of left, expr_of right)
  | G.BuildOr (left, right) -> Or (expr_of left, expr_of right)

let written_constraints (program : G.program) : expr list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DirectiveStmt { G.d = G.BuildConstraint (_, condition); _ } ->
          Some (expr_of condition)
      | _ -> None)
    program

let file_of ((path : Fpath.t), (program : G.program)) : file =
  let name = Fpath.basename path in
  let test_directory =
    if is_test_file_name name then Some (Fpath.to_string (Fpath.parent path))
    else None
  in
  match
    ( reduce
        (fun (left : expr) (right : expr) -> And (left, right))
        (written_constraints program
        @ Option.to_list (file_name_constraint name)),
      test_directory )
  with
  | None, None -> Unconstrained
  | written, _ ->
      Constrained
        {
          cofactors = cofactors_of (Option.value written ~default:(Const true));
          test_directory;
        }

let of_files (files : (Fpath.t * G.program) list) : t =
  let indexes : int File_tbl.t = File_tbl.create 16 in
  File_tbl.replace indexes Unconstrained 0;
  let configuration_of_file, (_ : int), configurations =
    List.fold_left
      (fun ((by_file : int SMap.t), (count : int), (configurations : file list))
           (((path : Fpath.t), (_ : G.program)) as file) ->
        match file_of file with
        | Unconstrained -> (by_file, count, configurations)
        | Constrained _ as constrained ->
            let index, count, configurations =
              match File_tbl.find_opt indexes constrained with
              | Some index -> (index, count, configurations)
              | None ->
                  File_tbl.replace indexes constrained count;
                  (count, count + 1, constrained :: configurations)
            in
            ( SMap.add (Fpath.to_string (Fpath.normalize path)) index by_file,
              count,
              configurations ))
      (SMap.empty, 1, [ Unconstrained ])
      files
  in
  {
    configurations = Array.of_list (List.rev configurations);
    configuration_of_file;
  }

let build_configuration (t : t) (path : Fpath.t) : int =
  Option.value
    (SMap.find_opt (Fpath.to_string (Fpath.normalize path))
       t.configuration_of_file)
    ~default:0

let file_constraint (t : t) (path : Fpath.t) : file =
  t.configurations.(build_configuration t path)

let test_directory_of (file : file) : string option =
  match file with
  | Unconstrained -> None
  | Constrained { test_directory; _ } -> test_directory

let satisfiable_together (files : file list) : bool =
  List.length
    (List_.uniq_by String.equal (List.filter_map test_directory_of files))
  <= 1
  && List.exists
       (fun ((residual : expr), (_ : int array)) -> satisfiable residual)
       (List.fold_left
          (fun (combined : cofactors) (file : file) ->
            match file with
            | Unconstrained -> combined
            | Constrained { cofactors; _ } -> conjoin combined cofactors)
          [ (Const true, Array.make (List.length known_os) all_arches) ]
          files)

let visible (source : file) (target : file) : bool =
  match (source, target) with
  | Unconstrained, Unconstrained -> true
  | _ -> (
      match test_directory_of target with
      | None -> satisfiable_together [ source; target ]
      | Some directory ->
          Option.equal String.equal (test_directory_of source) (Some directory)
          && satisfiable_together [ source; target ])

let file_visible_from (t : t) (source : Fpath.t) (target : Fpath.t) : bool =
  visible (file_constraint t source) (file_constraint t target)

let compiled_in (t : t) (build_configuration : int) (func : Func_info.t) :
    bool =
  SMap.is_empty t.configuration_of_file
  ||
  match Func_info.def_file_opt func with
  | Some (path : Fpath.t) ->
      visible t.configurations.(build_configuration) (file_constraint t path)
  | None -> true

let files_compiled_together (t : t) (files : Fpath.t list) : bool =
  SMap.is_empty t.configuration_of_file
  || satisfiable_together (List.map (file_constraint t) (List_.uniq_by Fpath.equal files))

let compiled_together (t : t) (funcs : Func_info.t list) : bool =
  files_compiled_together t (List.filter_map Func_info.def_file_opt funcs)
