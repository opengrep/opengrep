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

let world_value ~(goos : string) ~(goarch : string) (tag : string) :
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

let all_of (combine : expr -> expr -> expr) (parts : expr list) : expr option =
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

type normal_form = (expr * int array) list

let all_arches : int = (1 lsl List.length known_arch) - 1

let normal_form (e : expr) : normal_form =
  let per_os =
    List.map
      (fun (goos : string) ->
        List.filter_map
          (fun ((arch_index : int), (goarch : string)) ->
            match simplify (assign (world_value ~goos ~goarch) e) with
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

let conjoin (left : normal_form) (right : normal_form) : normal_form =
  List.concat_map
    (fun ((left_residual : expr), (left_worlds : int array)) ->
      List.filter_map
        (fun ((right_residual : expr), (right_worlds : int array)) ->
          let worlds = Array.map2 ( land ) left_worlds right_worlds in
          if Array.for_all (Int.equal 0) worlds then None
          else
            match simplify (And (left_residual, right_residual)) with
            | Const false -> None
            | residual -> Some (residual, worlds))
        right)
    left

type file =
  | Unconstrained
  | Constrained of {
      normal_form : normal_form;
      test_directory : string option;
    }

type t = file SMap.t

let empty : t = SMap.empty

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
    ( all_of
        (fun (left : expr) (right : expr) -> And (left, right))
        (written_constraints program
        @ Option.to_list (file_name_constraint name)),
      test_directory )
  with
  | None, None -> Unconstrained
  | written, _ ->
      Constrained
        {
          normal_form = normal_form (Option.value written ~default:(Const true));
          test_directory;
        }

let of_files (files : (Fpath.t * G.program) list) : t =
  List.fold_left
    (fun (table : t) (((path : Fpath.t), (_ : G.program)) as file) ->
      match file_of file with
      | Unconstrained -> table
      | Constrained _ as constrained ->
          SMap.add (Fpath.to_string (Fpath.normalize path)) constrained table)
    empty files

let entry (t : t) (path : Fpath.t) : file =
  Option.value
    (SMap.find_opt (Fpath.to_string (Fpath.normalize path)) t)
    ~default:Unconstrained

let test_directory_of (file : file) : string option =
  match file with
  | Unconstrained -> None
  | Constrained { test_directory; _ } -> test_directory

let jointly (files : file list) : bool =
  List.length
    (List_.uniq_by String.equal (List.filter_map test_directory_of files))
  <= 1
  && List.exists
       (fun ((residual : expr), (_ : int array)) -> satisfiable residual)
       (List.fold_left
          (fun (combined : normal_form) (file : file) ->
            match file with
            | Unconstrained -> combined
            | Constrained { normal_form; _ } -> conjoin combined normal_form)
          [ (Const true, Array.make (List.length known_os) all_arches) ]
          files)

let visible (source : file) (target : file) : bool =
  match (source, target) with
  | Unconstrained, Unconstrained -> true
  | _ -> (
      match test_directory_of target with
      | None -> jointly [ source; target ]
      | Some directory ->
          Option.equal String.equal (test_directory_of source) (Some directory)
          && jointly [ source; target ])

let file_visible_from (t : t) (source : Fpath.t) (target : Fpath.t) : bool =
  visible (entry t source) (entry t target)

let visible_from (t : t) (source : Fpath.t) : Func_info.t -> bool =
  if SMap.is_empty t then fun (_ : Func_info.t) -> true
  else
    let source = entry t source in
    fun (func : Func_info.t) ->
      match Func_info.def_file_opt func with
      | Some (path : Fpath.t) -> visible source (entry t path)
      | None -> true

let files_compiled_together (t : t) (files : Fpath.t list) : bool =
  SMap.is_empty t || jointly (List.map (entry t) (List_.uniq_by Fpath.equal files))

let compiled_together (t : t) (funcs : Func_info.t list) : bool =
  files_compiled_together t (List.filter_map Func_info.def_file_opt funcs)
