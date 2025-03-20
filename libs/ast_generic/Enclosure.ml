module AST = AST_generic

type delimiter_kind = Module | Class | Func
[@@deriving show, eq]

(* delimiter_info values are included in Core_match.t, which is why we
 * don't include the relevant AST nodes here, not to block the GC from
 * cleaning up the AST of targets that have been already processed.
 *)
type delimiter_info = {
  kind : delimiter_kind;
  name : string;
  range : (Tok.location * Tok.location) option
}
[@@deriving show, eq]

type t = delimiter_info list
[@@deriving show, eq]


let human_readable_entity_name (name : AST.entity_name) : string option =
  match name with
  | AST.EN (Id ((i, _tok), _)) -> Some i
  | AST.EN (IdQualified {name_last = ((i, _tok), _type_args); _}) -> Some i
  | _ -> None

let delimiter_kind_of_definition_kind (kind : AST.definition_kind)
  : delimiter_kind option =
  match kind with
  | AST.FuncDef _ -> Some Func
  | AST.ClassDef _ -> Some Class
  | AST.ModuleDef _ -> Some Module
  | _ -> None

let (let*) = Option.bind

let delimiter_info_of_stmt (stmt : AST.stmt) : delimiter_info option =
  match stmt.s with
  | AST.DefStmt ({name = EN _; _} as ast_entity, ast_kind) ->
      let* kind = delimiter_kind_of_definition_kind ast_kind in
      let* name = human_readable_entity_name ast_entity.name in
      let range = AST_generic_helpers.range_of_any_opt (S stmt) in
      Some {kind; name; range}
  | _ -> None

let delimiter_kind_for_output (k : delimiter_kind) : string =
  match k with
  | Module -> "module"
  | Class -> "class"
  | Func -> "function"
