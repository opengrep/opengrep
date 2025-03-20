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

let human_readable_entity_name (name : AST.entity_name) : string =
  match name with
  | AST.EN (Id ((i, _tok), _)) -> i
  | AST.EN (IdQualified {name_last = ((i, _tok), _type_args); _}) -> i
  | _ -> failwith "impossible"

let delimiter_kind_of_definition_kind (kind : AST.definition_kind) : delimiter_kind =
  match kind with
  | AST.FuncDef _ -> Func
  | AST.ClassDef _ -> Class
  | AST.ModuleDef _ -> Module
  | _ -> failwith "impossible"

let delimiter_info_of_stmt (stmt : AST.stmt) : delimiter_info =
  match stmt.s with
  | AST.DefStmt (entity, kind) ->
      { kind = delimiter_kind_of_definition_kind kind;
        name = human_readable_entity_name entity.name;
        range = AST_generic_helpers.range_of_any_opt (S stmt) }
  | _ -> failwith "impossible"

let is_stmt_named_delimiter (stmt : AST.stmt) : bool =
  match stmt.s with
  | AST.DefStmt ({name = EN _; _},
      (AST.FuncDef _ | AST.ClassDef _ | AST.ModuleDef _)) -> true
  | _ -> false

let delimiter_kind_for_output (k : delimiter_kind) : string =
  match k with
  | Module -> "module"
  | Class -> "class"
  | Func -> "function"
