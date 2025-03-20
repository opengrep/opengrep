(* This module defines functions that help track enclosure of a match.
 * By "enclosure" we mean named delimiters that the match resides in,
 * e.g. a method foo within a class C.
*)

type delimiter_kind = Module | Class | Func
[@@deriving show, eq]

type delimiter_info = {
  kind : delimiter_kind;
  name : string;
  range : (Tok.location * Tok.location) option
}
[@@deriving show, eq]

type t = delimiter_info list
[@@deriving show, eq]

val is_stmt_named_delimiter : AST_generic.stmt -> bool
val delimiter_info_of_stmt : AST_generic.stmt -> delimiter_info
val delimiter_kind_for_output : delimiter_kind -> string
