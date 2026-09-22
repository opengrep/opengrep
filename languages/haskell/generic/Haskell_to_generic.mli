(** Typed Haskell CST → AST_generic translator.
 *
 *  Follows the style of Parse_python_tree_sitter.ml / semgrep-clojure:
 *  exhaustive pattern matching on the CST polymorphic variants from
 *  [Tree_sitter_haskell.CST], producing [AST_generic] values directly.
 *)

module CST = Tree_sitter_haskell.CST
module G = AST_generic

type extra = {
  is_pattern_mode : bool;
  metavar_map : (string, string) Hashtbl.t;
  col_shift : int array;
}

type env = extra Parse_tree_sitter_helpers.env

val program : env -> CST.haskell -> G.program
