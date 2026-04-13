module G = AST_generic

type guard_branch = {
  guard : G.expr option;
  body : G.expr;
}

type clause_info = {
  name : string;
  name_tok : Tok.t;
  params : G.parameter list;
  branches : guard_branch list;
  frettype : G.type_ option;
}

type env = {
  file : Fpath.t;
  conv : (int * int) -> int;
  src : Tree_sitter_run.Src_file.t;
  type_signatures : (string, string) Hashtbl.t;
  function_clauses : (string, clause_info list ref) Hashtbl.t;
  is_pattern_mode : bool;
  metavar_map : (string, string) Hashtbl.t;
  in_do_block : bool;
}

type node = Tree_sitter_bindings.Tree_sitter_output_t.node

val program : env -> node -> G.program
