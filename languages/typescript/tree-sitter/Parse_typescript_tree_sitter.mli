(*
   Parse a typescript program into a javascript AST.

   The plan is to enrich the javascript AST progressively so as to support
   the full typescript language.

   We also want to support tsx (React syntax for typescript) which comes
   as a slightly different grammar and CST than typescript.
*)

type dialect = [ `Typescript | `TSX ]

(*
   Parse a file as pure typescript or as TSX. If unspecified, the
   dialect is guessed from the file extension. Pure typescript is the fallback
   if the extension is unknown.
*)
val parse :
  ?dialect:dialect ->
  Fpath.t ->
  (Ast_js.a_program, unit) Tree_sitter_run.Parsing_result.t

(* Parse a Vue Single-File Component by extracting its [<script>] block and
   parsing it as TSX (positions preserved, labelled with the .vue path). The
   dedicated Vue grammar was removed upstream; this recovers script-level
   analysis (sufficient for taint). *)
val parse_vue :
  Fpath.t -> (Ast_js.a_program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (Ast_js.any, unit) Tree_sitter_run.Parsing_result.t
