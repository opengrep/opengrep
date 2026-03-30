(* Post-naming pass for Ruby: rewrite bare unresolved identifiers as
   zero-argument method calls.

   In Ruby, a bare identifier like [source] (no parens, no receiver) is a
   method call ([self.source]) unless the parser has previously seen an
   assignment to that name in the same scope.  Tree-sitter does not replicate
   this disambiguation, so after parsing all bare identifiers are [N(Id(...))].

   After [Naming_AST.resolve] runs, local variables and parameters have
   [id_resolved = Some _].  This pass rewrites every remaining unresolved
   lowercase [N(Id(name, info))] into [Call(N(Id(name, info)), [])], matching
   what Ruby's own parser would produce. *)

val disambiguate : AST_generic.program -> AST_generic.program
