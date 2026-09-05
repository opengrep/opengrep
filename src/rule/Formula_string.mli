(* The rule part of the match-based id, computed from the generic AST of a
 * rule (a YAML mapping) exactly as pysemgrep's rule.py formula_string()
 * computes it from the loaded YAML. *)
val of_rule : AST_generic.expr -> string
