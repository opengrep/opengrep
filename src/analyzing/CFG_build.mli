val cfg_of_stmts : ?tok:Tok.t -> IL.stmt list -> IL.cfg * IL.lambdas_cfgs
(** Compute the control flow graph of a sequence of statements.
 *
 * This is useful in cases such as handling top-level instructions that
 * do not constitute actual function definitions.
 *)

val cfg_of_fdef : IL.function_definition -> IL.fun_cfg
(** Compute the control flow graph of an IL function definition. *)

val cfg_of_gfdef :
  Lang.t -> AST_generic.function_definition -> IL.fun_cfg
(** Same as 'cfg_of_fdef' but takes a Generic function definition. *)

(* Lambda CFGs by the AST function definition they are lowered from, compared
   physically. *)
module Function_orig_table : Hashtbl.S with type key = AST_generic.function_definition

(* Records the CFG of every lambda of a function, at any depth. *)
val add_lambda_cfgs : IL.fun_cfg Function_orig_table.t -> IL.fun_cfg -> unit

(* The CFG of a function, from the table when lowering an enclosing function
   built it, else lowered now and recorded with its lambdas. *)
val cfg_of_gfdef_once :
  IL.fun_cfg Function_orig_table.t -> Lang.t -> AST_generic.function_definition -> IL.fun_cfg
