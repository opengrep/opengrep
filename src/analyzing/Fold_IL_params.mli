(**
Fold over the variables bound by the parameters of a function definition.

Used e.g. to construct taint environments, see 'Taint_input_env'.
*)

(** Yields the implicit binder of each [ParamPattern] plus every leaf
    identifier inside its pattern. Use for source matching where every
    destructured name is a candidate source. *)
val fold :
  ('acc ->
  AST_generic.ident ->
  AST_generic.id_info ->
  AST_generic.expr option (** default value *) ->
  'acc) ->
  'acc ->
  IL.param list ->
  'acc

(** Yields exactly one identifier per parameter — the resolved
    [name_param]. Use where the loop index must align with actual
    call-site argument positions (HOF signature extraction, lambda
    in-env setup); enumerating destructured leaves would fabricate
    extra Arg entries that no caller supplies. *)
val fold_top_level :
  ('acc ->
  AST_generic.ident ->
  AST_generic.id_info ->
  AST_generic.expr option ->
  'acc) ->
  'acc ->
  IL.param list ->
  'acc
