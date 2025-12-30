(* In reality, this only affect threading forms. *)
type macroexpansion_mode =
  (* Expand (-> ...) etc immediately in translation. *)
  | At_generic
  (* Create OtherExpr which is only later expanded when converting to IL.
   * Has issues so should not be used for now, but is kept because it can
   * have some advantages if fixed. *)
  | At_IL

(* For now, this is set here permanently, but later we can
 * choose one method for good or we can allow the user to choose. *)
(* FIXME: For now At_IL does not work as intended, so this should
 * stay as it until it's fixed. *)
let macroexpansion_mode = At_generic
