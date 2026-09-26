(* Python evaluates a definition's decorator expressions and default values
 * when the definition executes, in the enclosing scope (reference 8.7). In
 * the other languages attributes are not evaluated and default values are
 * evaluated at each call that omits the argument. *)
let evaluates_at_definition (lang : Lang.t) : bool =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      true
  | _ -> false
