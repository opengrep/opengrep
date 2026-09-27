module G = AST_generic

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

(* A by reference parameter: an assignment to it in the callee assigns the
   caller's variable. *)
let parameter_is_by_reference (lang : Lang.t) (p : G.parameter_classic) : bool =
  let has_attribute_in (names : string list) (attrs : G.attribute list) =
    List.exists
      (function
        | G.NamedAttr (_, G.Id ((s, _), _), _) -> List.mem s names
        | _ -> false)
      attrs
  in
  match lang with
  | Lang.Cpp -> (
      match p.ptype with
      | Some { t = G.TyRef _; _ } -> true
      | _ -> false)
  | Lang.Csharp -> has_attribute_in [ "ref"; "out" ] p.pattrs
  | Lang.Vb ->
      List.exists
        (function
          | G.OtherAttribute (("BYREF", _), _) -> true
          | _ -> false)
        p.pattrs
  | Lang.Swift -> (
      match p.ptype with
      | Some { t_attrs; _ } -> has_attribute_in [ "inout" ] t_attrs
      | None -> false)
  | _ -> false
