module G = AST_generic

let rec slice_element_of_ty (ty : G.type_) : G.type_ option =
  match ty.G.t with
  | G.TyArray (_, inner) -> Some inner
  | G.TyPointer (_, inner) | G.TyRef (_, inner) -> slice_element_of_ty inner
  | _ -> None
