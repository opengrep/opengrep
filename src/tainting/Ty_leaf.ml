module G = AST_generic

let leaf_of_qname (name : G.name) : G.name option =
  match name with
  | G.Id _ -> Some name
  | G.IdQualified { name_last = ((str, tok), _); _ } ->
    Some (G.Id ((str, tok), G.empty_id_info ()))

let leaf_of_name (name : G.name) : string option =
  match leaf_of_qname name with
  | Some (G.Id ((str, _), _)) -> Some str
  | _ -> None

let qualifier_of_name : G.name -> string option = function
  | G.IdQualified { G.name_middle = Some (G.QDots dots); _ } ->
    Option.map (fun ((qualifier, _), _) -> qualifier) (List_.last_opt dots)
  | _ -> None

(* Keeps the qualifier for cross-package disambiguation ([store.Store]). *)
let rec qualified_class_name_of_ty (ty : G.type_) : G.name option =
  match ty.G.t with
  | G.TyN name -> Some name
  | G.TyExpr { G.e = G.N name; _ } -> Some name
  | G.TyExpr { G.e = G.DotAccess (_, _, G.FN (G.Id ((str, tok), _))); _ } ->
    Some (G.Id ((str, tok), G.empty_id_info ()))
  | G.TyExpr { G.e = G.DotAccess (_, _, G.FN (G.IdQualified
        { name_last = ((str, tok), _); _ })); _ } ->
    Some (G.Id ((str, tok), G.empty_id_info ()))
  | G.TyPointer (_, inner) | G.TyRef (_, inner) ->
    qualified_class_name_of_ty inner
  | _ -> None

let class_name_of_ty (ty : G.type_) : G.name option =
  Option.bind (qualified_class_name_of_ty ty) leaf_of_qname

let rec inner_class_name_of_ty ?(through_funty = false) (ty : G.type_)
  : G.name option =
  let recur = inner_class_name_of_ty ~through_funty in
  match ty.G.t with
  | G.TyApply (inner, _) -> recur inner
  | G.TyFun (_, ret) when through_funty -> recur ret
  | G.TyPointer (_, inner) | G.TyRef (_, inner) -> recur inner
  | _ -> class_name_of_ty ty
