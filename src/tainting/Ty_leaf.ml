module G = AST_generic

let leaf_of_qname (n : G.name) : G.name option =
  match n with
  | G.Id _ -> Some n
  | G.IdQualified { name_last = ((s, tok), _); _ } ->
    Some (G.Id ((s, tok), G.empty_id_info ()))

let leaf_of_name (n : G.name) : string option =
  match leaf_of_qname n with
  | Some (G.Id ((s, _), _)) -> Some s
  | _ -> None

let qualifier_of_name : G.name -> string option = function
  | G.IdQualified { G.name_middle = Some (G.QDots dots); _ } ->
    Option.map (fun ((q, _), _) -> q) (List_.last_opt dots)
  | _ -> None

(* Keeps the qualifier for cross-package disambiguation ([store.Store]). *)
let rec qualified_class_name_of_ty (t : G.type_) : G.name option =
  match t.G.t with
  | G.TyN n -> Some n
  | G.TyExpr { G.e = G.N n; _ } -> Some n
  | G.TyExpr { G.e = G.DotAccess (_, _, G.FN (G.Id ((s, tok), _))); _ } ->
    Some (G.Id ((s, tok), G.empty_id_info ()))
  | G.TyExpr { G.e = G.DotAccess (_, _, G.FN (G.IdQualified
        { name_last = ((s, tok), _); _ })); _ } ->
    Some (G.Id ((s, tok), G.empty_id_info ()))
  | G.TyPointer (_, inner) | G.TyRef (_, inner) ->
    qualified_class_name_of_ty inner
  | _ -> None

let class_name_of_ty (t : G.type_) : G.name option =
  Option.bind (qualified_class_name_of_ty t) leaf_of_qname

let rec inner_class_name_of_ty ?(through_funty = false) (t : G.type_)
  : G.name option =
  let recur = inner_class_name_of_ty ~through_funty in
  match t.G.t with
  | G.TyApply (inner, _) -> recur inner
  | G.TyFun (_, ret) when through_funty -> recur ret
  | G.TyPointer (_, inner) | G.TyRef (_, inner) -> recur inner
  | _ -> class_name_of_ty t
