module G = AST_generic

let rec slice_element_of_ty (t : G.type_) : G.name option =
  match t.G.t with
  | G.TyArray (_, inner) -> Ty_leaf.class_name_of_ty inner
  | G.TyPointer (_, inner) | G.TyRef (_, inner) -> slice_element_of_ty inner
  | _ -> None

let name_of_string (s : string) : G.name =
  G.Id ((s, Tok.unsafe_fake_tok s), G.empty_id_info ())

let id_info_of_name : G.name -> G.id_info = function
  | G.Id (_, ii) -> ii
  | G.IdQualified qi -> qi.G.name_info

let declared_class_of_name (n : G.name) : G.name option =
  match !((id_info_of_name n).G.id_type) with
  | Some t -> Ty_leaf.qualified_class_name_of_ty t
  | None -> None

type ctx = {
  function_return : string -> G.name option;
  method_return : class_name:string -> method_name:string -> G.name option;
  field_type : class_name:string -> field_name:string -> G.name option;
  parent_of : string -> string option;
  has_class : string -> bool;
  current_class : G.name option;
  uses_new_keyword : bool;
}

let method_call_target
    ~(type_recv : G.expr -> G.name option)
    (callee : G.expr) : (G.name * string) option =
  match callee.G.e with
  | G.DotAccess (recv, _, G.FN meth) ->
    (match Ty_leaf.leaf_of_name meth with
     | None -> None
     | Some method_name ->
       (match type_recv recv with
        | None -> None
        | Some recv_class -> Some (recv_class, method_name)))
  | _ -> None

(* A same-package method return type is unqualified in the AST; attach the
   receiver's package qualifier so homonym classes stay distinguished
   ([r.Get()] on [repo.Repo] yields [repo.Inner], not bare [Inner]). *)
let propagate_qualifier ~(receiver : G.name) (ret : G.name) : G.name =
  match receiver, ret with
  | G.IdQualified { G.name_middle = Some _ as nm; name_top; _ }, G.Id (id, _) ->
    G.IdQualified { G.name_last = (id, None); name_middle = nm; name_top;
                    name_info = G.empty_id_info () }
  | _ -> ret

let rec type_of_expr ?(max_depth = 6) ~(ctx : ctx) (e : G.expr) : G.name option =
  if max_depth <= 0 then None else
  let recur ?(d = max_depth - 1) sub =
    type_of_expr ~max_depth:d ~ctx sub
  in
  match e.G.e with
  (* [super]: enclosing class's parent, else the class itself. *)
  | G.Call ({ e = G.IdSpecial (G.Super, _); _ }, _)
  | G.IdSpecial (G.Super, _)
  | G.Call ({ e = G.N (G.Id (("super", _), _)); _ }, _)
  | G.N (G.Id (("super", _), _)) ->
    (match ctx.current_class with
     | None -> None
     | Some n ->
       (match Ty_leaf.leaf_of_name n with
        | None -> None
        | Some cur ->
          Some (name_of_string (Option.value (ctx.parent_of cur) ~default:cur))))
  | G.N (G.Id (("cls", _), _)) when ctx.current_class <> None ->
    ctx.current_class
  | G.IdSpecial ((G.This | G.Self), _) -> ctx.current_class
  | G.Await (_, inner) -> recur inner
  | G.Call (({ G.e = G.DotAccess _; _ } as callee), _) ->
    (match method_call_target ~type_recv:(fun e -> recur e) callee with
     | Some (recv_class, method_name) ->
       (match Ty_leaf.leaf_of_name recv_class with
        | None -> None
        | Some class_name ->
          (match ctx.method_return ~class_name ~method_name with
           | Some ret -> Some (propagate_qualifier ~receiver:recv_class ret)
           | None -> None))
     | None -> None)
  | G.Call ({ G.e = G.N (G.IdQualified
      { name_last = ((method_name, _), _);
        name_middle = Some (G.QDots dots); _ }); _ }, _) ->
    (match List_.last_opt dots with
     | None -> None
     | Some (cls, _) ->
       (match ctx.method_return ~class_name:(fst cls) ~method_name with
        | Some _ as r -> r
        | None -> Some (G.Id (cls, G.empty_id_info ()))))
  (* Bare [foo()]: free-fn return, else no-[new] langs treat [Foo()] as constructor of [foo]. *)
  | G.Call ({ G.e = G.N (G.Id _ as n); _ }, _) ->
    (match Ty_leaf.leaf_of_name n with
     | None -> None
     | Some s ->
       (match ctx.function_return s with
        | Some _ as r -> r
        | None -> if ctx.uses_new_keyword then None else Some n))
  | G.DotAccess (obj, _, G.FN field_id) ->
    (match Ty_leaf.leaf_of_name field_id with
     | None -> None
     | Some field_name ->
       (match recur obj with
        | None -> None
        | Some obj_type ->
          (match Ty_leaf.leaf_of_name obj_type with
           | None -> None
           | Some class_name -> ctx.field_type ~class_name ~field_name)))
  | G.New (_, ty, _, _) -> Ty_leaf.class_name_of_ty ty
  | G.Cast (ty, _, _) -> Ty_leaf.class_name_of_ty ty
  (* Order: declared [id_type], then bare-name-as-class. *)
  | G.N name ->
    (match declared_class_of_name name with
     | Some _ as r -> r
     | None ->
       (match Ty_leaf.leaf_of_name name with
        | None -> None
        | Some leaf ->
          if ctx.has_class leaf then Some (name_of_string leaf) else None))
  | _ -> None

let infer_expr_type
    ?(max_depth = 6)
    ~(uses_new_keyword : bool)
    ~(type_state : Type_state.t)
    (e : G.expr) : G.name option =
  let ctx = {
    function_return = (fun s ->
      Type_state.get_function_return type_state (Names.Method_name.of_string s));
    method_return = (fun ~class_name ~method_name ->
      Type_state.get_method_return type_state
        (Names.Class_name.of_string class_name)
        (Names.Method_name.of_string method_name));
    field_type = (fun ~class_name ~field_name ->
      Type_state.get_field type_state
        (Names.Class_name.of_string class_name)
        (Names.Field_name.of_string field_name));
    parent_of = (fun cls ->
      Type_state.get_parent type_state (Names.Class_name.of_string cls)
      |> Option.map Names.Class_name.to_string);
    (* Conservative on projidx paths: no bare-name-as-class fallback. *)
    has_class = (fun _ -> false);
    current_class = None;
    uses_new_keyword;
  } in
  type_of_expr ~max_depth ~ctx e
