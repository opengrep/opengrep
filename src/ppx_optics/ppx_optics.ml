(*
   ppx_optics — generic optics generator (no AST hard-coding)

   Generates:
   - _<Constr> prisms for variants
   - <type>_children traversals
   - fold_<lhs>_to_<rhs> folds for every type head found inside lhs
*)

open Ppxlib
module B = Ast_builder.Default
let lid ~loc s = B.Located.lident ~loc s
let evar ~loc s = B.evar ~loc s
let pvar ~loc s = B.pvar ~loc s

(* Build (e1; e2; ...; ()) from a list of expressions *)
let seq_of ~loc (es : expression list) : expression =
  match es with
  | [] -> [%expr ()]
  | e1 :: rest ->
      List.fold_left (fun acc e -> B.pexp_sequence ~loc acc e) e1 rest

let is_same_head ~head ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident n; _ }, []) -> String.equal n head
  | _ -> false

let rec occurs_head target ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident n; _ }, params) ->
      String.equal n target || List.exists (occurs_head target) params
  | Ptyp_tuple ts -> List.exists (occurs_head target) ts
  | _ -> false

let is_ignored_head (s : string) : bool =
  match s with
  | "int" | "string" | "bool" | "float" | "char" | "bytes" | "unit"
  | "option" | "array" | "result" -> true
  | _ -> false

(* Prism generation *)
let gen_prism_for_constructor ~type_name ~is_single_constructor (cd : constructor_declaration)
  : structure_item option =
  let loc = cd.pcd_loc in
  let con = cd.pcd_name.txt in
  let prism_name = "_" ^ con in
  let s_typ = B.ptyp_constr ~loc (lid ~loc type_name) [] in

  (* classify payload shape and prepare patterns/expressions *)
  (* classify payload shape and prepare patterns/expressions *)
let shape, payload_pat, payload_expr, payload_ty, tuple_arg_pat_opt =
  match cd.pcd_args with
  | Pcstr_tuple [] ->
      (`Unit,
       B.ppat_construct ~loc (lid ~loc con) None,
       [%expr ()],
       [%type: unit],
       None)

  | Pcstr_tuple [t] ->
      let x = B.pvar ~loc "x" in
      (`Single,
       B.ppat_construct ~loc (lid ~loc con) (Some (x :> Ppxlib.pattern)),
       B.evar ~loc "x",
       t,
       None)

  | Pcstr_tuple ts ->
      let vars      = List.mapi (fun i _ -> B.pvar ~loc (Printf.sprintf "x%d" i)) ts in
      let pat_tuple = B.ppat_tuple ~loc (List.map (fun p -> (p :> Ppxlib.pattern)) vars) in
      let cons_pat  = B.ppat_construct ~loc (lid ~loc con) (Some pat_tuple) in
      let exp_tuple = B.pexp_tuple ~loc (List.mapi (fun i _ -> B.evar ~loc (Printf.sprintf "x%d" i)) ts) in
      let ty_tuple  = B.ptyp_tuple ~loc ts in
      (`Tuple, cons_pat, exp_tuple, ty_tuple, Some pat_tuple)

  | Pcstr_record lbls ->
      (* … your existing inline-record handling … *)
      (* keep: shape=`Inline_record lbls`, payload_pat=Con {…}, payload_expr=tuple or singleton, payload_ty, and tuple_arg_pat_opt = 
         - Some (ppat_tuple …) when >1 field
         - Some (pvar "x")    when 1 field
      *)
      let vs = List.mapi (fun i ld -> (Printf.sprintf "x%d" i, ld)) lbls in
      let arg_pat, tuple_e, payload_ty =
        match vs with
        | [ (_, ld) ] ->
            ( (B.pvar ~loc "x" :> Ppxlib.pattern),
              B.evar ~loc "x",
              ld.pld_type )
        | _ ->
            let pats = List.map (fun (v,_) -> (B.pvar ~loc v :> Ppxlib.pattern)) vs in
            ( B.ppat_tuple ~loc pats,
              B.pexp_tuple ~loc (List.map (fun (v,_) -> B.evar ~loc v) vs),
              B.ptyp_tuple ~loc (List.map (fun (_v,ld)-> ld.pld_type) vs) )
      in
      let rec_pat =
        let fields =
          List.mapi (fun i ld ->
            (B.Located.lident ~loc ld.pld_name.txt, List.nth (match arg_pat.ppat_desc with
               | Ppat_tuple ps -> ps
               | _ -> [arg_pat]) i)
          ) lbls
        in
        B.ppat_record ~loc fields Closed
      in
      (`Inline_record lbls,
       B.ppat_construct ~loc (lid ~loc con) (Some rec_pat),
       tuple_e,
       payload_ty,
       Some arg_pat)
    in

  (* preview: pattern-match the constructor and return its payload *)
  let preview_fun =
    if is_single_constructor then
      [%expr function [%p payload_pat] -> Some [%e payload_expr]]
    else
      [%expr function
        | [%p payload_pat] -> Some [%e payload_expr]
        | _ -> None]
  in

  
 let review_fun =
  match shape with
  | `Unit ->
      [%expr fun () -> [%e B.pexp_construct ~loc (lid ~loc con) None]]

  | `Single ->
      let x_pat : Ppxlib.pattern = B.pvar ~loc "x" in
      let body = B.pexp_construct ~loc (lid ~loc con) (Some (B.evar ~loc "x")) in
      B.pexp_fun ~loc Nolabel None x_pat body

  | `Tuple ->
      let arg_pat =
        match tuple_arg_pat_opt with
        | Some p -> p
        | None -> failwith "ppx_optics: missing tuple_arg_pat"
      in
      (* payload_expr is (x0, x1, ...) built earlier *)
      let body = B.pexp_construct ~loc (lid ~loc con) (Some payload_expr) in
      B.pexp_fun ~loc Nolabel None arg_pat body

  | `Inline_record _lbls ->
      (* rebuild record from the tuple/single arg prepared earlier *)
      let arg_pat =
        match tuple_arg_pat_opt with
        | Some p -> p
        | None -> B.pvar ~loc "x"
      in
      let rec_e =
        match cd.pcd_args with
        | Pcstr_record lbls ->
            let fields =
              match arg_pat.ppat_desc, lbls with
              | Ppat_var { txt = "x"; _ }, [ld] ->
                  [ (B.Located.lident ~loc ld.pld_name.txt, B.evar ~loc "x") ]
              | Ppat_tuple _, _ ->
                  List.mapi (fun i ld ->
                    (B.Located.lident ~loc ld.pld_name.txt,
                     B.evar ~loc (Printf.sprintf "x%d" i))) lbls
              | _ -> []
            in
            B.pexp_record ~loc fields None
        | _ -> assert false
      in
      let body = B.pexp_construct ~loc (lid ~loc con) (Some rec_e) in
      B.pexp_fun ~loc Nolabel None arg_pat body in

  let prism_ty = [%type: ([%t s_typ], [%t payload_ty]) Optics_types.prism] in
  Some [%stri
    let [%p B.pvar ~loc prism_name] : [%t prism_ty] =
      { preview = [%e preview_fun]; review = [%e review_fun] }]
(* Children traversal *)
let gen_children_traversal ~type_name td : structure_item option =
  let loc = td.ptype_loc in
  let name = type_name ^ "_children" in
  let s_typ = B.ptyp_constr ~loc (lid ~loc type_name) [] in
  let trav_ty = [%type: [%t s_typ] Optics_types.traversal] in
  match td.ptype_kind with
  | Ptype_variant constrs ->
      (* Check if any constructor has recursive children *)
      let has_recursive_children =
        List.exists (fun cd ->
          match cd.pcd_args with
          | Pcstr_tuple ts -> List.exists (is_same_head ~head:type_name) ts
          | _ -> false
        ) constrs
      in
      let f_name = if has_recursive_children then "f" else "_f" in

      let cases = List.map (fun cd ->
        let con = cd.pcd_name.txt in
        match cd.pcd_args with
        | Pcstr_tuple [] -> B.case ~lhs:(B.ppat_construct ~loc (lid ~loc con) None) ~guard:None ~rhs:(B.pexp_construct ~loc (lid ~loc con) None)
        | Pcstr_tuple ts ->
            let vars = List.mapi (fun i ty ->
              let is_recursive = is_same_head ~head:type_name ty in
              let v = if is_recursive then Printf.sprintf "x%d" i else Printf.sprintf "_x%d" i in
              (v, ty, is_recursive)
            ) ts in
            let pat_tuple = B.ppat_tuple ~loc (List.map (fun (v,_,_) -> pvar ~loc v) vars) in
            let pat = B.ppat_construct ~loc (lid ~loc con) (Some pat_tuple) in
            let exprs = List.map (fun (v,_ty,is_recursive) ->
              if is_recursive then [%expr [%e evar ~loc f_name] [%e evar ~loc v]]
              else evar ~loc v
            ) vars in
            let rebuilt = B.pexp_construct ~loc (lid ~loc con) (Some (B.pexp_tuple ~loc exprs)) in
            B.case ~lhs:pat ~guard:None ~rhs:rebuilt
        | Pcstr_record _ ->
            let v="_r" in
            let pat=B.ppat_construct ~loc (lid ~loc con) (Some (pvar ~loc v)) in
            let rebuilt=B.pexp_construct ~loc (lid ~loc con) (Some (evar ~loc v)) in
            B.case ~lhs:pat ~guard:None ~rhs:rebuilt) constrs in
      let match_expr = B.pexp_match ~loc [%expr s] cases in
      let body = [%expr { Optics_types.map_children = (fun [%p pvar ~loc f_name] s -> [%e match_expr]) }] in
      Some [%stri let [%p pvar ~loc name] : [%t trav_ty] = [%e body]]
  | _ -> None

(* Build a map of type aliases for expansion *)
(* Maps type name to (type_params, body_type) *)
let build_alias_map (tds : type_declaration list) : (string * (string list * core_type)) list =
  List.filter_map (fun td ->
    match td.ptype_kind, td.ptype_manifest with
    | Ptype_abstract, Some ct ->
        let params = List.filter_map (fun ((param : core_type), _) ->
          match param.ptyp_desc with
          | Ptyp_var s -> Some s
          | _ -> None
        ) td.ptype_params in
        Some (td.ptype_name.txt, (params, ct))
    | _ -> None
  ) tds

(* Substitute type variables in a type *)
let rec substitute_in_type (subst : (string * core_type) list) (ct : core_type) : core_type =
  match ct.ptyp_desc with
  | Ptyp_var v ->
      (match List.assoc_opt v subst with
       | Some replacement -> replacement
       | None -> ct)
  | Ptyp_constr (lid, params) ->
      { ct with ptyp_desc = Ptyp_constr (lid, List.map (substitute_in_type subst) params) }
  | Ptyp_tuple ts ->
      { ct with ptyp_desc = Ptyp_tuple (List.map (substitute_in_type subst) ts) }
  | Ptyp_arrow (lbl, t1, t2) ->
      { ct with ptyp_desc = Ptyp_arrow (lbl, substitute_in_type subst t1, substitute_in_type subst t2) }
  | _ -> ct

(* Expand type aliases with parameter substitution *)
let rec expand_type (alias_map : (string * (string list * core_type)) list) (ct : core_type) : core_type =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident n; loc }, params) ->
      (match List.assoc_opt n alias_map with
       | Some (type_params, body) when List.length type_params = List.length params ->
           (* Expand parameters first *)
           let expanded_params = List.map (expand_type alias_map) params in
           (* Build substitution map *)
           let subst = List.combine type_params expanded_params in
           (* Substitute in body *)
           let expanded_body = substitute_in_type subst body in
           (* Recursively expand the result in case it contains more aliases *)
           expand_type alias_map expanded_body
       | Some _ | None ->
           (* Not an alias, or parameter mismatch - recurse on parameters *)
           { ct with ptyp_desc = Ptyp_constr ({ txt = Lident n; loc }, List.map (expand_type alias_map) params) })
  | Ptyp_tuple ts ->
      { ct with ptyp_desc = Ptyp_tuple (List.map (expand_type alias_map) ts) }
  | _ -> ct

(* Collect heads for folds *)
let collect_heads_in_args ~alias_map (args : constructor_arguments) : string list =
  let rec heads acc (ct : core_type) =
    (* Expand aliases first to see through parameterized types *)
    let expanded = expand_type alias_map ct in
    match expanded.ptyp_desc with
    | Ptyp_constr ({ txt = Lident n; _ }, params) ->
        (* always recurse into params to discover inner heads *)
        let acc' = List.fold_left heads acc params in
        (* only add this head if it appears with ZERO parameters and isn't ignored *)
        if params = [] && not (is_ignored_head n) then n :: acc' else acc'
    | Ptyp_tuple ts ->
        List.fold_left heads acc ts
    | _ -> acc
  in
  match args with
  | Pcstr_tuple ts -> List.fold_left heads [] ts
  | Pcstr_record lbls -> List.fold_left (fun acc ld -> heads acc ld.pld_type) [] lbls
(* Fold generation *)
let gen_fold_to ~lhs_name ~rhs_head ~alias_map td : structure_item option =
  let loc = td.ptype_loc in
  let fold_name = Printf.sprintf "fold_%s_to_%s" lhs_name rhs_head in
  let lhs_typ = B.ptyp_constr ~loc (lid ~loc lhs_name) [] in
  let fold_ty = [%type: ([%t lhs_typ], [%t B.ptyp_constr ~loc (lid ~loc rhs_head) []]) Optics_types.fold] in

  (* We'll track if f is actually used *)
  let f_is_used = ref false in
  let mk_append e =
    f_is_used := true;
    [%expr acc := M.append !acc (f [%e e])] in

  (* inside gen_fold_to ... *)
let rec emit_for_ct (ct : core_type) (e : expression) : expression list =
  (* First expand type aliases *)
  let expanded_ct = expand_type alias_map ct in
  match expanded_ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident n; _ }, [t]) when String.equal n "option" ->
      (* If the inner emits no actions, don't bind a name; use Some _ -> () *)
      let acts_x = emit_for_ct t [%expr x] in
      if acts_x = [] then
        [ [%expr match [%e e] with None -> () | Some _ -> ()] ]
      else
        let body_x = seq_of ~loc acts_x in
        [ [%expr match [%e e] with None -> () | Some x -> [%e body_x]] ]

  | Ptyp_constr ({ txt = Lident n; _ }, [t]) when String.equal n "list" ->
      (* If the inner emits no actions, return empty list so variable isn't bound *)
      let acts_x = emit_for_ct t [%expr x] in
      if acts_x = [] then
        []
      else
        let body_x = seq_of ~loc acts_x in
        [ [%expr List.iter (fun x -> [%e body_x]) [%e e]] ]

  | Ptyp_constr ({ txt = Lident n; _ }, []) when String.equal n rhs_head ->
      [ mk_append e ]

  | Ptyp_tuple ts ->
      (* Use wildcards for tuple slots that don't contribute, to avoid unused vars *)
      let slots =
        List.mapi (fun i ty ->
          let vname = Printf.sprintf "t%d" i in
          let vpat  = B.pvar ~loc vname in
          let acts  = emit_for_ct ty (evar ~loc vname) in
          (vname, (vpat : Ppxlib.pattern), acts)
        ) ts
      in
      let pat_elems =
        List.map (fun (_vname, vpat, acts) ->
          if acts = [] then (B.ppat_any ~loc : Ppxlib.pattern) else vpat
        ) slots
      in
      let pat = B.ppat_tuple ~loc pat_elems in
      let acts_flat = List.concat (List.map (fun (_v, _p, a) -> a) slots) in
      let seq = seq_of ~loc acts_flat in
      [ [%expr match [%e e] with [%p pat] -> [%e seq]] ]

  | _ -> [] in
 let cases = match td.ptype_kind with
    | Ptype_variant constrs ->
        List.map (fun cd ->
          let con = cd.pcd_name.txt in
          match cd.pcd_args with
          | Pcstr_tuple [] -> B.case ~lhs:(B.ppat_construct ~loc (lid ~loc con) None) ~guard:None ~rhs:[%expr M.empty]
          | Pcstr_tuple ts ->
              let vars = List.mapi (fun i ty ->
                let vname = Printf.sprintf "x%d" i in
                let acts = emit_for_ct ty (evar ~loc vname) in
                let is_used = acts <> [] in
                (vname, is_used, ty, acts)
              ) ts in
              (* Build pattern: use actual wildcards for unused vars *)
              let pat_arg = match vars with
                | [(vname, is_used, _ty, _acts)] ->
                    (* Single argument: Con x or Con _ *)
                    if is_used then (pvar ~loc vname : Ppxlib.pattern)
                    else (B.ppat_any ~loc : Ppxlib.pattern)
                | _ ->
                    (* Multiple arguments: Con (x0, _, x2) *)
                    let pat_elems = List.map (fun (vname, is_used, _ty, _acts) ->
                      if is_used then (pvar ~loc vname : Ppxlib.pattern)
                      else (B.ppat_any ~loc : Ppxlib.pattern)
                    ) vars in
                    (B.ppat_tuple ~loc pat_elems : Ppxlib.pattern)
              in
              let pat = B.ppat_construct ~loc (lid ~loc con) (Some pat_arg) in
              let seq = List.fold_right (fun (_,_,_,acts) acc ->
                List.fold_right (fun a acc -> B.pexp_sequence ~loc a acc) acts acc
              ) vars [%expr ()] in
              let body = [%expr let acc = ref M.empty in [%e seq]; !acc] in
              B.case ~lhs:pat ~guard:None ~rhs:body
          | Pcstr_record lbls ->
              let v = "_r" in
              let pat = B.ppat_construct ~loc (lid ~loc con) (Some (pvar ~loc v)) in
              let seq = List.fold_right (fun ld acc -> let field = B.pexp_field ~loc (evar ~loc v) (B.Located.lident ~loc ld.pld_name.txt) in let acts = emit_for_ct ld.pld_type field in List.fold_right (fun a acc -> B.pexp_sequence ~loc a acc) acts acc) lbls [%expr ()] in
              let body = [%expr let acc = ref M.empty in [%e seq]; !acc] in
              B.case ~lhs:pat ~guard:None ~rhs:body) constrs
    | _ -> [] in
  if cases = [] then None else
  let rhs_typ = B.ptyp_constr ~loc (lid ~loc rhs_head) [] in
  let match_expr = B.pexp_match ~loc [%expr s] cases in
  let f_param = if !f_is_used then "f" else "_f" in
  let body = [%expr { Optics_types.fold_map = (fun (type m) (module M : Optics_types.MONOID with type t = m) ([%p pvar ~loc f_param] : [%t rhs_typ] -> m) (s : [%t lhs_typ]) -> [%e match_expr]) }] in
  Some [%stri let [%p pvar ~loc fold_name] : [%t fold_ty] = [%e body]]

let gen_for_type ~alias_map td : structure =
  let type_name = td.ptype_name.txt in
  let prisms = match td.ptype_kind with
    | Ptype_variant constrs ->
        let is_single_constructor = (List.length constrs = 1) in
        List.filter_map (gen_prism_for_constructor ~type_name ~is_single_constructor) constrs
    | _ -> [] in
  let children = gen_children_traversal ~type_name td |> Option.to_list in
  let heads = match td.ptype_kind with
    | Ptype_variant constrs -> constrs |> List.concat_map (fun cd -> collect_heads_in_args ~alias_map cd.pcd_args)
        |> List.filter (fun h -> h <> type_name && not (is_ignored_head h)) |> List.sort_uniq String.compare
    | _ -> [] in
  let folds = heads |> List.filter_map (fun h -> gen_fold_to ~lhs_name:type_name ~rhs_head:h ~alias_map td) in
  prisms @ children @ folds

let gen_structure (_rec_flag, tds) : structure =
  (* First pass: collect aliases *)
  let alias_map = build_alias_map tds in
  (* Second pass: generate optics with alias expansion *)
  List.concat_map (gen_for_type ~alias_map) tds

(* Collect all type aliases from an entire structure *)
let collect_aliases_from_structure (str : structure) : (string * (string list * core_type)) list =
  List.concat_map (fun item ->
    match item.pstr_desc with
    | Pstr_type (_, tds) ->
        List.filter_map (fun td ->
          match td.ptype_kind, td.ptype_manifest with
          | Ptype_abstract, Some ct ->
              let params = List.filter_map (fun ((param : core_type), _) ->
                match param.ptyp_desc with
                | Ptyp_var s -> Some s
                | _ -> None
              ) td.ptype_params in
              Some (td.ptype_name.txt, (params, ct))
          | _ -> None
        ) tds
    | _ -> []
  ) str

(* Check if a type declaration list has [@@deriving optics] *)
let has_optics_deriving (tds : type_declaration list) : bool =
  List.exists (fun td ->
    List.exists (fun attr ->
      attr.attr_name.txt = "deriving" &&
      match attr.attr_payload with
      | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident {txt = Lident "optics"; _}; _}, _); _}] -> true
      | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_tuple exprs; _}, _); _}] ->
          List.exists (fun e -> match e.pexp_desc with
            | Pexp_ident {txt = Lident "optics"; _} -> true
            | _ -> false) exprs
      | _ -> false
    ) td.ptype_attributes
  ) tds

(* Two-pass functional transformation *)
let transform_impl (str : structure) : structure =
  (* Pass 1: collect all type aliases in this file *)
  let global_aliases = collect_aliases_from_structure str in

  (* Pass 2: expand type groups that have [@@deriving optics] *)
  List.concat_map (fun item ->
    match item.pstr_desc with
    | Pstr_type (_rec_flag, tds) when has_optics_deriving tds ->
        (* Merge file-global aliases with local aliases *)
        let local_aliases = build_alias_map tds in
        let combined_aliases = global_aliases @ local_aliases in

        (* Generate optics *)
        let optics_items = List.concat_map (gen_for_type ~alias_map:combined_aliases) tds in
        item :: optics_items
    | _ -> [item]
  ) str

let () =
  (* Register a NO-OP deriver to make [@@deriving optics] valid *)
  (* The actual work is done by the transformation below *)
  let derive_optics =
    Deriving.Generator.make_noarg (fun ~loc:_ ~path:_ _pair -> [])
  in
  ignore (Deriving.add "optics" ~str_type_decl:derive_optics : Deriving.t);

  (* Register transformation that handles [@@deriving optics] with file-global alias map *)
  (* This runs BEFORE the deriver and does all the work *)
  Driver.register_transformation "ppx_optics"
    ~impl:transform_impl
