open AST_generic

(* A bare lowercase identifier that naming could not resolve is a method call
   in Ruby.  Rewrite [N(Id(name, info))] to [Call(N(Id(name, info)), [])]
   so that downstream analyses (taint, matching) treat it the same as an
   explicit [source()] call. *)

(* A bare identifier is a method call candidate only when:
   - naming left it unresolved (not a local variable or parameter)
   - it starts with a lowercase letter, excluding [$] (global variables)
     and [@] (class variables [@@]) *)
let is_unresolved_method_call (name : ident) (info : id_info) : bool =
  let s, _tok = name in
  Option.is_none !(info.id_resolved)
  && String.length s > 0
  && Char.equal (Char.lowercase_ascii s.[0]) s.[0]
  && not (Char.equal s.[0] '$')
  && not (Char.equal s.[0] '@')

module SIdSet = Set.Make (SId)

let method_bindings (lang : Lang.t) (prog : program) : SIdSet.t =
  Visit_function_defs.fold_with_parent_path ~lang
    (fun (acc : SIdSet.t) ~object_literal_method:_ (opt_ent : entity option)
         _parent_path (fdef : function_definition) ->
      match (fst fdef.fkind, opt_ent) with
      | ( (Function | Method),
          Some
            { name = EN (Id (_, { id_resolved = { contents = Some (_, sid) }; _ }));
              _ } ) ->
          SIdSet.add sid acc
      | _ -> acc)
    SIdSet.empty prog

let refers_to_method (methods : SIdSet.t) (info : id_info) : bool =
  match !(info.id_resolved) with
  | Some (_, sid) -> SIdSet.mem sid methods
  | None -> false

class ['self] visitor (methods : SIdSet.t) =
  object (self : 'self)
    inherit [_] AST_generic.map as super

    method! visit_expr_kind env ek =
      match ek with
      (* Visit the callee of a Call unless it is a bare N(Id(...)) —
         visiting that would wrap it in another Call, producing a spurious
         Call(Call(f, []), args). For compound callees (DotAccess,
         ArrayAccess, etc.) we DO recurse so that nested bare identifiers
         like `helper` in `helper.process()` get properly wrapped. *)
      | Call ({ e = N (Id _); _ } as callee, args) ->
          let args = self#visit_arguments env args in
          Call (callee, args)
      | Call (callee, args) ->
          let callee = self#visit_expr env callee in
          let args = self#visit_arguments env args in
          Call (callee, args)
      (* Bare unresolved lowercase identifier -- wrap in a zero-arg Call. *)
      | N (Id (name, info))
        when is_unresolved_method_call name info
             || refers_to_method methods info ->
          Call (N (Id (name, info)) |> e, Tok.unsafe_fake_bracket [])
      | _ -> super#visit_expr_kind env ek
  end

let disambiguate (lang : Lang.t) (prog : program) : program =
  (new visitor (method_bindings lang prog))#visit_program () prog
