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

class ['self] visitor =
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
      | N (Id (name, info)) when is_unresolved_method_call name info ->
          Call (N (Id (name, info)) |> e, Tok.unsafe_fake_bracket [])
      | _ -> super#visit_expr_kind env ek
  end

let visitor_instance = new visitor

let disambiguate (prog : program) : program =
  visitor_instance#visit_program () prog
