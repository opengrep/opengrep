(* Brandon Wu
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module G = AST_generic

(* This one is used in OtherPattern(("ExprToPattern"),...) [E e]).
 * In such a case variables in e should be understood as pattern variables.
 * NOTE: we need two visitors, because not all expr-variables are
 * pat-variables (e.g. in PatWhen(p,e), vars in e are not pat-vars). *)
class ['self] expr_id_visitor =
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super

    method! visit_expr_kind store e =
      match e with
      | G.N (G.Id (id, id_info)) ->
          super#visit_expr_kind store e;
          Stack_.push (id, id_info) store
      | _ -> super#visit_expr_kind store e

    method! visit_pattern _store _pat =
      ()
  end

let expr_id_visitor_instance = new expr_id_visitor

class ['self] pat_id_visitor =
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    
    method! visit_pattern store pat =
      match pat with
      | PatAs (_, (id, id_info))
      | PatId (id, id_info) ->
          super#visit_pattern store pat;
          Stack_.push (id, id_info) store
      | G.OtherPat (_, [ G.E { e = G.Record (_b1, fields, _b2); _ } ]) ->
          (* JS allows object destructuring in the parameter list. *)
          (* TODO Handle array destructuring *)
          (* TODO Recurse into multi-level destructuring *)
          (* TODO Make this analysis field-sensitive *)
          List.iter
            (function
              | G.F
                  {
                    s =
                      G.DefStmt
                        ( { name = G.EN (G.Id (_id, _idinfo)); _ },
                          G.FieldDefColon
                            {
                              vinit =
                                Some
                                  { e = G.N (G.Id (local_id, local_idinfo)); _ };
                              _;
                            } );
                    _;
                  } ->
                  Stack_.push (local_id, local_idinfo) store
              | _else_ -> ())
            fields
      | G.OtherPat (("ExprToPattern", _), [ G.E { e; _ } ]) ->
          let ids = ref [] in
          expr_id_visitor_instance#visit_expr_kind ids e;
          store := !ids @ !store
      | PatRecord _
      | PatLiteral _
      | PatConstructor _
      | PatTuple _
      | PatList _
      | PatKeyVal _
      | PatWildcard _
      | PatDisj _
      | PatTyped _
      | PatWhen _
      | PatType _
      | PatEllipsis _
      | DisjPat _
      | OtherPat _ ->
          super#visit_pattern store pat
  end

let pat_id_visitor_instance = new pat_id_visitor
  
let visit : AST_generic.any -> (G.ident * G.id_info) list =
  fun any ->
    let ids = ref [] in
    pat_id_visitor_instance#visit_any ids any;
    !ids
[@@profiling]
