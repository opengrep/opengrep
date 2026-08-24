(* Iago Abal
 *
 * Copyright (C) 2023 r2c
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

type context = {
  in_class : G.ident option;
  in_static_block : bool;
  in_constructor : bool;
  in_lvalue : bool;
}

let initial_context =
  {
    in_class = None;
    in_static_block = false;
    in_constructor = false;
    in_lvalue = false;
  }

(* In principle you should just override the 'visit_xyz' methods and call
 * 'super#visit_xyz' to recurse, so you could mostly ignore the
 * 'with_context_visit_xyz' methods.
 *
 * Note that 'with_context_visit_expr' is only a fallback: ArrayAccess,
 * Assign and AssignOp nodes dispatch their children through
 * 'self#visit_expr' directly and never reach 'with_context_visit_expr'
 * themselves. Their children still do (via the '__else__' arm), so an
 * override of 'with_context_visit_expr' observes every expression except
 * those three node kinds. This differs from 'with_context_visit_definition',
 * which is applied to every definition, including ClassDef and FuncDef.
 *)
class virtual ['self] iter_with_context =
  object (self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super

    method with_context_visit_definition (env, ctx) x =
      super#visit_definition (env, ctx) x

    method! visit_definition (env, ctx) x =
      match x with
      | { name = EN (Id (id, _ii)); _ }, ClassDef _cdef ->
          self#with_context_visit_definition
            (env, { ctx with in_class = Some id })
            x
      | { name = EN (Id (id, _ii)); _ }, FuncDef _fdef -> (
          match ctx.in_class with
          | Some in_class_id when fst in_class_id = fst id ->
              self#with_context_visit_definition
                (env, { ctx with in_constructor = true })
                x
          | Some _
          | None ->
              self#with_context_visit_definition (env, ctx) x)
      | __else__ -> self#with_context_visit_definition (env, ctx) x

    method with_context_visit_stmt (env, ctx) x = super#visit_stmt (env, ctx) x

    method! visit_stmt (env, ctx) x =
      match x.s with
      | OtherStmtWithStmt (OSWS_Block ("Static", _), [], _block) ->
          self#with_context_visit_stmt
            (env, { ctx with in_static_block = true })
            x
      | __else__ -> self#with_context_visit_stmt (env, ctx) x

    method with_context_visit_expr (env, ctx) x = super#visit_expr (env, ctx) x

    method! visit_expr (env, ctx) x =
      match x.e with
      (* The sub-expressions are dispatched through [self#visit_expr] so that
         subclass overrides apply to them; [with_context_visit_expr] (= the
         base traversal) is only for [x] itself, where re-dispatching would
         loop. *)
      | ArrayAccess (e1, (_, e2, _)) ->
          self#visit_expr (env, ctx) e1;
          self#visit_expr (env, { ctx with in_lvalue = false }) e2
      | Assign (e1, _, e2)
      | AssignOp (e1, _, e2) ->
          self#visit_expr (env, { ctx with in_lvalue = true }) e1;
          (* reset in_lvalue: even when the assignment itself sits in lvalue
             position (e.g. `*(p = q) = v`, `(x = obj).prop = v`), its RHS is
             a pure read *)
          self#visit_expr (env, { ctx with in_lvalue = false }) e2
      | __else__ -> self#with_context_visit_expr (env, ctx) x
  end
