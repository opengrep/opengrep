(* Iago Abal
 *
 * Copyright (C) 2022 r2c
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
module H = AST_generic_helpers

class ['self] visitor =
  object (self : 'self)
    inherit [_] G.iter_no_id_info as super

    method! visit_definition f ((ent, def_kind) as def) =
      match def_kind with
      | G.FuncDef fdef ->
          f (Some ent) fdef;
          (* Go into nested functions
             but do NOT revisit the function definition again! *)
          let body = H.funcbody_to_stmt fdef.G.fbody in
          self#visit_stmt f body
      | G.VarDef { vinit = Some { e = G.Lambda fdef; _ }; _ } ->
          (* Handle lambda assignments like: const f = () => {...} *)
          f (Some ent) fdef;
          (* Go into nested functions but do NOT revisit the function definition again! *)
          let body = H.funcbody_to_stmt fdef.G.fbody in
          self#visit_stmt f body
      | __else__ -> super#visit_definition f def

    method! visit_function_definition f fdef =
      f None fdef;
      (* go into nested functions *)
      super#visit_function_definition f fdef
  end

class ['self] visitor_with_class_context =
  object (self : 'self)
    inherit [_] G.iter_no_id_info as super
    val current_class : G.name option ref = ref None

    method! visit_definition f ((ent, def_kind) as def) =
      match def_kind with
      | G.ClassDef _cdef ->
          let old_class = !current_class in
          (current_class :=
             match ent.name with
             | EN name -> Some name
             | _ -> None);
          super#visit_definition f def;
          current_class := old_class
      | G.FuncDef fdef ->
          f (Some ent) !current_class fdef;
          (* Go into nested functions
             but do NOT revisit the function definition again! *)
          let body = H.funcbody_to_stmt fdef.G.fbody in
          self#visit_stmt f body
      | G.VarDef { vinit = Some { e = G.Lambda fdef; _ }; _ } ->
          (* Handle lambda assignments like: const f = () => {...} *)
          f (Some ent) !current_class fdef;
          (* Go into nested functions but do NOT revisit the function definition again! *)
          let body = H.funcbody_to_stmt fdef.G.fbody in
          self#visit_stmt f body
      | __else__ -> super#visit_definition f def

    method! visit_field f field =
      match field with
      | G.F stmt -> (
          match stmt.G.s with
          | G.DefStmt (ent, G.FuncDef fdef) ->
              f (Some ent) !current_class fdef;
              (* Go into nested functions but do NOT revisit the function definition again! *)
              let body = H.funcbody_to_stmt fdef.G.fbody in
              self#visit_stmt f body
          | G.DefStmt (ent, G.VarDef { vinit = Some { e = G.Lambda fdef; _ }; _ }) ->
              (* Handle lambda assignments in class fields *)
              f (Some ent) !current_class fdef;
              let body = H.funcbody_to_stmt fdef.G.fbody in
              self#visit_stmt f body
          | _ -> super#visit_field f field)

    method! visit_function_definition f fdef =
      f None !current_class fdef;
      (* go into nested functions *)
      super#visit_function_definition f fdef
  end

(* NOTE: Removed [lazy] because it can crash when using domains. *)
let visitor_instance = new visitor

(* Visit all function definitions in an AST. *)
let visit (f : G.entity option -> G.function_definition -> unit)
    (ast : G.program) : unit =
  let v = visitor_instance in
  (* Check each function definition. *)
  v#visit_program f ast

(* Fold over all function definitions in an AST with an accumulator. *)
let fold (f : 'acc -> G.entity option -> G.function_definition -> 'acc)
    (init_acc : 'acc) (ast : G.program) : 'acc =
  let acc_ref = ref init_acc in
  let v = visitor_instance in
  v#visit_program (fun opt_ent fdef -> acc_ref := f !acc_ref opt_ent fdef) ast;
  !acc_ref

(* Visit all function definitions with class context. *)
let visit_with_class_context
    (f : G.entity option -> G.name option -> G.function_definition -> unit)
    (ast : G.program) : unit =
  let v = new visitor_with_class_context in
  v#visit_program f ast

(* Fold over all function definitions with class context. *)
let fold_with_class_context
    (f :
      'acc -> G.entity option -> G.name option -> G.function_definition -> 'acc)
    (init_acc : 'acc) (ast : G.program) : 'acc =
  let acc_ref = ref init_acc in
  let v = new visitor_with_class_context in
  v#visit_program
    (fun opt_ent class_name fdef ->
      acc_ref := f !acc_ref opt_ent class_name fdef)
    ast;
  !acc_ref
