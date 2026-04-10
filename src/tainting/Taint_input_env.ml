(* Iago Abal
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module G = AST_generic
module H = AST_generic_helpers
module T = Taint
module Effects = Shape_and_sig.Effects
module Shape = Shape_and_sig.Shape
module Log = Log_tainting.Log

let check_var_def (taint_inst : Taint_rule_inst.t) env id ii expr =
  let name = AST_to_IL.var_of_id_info id ii in
  let assign =
    G.Assign (G.N (G.Id (id, ii)) |> G.e, Tok.fake_tok (snd id) "=", expr)
    |> G.e |> G.exprstmt
  in
  let xs = AST_to_IL.stmt taint_inst.lang assign in
  let cfg, lambdas = CFG_build.cfg_of_stmts xs in
  Log.debug (fun m ->
      m
        "Taint_input_env:\n\
         --------------------\n\
         Checking var def %s\n\
         --------------------"
        (fst id));
  let effects, end_mapping =
    (* There could be taint effects indeed, e.g. if 'expr' is `sink(taint)`. *)
    Dataflow_tainting.fixpoint taint_inst ~in_env:env
      IL.{ params = []; cfg; lambdas }
  in
  let out_env = end_mapping.(cfg.exit).Dataflow_core.out_env in
  let lval : IL.lval = { base = Var name; rev_offset = [] } in
  let xtaint = Taint_lval_env.find_lval_xtaint out_env lval in
  (xtaint, effects)

let add_to_env_aux (taint_inst : Taint_rule_inst.t) env id ii opt_expr =
  let var = AST_to_IL.var_of_id_info id ii in
  let var_type = Typing.resolved_type_of_id_info taint_inst.lang var.id_info in
  let id_taints =
    taint_inst.preds.is_source (G.Tk (snd id))
    |> List_.map (fun (x : _ Taint_spec_match.t) -> (x.spec_pm, x.spec))
    (* These sources come from the parameters to a function,
        which are not within the normal control flow of a code.
        We can safely say there's no incoming taints to these sources.
    *)
    |> T.taints_of_pms ~incoming:T.Taint_set.empty
  in
  let expr_taints, expr_effects =
    match opt_expr with
    | Some e ->
        let xtaint, effects = check_var_def taint_inst env id ii e in
        (Xtaint.to_taints xtaint, effects)
    | None -> (T.Taint_set.empty, Effects.empty)
  in
  let taints = id_taints |> T.Taint_set.union expr_taints in
  let taints =
    Dataflow_tainting.drop_taints_if_bool_or_number taint_inst.options taints
      var_type
  in
  let env =
    env |> Taint_lval_env.add_lval (IL_helpers.lval_of_var var) taints
  in
  (env, expr_effects)

let add_to_env taint_inst (env, effects) id id_info opt_expr =
  let env, new_effects = add_to_env_aux taint_inst env id id_info opt_expr in
  (env, Effects.union new_effects effects)

let add_param_shape (i, env) id id_info _opt_expr =
  let var = AST_to_IL.var_of_id_info id id_info in
  let il_lval : IL.lval = { base = Var var; rev_offset = [] } in
  let taint_arg : T.arg = { name = fst id; index = i } in
  let env =
    Taint_lval_env.add_lval_shape il_lval T.Taint_set.empty (Shape.Arg taint_arg)
      env
  in
  (i + 1, env)

let mk_fun_param_shapes (fparams : IL.param list) =
  let _, env =
    fparams |> Fold_IL_params.fold add_param_shape (0, Taint_lval_env.empty)
  in
  env

let mk_fun_input_env taint_inst ?(glob_env = Taint_lval_env.empty)
    (fparams : IL.param list) =
  let add_to_env = add_to_env taint_inst in
  let env, effects =
    fparams
    (* The callback/HOF summaries rely on Arg-shaped parameter assumptions being
       available at function entry; otherwise the taint engine loses the link
       between callback parameters and the actual arguments supplied by callers. *)
    |> Fold_IL_params.fold add_to_env (glob_env, Effects.empty)
  in
  let param_assumptions = mk_fun_param_shapes fparams in
  (Taint_lval_env.union env param_assumptions, effects)

let is_global (id_info : G.id_info) =
  let* kind, _sid = !(id_info.id_resolved) in
  Some (H.name_is_global kind)

let mk_file_env taint_inst ast =
  let add_to_env = add_to_env taint_inst in
  let env = ref (Taint_lval_env.empty, Effects.empty) in
  let visitor =
    object (_self : 'self)
      inherit [_] G.iter_no_id_info as super

      method! visit_definition env (entity, def_kind) =
        match (entity, def_kind) with
        | { name = EN (Id (id, id_info)); _ }, VarDef { vinit; _ }
          when IdFlags.is_final !(id_info.id_flags)
               && is_global id_info =*= Some true ->
            env := add_to_env !env id id_info vinit
        | __else__ -> super#visit_definition env (entity, def_kind)

      method! visit_Assign env lhs tok expr =
        match lhs with
        | {
         e =
           ( N (Id (id, id_info))
           | DotAccess
               ( { e = IdSpecial ((This | Self), _); _ },
                 _,
                 FN (Id (id, id_info)) ) );
         _;
        }
          when IdFlags.is_final !(id_info.id_flags)
               && is_global id_info =*= Some true ->
            env := add_to_env !env id id_info (Some expr)
        | __else__ -> super#visit_Assign env lhs tok expr
    end
  in
  visitor#visit_program env ast;
  !env
