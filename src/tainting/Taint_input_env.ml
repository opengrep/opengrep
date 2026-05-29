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
module Log = Log_tainting.Log
module S = Shape_and_sig.Shape
module Signature_extractor = Taint_signature_extractor

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

let is_global (id_info : G.id_info) =
  let* kind, _sid = !(id_info.id_resolved) in
  Some (H.name_is_global kind)

let signature_of_object_method taint_inst method_name fdef =
  let fdef_il = AST_to_IL.function_definition taint_inst.Taint_rule_inst.lang fdef in
  let cfg = CFG_build.cfg_of_fdef fdef_il in
  let { Signature_extractor.signature; _ } =
    Signature_extractor.extract_signature taint_inst ~name:method_name cfg
  in
  signature

let add_object_method_shapes taint_inst object_name opt_expr env =
  let add_method env method_name fdef =
    let fun_sig = signature_of_object_method taint_inst method_name fdef in
    let lval : IL.lval =
      {
        base = Var object_name;
        rev_offset = [ { o = Dot method_name; oorig = NoOrig } ];
      }
    in
    Taint_lval_env.add_lval_shape lval T.Taint_set.empty (S.Fun fun_sig) env
  in
  let add_record_method env = function
    | G.F
        {
          s =
            G.DefStmt
              ( { G.name = G.EN (G.Id (id, id_info)); tparams = None; _ },
                G.FuncDef fdef );
          _;
        } ->
        let method_name = AST_to_IL.var_of_id_info id id_info in
        add_method env method_name fdef
    | _ -> env
  in
  match opt_expr with
  | Some { G.e = G.Record (_, fields, _); _ } ->
      List.fold_left add_record_method env fields
  | _ -> env

let add_to_env taint_inst (env, effects) id id_info opt_expr =
  let var = AST_to_IL.var_of_id_info id id_info in
  let env, new_effects = add_to_env_aux taint_inst env id id_info opt_expr in
  let env = add_object_method_shapes taint_inst var opt_expr env in
  (env, Effects.union new_effects effects)

let mk_fun_input_env taint_inst ?(glob_env = Taint_lval_env.empty)
    (fparams : IL.param list) =
  let add_to_env = add_to_env taint_inst in
  fparams
  (* For each argument, check if it's a source and, if so, add it to the input
     * environment. *)
  |> Fold_IL_params.fold add_to_env (glob_env, Effects.empty)

let alias_name_of_import (id : G.ident) (alias : G.alias option) :
    IL.name option =
  match alias with
  | Some (alias_id, alias_info) ->
      Some (AST_to_IL.var_of_id_info alias_id alias_info)
  | None ->
      if String.equal (fst id) Ast_js.default_entity then None
      else
        Some
          IL.
            {
              ident = id;
              sid = G.SId.unsafe_default;
              id_info = G.empty_id_info ();
            }

let add_import_alias_to_env env (module_name : G.module_name)
    ((imported_id, alias) : G.ident * G.alias option) =
  let* alias_name = alias_name_of_import imported_id alias in
  let module_path_parts =
    Dataflow_tainting.import_path_parts_of_module_name module_name
  in
  let* (S.Cell (xtaint, shape)) =
    Dataflow_tainting.find_exported_global_cell env ~module_path_parts
      ~export_name:(fst imported_id)
  in
  let il_lval : IL.lval = { base = Var alias_name; rev_offset = [] } in
  Some
    (Taint_lval_env.add_lval_shape il_lval (Xtaint.to_taints xtaint) shape env)

let wildcard_alias_name import_tok (exported_name : IL.name) : IL.name =
  IL.
    {
      ident = (fst exported_name.ident, import_tok);
      sid = G.SId.unsafe_default;
      id_info = G.empty_id_info ();
    }

let add_wildcard_import_aliases_to_env env import_tok module_name =
  let module_path_parts =
    Dataflow_tainting.import_path_parts_of_module_name module_name
  in
  Dataflow_tainting.exported_global_cells env ~module_path_parts
  |> List.fold_left
       (fun env (exported_name, S.Cell (xtaint, shape)) ->
         let alias_name = wildcard_alias_name import_tok exported_name in
         let il_lval : IL.lval = { base = Var alias_name; rev_offset = [] } in
         Taint_lval_env.add_lval_shape il_lval (Xtaint.to_taints xtaint) shape
           env)
       env

type import_alias =
  | ImportNames of G.module_name * (G.ident * G.alias option) list
  | ImportWildcard of G.module_name * Tok.t

let collect_import_alias_directives (ast : G.program) : import_alias list =
  let imports = ref [] in
  let visitor =
    object (_self : 'self)
      inherit [_] G.iter_no_id_info as super

      method! visit_directive env directive =
        (match directive.G.d with
        | G.ImportFrom (_, module_name, imported_names) ->
            imports := ImportNames (module_name, imported_names) :: !imports
        | G.ImportAll (import_tok, module_name, _) ->
            imports := ImportWildcard (module_name, import_tok) :: !imports
        | _ -> ());
        super#visit_directive env directive
    end
  in
  visitor#visit_program () ast;
  List.rev !imports

let add_import_aliases imports env =
  let pass env =
    imports
    |> List.fold_left
         (fun env import_alias ->
           match import_alias with
           | ImportNames (module_name, imported_names) ->
               imported_names
               |> List.fold_left
                    (fun env imported_name ->
                      add_import_alias_to_env env module_name imported_name
                      ||| env)
                    env
           | ImportWildcard (module_name, import_tok) ->
               add_wildcard_import_aliases_to_env env import_tok module_name)
         env
  in
  let max_passes = List.length imports + 1 in
  let rec loop remaining env =
    if remaining <= 0 then env
    else
      let env' = pass env in
      if Taint_lval_env.equal env env' then env'
      else loop (remaining - 1) env'
  in
  loop max_passes env

let mk_file_env taint_inst ast =
  let add_to_env = add_to_env taint_inst in
  let env = ref (Taint_lval_env.empty, Effects.empty) in
  let imports = collect_import_alias_directives ast in
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
           DotAccess
             ( {
                 e =
                   DotAccess
                     ( { e = N (Id (("module", _), _)); _ },
                       _,
                       FN (Id (("exports", _), _)) );
                 _;
               },
               _,
               FN (Id (id, id_info)) );
         _;
        } ->
            env := add_to_env !env id id_info (Some expr)
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
  let env, effects = !env in
  (add_import_aliases imports env, effects)
