open Common
module G = AST_generic
module Log = Log_tainting.Log
open Shape_and_sig

(* Type for function information including AST node *)
type func_info = {
  fn_id : fn_id;
  entity : G.entity option;
  fdef : G.function_definition;
}

let compare_as_str f1 f2 =
  let compare_il_name n1 n2 =
    String.compare (fst n1.IL.ident) (fst n2.IL.ident)
  in
  let cmp_opt = Option.compare compare_il_name in
  let c1 = cmp_opt f1.class_name f2.class_name in
  if c1 <> 0 then c1 else cmp_opt f1.name f2.name

(* OCamlgraph: Define vertex module for functions *)
module FuncVertex = struct
  type t = fn_id

  (* Compare by string name and position only to handle
     cases where call site and definition have different SIds. We use
     position as well to differentiate methods with the same name.
     We need to guard for FakeToks. *)
  let compare f1 f2 =
    let compare_il_name n1 n2 =
      let open Tok in
      let st = String.compare (fst n1.IL.ident) (fst n2.IL.ident) in
      if st <> 0 then st
      else
        match (snd n1.IL.ident, snd n2.IL.ident) with
        | FakeTok _, FakeTok _ -> 0
        | FakeTok _, _ -> -1
        | _, FakeTok _ -> 1
        | _ -> Tok.compare_pos (snd n1.IL.ident) (snd n2.IL.ident)
    in
    let cmp_opt = Option.compare compare_il_name in
    let c1 = cmp_opt f1.class_name f2.class_name in
    if c1 <> 0 then c1 else cmp_opt f1.name f2.name

  let hash (f : fn_id) =
    let h1 =
      Option.fold ~none:0 ~some:(fun n -> Hashtbl.hash (fst n.IL.ident)) f.name
    in
    let h2 =
      Option.fold ~none:0
        ~some:(fun c -> Hashtbl.hash (fst c.IL.ident))
        f.class_name
    in
    Hashtbl.hash (h1, h2)

  let equal f1 f2 = Int.equal (compare f1 f2) 0
end

(* OCamlgraph: Create imperative bidirectional graph *)
module FuncGraph = Graph.Imperative.Digraph.ConcreteBidirectional (FuncVertex)

(* OCamlgraph: Use built-in algorithms *)
module Topo = Graph.Topological.Make (FuncGraph)
module SCC = Graph.Components.Make (FuncGraph)

(* Reachability module for computing relevant subgraphs *)
module Reachable = Graph_functor.Reachable (FuncGraph)

(* Graphviz output for debugging *)
module Dot = Graph.Graphviz.Dot (struct
  include FuncGraph

  let graph_attributes _ = []
  let default_vertex_attributes _ = []

  let vertex_name v =
    let class_part =
      match v.class_name with
      | Some c -> fst c.IL.ident ^ "."
      | None -> ""
    in
    let name =
      match v.name with
      | Some n -> fst n.IL.ident
      | None -> "???"
    in
    Printf.sprintf "\"%s%s\"" class_part name

  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end)

(* Extract Go receiver type from method *)
let extract_go_receiver_type (fdef : G.function_definition) : string option =
  let params = Tok.unbracket fdef.fparams in
  match params with
  (* Non-pointer receiver: func (r Type) ... *)
  | G.ParamReceiver { ptype = Some { t = G.TyN (G.Id ((name, _), _)); _ }; _ }
    :: _ ->
      Some name
  (* Pointer receiver: func (r *Type) ... *)
  | G.ParamReceiver
      {
        ptype =
          Some
            { t = G.TyPointer (_, { t = G.TyN (G.Id ((name, _), _)); _ }); _ };
        _;
      }
    :: _ ->
      Some name
  | _ -> None

(* Build fn_id from entity *)
let fn_id_of_entity ~(lang : Lang.t) (opt_ent : G.entity option)
    (class_name : G.name option) (fdef : G.function_definition) : fn_id option =
  let* ent = opt_ent in
  let* name = AST_to_IL.name_of_entity ent in
  (* For Go methods, extract receiver type as class name *)
  let go_receiver_name =
    match lang with
    | Lang.Go -> extract_go_receiver_type fdef
    | _ -> None
  in
  let class_name_il =
    match go_receiver_name with
    | Some recv_name ->
        (* Create IL name from Go receiver type *)
        let fake_tok = Tok.unsafe_fake_tok recv_name in
        Some
          IL.
            {
              ident = (recv_name, fake_tok);
              sid = AST_generic.SId.unsafe_default;
              id_info = AST_generic.empty_id_info ();
            }
    | None -> Option.map AST_to_IL.var_of_name class_name
  in
  Some { class_name = class_name_il; name = Some name }

(* Extract all calls from a function body and resolve them to fn_ids *)
(* Helper function to identify the callee fn_id from a call expression's callee *)
let identify_callee ?(object_mappings = []) ?(all_funcs = [])
    (current_class : IL.name option) (callee : G.expr) : fn_id option =
  match callee.G.e with
        (* Simple function call: foo() *)
        | G.N (G.Id (id, id_info)) ->
            let callee_name = AST_to_IL.var_of_id_info id id_info in
            let callee_name_str = fst callee_name.IL.ident in
            (* For class-based languages, foo() might be an implicit this.foo() call.
               Check if a method with this name exists in the current class. *)
            (match current_class with
            | Some class_name ->
                let class_name_str = fst class_name.IL.ident in
                let method_fn_id = { class_name = Some class_name; name = Some callee_name } in
                (* Check if this method exists in the class *)
                let method_exists = List.exists (fun f ->
                  Int.equal (compare_as_str f.fn_id method_fn_id) 0
                ) all_funcs in
                (* Debug: show all function names *)
                let all_names = all_funcs |> List.map (fun f ->
                  let fn_class = match f.fn_id.class_name with Some c -> fst c.IL.ident | None -> "None" in
                  let fn_name = match f.fn_id.name with Some n -> fst n.IL.ident | None -> "None" in
                  fn_class ^ "." ^ fn_name
                ) |> String.concat ", " in
                Log.debug (fun m -> m "CALL_EXTRACT: In class %s, call to %s, checking %d funcs, method_exists=%b, ALL: [%s]"
                  class_name_str callee_name_str (List.length all_funcs) method_exists all_names);
                if method_exists then
                  Some method_fn_id
                else
                  (* It's a free function call, not a method *)
                  Some { class_name = None; name = Some callee_name }
            | None ->
                Some { class_name = None; name = Some callee_name })
        (* Qualified call: Module.foo() *)
        | G.N (G.IdQualified { name_last = id, _; name_info; _ }) ->
            let callee_name = AST_to_IL.var_of_id_info id name_info in
            Some { class_name = None; name = Some callee_name }
        (* Method call: this.method() or self.method() *)
        | G.DotAccess
            ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
              _,
              G.FN (G.Id (id, id_info)) ) ->
            let method_name = AST_to_IL.var_of_id_info id id_info in
            Some { class_name = current_class; name = Some method_name }
        (* Method call: obj.method() - look up obj's class *)
        | G.DotAccess
            ( { e = G.N (G.Id ((obj_name, _), _)); _ },
              _,
              G.FN (G.Id (id, id_info)) ) ->
            let method_name = AST_to_IL.var_of_id_info id id_info in
            (* Look up obj's class in object_mappings *)
            let obj_class =
              object_mappings
              |> List.find_opt (fun (var_name, _class_name) ->
                     match var_name with
                     | G.Id ((var_str, _), _) -> var_str = obj_name
                     | _ -> false)
              |> Option.map (fun (_var_name, class_name) ->
                     AST_to_IL.var_of_name class_name)
            in
            Some { class_name = obj_class; name = Some method_name }
        | _ ->
            Log.debug (fun m ->
                m "CALL_EXTRACT: Unmatched call pattern: %s"
                  (G.show_expr callee));
            None

let extract_calls ?(object_mappings = []) ?(all_funcs = []) (current_class : IL.name option)
    (fdef : G.function_definition) : fn_id list =
  Log.debug (fun m -> m "CALL_EXTRACT: Starting extraction for function, current_class=%s"
    (match current_class with Some c -> fst c.IL.ident | None -> "None"));
  let calls = ref [] in
  let v =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        | G.Call (callee, args) ->
            (match identify_callee ~object_mappings ~all_funcs current_class callee with
            | Some fn_id -> calls := fn_id :: !calls
            | None -> ());
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        | _ -> super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  (* Deduplicate calls by comparing fn_id *)
  let unique_calls =
    !calls |> List.sort_uniq (fun f1 f2 -> FuncVertex.compare f1 f2)
  in
  unique_calls

(* Detect if a function is a user-defined HOF by checking if it calls any of its parameters.
   Returns a list of (parameter_name, parameter_index) for called parameters. *)
let detect_user_hof (fdef : G.function_definition) : (string * int) list =
  (* Get parameter names *)
  let (_lp, params, _rp) = fdef.fparams in
  let param_names =
    params
    |> List.filter_map (fun param ->
        match param with
        | G.Param { pname = Some (id, _); _ } -> Some id
        | _ -> None)
  in
  (* Check which parameters are called in the function body *)
  let called_params = ref [] in
  let v =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        (* Check for calls to parameter names *)
        | G.Call ({ e = G.N (G.Id ((name, _), _)); _ }, _args) ->
            (match List.find_index (fun p -> p = name) param_names with
            | Some idx ->
                called_params := (name, idx) :: !called_params;
                super#visit_expr env e
            | None -> super#visit_expr env e)
        | _ -> super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  !called_params |> List.sort_uniq (fun (n1, i1) (n2, i2) ->
    let c = String.compare n1 n2 in
    if c <> 0 then c else Int.compare i1 i2)

(* Helper to extract callback name from an argument expression.
   Handles: foo, &foo, Module.foo *)
let extract_callback_from_arg (arg_expr : G.expr) : IL.name option =
  match arg_expr.G.e with
  (* Plain identifier: foo *)
  | G.N (G.Id (id, id_info)) ->
      let callback_name = AST_to_IL.var_of_id_info id id_info in
      Some callback_name
  (* Address-of operator: &foo (C/C++ function pointers) *)
  | G.Ref (_, { e = G.N (G.Id (id, id_info)); _ }) ->
      let callback_name = AST_to_IL.var_of_id_info id id_info in
      Some callback_name
  (* Qualified identifier: Module.foo *)
  | G.N (G.IdQualified { name_last = id, _; name_info; _ }) ->
      let callback_name = AST_to_IL.var_of_id_info id name_info in
      Some callback_name
  | _ -> None

(* Extract HOF callbacks from a function body, returning fn_ids of callbacks.
   Uses same identification logic as extract_calls to build proper fn_ids. *)
let extract_hof_callbacks ?(_object_mappings = []) ?(user_hofs = []) (current_class : IL.name option)
    ~(lang : Lang.t) (fdef : G.function_definition) : fn_id list =
  let hof_configs = Builtin_models.get_hof_configs lang in

  (* Build lists of HOF method/function names from configs *)
  let method_hofs =
    List.concat_map (fun config ->
      match config with
      | Builtin_models.MethodHOF { methods; _ } -> methods
      | _ -> []
    ) hof_configs
  in

  let function_hofs =
    List.filter_map (fun config ->
      match config with
      | Builtin_models.FunctionHOF { functions; callback_index; _ } ->
          Some (functions, callback_index)
      | _ -> None
    ) hof_configs
  in

  let callbacks = ref [] in
  let v =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        (* Method HOF call: arr.map(callback) - callback at index 0 *)
        | G.Call ({ e = G.DotAccess (_, _, G.FN (G.Id ((method_name, _), _))); _ }, args)
          when List.mem method_name method_hofs ->
            (match Tok.unbracket args with
            (* Simple callback: arr.map(foo) *)
            | G.Arg { G.e = G.N (G.Id (id, id_info)); _ } :: _ ->
                let callback_name = AST_to_IL.var_of_id_info id id_info in
                let fn_id = { class_name = None; name = Some callback_name } in
                callbacks := fn_id :: !callbacks;
                super#visit_arguments env args
            (* Qualified callback: arr.map(Module.foo) *)
            | G.Arg { G.e = G.N (G.IdQualified { name_last = id, _; name_info; _ }); _ } :: _ ->
                let callback_name = AST_to_IL.var_of_id_info id name_info in
                let fn_id = { class_name = None; name = Some callback_name } in
                callbacks := fn_id :: !callbacks;
                super#visit_arguments env args
            (* Method callback: arr.map(this.foo) *)
            | G.Arg { G.e = G.DotAccess ({ e = G.IdSpecial ((G.This | G.Self), _); _ }, _, G.FN (G.Id (id, id_info))); _ } :: _ ->
                let callback_name = AST_to_IL.var_of_id_info id id_info in
                let fn_id = { class_name = current_class; name = Some callback_name } in
                callbacks := fn_id :: !callbacks;
                super#visit_arguments env args
            | _ -> super#visit_arguments env args)

        (* Function HOF call: map(callback, arr) or array_map(callback, arr) or user_hof(arr, callback) *)
        | G.Call ({ e = G.N (G.Id (id, id_info)); _ }, args) ->
            let func_name = fst id in
            let callee_name = AST_to_IL.var_of_id_info id id_info in
            (* Check built-in HOFs first *)
            (match List.find_opt (fun (names, _) -> List.mem func_name names) function_hofs with
            | Some (_, callback_index) ->
                (match List.nth_opt (Tok.unbracket args) callback_index with
                (* Simple callback: map(foo, arr) *)
                | Some (G.Arg { G.e = G.N (G.Id (id, id_info)); _ }) ->
                    let callback_name = AST_to_IL.var_of_id_info id id_info in
                    let fn_id = { class_name = None; name = Some callback_name } in
                    callbacks := fn_id :: !callbacks;
                    super#visit_arguments env args
                (* Qualified callback: map(Module.foo, arr) *)
                | Some (G.Arg { G.e = G.N (G.IdQualified { name_last = id, _; name_info; _ }); _ }) ->
                    let callback_name = AST_to_IL.var_of_id_info id name_info in
                    let fn_id = { class_name = None; name = Some callback_name } in
                    callbacks := fn_id :: !callbacks;
                    super#visit_arguments env args
                | _ -> super#visit_arguments env args)
            | None ->
                (* Check if this is a user-defined HOF *)
                let hof_fn_id = { class_name = current_class; name = Some callee_name } in
                (match List.find_opt (fun (fn_id, _) -> Int.equal (compare_as_str fn_id hof_fn_id) 0) user_hofs with
                | Some (_, called_params) ->
                    (* Extract callbacks from the arguments at the indices where parameters are called *)
                    let arg_list = Tok.unbracket args in
                    List.iter (fun (_param_name, param_idx) ->
                      match List.nth_opt arg_list param_idx with
                      | Some (G.Arg arg_expr) ->
                          (match extract_callback_from_arg arg_expr with
                          | Some callback_name ->
                              let fn_id = { class_name = None; name = Some callback_name } in
                              Log.debug (fun m -> m "USER_HOF_CALLBACK: Found callback %s at arg %d in call to %s"
                                (fst callback_name.IL.ident) param_idx func_name);
                              callbacks := fn_id :: !callbacks
                          | None -> ())
                      | _ -> ()
                    ) called_params;
                    super#visit_arguments env args
                | None ->
                    (* Not a builtin or user HOF, check if it's a user HOF without class *)
                    let free_hof_fn_id = { class_name = None; name = Some callee_name } in
                    (match List.find_opt (fun (fn_id, _) -> Int.equal (compare_as_str fn_id free_hof_fn_id) 0) user_hofs with
                    | Some (_, called_params) ->
                        let arg_list = Tok.unbracket args in
                        List.iter (fun (_param_name, param_idx) ->
                          match List.nth_opt arg_list param_idx with
                          | Some (G.Arg arg_expr) ->
                              (match extract_callback_from_arg arg_expr with
                              | Some callback_name ->
                                  let fn_id = { class_name = None; name = Some callback_name } in
                                  Log.debug (fun m -> m "USER_HOF_CALLBACK: Found callback %s at arg %d in call to %s"
                                    (fst callback_name.IL.ident) param_idx func_name);
                                  callbacks := fn_id :: !callbacks
                              | None -> ())
                          | _ -> ()
                        ) called_params;
                        super#visit_arguments env args
                    | None -> super#visit_expr env e)))

        | _ -> super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  (* Deduplicate callbacks by comparing fn_id *)
  let unique_callbacks =
    !callbacks |> List.sort_uniq (fun f1 f2 -> FuncVertex.compare f1 f2)
  in
  unique_callbacks

(* Build call graph - Visit_function_defs handles regular functions,
   arrow functions, and lambda assignments like const x = () => {} *)
let build_call_graph ~(lang : Lang.t) ?(object_mappings = []) (ast : G.program)
    : FuncGraph.t =
  let graph = FuncGraph.create () in

  (* Create a special top_level node to represent code outside functions *)
  let top_level_name =
    let fake_tok = Tok.unsafe_fake_tok "<top_level>" in
    Some IL.{ ident = ("<top_level>", fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () }
  in
  let top_level_fn_id = Shape_and_sig.{ class_name = None; name = top_level_name } in
  FuncGraph.add_vertex graph top_level_fn_id;

  let funcs =
    Visit_function_defs.fold_with_class_context
      (fun funcs opt_ent class_name fdef ->
        match fn_id_of_entity ~lang opt_ent class_name fdef with
        | Some fn_id ->
            let func = { fn_id; entity = opt_ent; fdef } in
            FuncGraph.add_vertex graph fn_id;
            func :: funcs
        | None -> funcs)
      [] ast
  in
  (* Detect user-defined HOFs *)
  let user_hofs =
    funcs
    |> List.filter_map (fun { fn_id; fdef; _ } ->
        let called_params = detect_user_hof fdef in
        if List.length called_params > 0 then (
          let fn_name = match fn_id.name with Some n -> fst n.IL.ident | None -> "?" in
          let class_name = match fn_id.class_name with Some c -> fst c.IL.ident | None -> "None" in
          Log.debug (fun m -> m "USER_HOF: Detected %s.%s calls parameters: %s"
            class_name fn_name
            (called_params |> List.map (fun (name, idx) -> Printf.sprintf "%s@%d" name idx) |> String.concat ", "));
          Some (fn_id, called_params)
        ) else
          None)
  in
  (* Build a map from function body ranges to fn_id for context tracking *)
  let func_ranges = ref [] in
  List.iter (fun func ->
    let body_stmt = AST_generic_helpers.funcbody_to_stmt func.fdef.G.fbody in
    match AST_generic_helpers.range_of_any_opt (G.S body_stmt) with
    | Some (loc_start, loc_end) ->
        let body_start = loc_start.pos.bytepos in
        let body_end = loc_end.pos.bytepos in
        func_ranges := (body_start, body_end, func.fn_id) :: !func_ranges
    | None -> ())
    funcs;

  (* Visit all calls in the AST, tracking the current function context *)
  Visit_function_defs.visit_with_class_context
    (fun opt_ent class_name fdef ->
      match fn_id_of_entity ~lang opt_ent class_name fdef with
      | Some fn_id ->
          (* Extract calls with object context *)
          (* For Go methods, use the receiver type as current_class *)
          let current_class_il =
            match lang with
            | Lang.Go -> (
                match extract_go_receiver_type fdef with
                | Some recv_name ->
                    let fake_tok = Tok.unsafe_fake_tok recv_name in
                    Some
                      IL.
                        {
                          ident = (recv_name, fake_tok);
                          sid = AST_generic.SId.unsafe_default;
                          id_info = AST_generic.empty_id_info ();
                        }
                | None -> Option.map AST_to_IL.var_of_name class_name)
            | _ -> Option.map AST_to_IL.var_of_name class_name
          in
          let callee_fn_names =
            extract_calls ~object_mappings ~all_funcs:funcs current_class_il fdef
          in
          let callee_fn_ids =
            List.fold_left
              (fun acc x ->
                acc
                @ List.filter_map
                    (fun y ->
                      if Int.equal (compare_as_str x y.fn_id) 0 then
                        Some y.fn_id
                      else None)
                    funcs)
              [] callee_fn_names
          in

          (* Add edges for each call - edge from callee to caller for bottom-up analysis *)
          List.iter
            (fun callee_fn_id -> FuncGraph.add_edge graph callee_fn_id fn_id)
            callee_fn_ids;

          (* Extract HOF callbacks and add edges: callback -> caller *)
          let callback_fn_names =
            extract_hof_callbacks ~_object_mappings:object_mappings ~user_hofs current_class_il ~lang fdef
          in
          let callback_fn_ids =
            List.fold_left
              (fun acc x ->
                acc
                @ List.filter_map
                    (fun y ->
                      if Int.equal (compare_as_str x y.fn_id) 0 then
                        Some y.fn_id
                      else None)
                    funcs)
              [] callback_fn_names
          in
          (* Add edges for each callback - edge from callback to caller for bottom-up analysis *)
          List.iter
            (fun callback_fn_id -> FuncGraph.add_edge graph callback_fn_id fn_id)
            callback_fn_ids
      | None -> ())
    ast;

  (* Visitor that tracks class and function context *)
  (* Store G.name, convert to IL.name when needed *)
  let current_class_name = ref None in
  let current_fn_id = ref None in

  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_stmt (env : unit) (stmt : G.stmt) =
        match stmt.G.s with
        | G.DefStmt (ent, G.ClassDef { cbody = _; _ }) ->
            (* Enter class context *)
            let old_class = !current_class_name in
            let old_fn = !current_fn_id in
            (match ent.G.name with
            | G.EN name ->
                current_class_name := Some name;
                current_fn_id := None
            | _ -> ());
            super#visit_stmt env stmt;
            current_class_name := old_class;
            current_fn_id := old_fn
        | G.DefStmt (ent, G.FuncDef fdef) ->
            (* Enter function context - save both class and function *)
            let old_class = !current_class_name in
            let old_fn = !current_fn_id in
            let new_fn_id = fn_id_of_entity ~lang (Some ent) !current_class_name fdef in
            current_fn_id := new_fn_id;

            (* Extract calls within this function using existing logic *)
            (match new_fn_id with
            | Some fn_id ->
                let current_class_il = Option.map AST_to_IL.var_of_name !current_class_name in
                let callee_fn_names =
                  extract_calls ~object_mappings ~all_funcs:funcs current_class_il fdef
                in
                let callee_fn_ids =
                  List.fold_left
                    (fun acc x ->
                      acc
                      @ List.filter_map
                          (fun y ->
                            if Int.equal (compare_as_str x y.fn_id) 0 then
                              Some y.fn_id
                            else None)
                          funcs)
                    [] callee_fn_names
                in
                List.iter
                  (fun callee_fn_id -> FuncGraph.add_edge graph callee_fn_id fn_id)
                  callee_fn_ids;

                let callback_fn_names =
                  extract_hof_callbacks ~_object_mappings:object_mappings ~user_hofs current_class_il ~lang fdef
                in
                let callback_fn_ids =
                  List.fold_left
                    (fun acc x ->
                      acc
                      @ List.filter_map
                          (fun y ->
                            if Int.equal (compare_as_str x y.fn_id) 0 then
                              Some y.fn_id
                            else None)
                          funcs)
                    [] callback_fn_names
                in
                List.iter
                  (fun callback_fn_id -> FuncGraph.add_edge graph callback_fn_id fn_id)
                  callback_fn_ids
            | None -> ());

            super#visit_stmt env stmt;
            current_class_name := old_class;
            current_fn_id := old_fn
        | _ -> super#visit_stmt env stmt

      method! visit_expr (env : unit) (e : G.expr) =
        (match e.G.e with
        | G.Call (callee, _args) when Option.is_none !current_fn_id ->
            (* This is a call NOT inside a function - handle it here *)
            let caller_fn_id = match !current_class_name with
              | Some g_name ->
                  (* Inside class but not in function - create class node *)
                  let il_class_name = AST_to_IL.var_of_name g_name in
                  let class_str = fst il_class_name.IL.ident in
                  let class_node_name =
                    let fake_tok = Tok.unsafe_fake_tok ("Class:" ^ class_str) in
                    Some IL.{ ident = ("Class:" ^ class_str, fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () }
                  in
                  let class_fn_id = Shape_and_sig.{ class_name = None; name = class_node_name } in
                  FuncGraph.add_vertex graph class_fn_id;
                  class_fn_id
              | None -> top_level_fn_id  (* Top-level *)
            in
            (* Extract callee using the same helper function as extract_calls *)
            let current_class_il = Option.map AST_to_IL.var_of_name !current_class_name in
            let callee_fn_id_opt = identify_callee ~object_mappings ~all_funcs:funcs current_class_il callee in
            (match callee_fn_id_opt with
            | Some callee_fn_id ->
                if List.exists (fun f -> Int.equal (compare_as_str callee_fn_id f.fn_id) 0) funcs then
                  FuncGraph.add_edge graph callee_fn_id caller_fn_id
            | None -> ())
        | _ -> ());
        super#visit_expr env e
    end
  in
  visitor#visit_program () ast;

  (* Add implicit edges from constructors to all methods in the same class.
     Constructors always execute before any method can be called on an object. *)
  List.iter
    (fun func ->
      let func_name =
        Option.fold ~none:"" ~some:(fun n -> fst n.IL.ident) func.fn_id.name
      in
      let class_name_str =
        Option.map (fun n -> fst n.IL.ident) func.fn_id.class_name
      in
      if Object_initialization.is_constructor lang func_name class_name_str then
        (* Find all methods in the same class *)
        let same_class_methods =
          List.filter
            (fun other ->
              let other_name =
                Option.fold ~none:""
                  ~some:(fun n -> fst n.IL.ident)
                  other.fn_id.name
              in
              let other_class_name_str =
                Option.map (fun n -> fst n.IL.ident) other.fn_id.class_name
              in
              (not
                 (Object_initialization.is_constructor lang other_name
                    other_class_name_str))
              && Option.equal
                   (fun n1 n2 ->
                     String.equal (fst n1.IL.ident) (fst n2.IL.ident))
                   func.fn_id.class_name other.fn_id.class_name)
            funcs
        in
        (* Add edge from constructor to each method *)
        List.iter
          (fun method_func ->
            FuncGraph.add_edge graph func.fn_id method_func.fn_id)
          same_class_methods)
    funcs;

  graph

(* Identify functions that contain byte ranges (from pattern matches) *)
let find_functions_containing_ranges ~(lang : Lang.t) (ast : G.program)
    (ranges : Range.t list) : fn_id list =
  (* Hash table to track ALL functions containing each range, along with function size *)
  let range_to_funcs : (Range.t, (fn_id * int) list) Hashtbl.t = Hashtbl.create 10 in
  List.iter (fun range -> Hashtbl.add range_to_funcs range []) ranges;

  let visitor = object
    inherit [_] G.iter_no_id_info as super
    val current_class : G.name option ref = ref None

    method! visit_definition (env : unit) ((ent, def_kind) as def) =
      match def_kind with
      | G.ClassDef cdef ->
          let old_class = !current_class in
          (current_class :=
             match ent.name with
             | EN name -> Some name
             | _ -> None);

          (* Get the class body range *)
          let (_, cbody_stmts, _) = cdef.cbody in
          let cbody_range_opt = AST_generic_helpers.range_of_any_opt (G.Flds cbody_stmts) in
          (match cbody_range_opt with
          | Some (loc_start, loc_end) ->
              let class_start = loc_start.pos.bytepos in
              let class_end = loc_end.pos.bytepos in
              let class_size = class_end - class_start in

              (* For each range, check if it's inside this class *)
              List.iter (fun (range : Range.t) ->
                if class_start <= range.Range.start && range.Range.end_ <= class_end then (
                  (* This class contains this range - add it to the list *)
                  match !current_class with
                  | Some class_g_name ->
                      let class_il_name = AST_to_IL.var_of_name class_g_name in
                      let class_str = fst class_il_name.IL.ident in
                      let class_node_name =
                        let fake_tok = Tok.unsafe_fake_tok ("Class:" ^ class_str) in
                        Some IL.{ ident = ("Class:" ^ class_str, fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () }
                      in
                      let class_fn_id = Shape_and_sig.{ class_name = None; name = class_node_name } in
                      let existing = Hashtbl.find range_to_funcs range in
                      if not (List.exists (fun (fid, _) -> FuncVertex.equal fid class_fn_id) existing) then
                        Hashtbl.replace range_to_funcs range ((class_fn_id, class_size) :: existing)
                  | None -> ()
                )
              ) ranges;

              super#visit_definition env def
          | None -> super#visit_definition env def);
          current_class := old_class
      | G.FuncDef fdef ->
          (* Get the entire function definition range (including parameters) *)
          let func_range_opt = AST_generic_helpers.range_of_any_opt (G.Def def) in
          (match func_range_opt with
          | Some (loc_start, loc_end) ->
              let func_start = loc_start.pos.bytepos in
              let func_end = loc_end.pos.bytepos in
              let func_size = func_end - func_start in

              (* For each range, check if it's inside this function *)
              List.iter (fun (range : Range.t) ->
                if func_start <= range.Range.start && range.Range.end_ <= func_end then (
                  (* This function contains this range - add it to the list *)
                  match fn_id_of_entity ~lang (Some ent) !current_class fdef with
                  | Some fn_id ->
                      let existing = Hashtbl.find range_to_funcs range in
                      if not (List.exists (fun (fid, _) -> FuncVertex.equal fid fn_id) existing) then
                        Hashtbl.replace range_to_funcs range ((fn_id, func_size) :: existing)
                  | None -> ()
                )
              ) ranges;

              super#visit_definition env def
          | None -> super#visit_definition env def)
      | _ -> super#visit_definition env def
  end in

  visitor#visit_program () ast;

  (* Now select the innermost (smallest) function for each range *)
  let matching_funcs = ref [] in
  List.iter (fun range ->
    let funcs_list = Hashtbl.find range_to_funcs range in
    if funcs_list <> [] then (
      (* Sort by size and pick the smallest (innermost) *)
      let sorted = List.sort (fun (_, size1) (_, size2) -> compare size1 size2) funcs_list in
      let (innermost_fn_id, _) = List.hd sorted in
      if not (List.mem innermost_fn_id !matching_funcs) then
        matching_funcs := innermost_fn_id :: !matching_funcs
    ) else (
      (* No function contains this range - it's at top level *)
      let top_level_name =
        let fake_tok = Tok.unsafe_fake_tok "<top_level>" in
        Some IL.{ ident = ("<top_level>", fake_tok); sid = G.SId.unsafe_default; id_info = AST_generic.empty_id_info () }
      in
      let top_level_fn_id = Shape_and_sig.{ class_name = None; name = top_level_name } in
      if not (List.mem top_level_fn_id !matching_funcs) then
        matching_funcs := top_level_fn_id :: !matching_funcs
    )
  ) ranges;

  !matching_funcs

(* Compute the subgraph containing only functions relevant for taint flow
   from sources to sinks. This uses the nearest common descendant algorithm
   to find all paths between source and sink functions. *)
let compute_relevant_subgraph (graph : FuncGraph.t) (sources : fn_id list)
    (sinks : fn_id list) : FuncGraph.t =
  match (sources, sinks) with
  | [], _ | _, [] ->
      (* No sources or sinks, return empty graph *)
      FuncGraph.create ()
  | _ :: _, _ :: _ ->
      (* Compute union of all nearest common descendant subgraphs
         for each source-sink pair *)
      let result = FuncGraph.create () in
      List.iter
        (fun source ->
          List.iter
            (fun sink ->
              let subgraph =
                Reachable.nearest_common_descendant_subgraph graph source sink
              in
              (* Add all vertices and edges from subgraph to result *)
              FuncGraph.iter_vertex (FuncGraph.add_vertex result) subgraph;
              FuncGraph.iter_edges_e (FuncGraph.add_edge_e result) subgraph)
            sinks)
        sources;
      result
