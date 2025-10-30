open Common
module G = AST_generic
module Log = Log_tainting.Log
open Shape_and_sig
open Optics_types

(* List monoid for collecting results *)
module ListMonoid (T : sig type t end) : MONOID with type t = T.t list = struct
  type t = T.t list
  let empty = []
  let append = (@)
end

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

(* OCamlgraph modules *)
module FuncVertex = struct
  type t = fn_id

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

module FuncGraph = Graph.Persistent.Digraph.Concrete (FuncVertex)
module Topo = Graph.Topological.Make (FuncGraph)
module SCC = Graph.Components.Make (FuncGraph)

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

let extract_go_receiver_type (fdef : G.function_definition) : string option =
  let params = Tok.unbracket fdef.fparams in
  match params with
  | G.ParamReceiver { ptype = Some { t = G.TyN (G.Id ((name, _), _)); _ }; _ }
    :: _ ->
      Some name
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

let fn_id_of_entity ~(lang : Lang.t) (opt_ent : G.entity option)
    (class_name : G.name option) (fdef : G.function_definition) : fn_id option =
  let* ent = opt_ent in
  let* name = AST_to_IL.name_of_entity ent in
  let go_receiver_name =
    match lang with
    | Lang.Go -> extract_go_receiver_type fdef
    | _ -> None
  in
  let class_name_il =
    match go_receiver_name with
    | Some recv_name ->
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

(* Extract fn_id from a single Call expression *)
let extract_call_from_expr ?(object_mappings = []) (current_class : IL.name option)
    (e : G.expr) : fn_id list =
  match e.G.e with
  (* Simple function call: foo() *)
  | G.Call ({ e = G.N (G.Id (id, id_info)); _ }, _) ->
      let callee_name = AST_to_IL.var_of_id_info id id_info in
      [{ class_name = None; name = Some callee_name }]
  (* Qualified call: Module.foo() *)
  | G.Call ({ e = G.N (G.IdQualified { name_last = id, _; name_info; _ }); _ }, _) ->
      let callee_name = AST_to_IL.var_of_id_info id name_info in
      [{ class_name = None; name = Some callee_name }]
  (* Method call: this.method() or self.method() *)
  | G.Call
      ( {
          e =
            G.DotAccess
              ( { e = G.IdSpecial ((G.This | G.Self), _); _ },
                _,
                G.FN (G.Id (id, id_info)) );
          _;
        },
        _ ) ->
      let method_name = AST_to_IL.var_of_id_info id id_info in
      [{ class_name = current_class; name = Some method_name }]
  (* Method call: obj.method() - look up obj's class *)
  | G.Call
      ( {
          e =
            G.DotAccess
              ( { e = G.N (G.Id ((obj_name, _), _)); _ },
                _,
                G.FN (G.Id (id, id_info)) );
          _;
        },
        _ ) ->
      let method_name = AST_to_IL.var_of_id_info id id_info in
      let obj_class =
        object_mappings
        |> List.find_opt (fun (var_name, _class_name) ->
               match var_name with
               | G.Id ((var_str, _), _) -> var_str = obj_name
               | _ -> false)
        |> Option.map (fun (_var_name, class_name) ->
               AST_to_IL.var_of_name class_name)
      in
      [{ class_name = obj_class; name = Some method_name }]
  | _ -> []

(* Collect all statements recursively from a statement using the generated fold *)
let collect_all_stmts (stmt : G.stmt) : G.stmt list =
  let module M = ListMonoid(struct type t = G.stmt end) in
  let nested = G.fold_stmt_kind_to_stmt.fold_map (module M) (fun s -> [s]) stmt.G.s in
  stmt :: nested

(* Collect all expressions from a statement (non-recursive, just this level) *)
let collect_exprs_from_stmt_kind (sk : G.stmt_kind) : G.expr list =
  let module M = ListMonoid(struct type t = G.expr end) in
  G.fold_stmt_kind_to_expr.fold_map (module M) (fun e -> [e]) sk

(* Collect all expressions recursively from an expression *)
let collect_all_exprs (expr : G.expr) : G.expr list =
  let module M = ListMonoid(struct type t = G.expr end) in
  let nested = G.fold_expr_kind_to_expr.fold_map (module M) (fun e -> [e]) expr.G.e in
  expr :: nested

(* Extract all calls from a function definition using optics folds *)
let extract_calls ?(object_mappings = []) (current_class : IL.name option)
    (fdef : G.function_definition) : fn_id list =

  (* Collect all expressions from the function body *)
  let all_exprs =
    match fdef.fbody with
    | G.FBStmt stmt ->
        (* First collect all nested statements recursively *)
        let all_stmts = collect_all_stmts stmt in
        (* Then collect expressions from each statement *)
        let top_level_exprs = all_stmts |> List.concat_map (fun s -> collect_exprs_from_stmt_kind s.G.s) in
        (* Then recursively collect from those expressions *)
        top_level_exprs |> List.concat_map collect_all_exprs
    | G.FBExpr expr ->
        collect_all_exprs expr
    | G.FBDecl _ | G.FBNothing -> []
  in

  (* Extract fn_ids from all Call expressions *)
  let calls =
    all_exprs
    |> List.concat_map (extract_call_from_expr ~object_mappings current_class)
  in

  (* Deduplicate *)
  calls |> List.sort_uniq (fun f1 f2 -> FuncVertex.compare f1 f2)

(* Extract function definitions from a statement with context *)
let rec extract_funcs_from_stmt (stmt : G.stmt) : (G.entity option * G.name option * G.function_definition) list =
  match stmt.s with
  | G.DefStmt (ent, def_kind) -> (
      match def_kind with
      | G.FuncDef fdef ->
          (* Found a function - collect it AND recursively search its body for nested functions *)
          let this_func = [(Some ent, None, fdef)] in
          let nested_funcs = match fdef.G.fbody with
            | G.FBStmt body_stmt ->
                (* Recursively collect nested functions from the body *)
                extract_funcs_from_stmt body_stmt
            | _ -> []
          in
          this_func @ nested_funcs
      | G.ClassDef { cbody = (_, fields, _); _ } ->
          (* Extract methods from class fields *)
          let class_name = match ent.name with
            | G.EN n -> Some n
            | G.EDynamic _ | G.EPattern _ | G.OtherEntity _ -> None
          in
          fields |> List.concat_map (fun field ->
            let (G.F stmt) = field in
            (* Recursively extract from field statement, preserving class context *)
            extract_funcs_from_stmt stmt |> List.map (fun (method_ent, _, fdef) ->
              (* Preserve the method's entity, set class name from parent *)
              (method_ent, class_name, fdef))
          )
      | _ -> [])
  | _ ->
      (* Recursively collect all nested statements and extract from each *)
      let module M = ListMonoid(struct type t = G.stmt end) in
      let all_stmts = G.fold_stmt_kind_to_stmt.fold_map (module M) (fun s -> [s]) stmt.s in
      all_stmts |> List.concat_map extract_funcs_from_stmt

(* Collect all function definitions from the AST *)
let collect_all_funcs (ast : G.program) : (G.entity option * G.name option * G.function_definition) list =
  ast |> List.concat_map extract_funcs_from_stmt

let build_call_graph ~(lang : Lang.t) ?(object_mappings = []) (ast : G.program)
    : FuncGraph.t =
  (* Collect all function definitions using optics *)
  let all_func_contexts = collect_all_funcs ast in

  let funcs, graph =
    List.fold_left
      (fun (funcs, graph) (opt_ent, class_name, fdef) ->
        match fn_id_of_entity ~lang opt_ent class_name fdef with
        | Some fn_id ->
            let func = { fn_id; entity = opt_ent; fdef } in
            let graph = FuncGraph.add_vertex graph fn_id in
            (func :: funcs, graph)
        | None -> (funcs, graph))
      ([], FuncGraph.empty) all_func_contexts
  in

  let graph =
    List.fold_left
      (fun graph (opt_ent, class_name, fdef) ->
        match fn_id_of_entity ~lang opt_ent class_name fdef with
        | Some fn_id ->
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
              extract_calls ~object_mappings current_class_il fdef
            in
            let callee_fn_id =
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

            let graph =
              List.fold_left
                (fun g callee_fn_id -> FuncGraph.add_edge g callee_fn_id fn_id)
                graph callee_fn_id
            in
            graph
        | None -> graph)
      graph all_func_contexts
  in

  (* Add implicit edges from constructors to methods *)
  let graph =
    List.fold_left
      (fun g func ->
        let func_name =
          Option.fold ~none:"" ~some:(fun n -> fst n.IL.ident) func.fn_id.name
        in
        let class_name_str =
          Option.map (fun n -> fst n.IL.ident) func.fn_id.class_name
        in
        if Object_initialization.is_constructor lang func_name class_name_str
        then
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
          List.fold_left
            (fun g2 method_func ->
              FuncGraph.add_edge g2 func.fn_id method_func.fn_id)
            g same_class_methods
        else g)
      graph funcs
  in

  graph
