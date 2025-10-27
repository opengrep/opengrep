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

(* OCamlgraph: Create directed graph *)
module FuncGraph = Graph.Persistent.Digraph.Concrete (FuncVertex)

(* OCamlgraph: Use built-in algorithms *)
module Topo = Graph.Topological.Make (FuncGraph)
module SCC = Graph.Components.Make (FuncGraph)

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
let extract_calls ?(object_mappings = []) (current_class : IL.name option)
    (fdef : G.function_definition) : fn_id list =
  let calls = ref [] in
  let v =
    object
      inherit [_] G.iter as super

      method! visit_expr env e =
        match e.G.e with
        (* Simple function call: foo() *)
        | G.Call ({ e = G.N (G.Id (id, id_info)); _ }, args) ->
            let callee_name = AST_to_IL.var_of_id_info id id_info in
            let fn_id = { class_name = None; name = Some callee_name } in
            calls := fn_id :: !calls;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        (* Qualified call: Module.foo() *)
        | G.Call
            ( { e = G.N (G.IdQualified { name_last = id, _; name_info; _ }); _ },
              args ) ->
            let callee_name = AST_to_IL.var_of_id_info id name_info in
            let fn_id = { class_name = None; name = Some callee_name } in
            calls := fn_id :: !calls;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
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
              args ) ->
            let method_name = AST_to_IL.var_of_id_info id id_info in
            let fn_id =
              { class_name = current_class; name = Some method_name }
            in
            calls := fn_id :: !calls;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
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
              args ) ->
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
            let fn_id = { class_name = obj_class; name = Some method_name } in
            calls := fn_id :: !calls;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        | G.Call _ -> super#visit_expr env e
        | _ -> super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  (* Deduplicate calls by comparing fn_id *)
  let unique_calls =
    !calls |> List.sort_uniq (fun f1 f2 -> FuncVertex.compare f1 f2)
  in
  unique_calls

(* Build call graph - Visit_function_defs handles regular functions,
   arrow functions, and lambda assignments like const x = () => {} *)
let build_call_graph ~(lang : Lang.t) ?(object_mappings = []) (ast : G.program)
    : FuncGraph.t =
  let funcs, graph =
    Visit_function_defs.fold_with_class_context
      (fun (funcs, graph) opt_ent class_name fdef ->
        match fn_id_of_entity ~lang opt_ent class_name fdef with
        | Some fn_id ->
            let func = { fn_id; entity = opt_ent; fdef } in
            let graph = FuncGraph.add_vertex graph fn_id in
            (func :: funcs, graph)
        | None -> (funcs, graph))
      ([], FuncGraph.empty) ast
  in
  let graph =
    Visit_function_defs.fold_with_class_context
      (fun graph opt_ent class_name fdef ->
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

            (* Add edges for each call - edge from callee to caller for bottom-up analysis *)
            let graph =
              List.fold_left
                (fun g callee_fn_id -> FuncGraph.add_edge g callee_fn_id fn_id)
                graph callee_fn_id
            in
            graph
        | None -> graph)
      graph ast
  in

  (* Add implicit edges from constructors to all methods in the same class.
     Constructors always execute before any method can be called on an object. *)
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
          List.fold_left
            (fun g2 method_func ->
              FuncGraph.add_edge g2 func.fn_id method_func.fn_id)
            g same_class_methods
        else g)
      graph funcs
  in

  graph
