open Common
module G = AST_generic
module Log = Log_tainting.Log

(* Use the same function identifier type as the signature database *)
type fn_id = Shape_and_sig.fn_id = {
  class_name : IL.name option;
  name : IL.name option;
}

(* Type for function information including AST node *)
type func_info = {
  fn_id : fn_id;
  entity : G.entity option;
  fdef : G.function_definition;
}

(* OCamlgraph: Define vertex module for functions *)
module FuncVertex = struct
  type t = fn_id

  (* Compare by string name only (same as signature database) to handle
     cases where call site and definition have different SIds *)
  let compare f1 f2 =
    let compare_il_name n1 n2 = String.compare (fst n1.IL.ident) (fst n2.IL.ident) in
    let cmp_opt = Option.compare compare_il_name in
    let c1 = cmp_opt f1.class_name f2.class_name in
    if c1 <> 0 then c1 else cmp_opt f1.name f2.name

  let hash (f : fn_id) =
    let h1 = Option.fold ~none:0 ~some:(fun n -> Hashtbl.hash (fst n.IL.ident)) f.name in
    let h2 =
      Option.fold ~none:0 ~some:(fun c -> Hashtbl.hash (fst c.IL.ident)) f.class_name
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
    let name = match v.name with
      | Some n -> fst n.IL.ident
      | None -> "???"
    in
    Printf.sprintf "\"%s\"" name
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end)

(* Build fn_id from entity *)
let fn_id_of_entity (opt_ent : G.entity option) (class_name : G.name option) :
    fn_id option =
  let* ent = opt_ent in
  let* name = AST_to_IL.name_of_entity ent in
  let class_name_il = Option.map AST_to_IL.var_of_name class_name in
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
            ({ e = G.N (G.IdQualified { name_last = id, _; name_info; _ }); _ }, args)
          ->
            let callee_name = AST_to_IL.var_of_id_info id name_info in
            let fn_id = { class_name = None; name = Some callee_name } in
            calls := fn_id :: !calls;
            (* Continue visiting arguments for nested calls *)
            super#visit_arguments env args
        (* Method call: this.method() or self.method() *)
        | G.Call ({ e = G.DotAccess ({ e = G.IdSpecial ((G.This | G.Self), _); _ }, _, G.FN (G.Id (id, id_info))); _ }, args) ->
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
        | G.Call _ ->
            super#visit_expr env e
        | _ ->
            super#visit_expr env e
    end
  in
  v#visit_function_definition () fdef;
  (* Deduplicate calls by comparing fn_id *)
  let unique_calls =
    !calls
    |> List.sort_uniq (fun f1 f2 -> FuncVertex.compare f1 f2)
  in
  unique_calls

(* Build call graph - Visit_function_defs handles regular functions,
   arrow functions, and lambda assignments like const x = () => {} *)
let build_call_graph ~(lang : Lang.t) ?(object_mappings = []) (ast : G.program) : FuncGraph.t =
  let funcs, graph =
    Visit_function_defs.fold_with_class_context
      (fun (funcs, graph) opt_ent class_name fdef ->
        match fn_id_of_entity opt_ent class_name with
        | Some fn_id ->
            let func = { fn_id; entity = opt_ent; fdef } in
            let graph = FuncGraph.add_vertex graph fn_id in
            (* Extract calls with object context *)
            let current_class_il = Option.map AST_to_IL.var_of_name class_name in
            let callee_fn_ids =
              extract_calls ~object_mappings current_class_il fdef
            in
            (* Add edges for each call - edge from callee to caller for bottom-up analysis *)
            let graph =
              List.fold_left
                (fun g callee_fn_id -> FuncGraph.add_edge g callee_fn_id fn_id)
                graph callee_fn_ids
            in
            (func :: funcs, graph)
        | None -> (funcs, graph))
      ([], FuncGraph.empty) ast
  in

  (* Add implicit edges from constructors to all methods in the same class.
     Constructors always execute before any method can be called on an object. *)
  let graph =
    List.fold_left
      (fun g func ->
        let func_name = Option.fold ~none:"" ~some:(fun n -> fst n.IL.ident) func.fn_id.name in
        let class_name_str = Option.map (fun n -> fst n.IL.ident) func.fn_id.class_name in
        if Object_initialization.is_constructor lang func_name class_name_str then
          (* Find all methods in the same class *)
          let same_class_methods =
            List.filter
              (fun other ->
                let other_name = Option.fold ~none:"" ~some:(fun n -> fst n.IL.ident) other.fn_id.name in
                let other_class_name_str = Option.map (fun n -> fst n.IL.ident) other.fn_id.class_name in
                not (Object_initialization.is_constructor lang other_name other_class_name_str)
                && Option.equal
                     (fun n1 n2 -> String.equal (fst n1.IL.ident) (fst n2.IL.ident))
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

