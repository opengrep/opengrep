(* See the mli. *)

module Log = Log_tainting.Log
module FunctionMap = Shape_and_sig.FunctionMap
module SignatureSet = Shape_and_sig.SignatureSet

type db = Shape_and_sig.signature_database

let max_rounds = 20

module Sig_lattice = struct
  type t = SignatureSet.t

  (* Guard-aware: plain [equal] is guard-blind and would declare the
     component stable while an effect's guard still refines (Clojure
     length-atom guards exist even with [effect_guards] off). *)
  let equal = SignatureSet.equal_with_guards
end

module Sig_store = struct
  type t = db
  type node = Function_id.t
  type lattice = SignatureSet.t

  let get (n : Function_id.t) (db : db) : lattice =
    match FunctionMap.find_opt n db.Shape_and_sig.signatures with
    | Some s -> s
    | None -> SignatureSet.empty

  let set (n : Function_id.t) (s : lattice) (db : db) : db =
    { Shape_and_sig.signatures =
        FunctionMap.add n s db.Shape_and_sig.signatures }
end

module Engine = Graph_fixpoint.Make (Call_graph.G) (Sig_lattice) (Sig_store)

(* [SCC.scc_list] lists the components callers first for the graph's
   callee->caller edges. *)
let sccs_callees_first (graph : Call_graph.G.t) : Function_id.t list list =
  List.rev (Call_graph.SCC.scc_list graph)

let recursive_members (graph : Call_graph.G.t)
    (sccs : Function_id.t list list) : Function_id.t list =
  sccs
  |> List.concat_map (fun (members : Function_id.t list) ->
         match members with
         | [ fid ] -> if Call_graph.G.mem_edge graph fid fid then [ fid ] else []
         | _ -> members)

let store ?(max_shape_depth : int option) (fid : Function_id.t)
    (fresh : Shape_and_sig.extended_sig list) (db : db) : db =
  let cut (xs : Shape_and_sig.extended_sig) : Shape_and_sig.extended_sig =
    match max_shape_depth with
    | None -> xs
    | Some max_depth ->
        { xs with
          Shape_and_sig.sig_ =
            Taint_shape.truncate_signature ~max_depth xs.Shape_and_sig.sig_ }
  in
  let set =
    List.fold_left
      (fun acc xs -> SignatureSet.add (cut xs) acc)
      SignatureSet.empty fresh
  in
  Sig_store.set fid set db

let run ~(rule_id : Rule_ID.t) ~(graph : Call_graph.G.t)
    ~(sccs : Function_id.t list list)
    ~(analyze : Function_id.t -> db -> db) (db : db) : db =
  Engine.run ~max_iter:max_rounds
    ~on_max_iter:(fun (members : Function_id.t list) ->
      Log.warn (fun m ->
          m "rule %s: a signature component of %d functions is not stable \
             after %d rounds; its current signatures are kept"
            (Rule_ID.to_string rule_id) (List.length members) max_rounds))
    ~sccs ~graph ~analyze db
