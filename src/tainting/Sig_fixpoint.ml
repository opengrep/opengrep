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
    { db with
      Shape_and_sig.signatures =
        FunctionMap.add n s db.Shape_and_sig.signatures }
end

module Engine = Graph_fixpoint.Make (Call_graph.G) (Sig_lattice) (Sig_store)

module Fid_set = Set.Make (Function_id)

(* The members of a component, each before the members that call it where the
   cycles allow: the reverse postorder of a depth-first walk along the edges
   inside the component, which go from callee to caller. In that order one
   round carries a callee's new signature along a chain of callers, instead of
   one call edge per round. *)
let order_component (graph : Call_graph.G.t) (members : Function_id.t list) :
    Function_id.t list =
  let in_component = Fid_set.of_list members in
  let rec visit (v : Function_id.t)
      ((seen, order) : Fid_set.t * Function_id.t list) :
      Fid_set.t * Function_id.t list =
    if Fid_set.mem v seen then (seen, order)
    else
      let seen, order =
        Call_graph.G.fold_succ
          (fun (caller : Function_id.t) (acc : Fid_set.t * Function_id.t list) ->
            if Fid_set.mem caller in_component then visit caller acc else acc)
          graph v
          (Fid_set.add v seen, order)
      in
      (seen, v :: order)
  in
  let _, order =
    List.fold_left
      (fun (acc : Fid_set.t * Function_id.t list) (v : Function_id.t) ->
        visit v acc)
      (Fid_set.empty, []) members
  in
  order

(* [SCC.scc_list] lists the components callers first for the graph's
   callee->caller edges. *)
let sccs_callees_first (graph : Call_graph.G.t) : Function_id.t list list =
  List.rev (Call_graph.SCC.scc_list graph)
  |> List.map (fun (members : Function_id.t list) ->
         match members with
         | []
         | [ _ ] ->
             members
         | _ :: _ :: _ -> order_component graph members)

let recursive_members (graph : Call_graph.G.t)
    (sccs : Function_id.t list list) : Function_id.t list =
  sccs
  |> List.concat_map (fun (members : Function_id.t list) ->
         match members with
         | [ fid ] -> if Call_graph.G.mem_edge graph fid fid then [ fid ] else []
         | _ -> members)

let store ~(lang : Lang.t) ~(widen : bool) ?(max_shape_depth : int option)
    ~(previous : db) (fid : Function_id.t)
    (fresh : Shape_and_sig.extended_sig list) (db : db) : db =
  let cut (xs : Shape_and_sig.extended_sig) : Shape_and_sig.extended_sig =
    match max_shape_depth with
    | None -> xs
    | Some max_depth ->
        { xs with
          Shape_and_sig.sig_ =
            Taint_shape.truncate_signature ~max_depth xs.Shape_and_sig.sig_ }
  in
  let stored = Sig_store.get fid previous in
  let join_with_stored (xs : Shape_and_sig.extended_sig) :
      Shape_and_sig.extended_sig =
    let sig_ = xs.Shape_and_sig.sig_ in
    let effects, closures =
      SignatureSet.fold
        (fun (old : Shape_and_sig.extended_sig)
             ((effects, closures) :
               Shape_and_sig.Effects.t
               * Shape_and_sig.Shape.closure Shape_and_sig.ClosureMap.t) ->
          if
            Shape_and_sig.equal_sig_arity old.Shape_and_sig.arity
              xs.Shape_and_sig.arity
          then
            (* The stored effects are added to the fresh ones, so an effect
               of this round keeps its [Fun_ref]s, whose signatures are
               this round's; the closures they refer to are joined. *)
            let old_sig = old.Shape_and_sig.sig_ in
            ( Shape_and_sig.Effects.fold Shape_and_sig.Effects.add
                old_sig.Shape_and_sig.Signature.effects effects,
              Taint_shape.unify_closures ~lang
                old_sig.Shape_and_sig.Signature.closures closures )
          else (effects, closures))
        stored
        (sig_.Shape_and_sig.Signature.effects, sig_.Shape_and_sig.Signature.closures)
    in
    let joined =
      { sig_ with
        Shape_and_sig.Signature.effects = Taint_shape.join_returns ~lang effects;
        closures }
    in
    (* The taint variables of a member of a cycle are widened so that the
       cycle reaches a fixpoint. *)
    { xs with
      Shape_and_sig.sig_ =
        (if widen then Taint_shape.widen_signature_taints ~lang joined else joined) }
  in
  let set =
    List.fold_left
      (fun acc xs -> SignatureSet.add (cut (join_with_stored (cut xs))) acc)
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
