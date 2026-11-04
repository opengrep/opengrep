(* See the mli. *)

module Log = Log_tainting.Log
module FunctionMap = Shape_and_sig.FunctionMap
module SignatureSet = Shape_and_sig.SignatureSet

type db = Shape_and_sig.signature_database

let max_rounds = 20

(* The join of the signatures of a function in two rounds: one signature per
   arity, carrying the effects and the closures of both. An effect of the
   current round keeps its [Fun_ref]s, whose signatures are this round's. *)
let join_signatures ~(lang : Lang.t) (previous : SignatureSet.t) (current : SignatureSet.t) :
    SignatureSet.t =
  SignatureSet.fold
    (fun (old : Shape_and_sig.extended_sig) (acc : SignatureSet.t) ->
      match
        List.find_opt
          (fun (sig_ : Shape_and_sig.extended_sig) ->
            Shape_and_sig.equal_sig_arity sig_.arity old.arity)
          (SignatureSet.elements acc)
      with
      | Some (sig_ : Shape_and_sig.extended_sig) ->
          let current_sig = sig_.sig_ in
          let old_sig = old.sig_ in
          let effects =
            Shape_and_sig.Effects.fold Shape_and_sig.Effects.add
              old_sig.Shape_and_sig.Signature.effects
              current_sig.Shape_and_sig.Signature.effects
          in
          let closures =
            Taint_shape.unify_closures ~lang old_sig.Shape_and_sig.Signature.closures
              current_sig.Shape_and_sig.Signature.closures
          in
          acc |> SignatureSet.remove sig_
          |> SignatureSet.add
               { sig_ with
                 sig_ =
                   { current_sig with
                     Shape_and_sig.Signature.effects = Taint_shape.join_returns ~lang effects;
                     closures } }
      | None -> SignatureSet.add old acc)
    previous current

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
        (* In [Call_graph.G_ordered]'s order, which does not depend on the
           paths of the files. *)
        Call_graph.G.succ graph v
        |> Call_graph.G_ordered.sorted
        |> List.fold_left
             (fun (acc : Fid_set.t * Function_id.t list) (caller : Function_id.t) ->
               if Fid_set.mem caller in_component then visit caller acc else acc)
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

let store ~(lang : Lang.t) ~(widen : bool) ?(max_shape_depth : int option) (fid : Function_id.t)
    (fresh : Shape_and_sig.extended_sig list) (db : db) : db =
  let cut (xs : Shape_and_sig.extended_sig) : Shape_and_sig.extended_sig =
    match max_shape_depth with
    | None -> xs
    | Some max_depth ->
        { xs with
          Shape_and_sig.sig_ =
            Taint_shape.truncate_signature ~max_depth xs.Shape_and_sig.sig_ }
  in
  let joined_returns (xs : Shape_and_sig.extended_sig) : Shape_and_sig.extended_sig =
    let sig_ = xs.Shape_and_sig.sig_ in
    let joined =
      { sig_ with
        Shape_and_sig.Signature.effects =
          Taint_shape.join_returns ~lang sig_.Shape_and_sig.Signature.effects }
    in
    (* The taint variables of a member of a cycle are widened so that the
       cycle reaches a fixpoint. *)
    { xs with
      Shape_and_sig.sig_ =
        (if widen then Taint_shape.widen_signature_taints ~lang joined else joined) }
  in
  let set =
    List.fold_left
      (fun acc xs -> SignatureSet.add (cut (joined_returns (cut xs))) acc)
      SignatureSet.empty fresh
  in
  Sig_store.set fid set db

let run ~(lang : Lang.t) ~(rule_id : Rule_ID.t) ~(graph : Call_graph.G.t)
    ~(sccs : Function_id.t list list)
    ~(analyze : Function_id.t -> db -> db) (db : db) : db =
  let module Sig_lattice = struct
    type t = SignatureSet.t

    (* Guard-aware: plain [equal] is guard-blind and would declare the
       component stable while an effect's guard still refines (Clojure
       length-atom guards exist even with [effect_guards] off). *)
    let equal = SignatureSet.equal_with_guards
    let join = join_signatures ~lang
  end in
  let module Engine = Graph_fixpoint.Make (Call_graph.G) (Sig_lattice) (Sig_store) in
  Engine.run ~max_iter:max_rounds
    ~on_max_iter:(fun (members : Function_id.t list) ->
      Log.warn (fun m ->
          m "rule %s: a signature component of %d functions is not stable \
             after %d rounds; its current signatures are kept"
            (Rule_ID.to_string rule_id) (List.length members) max_rounds))
    ~sccs ~graph ~analyze db
