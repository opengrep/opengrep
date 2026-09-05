(* Argument-to-parameter symbolic value propagation (issue #499, "gap B").
 *
 * [apply_fn(sink, source())] with [def apply_fn(fn, x): fn(x)] produces no
 * finding: the sink pattern [sink(...)] can only match a call whose callee
 * reads [sink], and the binding [fn = sink] exists in no AST — naming
 * cannot stamp a parameter's [id_svalue] because its value is per call
 * site. This pass closes the hop: when EVERY known call site of a function
 * binds the same bare name to a parameter that the function body calls,
 * that binding is a static fact after all, and we stamp the parameter's
 * uses with [Sym <name>] exactly as [Naming_AST] does for a same-scope
 * alias ([f = sink]). Matching with [symbolic_propagation] then finds the
 * sink inside the callee body, and everything downstream — seeding,
 * signatures, instantiation — proceeds as for a defined callback.
 *
 * The same collection works same-file (naming links the call's callee to
 * the local def) and cross-file (projidx writes [id_resolved] links onto
 * the dispatch ASTs). [G.SId.t] is a binding counted in traversal order
 * within its file, or a definition's site, so stamps computed on one
 * parse apply to any other parse of the same bytes
 * — interfile matches specs on fresh Naming-only parses but runs dataflow
 * on projidx-stamped ASTs, and both must agree on the added matches.
 *
 * Stamps are only written where [id_svalue] is [None], and are inert
 * unless a rule enables [symbolic_propagation].
 *
 * No walk here enters [id_info] payloads.  A stamp value is a subtree of
 * the AST it was collected from (the dispatch AST, when mirrored onto the
 * extraction parse), and it holds occurrences of the stamped sid: a walk
 * that followed the stamp it had just written would reach such an
 * occurrence and stamp it with a value containing itself, and every later
 * traversal of the payload would loop.  [var l []T; l = append(l, x)] is
 * enough. *)

module G = AST_generic
module Log = Log_tainting.Log

(* Value observed for one parameter position at one call site: a bare name
   that is not itself a local/parameter/enclosed variable (those have no
   stable identity across call sites). *)
let acceptable_actual (e : G.expr) : (string * G.expr) option =
  match e.G.e with
  | G.N (G.Id ((s, _), ainfo)) -> (
      match !(ainfo.G.id_resolved) with
      | Some ((G.LocalVar | G.Parameter | G.EnclosedVar), _) -> None
      | Some _
      | None ->
          Some (s, e))
  | _ -> None

let sid_of_id_info (info : G.id_info) : G.SId.t option =
  match !(info.G.id_resolved) with
  | Some (_, sid) -> Some sid
  | None -> None

type def_entry = {
  (* Parameter sids by position; [None] for non-classic parameters. *)
  de_param_sids : G.SId.t option array;
  (* Positions whose parameter is used as a callee in the body: the only
     ones worth stamping — the motivation is making [fn(x)] matchable. *)
  de_callee_positions : (int, unit) Hashtbl.t;
}

(* Body walk: which of [param_sids] appear in callee position? *)
let callee_positions_of_body (param_sids : G.SId.t option array)
    (fbody : G.function_body) : (int, unit) Hashtbl.t =
  let positions = Hashtbl.create 4 in
  let visitor =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_expr () e =
        (match e.G.e with
        | G.Call ({ e = G.N (G.Id (_, cinfo)); _ }, _) -> (
            match sid_of_id_info cinfo with
            | None -> ()
            | Some csid ->
                Array.iteri
                  (fun i sid_opt ->
                    match sid_opt with
                    | Some sid when G.SId.equal sid csid ->
                        Hashtbl.replace positions i ()
                    | _ -> ())
                  param_sids)
        | _ -> ());
        super#visit_expr () e
    end
  in
  visitor#visit_function_body () fbody;
  positions

let param_sids_of_fdef (fdef : G.function_definition) : G.SId.t option array =
  Tok.unbracket fdef.G.fparams
  |> List_.map (fun (p : G.parameter) ->
         match p with
         | G.Param { pinfo; _ } -> sid_of_id_info pinfo
         | _ -> None)
  |> Array.of_list

(* One observation per (callee def, position): [Agree value] while every
   call site so far bound the same name, demoted to [Conflict] forever
   after the first disagreement or unacceptable actual. *)
type observation = Agree of string * G.expr | Conflict

let collect_stamps (asts : G.program list) : (G.SId.t * G.expr) list =
  let defs : (G.SId.t, def_entry) Hashtbl.t = Hashtbl.create 64 in
  let def_visitor =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_definition () ((ent, dkind) as def) =
        (match (ent.G.name, dkind) with
        | G.EN (G.Id (_, info)), G.FuncDef fdef -> (
            match sid_of_id_info info with
            | None -> ()
            | Some def_sid ->
                let de_param_sids = param_sids_of_fdef fdef in
                let de_callee_positions =
                  callee_positions_of_body de_param_sids fdef.G.fbody
                in
                if Hashtbl.length de_callee_positions > 0 then
                  Hashtbl.replace defs def_sid
                    { de_param_sids; de_callee_positions })
        | _ -> ());
        super#visit_definition () def
    end
  in
  List.iter (def_visitor#visit_program ()) asts;
  if Hashtbl.length defs = 0 then []
  else begin
    let observations : (G.SId.t * int, observation) Hashtbl.t =
      Hashtbl.create 16
    in
    let observe def_sid pos (actual : G.expr option) =
      let obs =
        match actual with
        | Some e -> (
            match acceptable_actual e with
            | Some (s, e) -> (
                match Hashtbl.find_opt observations (def_sid, pos) with
                | None -> Agree (s, e)
                | Some (Agree (s0, e0)) when String.equal s0 s -> Agree (s0, e0)
                | Some _ -> Conflict)
            | None -> Conflict)
        | None -> Conflict
      in
      Hashtbl.replace observations (def_sid, pos) obs
    in
    let call_visitor =
      object
        inherit [_] G.iter_no_id_info as super

        method! visit_expr () e =
          (match e.G.e with
          | G.Call ({ e = G.N (G.Id (_, cinfo)); _ }, (_, args, _)) -> (
              match sid_of_id_info cinfo with
              | None -> ()
              | Some csid -> (
                  match Hashtbl.find_opt defs csid with
                  | None -> ()
                  | Some de ->
                      (* Positional args only; a keyword or spread call site
                         demotes the positions it obscures to [Conflict]. *)
                      de.de_callee_positions
                      |> Hashtbl.iter (fun pos () ->
                             let actual =
                               match List.nth_opt args pos with
                               | Some (G.Arg arg_e) -> Some arg_e
                               | Some _
                               | None ->
                                   None
                             in
                             observe csid pos actual)))
          | _ -> ());
          super#visit_expr () e
      end
    in
    List.iter (call_visitor#visit_program ()) asts;
    Hashtbl.fold
      (fun (def_sid, pos) obs acc ->
        match obs with
        | Conflict -> acc
        | Agree (name, value) -> (
            match Hashtbl.find_opt defs def_sid with
            | None -> acc
            | Some de -> (
                match de.de_param_sids.(pos) with
                | Some param_sid ->
                    Log.debug (fun m ->
                        m
                          "callback svalue: stamping param %s (pos %d of %s) \
                           with Sym %s"
                          (G.SId.to_string param_sid)
                          pos
                          (G.SId.to_string def_sid)
                          name);
                    (param_sid, value) :: acc
                | None -> acc)))
      observations []
  end

(* Every [Sym] svalue in [ast], keyed by the carrying id's sid. Used to
   mirror projidx-published svalues (e.g. import-value aliases) onto the
   fresh Naming-only extraction parses, which never see projidx payloads;
   sids are positional so the keys transfer. Svalues naming stamped
   itself exist identically in both parses, and [apply_stamps] only
   fills empty slots, so mirroring them is a harmless no-op. *)
let collect_sym_stamps (ast : G.program) : (G.SId.t * G.expr) list =
  let acc : (G.SId.t, G.expr) Hashtbl.t = Hashtbl.create 8 in
  let visitor =
    object
      inherit [_] G.iter_no_id_info

      method! visit_id_info () (info : G.id_info) =
        (match (!(info.G.id_resolved), !(info.G.id_svalue)) with
        | Some (_, sid), Some (G.Sym value) ->
            if not (Hashtbl.mem acc sid) then Hashtbl.replace acc sid value
        | _ -> ())
    end
  in
  visitor#visit_program () ast;
  Hashtbl.fold (fun sid v l -> (sid, v) :: l) acc []

(* Returns the number of identifiers stamped: a file with a non-zero count
   may now match formulas its raw text cannot (the value's name is not in
   the file), so content-based prefilters must not skip it. *)
let apply_stamps (stamps : (G.SId.t * G.expr) list) (ast : G.program) : int =
  match stamps with
  | [] -> 0
  | _ ->
      let by_sid : (G.SId.t, G.expr) Hashtbl.t =
        Hashtbl.create (List.length stamps)
      in
      List.iter (fun (sid, v) -> Hashtbl.replace by_sid sid v) stamps;
      let stamped = ref 0 in
      let visitor =
        object
          inherit [_] G.iter_no_id_info

          method! visit_id_info () (info : G.id_info) =
            (match (sid_of_id_info info, !(info.G.id_svalue)) with
            | Some sid, None -> (
                match Hashtbl.find_opt by_sid sid with
                | Some value ->
                    info.G.id_svalue := Some (G.Sym value);
                    incr stamped
                | None -> ())
            | _ -> ())
        end
      in
      visitor#visit_program () ast;
      !stamped

(* Same-file entry point: collect and stamp within one AST. *)
let stamp_program (ast : G.program) : unit =
  ignore (apply_stamps (collect_stamps [ ast ]) ast)
