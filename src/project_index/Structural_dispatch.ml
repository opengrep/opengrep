module G = AST_generic
module FA = Graph_from_AST

(* TODO: [emit_dispatch_edges] below is very large and hard to read; split it
   into named helpers (candidate selection, package-visibility gating, edge
   emission) so it can be reviewed and tested in pieces. *)

(* The Dispatch edge's call_site is the impl's name token. *)
let dispatch_call_tok (c_m : FA.func_info) : Tok.t =
  match c_m.FA.fn_id with
  | [ _; Some m_il ] -> snd m_il.IL.ident
  | _ -> snd c_m.FA.fdef.G.fkind

(* Overload dispatch: the concrete functions of one scope sharing a name
   and an arity, Java's [handle(String)] and [handle(int)], form a group.
   For a language whose top level scope is the project, the scope spans
   files. Each other member gets a Dispatch edge to the earliest by
   position, which the reachability closure follows; a call records the
   members its arguments select in its [id_callee_definition]. Runs
   serially on the coordinator, like [emit_dispatch_edges]. *)
let emit_overload_edges ~(lang : Lang.t) ~(cfg : Index_lang_rules.t)
    ~(graph : Call_graph.G.t)
    ~(class_table : Class_table.t)
    (all_funcs : FA.func_info list) : int =
  let groups : (string, (Function_id.t * FA.func_info) list) Hashtbl.t =
    Hashtbl.create 64
  in
  let defining_class (cls : IL.name) : string option =
    Option.map
      (fun (defining : Class_table.cls) ->
        string_of_int (Class_table.index defining))
      (Option.bind
         (Class_table.binding_of_id_info cls.IL.id_info)
         (Class_table.class_of_binding class_table))
  in
  if not (Index_lang_rules.forms_overload_groups ~lang ~cfg) then 0
  else begin
  let file_in_key (func : FA.func_info) (file : Fpath.t) : string =
    if Index_lang_rules.top_level_scope_is_project cfg
       && cfg.Index_lang_rules.project_scope_admits func.FA.entity
    then ""
    else Fpath.to_string file
  in
  List.iter
    (fun (func : FA.func_info) ->
      let scope : string option option =
        match func.FA.fn_id with
        | [ Some cls; Some _ ] ->
            Option.map (fun (defining : string) -> Some defining)
              (defining_class cls)
        | [ None; Some _ ] -> Some (Func_info.entity_qualifier func)
        | _ -> None
      in
      let concrete = Func_info.has_body func.FA.fdef in
      match (scope, Func_info.bare_name func.FA.fn_id, Func_info.def_file_opt func) with
      | Some scope, Some bare_name, Some file when concrete ->
          let key =
            Printf.sprintf "%s\000%b\000%s\000%s\000%d"
              (file_in_key func file) (Option.is_some scope)
              (Option.value scope ~default:"")
              (fst bare_name.IL.ident)
              (List.length (Tok.unbracket func.FA.fdef.G.fparams))
          in
          let node = Function_id.of_il_name bare_name in
          let members = Option.value (Hashtbl.find_opt groups key) ~default:[] in
          Hashtbl.replace groups key ((node, func) :: members)
      | _ -> ())
    all_funcs;
  Hashtbl.fold
    (fun _ members n ->
      match
        List.sort
          (fun ((a : Function_id.t), _) ((b : Function_id.t), _) ->
            Function_id.compare a b)
          members
      with
      | (rep_node, _) :: (_ :: _ as others) ->
          List.fold_left
            (fun n ((node : Function_id.t), (other : FA.func_info)) ->
              Call_graph.add_edge ~kind:Call_graph.Dispatch graph ~src:node
                ~dst:rep_node ~call_tok:(dispatch_call_tok other);
              n + 1)
            n others
      | _ -> n)
    groups 0
  end

let emit_dispatch_edges
    ~(lang : Lang.t)
    ~(class_table : Class_table.t)
    ~(graph : Call_graph.G.t) : int =
  let members (cls : Class_table.cls) :
      Class_table.definition list Common.SMap.t =
    Class_table.member_definitions class_table cls
  in
  let emit_dispatch_edge (c_methods : Class_table.definition list) (n : int)
      (i_m : Class_table.definition) : int =
    match
      List.find_opt
        (fun (c_m : Class_table.definition) ->
          Class_table.satisfies ~required:i_m c_m)
        c_methods
    with
    | None -> n
    | Some { Class_table.func = c_m; _ } ->
      let i_m = i_m.Class_table.func in
      (match FA.fn_id_to_node c_m.FA.fn_id,
             FA.fn_id_to_node i_m.FA.fn_id with
       | Some src, Some dst ->
         (* Impl method's NAME token as the Dispatch edge's call_site. *)
         let call_tok = dispatch_call_tok c_m in
         Call_graph.add_edge ~kind:Call_graph.Dispatch
           graph ~src ~dst ~call_tok;
         n + 1
       | _ -> n)
  in
  if not (Lang_config.interfaces_are_structural lang) then 0
  else
    List.fold_left (fun n (interface : Class_table.cls) ->
      if not (Class_table.is_interface interface) then n
      else
        let required = Class_table.definition_table class_table interface in
        List.fold_left (fun n (implementor : Class_table.cls) ->
          if Class_table.is_interface implementor then n
          else
            let available = members implementor in
            Common.SMap.fold
              (fun (name : string) (i_methods : Class_table.definition list)
                   (n : int) ->
                List.fold_left
                  (emit_dispatch_edge
                     (Option.value (Common.SMap.find_opt name available)
                        ~default:[]))
                  n i_methods)
              required n
        ) n (Class_table.subclasses class_table interface)
    ) 0 (Class_table.classes class_table)
