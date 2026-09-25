module G = AST_generic
module Log = Log_call_graph.Log

(* Function identifier as a path from outermost to innermost scope.
 * For example:
 * - [Some class_name; Some method_name; Some nested_fn] for nested function
 * - [Some class_name; Some method_name] for a method
 * - [Some fn_name] for a top-level function
 * - [] for top-level/anonymous
 *)
type fn_id = Func_info.fn_id
[@@deriving show, eq, ord]

let show_fn_id (fn_id : fn_id) : string =
  match fn_id with
  | [] -> "<anonymous>"
  | path ->
      path
      |> List.map (fun name_opt ->
          Option.value ~default:"<anon>" (Option.map (fun name -> fst name.IL.ident) name_opt))
      |> String.concat "::"

(** Extract the function name (last element) from the fn_id path *)
let get_fn_name (fn_id : fn_id) : IL.name option =
  List_.last_opt fn_id |> Option.join

(* Re-expose Func_info.t's record so [.fn_id] etc. resolve unqualified here. *)
type func_info = Func_info.t = {
  fn_id : fn_id;
  entity : G.entity option;
  fdef : G.function_definition;
}

(* Arity of a function definition as seen from a call site: the parameters
   a call fills, so arity comparison against call sites matches for
   methods. *)
let get_func_arity ~(lang : Lang.t) (f : func_info) : int =
  Receiver.arity lang ~is_method:(Receiver.is_method f.fdef)
    ~is_static:(Receiver.is_static f.entity)
    (Tok.unbracket f.fdef.G.fparams)

(* Disambiguate among candidate functions matching a call site by name.
   [matches] are the candidates; [call_arity] is the number of arguments
   at the call site ([None] when arity info is not available). Returns
   [Some] only when the result is unambiguous:
   - exactly one candidate by name: use it regardless of arity;
   - multiple candidates with arity info: filter by exact arity, accept
     iff exactly one survives;
   - multiple candidates without arity info: cannot disambiguate, give
     up rather than guess.
   Unlike [Shape_and_sig.find_by_arity], which disambiguates among
   stored signatures of a single function and understands variadic tags
   ([Arity_at_least]), this helper only sees raw parameter lists and
   uses strict exact-arity matching. *)
let prefer_concrete (matches : func_info list) : func_info list =
  let concrete =
    List.filter (fun (f : func_info) -> Func_info.has_body f.fdef) matches
  in
  match concrete with
  | [] -> matches
  | _ -> concrete

let narrow_by_arity ~(lang : Lang.t) (call_arity : int option)
    (matches : func_info list) : func_info list =
  let matches = prefer_concrete matches in
  (* Reject a body-less synth candidate (Ruby [attr_reader]: [FBNothing]
     with an empty param list) against a positional-arg call.  A body-less
     decl WITH params is an interface/abstract declaration and stays
     resolvable: the call's stamp holds the definitions its arguments
     select, and dispatch adds the overriding definitions. *)
  let single_synth_with_args (f : func_info) : bool =
    match call_arity with
    | Some n ->
        n > 0
        && not (Func_info.has_body f.fdef)
        && List_.null (Tok.unbracket f.fdef.G.fparams)
    | None -> false
  in
  match matches with
  | [single_match] when single_synth_with_args single_match -> []
  | [_]
  | [] ->
      matches
  | _ ->
      (match call_arity with
      | Some arity ->
          List.filter (fun (f : func_info) ->
            Int.equal (get_func_arity ~lang f) arity
          ) matches
      | None ->
          Log.debug (fun m ->
            m "PICK_BY_ARITY: %d candidates, no arity info; giving up"
              (List.length matches));
          [])

let use_binding (info : G.id_info) : G.SId.t option =
  match !(info.G.id_resolved) with
  | Some (_, sid) when not (G.SId.is_unsafe_default sid) -> Some sid
  | Some _
  | None ->
      None

type static_type =
  | Declared_class of G.SId.t
  | Builtin_type of Type.builtin_type

let equal_static_type (left : static_type) (right : static_type) : bool =
  match (left, right) with
  | Declared_class left, Declared_class right -> G.SId.equal left right
  | Builtin_type left, Builtin_type right -> Type.equal_builtin_type left right
  | Declared_class _, Builtin_type _
  | Builtin_type _, Declared_class _ ->
      false

let static_type_of_type ~(lang : Lang.t) (ty : G.type_) : static_type option =
  match ty.G.t with
  | G.TyN name
  | G.TyExpr { G.e = G.N name; _ } -> (
      match use_binding (snd (AST_generic_helpers.id_of_name name)) with
      | Some sid -> Some (Declared_class sid)
      | None ->
          Option.map
            (fun (builtin : Type.builtin_type) -> Builtin_type builtin)
            (Type.builtin_type_of_type lang ty))
  | _ -> None

let static_type_of_argument ~(lang : Lang.t) (e : G.expr) : static_type option
    =
  match e.G.e with
  | G.N name ->
      Option.bind
        (Ty_bare_name.instance_or_declared_type
           (snd (AST_generic_helpers.id_of_name name)))
        (static_type_of_type ~lang)
  | G.L _ -> (
      match fst (Typing.type_of_expr lang e) with
      | Type.Builtin (builtin : Type.builtin_type) -> Some (Builtin_type builtin)
      | _ -> None)
  | G.New (_, ty, _, _) -> static_type_of_type ~lang ty
  | G.Call ({ G.e = G.N name; _ }, _) when Lang_config.constructs_by_bare_call lang
    -> (
      match !((snd (AST_generic_helpers.id_of_name name)).G.id_resolved) with
      | Some (G.TypeName, sid) -> Some (Declared_class sid)
      | Some _
      | None ->
          None)
  | _ -> None

let argument_types ~(lang : Lang.t)
    ~(type_of_call : G.expr -> static_type option) (args : G.argument list) :
    static_type option list =
  List_.map
    (fun (arg : G.argument) ->
      match arg with
      | G.Arg e -> (
          match (static_type_of_argument ~lang e, e.G.e) with
          | (Some _ as known), _ -> known
          | None, G.Call _ -> type_of_call e
          | None, _ -> None)
      | G.ArgKwd _
      | G.ArgKwdOptional _
      | G.ArgType _
      | G.OtherArg _ ->
          None)
    args

let is_numeric (builtin : Type.builtin_type) : bool =
  match builtin with
  | Type.Int
  | Type.Float
  | Type.Number ->
      true
  | Type.String
  | Type.Bool
  | Type.OtherBuiltins _ ->
      false

let types_disagree ~(argument : static_type) ~(parameter : static_type) : bool
    =
  match (argument, parameter) with
  | Declared_class _, Declared_class _ -> false
  | Builtin_type argument, Builtin_type parameter ->
      not
        (Type.equal_builtin_type argument parameter
        || (is_numeric argument && is_numeric parameter))
  | Declared_class _, Builtin_type _
  | Builtin_type _, Declared_class _ ->
      true

let call_parameters ~(lang : Lang.t) (f : func_info) : G.parameter list =
  let is_method = Receiver.is_method f.fdef in
  let is_static = Receiver.is_static f.entity in
  Tok.unbracket f.fdef.G.fparams
  |> List.filteri (fun (i : int) (param : G.parameter) ->
         not
           (Receiver.implicit_param lang ~is_method ~is_static
              ~is_first:(Int.equal i 0) param))

type static_typing = {
  type_of_call : G.expr -> static_type option;
  is_class : G.SId.t -> bool;
}

let narrow_by_argument_types ~(lang : Lang.t) ~(typing : static_typing)
    (args : G.argument list) (candidates : func_info list) : func_info list =
  if not (Lang_config.overloads_by_type lang) then candidates
  else
    let decided (known : static_type) : static_type option =
      match known with
      | Declared_class sid when not (typing.is_class sid) -> None
      | Declared_class _
      | Builtin_type _ ->
          Some known
    in
    let arguments =
      List_.map
        (fun (argument : static_type option) -> Option.bind argument decided)
        (argument_types ~lang ~type_of_call:typing.type_of_call args)
    in
    List.filter
      (fun (f : func_info) ->
        let parameters = call_parameters ~lang f in
        not
          (List.exists Fun.id
             (List.mapi
                (fun (i : int) (argument : static_type option) ->
                  match (argument, List.nth_opt parameters i) with
                  | Some argument, Some (G.Param { G.ptype = Some ty; _ }) -> (
                      match Option.bind (static_type_of_type ~lang ty) decided with
                      | Some parameter -> types_disagree ~argument ~parameter
                      | None -> false)
                  | _ -> false)
                arguments)))
      candidates

let narrow_by_call ~(lang : Lang.t) ~(typing : static_typing)
    (call_args : G.argument list option) (candidates : func_info list) :
    func_info list =
  let by_arity =
    narrow_by_arity ~lang (Option.map List.length call_args) candidates
  in
  match call_args with
  | Some args -> narrow_by_argument_types ~lang ~typing args by_arity
  | None -> by_arity

let return_type ~(lang : Lang.t) (funcs : func_info list) : static_type option
    =
  match
    List_.uniq_by
      (Option.equal equal_static_type)
      (List_.map
         (fun (f : func_info) ->
           Option.bind f.fdef.G.frettype (static_type_of_type ~lang))
         funcs)
  with
  | [ (Some _ as common) ] -> common
  | _ -> None

let rec typing ~(lang : Lang.t) ~(resolve : G.expr -> func_info list)
    ~(is_class : G.SId.t -> bool) : static_typing =
  { type_of_call = type_of_call ~lang ~resolve ~is_class; is_class }

and type_of_call ~(lang : Lang.t) ~(resolve : G.expr -> func_info list)
    ~(is_class : G.SId.t -> bool) (e : G.expr) : static_type option =
  match e.G.e with
  | G.Call (callee, (_, args, _)) ->
      resolve callee
      |> narrow_by_call ~lang ~typing:(typing ~lang ~resolve ~is_class)
           (Some args)
      |> return_type ~lang
  | _ -> None

(* Graph node type - reuse from Call_graph for consistency *)
type node = Call_graph.node

(* Extract graph node from fn_id - takes the last element *)
let fn_id_to_node (fn_id : fn_id) : node option =
  match List.rev fn_id with
  | Some name :: _ ->
    let ident_node = Function_id.of_il_name name in
    (* An alias-synthetic bare name is recorded at
       the target's position, while the resolved sid carries the target's
       own name. That pair, one position with two names, identifies an
       alias, and the node must be the target's identity, because the
       target holds the definition and its signature. A real definition
       resolves to its own name, so this branch changes nothing for it. *)
    (match !(name.IL.id_info.G.id_resolved) with
     | Some (_, sid) when not (G.SId.is_unsafe_default sid) ->
       let sid_node = Function_id.of_sid sid in
       let sid_name, _, _, _ = G.SId.to_loc sid in
       let same_position =
         let f1, l1, c1 = Function_id.to_file_line_col ident_node in
         let f2, l2, c2 = Function_id.to_file_line_col sid_node in
         String.equal f1 f2 && Int.equal l1 l2 && Int.equal c1 c2
       in
       if same_position
          && not (String.equal sid_name (fst name.IL.ident))
       then Some sid_node
       else Some ident_node
     | _ -> Some ident_node)
  | _ -> None


(* Helper function to identify the callee fn_id from a call expression's callee *)
let uses_new_keyword (lang : Lang.t) : bool =
  Lang_config.uses_new_keyword lang

let expr_of_type_name (ty : G.type_) : G.expr option =
  match ty.G.t with
  | G.TyN (name : G.name) -> Some (G.N name |> G.e)
  | G.TyExpr (e : G.expr) -> Some e
  | _ -> None

type callee_use =
  | Name_use of G.SId.t
  | Member_use of {
      receiver : G.SId.t;
      receiver_type : G.type_ option;
      member : string;
    }

let same_use_binding (left : G.SId.t) (right : G.SId.t) : bool =
  G.SId.equal left right && G.SId.same_site left right

module Callee_use_tbl = Hashtbl.Make (struct
  type t = callee_use * static_type option list option

  let equal ((left, left_arguments) : t) ((right, right_arguments) : t) :
      bool =
    Option.equal
      (List.equal (Option.equal equal_static_type))
      left_arguments right_arguments
    &&
    match (left, right) with
    | Name_use left_sid, Name_use right_sid ->
        same_use_binding left_sid right_sid
    | Member_use left, Member_use right ->
        same_use_binding left.receiver right.receiver
        && Option.equal G.equal_type_ left.receiver_type right.receiver_type
        && String.equal left.member right.member
    | Name_use _, Member_use _
    | Member_use _, Name_use _ ->
        false

  let hash ((use, arguments) : t) : int =
    let arity = Option.map List.length arguments in
    match use with
    | Name_use sid -> Hashtbl.hash (G.SId.hash sid, arity)
    | Member_use { receiver; member; _ } ->
        Hashtbl.hash (G.SId.hash receiver, member, arity)
end)

let callee_use_of_name (info : G.id_info) : callee_use option =
  if Option.is_some !(info.G.id_svalue) then None
  else Option.map (fun (sid : G.SId.t) -> Name_use sid) (use_binding info)

let callee_use_of_member ~(receiver : G.id_info) (member : string) :
    callee_use option =
  Option.map
    (fun (sid : G.SId.t) ->
      Member_use
        { receiver = sid; receiver_type = !(receiver.G.id_instance_type); member })
    (use_binding receiver)

type call_site_resolver =
  caller_parent_path:IL.name option list ->
  call_args:G.argument list option ->
  G.expr ->
  fn_id list

type construction_resolver =
  call_args:G.argument list -> G.type_ -> fn_id list

type invocation_resolver =
  caller_parent_path:IL.name option list -> G.expr -> fn_id list

type argument_typer =
  caller_parent_path:IL.name option list ->
  G.argument list ->
  static_type option list


type binding_target =
  | Bound_module of Names.Module_qn.t
  | Bound_class of Names.Class_qn.t
  | Bound_functions of func_info list
  | Bound_object of func_info list Common.SMap.t

type name_position =
  | Term_position
  | Type_position

type dotted_chain = {
  dc_rooted : bool;
  dc_segments : string list;
}

let dotted_chain_of_name (name : G.name) : dotted_chain option =
  match name with
  | G.Id ((segment, _), _) ->
    Some { dc_rooted = false; dc_segments = [ segment ] }
  | G.IdQualified { name_last = ((last, _), _); name_middle = None;
                    name_top; _ } ->
    Some { dc_rooted = Option.is_some name_top; dc_segments = [ last ] }
  | G.IdQualified { name_last = ((last, _), _);
                    name_middle = Some (G.QDots middle);
                    name_top; _ } ->
    Some
      { dc_rooted = Option.is_some name_top;
        dc_segments =
          List.map (fun (((segment, _), _) : G.ident * _) -> segment) middle
          @ [ last ] }
  | G.IdQualified _ -> None

let entries_in_scope ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (name : string)
    : Func_lookup.scope_entry list =
  Func_lookup.resolve_in_scope func_lookup ~caller_parent_path name

let head_binding ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) ~(position : name_position)
    (segment : string) : binding_target option =
  let entries = entries_in_scope ~func_lookup ~caller_parent_path segment in
  match
    (position, Func_lookup.companion_of_entries entries)
  with
  | Term_position, Some (companion_qn : Names.Class_qn.t) ->
    Some (Bound_class companion_qn)
  | Term_position, None
  | Type_position, _ -> (
  match Func_lookup.class_of_entries entries with
  | Some (class_qn : Names.Class_qn.t) -> Some (Bound_class class_qn)
  | None -> (
    match Func_lookup.object_of_entries entries with
    | Some (members : func_info list Common.SMap.t) ->
      Some (Bound_object members)
    | None ->
      Option.map
        (fun (qn : Names.Module_qn.t) -> Bound_module qn)
        (Func_lookup.resolve_alias func_lookup segment)))

let target_of_attribute ~(position : name_position)
    (attribute : Func_lookup.module_attribute) : binding_target =
  match attribute with
  | Func_lookup.Attr_functions funcs -> Bound_functions funcs
  | Func_lookup.Attr_class class_qn -> Bound_class class_qn
  | Func_lookup.Attr_class_with_companion (class_qn, companion_qn) -> (
    match position with
    | Term_position -> Bound_class companion_qn
    | Type_position -> Bound_class class_qn)
  | Func_lookup.Attr_module submodule_qn -> Bound_module submodule_qn

let attribute_of ~(table : Symbol_table.t) ~(func_lookup : Func_lookup.t)
    ~(position : name_position) (target : binding_target) (segment : string)
    : binding_target option =
  match target with
  | Bound_module (module_qn : Names.Module_qn.t) ->
    Option.map (target_of_attribute ~position)
      (Func_lookup.module_attribute func_lookup module_qn segment)
  | Bound_class (class_qn : Names.Class_qn.t) -> (
    let qualified = Names.Class_qn.concat class_qn segment in
    let nested () : binding_target option =
      if Option.is_some (Func_lookup.class_of_qn func_lookup qualified) then
        Some (Bound_class qualified)
      else None
    in
    let defined_by_name () : binding_target option =
      match
        Func_lookup.definition func_lookup (Names.Class_qn.to_string qualified)
      with
      | Some (attribute : Func_lookup.module_attribute) ->
        Some (target_of_attribute ~position attribute)
      | None -> nested ()
    in
    match Func_lookup.class_of_qn func_lookup class_qn with
    | Some (cls : Class_table.cls) -> (
      match
        Symbol_table.resolve_member table (Symbol_table.Class_object cls)
          segment
      with
      | Symbol_table.Defined (_ :: _ as funcs) -> Some (Bound_functions funcs)
      | Symbol_table.Defined []
      | Symbol_table.External ->
        let classes = Symbol_table.class_table table in
        if
          Common.SMap.mem segment
            (Class_table.members_along
               (Class_table.order classes cls).Linearisation.order)
        then nested ()
        else defined_by_name ())
    | None -> defined_by_name ())
  | Bound_object (members : func_info list Common.SMap.t) ->
    Option.map
      (fun (funcs : func_info list) -> Bound_functions funcs)
      (Common.SMap.find_opt segment members)
  | Bound_functions _ -> None

let longest_accepted_prefix ~(accept : string list -> bool)
    (chain : string list) : (string list * string list) option =
  let rec search (taken : string list) (rest : string list)
      (best : (string list * string list) option)
      : (string list * string list) option =
    match rest with
    | []
    | [ _ ] -> best
    | segment :: after ->
      let taken = taken @ [ segment ] in
      let best = if accept taken then Some (taken, after) else best in
      search taken after best
  in
  search [] chain None

let global_attribute_binding ~(func_lookup : Func_lookup.t)
    ~(position : name_position) (chain : string list)
    : (binding_target * string list) option =
  match chain with
  | [ (segment : string) ] ->
    Option.map
      (fun (attribute : Func_lookup.module_attribute) ->
        (target_of_attribute ~position attribute, []))
      (Func_lookup.module_attribute func_lookup Names.Module_qn.empty segment)
  | []
  | _ :: _ :: _ -> None

let qualified_prefix_binding ~(func_lookup : Func_lookup.t)
    ~(position : name_position) (chain : string list)
    : (binding_target * string list) option =
  match
    longest_accepted_prefix
      ~accept:(fun (prefix : string list) ->
        Option.is_some
          (Func_lookup.class_of_qn func_lookup (Names.Class_qn.of_parts prefix)))
      chain
  with
  | Some ((prefix : string list), (segments : string list)) ->
    let class_qn = Names.Class_qn.of_parts prefix in
    let bound =
      match position with
      | Term_position -> (
        match Func_lookup.companion_of func_lookup class_qn with
        | Some (companion_qn : Names.Class_qn.t) -> Bound_class companion_qn
        | None -> Bound_class class_qn)
      | Type_position -> Bound_class class_qn
    in
    Some (bound, segments)
  | None ->
    Option.map
      (fun ((prefix : string list), (segments : string list)) ->
        (Bound_module (Names.Module_qn.of_parts prefix), segments))
      (longest_accepted_prefix
         ~accept:(fun (prefix : string list) ->
           Func_lookup.is_known_module func_lookup
             (Names.Module_qn.of_parts prefix))
         chain)

let in_own_modules ~(func_lookup : Func_lookup.t) ~(position : name_position)
    (chain : string list) : (binding_target * string list) option =
  List.find_map
    (fun (namespace_scope : Names.Module_qn.t) ->
      if Names.Module_qn.is_empty namespace_scope then None
      else
        qualified_prefix_binding ~func_lookup ~position
          (Names.Module_qn.parts namespace_scope @ chain))
    (Func_lookup.own_modules func_lookup)

let completed ~(table : Symbol_table.t) ~(func_lookup : Func_lookup.t)
    ~(position : name_position)
    ((target : binding_target), (segments : string list))
    : binding_target option =
  List.fold_left
    (fun (target : binding_target option) (segment : string) ->
      Option.bind target (fun target ->
        attribute_of ~table ~func_lookup ~position target segment))
    (Some target) segments

let follow_chain ~(table : Symbol_table.t) ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) ~(position : name_position)
    (chain : dotted_chain) : binding_target option =
  let segments_of_chain = chain.dc_segments in
  let completed = completed ~table ~func_lookup ~position in
  match segments_of_chain with
  | [] -> None
  | head :: segments -> (
    if chain.dc_rooted then
      match
        match
          qualified_prefix_binding ~func_lookup ~position segments_of_chain
        with
        | Some _ as bound -> bound
        | None ->
          global_attribute_binding ~func_lookup ~position segments_of_chain
      with
      | None -> None
      | Some (start : binding_target * string list) -> completed start
    else
      match head_binding ~func_lookup ~caller_parent_path ~position head with
      | Some (target : binding_target) -> completed (target, segments)
      | None ->
        List.find_map
          (fun (start : unit -> (binding_target * string list) option) ->
            Option.bind (start ()) completed)
          [ (fun () ->
              in_own_modules ~func_lookup ~position segments_of_chain);
            (fun () ->
              qualified_prefix_binding ~func_lookup ~position
                segments_of_chain) ])

let id_info_of_name (name : G.name) : G.id_info =
  match name with
  | G.Id (_, id_info) -> id_info
  | G.IdQualified { name_info; _ } -> name_info

let resolution_of_target ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) ~(construct : bool)
    (target : binding_target option) : Symbol_table.resolution =
  match target with
  | Some (Bound_functions funcs) -> Symbol_table.Defined funcs
  | Some (Bound_class class_qn) -> (
    match Func_lookup.class_of_qn func_lookup class_qn with
    | Some (cls : Class_table.cls) when construct ->
      Symbol_table.constructors_of_class table cls
    | Some _ -> Symbol_table.Defined []
    | None -> Symbol_table.External)
  | Some (Bound_object _)
  | Some (Bound_module _) -> Symbol_table.Defined []
  | None -> Symbol_table.External

type chain_root =
  | Imported_root of dotted_chain
  | Unbound_root of dotted_chain
  | Local_root

let rec root_and_members (e : G.expr) : (G.name * string list) option =
  match e.G.e with
  | G.N name
  | G.Ref (_, { G.e = G.N name; _ }) ->
    Some (name, [])
  | G.DotAccess (receiver, _, G.FN (G.Id ((segment, _), _))) ->
    Option.map
      (fun ((root : G.name), (members : string list)) ->
        (root, members @ [ segment ]))
      (root_and_members receiver)
  | _ -> None

let root_of_chain ~(func_lookup : Func_lookup.t) (e : G.expr) : chain_root =
  match root_and_members e with
  | Some ((name : G.name), (members : string list)) -> (
    let chain =
      Option.map
        (fun (chain : dotted_chain) ->
          { chain with dc_segments = chain.dc_segments @ members })
        (dotted_chain_of_name name)
    in
    match (!((id_info_of_name name).G.id_resolved), chain) with
    | Some ((G.ImportedEntity _ | G.ImportedModule _ | G.GlobalName _), _),
      Some (chain : dotted_chain) ->
      Imported_root chain
    | Some (_, sid), Some chain when Func_lookup.is_import func_lookup sid ->
      Imported_root chain
    | None, Some chain -> Unbound_root chain
    | _ -> Local_root)
  | None -> Local_root

let member_of_member_classes ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) (name : string) : func_info list =
  List.find_map
    (fun (class_qn : Names.Class_qn.t) ->
      Option.bind (Func_lookup.class_of_qn func_lookup class_qn)
        (fun (cls : Class_table.cls) ->
          match
            Symbol_table.resolve_member table (Symbol_table.Class_object cls)
              name
          with
          | Symbol_table.Defined (_ :: _ as funcs) -> Some funcs
          | Symbol_table.Defined []
          | Symbol_table.External -> None))
    (Func_lookup.member_classes func_lookup)
  |> Option.value ~default:[]

let resolve_name_in_scope ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) ~(caller : Function_id.t option)
    ~(caller_parent_path : IL.name option list) ~(construct : bool)
    ~(unbound : bool) (name : string) : Symbol_table.resolution =
  let of_self_class () : func_info list =
    if unbound && Naming_AST.members_in_scope_in_methods lang then
      match
        Symbol_table.resolve_member table
          (Symbol_table.self_receiver table ~caller) name
      with
      | Symbol_table.Defined funcs -> funcs
      | Symbol_table.External -> []
    else []
  in
  match of_self_class () with
  | _ :: _ as funcs -> Symbol_table.Defined funcs
  | [] -> (
    let entries = entries_in_scope ~func_lookup ~caller_parent_path name in
    match
      (head_binding ~func_lookup ~caller_parent_path ~position:Term_position
         name,
       Func_lookup.functions_of_entries entries)
    with
    | Some ((Bound_class _ | Bound_object _) as target), _ ->
      resolution_of_target ~table ~func_lookup ~construct (Some target)
    | _, (_ :: _ as funcs) -> Symbol_table.Defined funcs
    | (Some (Bound_module _ | Bound_functions _) | None), [] -> (
      match member_of_member_classes ~table ~func_lookup name with
      | _ :: _ as funcs -> Symbol_table.Defined funcs
      | [] -> Symbol_table.External))

let resolve_chain ~(table : Symbol_table.t) ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) ~(position : name_position)
    ~(construct : bool) (e : G.expr) : Symbol_table.resolution =
  let target =
    match root_of_chain ~func_lookup e with
    | Imported_root (chain : dotted_chain)
    | Unbound_root chain ->
      follow_chain ~table ~func_lookup ~caller_parent_path ~position chain
    | Local_root -> None
  in
  resolution_of_target ~table ~func_lookup ~construct target

let defined_of_any (resolutions : Symbol_table.resolution list)
    : Symbol_table.resolution =
  match
    List.concat_map
      (fun (resolution : Symbol_table.resolution) ->
        match resolution with
        | Symbol_table.Defined (funcs : func_info list) -> funcs
        | Symbol_table.External -> [])
      resolutions
  with
  | [] when List.for_all
              (fun (resolution : Symbol_table.resolution) ->
                match resolution with
                | Symbol_table.External -> true
                | Symbol_table.Defined _ -> false)
              resolutions ->
    Symbol_table.External
  | (funcs : func_info list) -> Symbol_table.Defined funcs

let rec resolve_outside_file_from ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) ~(caller : Function_id.t option)
    ~(caller_parent_path : IL.name option list) ~(use : Symbol_table.use)
    ~(visited : G.SId.t list) (e : G.expr) : Symbol_table.resolution =
  let construct = Symbol_table.constructs table use in
  match e.G.e with
  | G.N (G.Id ((name, _), info))
  | G.N
      (G.IdQualified
        {
          G.name_last = (name, _), _;
          name_middle = None;
          name_top = None;
          name_info = info;
        })
  | G.Ref (_, { G.e = G.N (G.Id ((name, _), info)); _ }) -> (
    match root_of_chain ~func_lookup e with
    | Unbound_root _ ->
      resolve_name_in_scope ~lang ~table ~func_lookup ~caller
        ~caller_parent_path ~construct ~unbound:true name
    | Imported_root _ ->
      resolve_name_in_scope ~lang ~table ~func_lookup ~caller
        ~caller_parent_path ~construct ~unbound:false name
    | Local_root -> (
      let follow (sid : G.SId.t) (value : G.expr) : Symbol_table.resolution =
        resolve_outside_file_from ~lang ~table ~func_lookup ~caller
          ~caller_parent_path ~use ~visited:(sid :: visited) value
      in
      match use_binding info with
      | Some (sid : G.SId.t) when not (List.exists (G.SId.equal sid) visited)
        -> (
        match
          (Symbol_table.values_in_force table ~caller sid, !(info.G.id_svalue))
        with
        | (_ :: _ as assigned), _ ->
          defined_of_any (List.map (follow sid) assigned)
        | [], Some (G.Sym (value : G.expr)) -> follow sid value
        | [], (Some _ | None) -> Symbol_table.External)
      | Some _
      | None -> Symbol_table.External))
  | G.N (G.IdQualified _)
  | G.DotAccess _ ->
    resolve_chain ~table ~func_lookup ~caller_parent_path
      ~position:Term_position ~construct e
  | G.ArrayAccess (indexed, _) ->
    resolve_outside_file_from ~lang ~table ~func_lookup ~caller
      ~caller_parent_path ~use ~visited indexed
  | _ -> Symbol_table.External

let resolve_outside_file ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) ~(caller : Function_id.t option)
    ~(caller_parent_path : IL.name option list) ~(use : Symbol_table.use)
    (e : G.expr) : Symbol_table.resolution =
  resolve_outside_file_from ~lang ~table ~func_lookup ~caller
    ~caller_parent_path ~use ~visited:[] e

let resolve_construction_outside_file ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) ~(caller_parent_path : IL.name option list)
    (ty : G.type_) : Symbol_table.resolution =
  match expr_of_type_name ty with
  | Some (e : G.expr) ->
    resolve_chain ~table ~func_lookup ~caller_parent_path
      ~position:Type_position ~construct:true e
  | None -> Symbol_table.External

let extension_visible ~(func_lookup : Func_lookup.t) (name : string)
    (func : func_info) : bool =
  let entries = entries_in_scope ~func_lookup ~caller_parent_path:[] name in
  List.exists
    (fun (visible : func_info) -> visible.fdef == func.fdef)
    (Func_lookup.extensions_of_entries entries
    @ Func_lookup.functions_of_entries entries)
