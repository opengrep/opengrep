open Types

type positioned_binding = {
  pb_pos : Pos.t option;
  pb_name : string;
  pb_parent_path : IL.name option list;
  pb_kinds : Func_lookup.scope_kind list;
}

let position_of_tok (tok : Tok.t) : Pos.t option =
  match Tok.loc_of_tok tok with
  | Ok (loc : Tok.location) -> Some loc.Tok.pos
  | Error _ -> None

let equal_parent_path (first : IL.name option list)
    (second : IL.name option list) : bool =
  List.equal (Option.equal Function_id.equal_il_name) first second

let function_binding_of ~(pos : Pos.t option)
    ~(parent_path : IL.name option list) (name : string)
    (funcs : Func_info.t list) : positioned_binding list =
  match funcs with
  | [] -> []
  | _ :: _ ->
    [ { pb_pos = pos; pb_name = name;
        pb_parent_path = parent_path;
        pb_kinds =
          List.map
            (fun (func : Func_info.t) -> Func_lookup.Scope_function func)
            funcs } ]

let class_binding_of ~(pos : Pos.t option)
    ~(parent_path : IL.name option list) (name : string)
    (class_qn : Names.Class_qn.t) : positioned_binding =
  { pb_pos = pos; pb_name = name;
    pb_parent_path = parent_path;
    pb_kinds = [ Func_lookup.Scope_class class_qn ] }

type bound_in_scope = {
  bs_pos : Pos.t option;
  bs_parent_path : IL.name option list;
  bs_kinds : Func_lookup.scope_kind list;
}

let bindings_of_positioned (bindings : positioned_binding list)
    : Func_lookup.scope_entry list Common.SMap.t =
  let in_file_order =
    List.stable_sort
      (fun (first : positioned_binding) (second : positioned_binding) ->
        Option.compare Pos.compare first.pb_pos second.pb_pos)
      bindings
  in
  List.fold_left
    (fun (bound : bound_in_scope list Common.SMap.t)
         (binding : positioned_binding) ->
      let in_name =
        Option.value (Common.SMap.find_opt binding.pb_name bound) ~default:[]
      in
      let same_scope, other_scopes =
        List.partition
          (fun (entry : bound_in_scope) ->
            equal_parent_path entry.bs_parent_path binding.pb_parent_path)
          in_name
      in
      let bound_now =
        match same_scope with
        | [ (earlier : bound_in_scope) ]
          when Option.equal Pos.equal earlier.bs_pos binding.pb_pos ->
          { earlier with bs_kinds = earlier.bs_kinds @ binding.pb_kinds }
        | _ ->
          { bs_pos = binding.pb_pos; bs_parent_path = binding.pb_parent_path;
            bs_kinds = binding.pb_kinds }
      in
      Common.SMap.add binding.pb_name (bound_now :: other_scopes) bound)
    Common.SMap.empty in_file_order
  |> Common.SMap.map
       (fun (in_name : bound_in_scope list) ->
         List.concat_map
           (fun (entry : bound_in_scope) ->
             List.map
               (fun (kind : Func_lookup.scope_kind) ->
                 { Func_lookup.kind; parent_path = entry.bs_parent_path })
               entry.bs_kinds)
           in_name)

let enclosing_scope_of_class
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    (ci : class_info) : IL.name option list option =
  let encloses_itself (parent_path : IL.name option list) : bool =
    match List.rev parent_path with
    | Some (innermost : IL.name) :: _ ->
      Function_id.equal_name ci.ci_id innermost
    | None :: _
    | [] -> false
  in
  Option.bind
    (Option.bind
       (Common.SMap.find_opt (Function_id.show ci.ci_id) class_parent_paths)
       (List.find_opt
          (fun (((defining : Function_id.t), _) :
                  Function_id.t * IL.name option list) ->
            Function_id.equal defining ci.ci_id)))
    (fun (((_ : Function_id.t), (parent_path : IL.name option list))) ->
      if encloses_itself parent_path then None else Some parent_path)

let class_il_name_of (ci : class_info) : IL.name =
  IL.{ ident = (Function_id.show ci.ci_id, Function_id.tok ci.ci_id);
       sid = AST_generic.SId.unsafe_default;
       id_info = AST_generic.empty_id_info () }

let classes_by_qn (classes : class_info list) : class_info Common.SMap.t =
  List.fold_left
    (fun (by_qn : class_info Common.SMap.t) (ci : class_info) ->
      Common.SMap.add (Names.Class_qn.to_string ci.ci_qn) ci by_qn)
    Common.SMap.empty classes

let bindings_in_class (ci : class_info)
    (bindings :
       pos:Pos.t option -> parent_path:IL.name option list ->
       positioned_binding list) : positioned_binding list =
  bindings ~pos:(position_of_tok (Function_id.tok ci.ci_id))
    ~parent_path:[ Some (class_il_name_of ci) ]

let class_member_bindings
    ~(members_of : Names.Class_qn.t -> (string * Func_info.t list) list)
    (classes : class_info list) : positioned_binding list =
  List.concat_map
    (fun (ci : class_info) ->
      bindings_in_class ci (fun ~pos ~parent_path ->
        List.concat_map
          (fun ((name : string), (funcs : Func_info.t list)) ->
            function_binding_of ~pos ~parent_path name funcs)
          (members_of ci.ci_qn)))
    classes

let companion_binding_of ~(pos : Pos.t option)
    ~(parent_path : IL.name option list) (name : string)
    (class_qn : Names.Class_qn.t) : positioned_binding =
  { pb_pos = pos; pb_name = name;
    pb_parent_path = parent_path;
    pb_kinds = [ Func_lookup.Scope_companion class_qn ] }

let no_companion (_ : class_info) : bool = false

let own_class_bindings
    ~(companion : class_info -> bool)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(binds_at_file_scope : Names.Class_qn.t -> bool)
    ~(scope_of_owner : Names.Class_qn.t -> IL.name option list option)
    (classes : class_info list) : positioned_binding list =
  classes
  |> List.filter_map (fun (ci : class_info) ->
       match Names.Class_qn.split_last ci.ci_qn with
       | None -> None
       | Some ((parent : Names.Class_qn.t), _) ->
         let bind (parent_path : IL.name option list) : positioned_binding =
           let pos = position_of_tok (Function_id.tok ci.ci_id) in
           if companion ci then
             companion_binding_of ~pos ~parent_path ci.ci_name ci.ci_qn
           else class_binding_of ~pos ~parent_path ci.ci_name ci.ci_qn
         in
         if binds_at_file_scope parent then Some (bind [])
         else
           match scope_of_owner parent with
           | Some (parent_path : IL.name option list) -> Some (bind parent_path)
           | None ->
             Option.map bind (enclosing_scope_of_class ~class_parent_paths ci))

let own_definitions_of_file
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(fi_file_str : string) : positioned_binding list =
  let own_funcs =
    Option.value (Hashtbl.find_opt file_funcs_index fi_file_str) ~default:[]
  in
  List.concat_map
      (fun (func : Func_info.t) ->
        let bind (parent_path : IL.name option list) (name : IL.name)
            : positioned_binding list =
          function_binding_of ~pos:(position_of_tok (snd name.IL.ident))
            ~parent_path (fst name.IL.ident) [ func ]
        in
        match Func_info.as_method func.Func_info.fn_id with
        | Some _ -> []
        | None -> (
          match Func_info.as_free func.Func_info.fn_id with
          | Some (bare_name : IL.name) -> bind [] bare_name
          | None -> (
            match List_.init_and_last_opt func.Func_info.fn_id with
            | Some ((parent_path : IL.name option list),
                    Some (bare_name : IL.name)) ->
              bind parent_path bare_name
            | _ -> [])))
    own_funcs

let own_alias_bindings
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(fi_file_str : string) : positioned_binding list =
  Option.value (Hashtbl.find_opt file_funcs_index fi_file_str) ~default:[]
  |> List.concat_map (fun (func : Func_info.t) ->
       match
         (func.Func_info.entity, List_.init_and_last_opt func.Func_info.fn_id)
       with
       | Some (ent : AST_generic.entity),
         Some ((parent_path : IL.name option list),
               Some (bare_name : IL.name)) -> (
         let parent_path =
           match Func_info.as_free func.Func_info.fn_id with
           | Some _ -> []
           | None -> parent_path
         in
         match Index_lang_rules.entity_simple_name ent with
         | Some (alias : string)
           when not (String.equal alias (fst bare_name.IL.ident)) ->
           function_binding_of ~pos:(position_of_tok (snd bare_name.IL.ident))
             ~parent_path alias [ func ]
         | Some _
         | None -> [])
       | _ -> [])

let bindings_of_attributes ~(pos : Pos.t option)
    ~(keep : string -> Func_lookup.module_attribute -> bool)
    (attributes : Func_lookup.module_attribute Common.SMap.t)
    : positioned_binding list =
  Common.SMap.fold
    (fun (name : string) (attribute : Func_lookup.module_attribute)
         (bindings : positioned_binding list) ->
      if not (keep name attribute) then bindings
      else
        match attribute with
        | Func_lookup.Attr_functions (funcs : Func_info.t list) ->
          function_binding_of ~pos ~parent_path:[] name funcs @ bindings
        | Func_lookup.Attr_class (class_qn : Names.Class_qn.t) ->
          class_binding_of ~pos ~parent_path:[] name class_qn :: bindings
        | Func_lookup.Attr_class_with_companion (class_qn, companion_qn) ->
          class_binding_of ~pos ~parent_path:[] name class_qn
          :: companion_binding_of ~pos ~parent_path:[] name companion_qn
          :: bindings
        | Func_lookup.Attr_module _ -> bindings)
    attributes []

let bindings_of_every_attribute ~(pos : Pos.t option)
    (attributes : Func_lookup.module_attribute Common.SMap.t)
    : positioned_binding list =
  bindings_of_attributes ~pos ~keep:(fun _ _ -> true) attributes

type region_bindings = {
  rb_in_region : positioned_binding list;
  rb_names : unit Common.SMap.t;
}

let bindings_in_region (bound : region_bindings) : positioned_binding list =
  bound.rb_in_region

let build_region_bindings
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(file_infos : Types.file_info list) : region_bindings Common.SMap.t =
  let of_region (region : Names.Module_qn.t) : region_bindings =
    let in_region =
      bindings_of_every_attribute ~pos:None
        (Func_lookup.attributes_of_module attributes_by_module region)
    in
    { rb_in_region = in_region;
      rb_names =
        List.fold_left
          (fun (names : unit Common.SMap.t) (binding : positioned_binding) ->
            Common.SMap.add binding.pb_name () names)
          Common.SMap.empty in_region }
  in
  List.fold_left
    (fun (by_region : region_bindings Common.SMap.t)
         (fi : Types.file_info) ->
      List.fold_left
        (fun (by_region : region_bindings Common.SMap.t)
             (region : Names.Module_qn.t) ->
          let key = Names.Module_qn.to_string region in
          if Common.SMap.mem key by_region then by_region
          else Common.SMap.add key (of_region region) by_region)
        by_region fi.Types.fi_module_regions)
    Common.SMap.empty file_infos

let top_level_bindings ~(keep : Func_info.t -> bool)
    ~(definitions_by_qn : definition Common.SMap.t)
    : positioned_binding list =
  Common.SMap.fold
    (fun (qn : string) (definition : definition)
         (bindings : positioned_binding list) ->
      match Names.Def_qn.split_last (Names.Def_qn.of_string qn) with
      | Some ((owner : Names.Def_qn.t), (name : string))
        when Names.Def_qn.is_empty owner -> (
        match definition with
        | Function_definitions (funcs : Func_info.t list) ->
          function_binding_of ~pos:None ~parent_path:[] name
            (List.filter keep funcs)
          @ bindings
        | Class_definition { class_qn; _ } ->
          class_binding_of ~pos:None ~parent_path:[] name class_qn :: bindings)
      | Some _
      | None -> bindings)
    definitions_by_qn []
