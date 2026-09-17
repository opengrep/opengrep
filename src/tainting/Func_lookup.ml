(* An index type built from a [Hashtbl.t] keeps that table with no copy, so
   the caller must not mutate the table after building the index. *)

(* [Override (front, back)] returns [front]'s functions when [front] holds
   the bare name, and [back]'s otherwise. It reads [back], the project table
   shared by every file, and never writes to it; [front] is a small table
   built for one file. *)
type bare_name_index =
  | Table of (string, Func_info.t list) Hashtbl.t
  | Override of bare_name_index * bare_name_index
type module_index = (Names.Module_qn.t, Func_info.t list) Hashtbl.t
type alias_index = (string, Names.Module_qn.t) Hashtbl.t
type file_module_index = (string, Names.Module_qn.t) Hashtbl.t
type name_set = (string, unit) Hashtbl.t

(* Local import name -> the exported name it binds and the files that
   export it.  [import { C as Alias }] names its origin exactly, which is
   what tells two same-named imported classes apart. *)

type constructor_index = Func_info.t list Common.SMap.t

type module_attribute =
  | Attr_functions of Func_info.t list
  | Attr_class of Names.Class_qn.t
  | Attr_class_with_companion of Names.Class_qn.t * Names.Class_qn.t
  | Attr_module of Names.Module_qn.t

type module_attributes = module_attribute Common.SMap.t Common.SMap.t

type resolution_orders = Names.Class_qn.t list Common.SMap.t

module Class_qn_map = Map.Make (struct
  type t = Names.Class_qn.t
  let compare = Names.Class_qn.compare
end)

type methods_by_class = Func_info.t list Common.SMap.t Class_qn_map.t

type method_receiver =
  | On_class
  | On_instance
  | On_any

type singleton_names = unit Common.SMap.t Class_qn_map.t

type companion_index = Names.Class_qn.t Class_qn_map.t

type project_classes = unit Class_qn_map.t

type class_qn_by_definition =
  (Function_id.t * Names.Class_qn.t) list Common.SMap.t

let attributes_of_module (attributes : module_attributes)
    (qn : Names.Module_qn.t) : module_attribute Common.SMap.t =
  Option.value
    (Common.SMap.find_opt (Names.Module_qn.to_string qn) attributes)
    ~default:Common.SMap.empty

type scope_kind =
  | Scope_function of Func_info.t
  | Scope_class of Names.Class_qn.t
  | Scope_companion of Names.Class_qn.t
  | Scope_extension of Func_info.t
  | Scope_object of Func_info.t list Common.SMap.t
  | Scope_local_value

type scope_entry = {
  kind : scope_kind;
  parent_path : IL.name option list;
}

type scope_table =
  | Scope_bindings of scope_entry list Common.SMap.t
  | Scope_union of scope_table * scope_table
  | Scope_shadowing of scope_table * scope_table

let scope_table_of_map (bindings : scope_entry list Common.SMap.t)
    : scope_table = Scope_bindings bindings

let scope_table_union ~(front : scope_table) ~(back : scope_table)
    : scope_table = Scope_union (front, back)

let scope_table_shadowing ~(front : scope_table) ~(back : scope_table)
    : scope_table = Scope_shadowing (front, back)

let class_of_entries (entries : scope_entry list) : Names.Class_qn.t option =
  List.find_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_class (class_qn : Names.Class_qn.t) -> Some class_qn
      | Scope_companion _
      | Scope_function _
      | Scope_object _
      | Scope_local_value
      | Scope_extension _ -> None)
    entries

let companion_of_entries (entries : scope_entry list)
    : Names.Class_qn.t option =
  List.find_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_companion (class_qn : Names.Class_qn.t) -> Some class_qn
      | Scope_class _
      | Scope_function _
      | Scope_object _
      | Scope_local_value
      | Scope_extension _ -> None)
    entries

let functions_of_entries (entries : scope_entry list) : Func_info.t list =
  List.filter_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_function (func : Func_info.t) -> Some func
      | Scope_class _
      | Scope_companion _
      | Scope_object _
      | Scope_local_value
      | Scope_extension _ -> None)
    entries

let extensions_of_entries (entries : scope_entry list) : Func_info.t list =
  List.filter_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_extension (func : Func_info.t) -> Some func
      | Scope_class _
      | Scope_companion _
      | Scope_object _
      | Scope_local_value
      | Scope_function _ -> None)
    entries

let object_of_entries (entries : scope_entry list)
    : Func_info.t list Common.SMap.t option =
  List.find_map
    (fun (entry : scope_entry) ->
      match entry.kind with
      | Scope_object (members : Func_info.t list Common.SMap.t) -> Some members
      | Scope_class _
      | Scope_companion _
      | Scope_function _
      | Scope_local_value
      | Scope_extension _ -> None)
    entries

let empty_scope_table : scope_table = Scope_bindings Common.SMap.empty

let equal_kind (left : scope_kind) (right : scope_kind) : bool =
  match (left, right) with
  | Scope_function (first : Func_info.t), Scope_function (second : Func_info.t)
  | Scope_extension (first : Func_info.t), Scope_extension (second : Func_info.t)
    -> Func_info.equal_fn_id first.fn_id second.fn_id
  | Scope_class (first : Names.Class_qn.t), Scope_class (second : Names.Class_qn.t)
  | Scope_companion (first : Names.Class_qn.t),
    Scope_companion (second : Names.Class_qn.t)
    -> Names.Class_qn.equal first second
  | Scope_local_value, Scope_local_value -> true
  | Scope_object _, Scope_object _ -> false
  | (Scope_function _ | Scope_extension _ | Scope_class _ | Scope_companion _
    | Scope_object _ | Scope_local_value), _ -> false

let keep_distinct (entries : scope_entry list) : scope_entry list =
  List_.uniq_by
    (fun (left : scope_entry) (right : scope_entry) ->
      equal_kind left.kind right.kind
      && List.equal (Option.equal Function_id.equal_il_name) left.parent_path
           right.parent_path)
    entries

let bare_name_index_of_hashtbl tbl = Table tbl
let bare_name_index_override ~front ~back = Override (front, back)

let rec find_in_index (idx : bare_name_index) (key : string) : Func_info.t list =
  match idx with
  | Table tbl -> Option.value (Hashtbl.find_opt tbl key) ~default:[]
  | Override (front, back) -> (
      match find_in_index front key with
      | [] -> find_in_index back key
      | xs -> xs)

let module_index_of_hashtbl tbl = tbl
let alias_index_of_hashtbl tbl = tbl
let file_module_index_of_hashtbl tbl = tbl
let name_set_of_hashtbl tbl = tbl

let constructor_index_of_funcs ~(lang : Lang.t)
    (funcs : Func_info.t list) : constructor_index =
  List.fold_left
    (fun (constructors_by_class : constructor_index) (func : Func_info.t) ->
      match Func_info.as_method func.fn_id with
      | None -> constructors_by_class
      | Some ((cls : IL.name), (meth : IL.name)) ->
        let class_name = fst cls.IL.ident in
        if
          not
            (Object_initialization.is_constructor lang (fst meth.IL.ident)
               (Some class_name))
        then constructors_by_class
        else
          Common.SMap.update class_name
            (function
              | None -> Some [ func ]
              | Some (constructors : Func_info.t list) ->
                Some (func :: constructors))
            constructors_by_class)
    Common.SMap.empty funcs
  |> Common.SMap.map List.rev

type t = {
  funcs_by_name : bare_name_index option;
  project_funcs_by_name : bare_name_index option;
  funcs_by_module_qn : module_index option;
  alias_to_module_qn : alias_index option;
  same_file_funcs_by_name : bare_name_index option;
  (* Disambiguates methods with the same simple name across same-basename
     packages by exact import path. *)
  file_module_qn : file_module_index option;
  local_imports : name_set option;
  constructors : constructor_index option;
  project_constructors : constructor_index option;
  (* The project index widens an overload group's representative to the
     union of the group (see [Structural_dispatch.emit_overload_edges]),
     so a same-arity tie resolves to it; a single-file graph has no such
     union and gives up on the tie. *)
  overload_groups : bool;
  top_level_scope_is_project : bool;
  scope_table : scope_table;
  own_modules : Names.Module_qn.t list;
  module_attributes : module_attributes;
  resolution_orders : resolution_orders;
  class_qn_by_definition : class_qn_by_definition;
  methods_by_class : methods_by_class;
  singleton_names : singleton_names;
  companions : companion_index;
  project_classes : project_classes;
  method_sets : Lang_config.method_sets;
}

let overload_groups (t : t) : bool = t.overload_groups

let top_level_scope_is_project (t : t) : bool = t.top_level_scope_is_project

let own_modules (t : t) : Names.Module_qn.t list = t.own_modules

let rec parent_path_is_prefix (pre : IL.name option list)
    (path : IL.name option list) : bool =
  match pre, path with
  | [], _ -> true
  | p :: ps, x :: xs ->
    Option.equal Function_id.equal_il_name p x
    && parent_path_is_prefix ps xs
  | _ :: _, [] -> false

let nearest_scope_entries (entries : scope_entry list) : scope_entry list =
  let depth (entry : scope_entry) : int =
    List.length entry.parent_path
  in
  match entries with
  | [] -> []
  | first :: rest ->
    let nearest =
      List.fold_left
        (fun (deepest : int) (entry : scope_entry) ->
          max deepest (depth entry))
        (depth first) rest
    in
    List.filter
      (fun (entry : scope_entry) -> Int.equal (depth entry) nearest)
      entries

let resolve_in_scope (t : t) ~(caller_parent_path : IL.name option list)
    (name : string) : scope_entry list =
  let visible (entry : scope_entry) : bool =
    parent_path_is_prefix entry.parent_path caller_parent_path
  in
  let rec entries (table : scope_table) : scope_entry list =
    match table with
    | Scope_bindings (bindings : scope_entry list Common.SMap.t) ->
      List.filter visible
        (Option.value (Common.SMap.find_opt name bindings) ~default:[])
    | Scope_union (front, back) -> (
      match (entries front, entries back) with
      | found, [] -> found
      | [], found -> found
      | front_entries, back_entries ->
        keep_distinct (front_entries @ back_entries))
    | Scope_shadowing (front, back) -> (
      match entries front with
      | [] -> entries back
      | found -> found)
  in
  nearest_scope_entries (entries t.scope_table)

let module_attribute (t : t) (qn : Names.Module_qn.t) (name : string)
    : module_attribute option =
  Common.SMap.find_opt name (attributes_of_module t.module_attributes qn)

let companion_of (t : t) (class_qn : Names.Class_qn.t)
    : Names.Class_qn.t option =
  Class_qn_map.find_opt class_qn t.companions

let resolution_order (t : t) (class_qn : Names.Class_qn.t)
    : Names.Class_qn.t list =
  Option.value
    (Common.SMap.find_opt (Names.Class_qn.to_string class_qn)
       t.resolution_orders)
    ~default:[]

let is_known_class (t : t) (class_qn : Names.Class_qn.t) : bool =
  Class_qn_map.mem class_qn t.project_classes

let has_class (t : t) (class_qn : Names.Class_qn.t) : bool =
  Common.SMap.mem (Names.Class_qn.to_string class_qn) t.resolution_orders

let is_known_module (t : t) (module_qn : Names.Module_qn.t) : bool =
  match
    Common.SMap.find_opt (Names.Module_qn.to_string module_qn)
      t.module_attributes
  with
  | None -> false
  | Some (attributes : module_attribute Common.SMap.t) ->
    not (Common.SMap.is_empty attributes)

let class_qn_of_definition (t : t) (definition : IL.name)
    : Names.Class_qn.t option =
  match
    Common.SMap.find_opt (fst definition.IL.ident) t.class_qn_by_definition
  with
  | None -> None
  | Some (classes : (Function_id.t * Names.Class_qn.t) list) ->
    Option.map snd
      (List.find_opt
         (fun (((id : Function_id.t), _) :
                 Function_id.t * Names.Class_qn.t) ->
           Function_id.equal_name id definition)
         classes)

let find_along_order (t : t) ~(receiver : method_receiver)
    (order : Names.Class_qn.t list)
    (names_of : Names.Class_qn.t -> string list) : Func_info.t list =
  let bound_on
      ~(keep : Names.Class_qn.t -> string -> Func_info.t -> bool)
      (class_qn : Names.Class_qn.t) : Func_info.t list =
    match Class_qn_map.find_opt class_qn t.methods_by_class with
    | None -> []
    | Some (by_name : Func_info.t list Common.SMap.t) ->
      List.concat_map
        (fun (name : string) ->
          List.filter (keep class_qn name)
            (Option.value (Common.SMap.find_opt name by_name) ~default:[]))
        (names_of class_qn)
  in
  let rec first_binder
      ~(keep : Names.Class_qn.t -> string -> Func_info.t -> bool)
      (order : Names.Class_qn.t list) : Func_info.t list =
    match order with
    | [] -> []
    | class_qn :: rest -> (
      match bound_on ~keep class_qn with
      | [] -> first_binder ~keep rest
      | (_ :: _) as found -> found)
  in
  let every (_ : Names.Class_qn.t) (_ : string) (_ : Func_info.t) : bool =
    true
  in
  let exposed_by (class_qn : Names.Class_qn.t) (name : string) : bool =
    match Class_qn_map.find_opt class_qn t.singleton_names with
    | None -> false
    | Some (names : unit Common.SMap.t) -> Common.SMap.mem name names
  in
  let singleton (class_qn : Names.Class_qn.t) (name : string)
      (func : Func_info.t) : bool =
    Receiver.is_static func.entity || exposed_by class_qn name
  in
  let instance (_ : Names.Class_qn.t) (_ : string) (func : Func_info.t) : bool =
    not (Receiver.is_static func.entity)
  in
  match t.method_sets with
  | Lang_config.Shared_by_class_and_instance -> first_binder ~keep:every order
  | Lang_config.Separate_for_class_and_instance -> (
    match receiver with
    | On_any -> first_binder ~keep:every order
    | On_instance -> first_binder ~keep:instance order
    | On_class -> first_binder ~keep:singleton order)

let empty = {
  funcs_by_name = None;
  project_funcs_by_name = None;
  funcs_by_module_qn = None;
  alias_to_module_qn = None;
  same_file_funcs_by_name = None;
  file_module_qn = None;
  local_imports = None;
  constructors = None;
  project_constructors = None;
  overload_groups = false;
  top_level_scope_is_project = false;
  scope_table = empty_scope_table;
  own_modules = [];
  module_attributes = Common.SMap.empty;
  resolution_orders = Common.SMap.empty;
  class_qn_by_definition = Common.SMap.empty;
  methods_by_class = Class_qn_map.empty;
  singleton_names = Class_qn_map.empty;
  companions = Class_qn_map.empty;
  project_classes = Class_qn_map.empty;
  method_sets = Lang_config.Shared_by_class_and_instance;
}

let create
    ?funcs_by_name ?project_funcs_by_name
    ?funcs_by_module_qn ?alias_to_module_qn
    ?same_file_funcs_by_name ?file_module_qn
    ?local_imports ?constructors ?project_constructors
    ?(overload_groups = false)
    ?(top_level_scope_is_project = false)
    ?(own_modules : Names.Module_qn.t list = [])
    ?(companions : companion_index = Class_qn_map.empty)
    ?(project_classes : project_classes = Class_qn_map.empty)
    ~(module_attributes : module_attributes)
    ~(resolution_orders : resolution_orders)
    ~(class_qn_by_definition : class_qn_by_definition)
    ~(methods_by_class : methods_by_class)
    ~(singleton_names : singleton_names)
    ~(method_sets : Lang_config.method_sets)
    ~(scope_table : scope_table) () =
  { funcs_by_name;
    project_funcs_by_name;
    funcs_by_module_qn;
    alias_to_module_qn;
    same_file_funcs_by_name;
    file_module_qn;
    local_imports;
    constructors;
    project_constructors;
    overload_groups;
    top_level_scope_is_project;
    scope_table;
    own_modules;
    module_attributes;
    resolution_orders;
    class_qn_by_definition;
    methods_by_class;
    singleton_names;
    companions;
    project_classes;
    method_sets }

let with_local_imports t local_imports : t =
  { t with local_imports }

let is_locally_imported t name =
  match t.local_imports with
  | Some idx -> Hashtbl.mem idx name
  | None -> false

let funcs_with_bare_name t ~all_funcs bare_name =
  match t.project_funcs_by_name with
  | Some idx -> find_in_index idx bare_name
  | None ->
    List.filter (fun (func : Func_info.t) ->
      match List_.init_and_last_opt func.fn_id with
      | Some (_, Some name) -> String.equal (fst name.IL.ident) bare_name
      | _ -> false
    ) all_funcs

let narrow_candidates_by_bare_name t bare_name =
  match t.funcs_by_name with
  | Some idx ->
    Some (find_in_index idx bare_name)
  | None -> None

let constructors_of_class (t : t) (class_name : string)
  : Func_info.t list =
  let constructors_of (constructors_by_class : constructor_index)
      : Func_info.t list =
    Option.value (Common.SMap.find_opt class_name constructors_by_class)
      ~default:[]
  in
  match t.constructors with
  | Some (file_constructors : constructor_index) ->
    constructors_of file_constructors
  | None ->
    Option.fold ~none:[] ~some:constructors_of t.project_constructors

let nested_in_same_file t bare_name =
  match t.same_file_funcs_by_name with
  | Some idx -> find_in_index idx bare_name
  | None -> []

let resolve_alias t name =
  match t.alias_to_module_qn with
  | Some idx -> Hashtbl.find_opt idx name
  | None -> None

let module_qn_of_file t file =
  match t.file_module_qn with
  | Some idx -> Hashtbl.find_opt idx file
  | None -> None

let imports_indexed t =
  Option.is_some t.alias_to_module_qn
  && Option.is_some t.funcs_by_module_qn

let funcs_in_module t qn =
  match t.funcs_by_module_qn with
  | Some idx -> (Option.value (Hashtbl.find_opt idx qn) ~default:[])
  | None -> []
