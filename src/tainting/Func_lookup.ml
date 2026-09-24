(* An index type built from a [Hashtbl.t] keeps that table with no copy, so
   the caller must not mutate the table after building the index. *)

type alias_index = (string, Names.Module_qn.t) Hashtbl.t

(* Local import name -> the exported name it binds and the files that
   export it.  [import { C as Alias }] names its origin exactly, which is
   what tells two same-named imported classes apart. *)

type module_attribute =
  | Attr_functions of Func_info.t list
  | Attr_class of Names.Class_qn.t
  | Attr_class_with_companion of Names.Class_qn.t * Names.Class_qn.t
  | Attr_module of Names.Module_qn.t

type module_attributes = module_attribute Common.SMap.t Common.SMap.t

module Class_qn_map = Map.Make (struct
  type t = Names.Class_qn.t
  let compare = Names.Class_qn.compare
end)

type companion_index = Names.Class_qn.t Class_qn_map.t

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

let alias_index_of_hashtbl tbl = tbl

type t = {
  alias_to_module_qn : alias_index option;
  scope_table : scope_table;
  own_modules : Names.Module_qn.t list;
  module_attributes : module_attributes;
  companions : companion_index;
  class_of_qn : Names.Class_qn.t -> Class_table.cls option;
  is_import : AST_generic.SId.t -> bool;
  definition : string -> module_attribute option;
  member_classes : Names.Class_qn.t list;
}

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

let class_of_qn (t : t) (class_qn : Names.Class_qn.t) : Class_table.cls option =
  t.class_of_qn class_qn

let is_known_module (t : t) (module_qn : Names.Module_qn.t) : bool =
  match
    Common.SMap.find_opt (Names.Module_qn.to_string module_qn)
      t.module_attributes
  with
  | None -> false
  | Some (attributes : module_attribute Common.SMap.t) ->
    not (Common.SMap.is_empty attributes)

let is_import (t : t) (sid : AST_generic.SId.t) : bool = t.is_import sid

let member_classes (t : t) : Names.Class_qn.t list = t.member_classes

let definition (t : t) (qualified_name : string) : module_attribute option =
  t.definition qualified_name

let create
    ?alias_to_module_qn
    ?(own_modules : Names.Module_qn.t list = [])
    ?(companions : companion_index = Class_qn_map.empty)
    ?(member_classes : Names.Class_qn.t list = [])
    ~(module_attributes : module_attributes)
    ~(class_of_qn : Names.Class_qn.t -> Class_table.cls option)
    ~(is_import : AST_generic.SId.t -> bool)
    ~(definition : string -> module_attribute option)
    ~(scope_table : scope_table) () =
  { alias_to_module_qn;
    scope_table;
    own_modules;
    module_attributes;
    companions;
    class_of_qn;
    is_import;
    definition;
    member_classes }

let resolve_alias t name =
  match t.alias_to_module_qn with
  | Some idx -> Hashtbl.find_opt idx name
  | None -> None
