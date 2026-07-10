module Class_name_map = Map.Make (struct
  type t = Names.Class_name.t
  let compare = Names.Class_name.compare
end)

module Module_qn_map = Map.Make (struct
  type t = Names.Module_qn.t
  let compare = Names.Module_qn.compare
end)

module Method_name_map = Map.Make (struct
  type t = Names.Method_name.t
  let compare = Names.Method_name.compare
end)

module Method_key = struct
  type t = Names.Class_name.t * Names.Method_name.t
  let compare (c1, m1) (c2, m2) =
    match Names.Class_name.compare c1 c2 with
    | 0 -> Names.Method_name.compare m1 m2
    | n -> n
end
module Method_map = Map.Make (Method_key)

module Field_key = struct
  type t = Names.Class_name.t * Names.Field_name.t
  let compare (c1, f1) (c2, f2) =
    match Names.Class_name.compare c1 c2 with
    | 0 -> Names.Field_name.compare f1 f2
    | n -> n
end
module Field_map = Map.Make (Field_key)

(* Keys are bare class-name leaves and setters are last-wins, so homonym
   classes across packages share one slot; edge-time qualifier hints in
   [Graph_from_AST] disambiguate the common Go case. *)
type t = {
  inherited_methods : Func_info.t list Class_name_map.t;
  parent_class : Names.Class_name.t Class_name_map.t;
  module_singletons : AST_generic.name Module_qn_map.t;
  method_returns : AST_generic.name Method_map.t;
  (* Class leaves unqualified; list disambiguates by package. *)
  fields : (Fpath.t * AST_generic.name) list Field_map.t;
  methods : Func_info.t list Class_name_map.t;
  function_returns : AST_generic.name Method_name_map.t;
  function_return_tuples : AST_generic.name option list Method_name_map.t;
  method_return_tuples : AST_generic.name option list Method_map.t;
}

let empty = {
  inherited_methods = Class_name_map.empty;
  parent_class = Class_name_map.empty;
  module_singletons = Module_qn_map.empty;
  method_returns = Method_map.empty;
  fields = Field_map.empty;
  methods = Class_name_map.empty;
  function_returns = Method_name_map.empty;
  function_return_tuples = Method_name_map.empty;
  method_return_tuples = Method_map.empty;
}

(* Last-wins on homonym class names across files. *)
let set_parent t child parent =
  { t with parent_class = Class_name_map.add child parent t.parent_class }

let get_parent t child = Class_name_map.find_opt child t.parent_class

let set_module_singleton t qn ty =
  { t with module_singletons = Module_qn_map.add qn ty t.module_singletons }

let get_module_singleton t qn = Module_qn_map.find_opt qn t.module_singletons

(* Last-wins; monotone call sites must gate on an already-known check. *)
let set_method_return t cls meth ty =
  { t with method_returns = Method_map.add (cls, meth) ty t.method_returns }

let get_method_return t cls meth =
  Method_map.find_opt (cls, meth) t.method_returns

let set_field t cls field def_file ty =
  let cur =
    Option.value (Field_map.find_opt (cls, field) t.fields) ~default:[]
  in
  { t with fields = Field_map.add (cls, field) ((def_file, ty) :: cur) t.fields }

let get_field_entries t cls field =
  Option.value (Field_map.find_opt (cls, field) t.fields) ~default:[]

let get_field t cls field =
  match get_field_entries t cls field with
  | [] -> None
  | (_, ty) :: _ -> Some ty

let set_methods t cls ms =
  { t with methods = Class_name_map.add cls ms t.methods }

let add_method t cls method_info =
  let cur =
    Option.value (Class_name_map.find_opt cls t.methods) ~default:[]
  in
  { t with methods = Class_name_map.add cls (method_info :: cur) t.methods }

let get_methods t cls = Class_name_map.find_opt cls t.methods

let has_methods t cls = Class_name_map.mem cls t.methods

let set_function_return t fn ty =
  { t with function_returns = Method_name_map.add fn ty t.function_returns }

let get_function_return t fn = Method_name_map.find_opt fn t.function_returns

let set_function_return_tuple t fn tys =
  { t with function_return_tuples =
      Method_name_map.add fn tys t.function_return_tuples }

let get_function_return_tuple t fn =
  Method_name_map.find_opt fn t.function_return_tuples

let set_method_return_tuple t cls meth tys =
  { t with method_return_tuples =
      Method_map.add (cls, meth) tys t.method_return_tuples }

let get_method_return_tuple t cls meth =
  Method_map.find_opt (cls, meth) t.method_return_tuples

let method_leaf_names (fs : Func_info.t list) : (string, unit) Hashtbl.t =
  let tbl = Hashtbl.create (List.length fs) in
  List.iter (fun (func : Func_info.t) ->
    match Func_info.as_method func.fn_id with
    | Some (_, meth) -> Hashtbl.replace tbl (fst meth.IL.ident) ()
    | None -> ()
  ) fs;
  tbl

let add_inherited t cls (newcomers : Func_info.t list) =
  if newcomers = [] then t
  else
    let existing =
      Option.value (Class_name_map.find_opt cls t.inherited_methods)
        ~default:[]
    in
    let seen = method_leaf_names existing in
    let added =
      List.filter (fun (func : Func_info.t) ->
        match Func_info.as_method func.fn_id with
        | Some (_, meth) ->
          let name = fst meth.IL.ident in
          if Hashtbl.mem seen name then false
          else (Hashtbl.replace seen name (); true)
        | None -> false
      ) newcomers
    in
    if added = [] then t
    else
      { t with inherited_methods =
          Class_name_map.add cls (added @ existing) t.inherited_methods }

let get_inherited t cls =
  Option.value (Class_name_map.find_opt cls t.inherited_methods)
    ~default:[]

(* Full qualified path: leaf-only misses [pkg_a.Store]→[pkg_b.Store] flips. *)
let g_name_key (name : AST_generic.name) : string list =
  AST_generic_helpers.dotted_ident_of_name name |> List.map fst
let g_name_equal a b = List.equal String.equal (g_name_key a) (g_name_key b)
let g_name_opt_equal = Option.equal g_name_equal
(* id-only equality misses body-driven [fdef] changes → never converges. *)
let func_info_equal (a : Func_info.t) (b : Func_info.t) : bool =
  Func_info.equal_fn_id a.fn_id b.fn_id
  && AST_generic.equal_function_definition a.fdef b.fdef
  && Option.equal AST_generic.equal_entity a.entity b.entity
let list_equal eq a b =
  List.length a = List.length b && List.for_all2 eq a b
let equal a b =
  Class_name_map.equal (list_equal func_info_equal)
    a.inherited_methods b.inherited_methods
  && Class_name_map.equal Names.Class_name.equal
       a.parent_class b.parent_class
  && Module_qn_map.equal g_name_equal
       a.module_singletons b.module_singletons
  && Method_map.equal g_name_equal
       a.method_returns b.method_returns
  && Field_map.equal
       (list_equal (fun (fa, ta) (fb, tb) ->
          Fpath.equal fa fb && g_name_equal ta tb))
       a.fields b.fields
  && Class_name_map.equal (list_equal func_info_equal)
       a.methods b.methods
  && Method_name_map.equal g_name_equal
       a.function_returns b.function_returns
  && Method_name_map.equal (list_equal g_name_opt_equal)
       a.function_return_tuples b.function_return_tuples
  && Method_map.equal (list_equal g_name_opt_equal)
       a.method_return_tuples b.method_return_tuples

(* String-keyed class views for the engine's callee resolver. [empty] behaves
   as "no project info": every lookup misses. *)
let has_class t cls =
  let cn = Names.Class_name.of_string cls in
  has_methods t cn || get_inherited t cn <> []

let find_methods t ~fallback ~class_name ~method_name =
  let cn = Names.Class_name.of_string class_name in
  let pool =
    if has_class t class_name then
      Option.value (get_methods t cn) ~default:[] @ get_inherited t cn
    else fallback
  in
  List.filter (fun (func : Func_info.t) ->
    Func_info.is_method_of ~class_name ~method_name func.fn_id
  ) pool

let parent t cls =
  get_parent t (Names.Class_name.of_string cls)
  |> Option.map Names.Class_name.to_string

let super_class t cls = match parent t cls with Some p -> p | None -> cls

let method_return t ~class_name ~method_name =
  get_method_return t
    (Names.Class_name.of_string class_name)
    (Names.Method_name.of_string method_name)

(* When [caller_dir] is given, prefer the field entry defined in that directory
   (Go: same package) to disambiguate same-leaf classes across packages. *)
let field_type_for_caller t ~class_name ~field_name ~caller_dir =
  let cls = Names.Class_name.of_string class_name in
  let field = Names.Field_name.of_string field_name in
  let same_dir_entry =
    Option.bind caller_dir (fun cdir ->
      let cdir_fp = Fpath.v cdir |> Fpath.normalize in
      List.find_opt (fun (df, _) ->
        Fpath.equal (Fpath.parent df |> Fpath.normalize) cdir_fp)
        (get_field_entries t cls field))
  in
  match same_dir_entry with
  | Some (_, ty) -> Some ty
  | None -> get_field t cls field

