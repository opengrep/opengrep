module G = AST_generic

type key = Class_table.cls

module Member_map = Map.Make (struct
  type t = int * string

  let compare ((left_class : int), (left_member : string))
      ((right_class : int), (right_member : string)) : int =
    match Int.compare left_class right_class with
    | 0 -> String.compare left_member right_member
    | order -> order
end)

module Node_map = Map.Make (Function_id)

module Module_qn_map = Map.Make (struct
  type t = Names.Module_qn.t

  let compare = Names.Module_qn.compare
end)

module Site_set = Set.Make (G.SId)

type t = {
  module_singletons : key Module_qn_map.t;
  method_returns : key Member_map.t;
  method_return_tuples : key option list Member_map.t;
  fields : key Member_map.t;
  field_elements : key Member_map.t;
  function_returns : key Node_map.t;
  function_return_tuples : key option list Node_map.t;
  value_type_sites : Site_set.t;
}

let empty : t =
  {
    module_singletons = Module_qn_map.empty;
    method_returns = Member_map.empty;
    method_return_tuples = Member_map.empty;
    fields = Member_map.empty;
    field_elements = Member_map.empty;
    function_returns = Node_map.empty;
    function_return_tuples = Node_map.empty;
    value_type_sites = Site_set.empty;
  }

let member_key (cls : key) (member : string) : int * string =
  (Class_table.index cls, member)

let set_module_singleton (t : t) (qn : Names.Module_qn.t) (cls : key) : t =
  { t with module_singletons = Module_qn_map.add qn cls t.module_singletons }

let get_module_singleton (t : t) (qn : Names.Module_qn.t) : key option =
  Module_qn_map.find_opt qn t.module_singletons

let set_method_return (t : t) (cls : key) (meth : string) (ret : key) : t =
  {
    t with
    method_returns = Member_map.add (member_key cls meth) ret t.method_returns;
  }

let method_return (t : t) (cls : key) (meth : string) : key option =
  Member_map.find_opt (member_key cls meth) t.method_returns

let set_method_return_tuple (t : t) (cls : key) (meth : string)
    (elements : key option list) : t =
  {
    t with
    method_return_tuples =
      Member_map.add (member_key cls meth) elements t.method_return_tuples;
  }

let method_return_tuple (t : t) (cls : key) (meth : string) :
    key option list option =
  Member_map.find_opt (member_key cls meth) t.method_return_tuples

let set_field (t : t) (cls : key) (field_name : string) (ty : key) : t =
  { t with fields = Member_map.add (member_key cls field_name) ty t.fields }

let field (t : t) (cls : key) (field_name : string) : key option =
  Member_map.find_opt (member_key cls field_name) t.fields

let set_field_element (t : t) (cls : key) (field_name : string) (element : key)
    : t =
  {
    t with
    field_elements =
      Member_map.add (member_key cls field_name) element t.field_elements;
  }

let field_element (t : t) (cls : key) (field_name : string) : key option =
  Member_map.find_opt (member_key cls field_name) t.field_elements

let set_function_return (t : t) (node : Function_id.t) (ret : key) : t =
  { t with function_returns = Node_map.add node ret t.function_returns }

let function_return (t : t) (node : Function_id.t) : key option =
  Node_map.find_opt node t.function_returns

let set_function_return_tuple (t : t) (node : Function_id.t)
    (elements : key option list) : t =
  {
    t with
    function_return_tuples = Node_map.add node elements t.function_return_tuples;
  }

let function_return_tuple (t : t) (node : Function_id.t) :
    key option list option =
  Node_map.find_opt node t.function_return_tuples

let add_value_type_site (t : t) (site : G.SId.t) : t =
  { t with value_type_sites = Site_set.add site t.value_type_sites }

let is_value_type (t : t) (ty : G.type_) : bool =
  match Option.bind (Class_table.name_of_type ty) Class_table.site_of_name with
  | Some (site : G.SId.t) -> Site_set.mem site t.value_type_sites
  | None -> false

let equal (left : t) (right : t) : bool =
  let same_elements = List.equal (Option.equal Class_table.same) in
  Module_qn_map.equal Class_table.same left.module_singletons
    right.module_singletons
  && Member_map.equal Class_table.same left.method_returns right.method_returns
  && Member_map.equal same_elements left.method_return_tuples
       right.method_return_tuples
  && Member_map.equal Class_table.same left.fields right.fields
  && Member_map.equal Class_table.same left.field_elements right.field_elements
  && Node_map.equal Class_table.same left.function_returns
       right.function_returns
  && Node_map.equal same_elements left.function_return_tuples
       right.function_return_tuples
  && Site_set.equal left.value_type_sites right.value_type_sites
