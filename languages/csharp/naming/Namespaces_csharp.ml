(* C# keeps types apart from locals and the other members of a type. A type
   position refers to a type parameter, a type or a namespace (C# 7.8,
   namespace and type names). A simple name in an expression refers to a
   local or a parameter, then to a member of an enclosing type, nested types
   included, then to a type of an enclosing namespace (C# 12.8.4, simple
   names). *)
type t =
  | Variable
  | Type

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Variable -> 0
  | Type -> 1

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Function
  | Method
  | Constructor
  | Module ->
      [ Variable ]
  (* a using alias refers to a namespace or a type *)
  | Class
  | Type_definition
  | Type_alias
  | Type_parameter
  | Import ->
      [ Type ]
  (* a static import imports the static members of a type *)
  | Static_import -> [ Variable ]

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Expression
  | Callee ->
      [ [ Variable; Type ] ]
  | Type_position
  | Elaborated_type
  | Qualifier ->
      [ [ Type ] ]
  (* an attribute refers to a type *)
  | Name -> [ [ Type ]; [ Variable ] ]

let class_rebinds_import = true
let class_in_scope_in_its_definition = true
