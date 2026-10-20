(* TypeScript keeps values apart from types: a declaration creates a value, a
   type or both, so a value and a type may have the same name, as in
   [const User = ...] and [type User = ...]. A class and an enumeration create
   a value and a type; an interface, a type alias and a type parameter create a
   type; an import has every meaning of what it imports. *)
type t =
  | Value
  | Type

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Value -> 0
  | Type -> 1

let variables = Value

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Function
  | Method
  | Constructor
  | Module ->
      [ Value ]
  | Class
  | Type_definition
  | Import
  | Static_import ->
      [ Value; Type ]
  | Interface
  | Type_alias
  | Type_parameter ->
      [ Type ]

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Expression
  | Callee
  | Qualifier
  | Name ->
      [ [ Value ] ]
  (* a class's heritage, after [extends], is an expression *)
  | Base_class -> [ [ Value ] ]
  | Type_position
  | Elaborated_type ->
      [ [ Type ] ]

let class_rebinds_import = Lang_namespaces.Every_scope
let class_in_scope_in_its_definition = true
