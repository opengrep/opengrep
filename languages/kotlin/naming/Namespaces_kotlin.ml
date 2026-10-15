(* Kotlin keeps functions apart from variables and properties: a property and
   a function may have the same name. *)
type t =
  | Variable
  | Function

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Variable -> 0
  | Function -> 1

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Function
  | Method ->
      [ Function ]
  | Module
  | Import
  | Static_import ->
      [ Variable ]
  | Class
  | Type_alias
  | Type_parameter ->
      []

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Callee -> [ [ Function ] ]
  | Expression
  | Type_position
  | Qualifier
  | Name ->
      [ [ Variable ] ]

let class_rebinds_import = true
let class_in_scope_in_its_definition = true
