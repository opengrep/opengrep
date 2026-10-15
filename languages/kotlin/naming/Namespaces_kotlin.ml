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
  | Import ->
      [ Variable ]
  | Class
  | Type_alias ->
      []

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Callee -> [ [ Function ] ]
  | Expression
  | Type_position
  | Qualifier
  | Name ->
      [ [ Variable ] ]
