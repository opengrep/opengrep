(* Kotlin keeps functions apart from variables and properties: a property and
   a function may have the same name. *)
type t =
  | Variable
  | Function
[@@deriving eq]

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t option = function
  | Function
  | Method ->
      Some Function
  | Module
  | Import ->
      Some Variable
  | Class _
  | Type_alias ->
      None

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Callee -> [ [ Function ] ]
  | Expression
  | Type_position
  | Qualifier
  | Name ->
      [ [ Variable ] ]
