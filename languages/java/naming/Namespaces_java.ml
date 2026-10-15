(* Java keeps methods apart from variables: a field and a method may have the
   same name, and a method invocation refers to a method. *)
type t =
  | Variable
  | Method
[@@deriving eq]

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t option = function
  | Function
  | Method ->
      Some Method
  | Module
  | Import ->
      Some Variable
  | Class _
  | Type_alias ->
      None

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Callee -> [ [ Method ] ]
  | Expression
  | Type_position
  | Qualifier
  | Name ->
      [ [ Variable ] ]
