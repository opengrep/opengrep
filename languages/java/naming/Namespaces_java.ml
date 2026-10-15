(* Java keeps methods apart from variables: a field and a method may have the
   same name, and a method invocation refers to a method. *)
type t =
  | Variable
  | Method

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Variable -> 0
  | Method -> 1

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Function
  | Method ->
      [ Method ]
  | Module
  | Import ->
      [ Variable ]
  | Class
  | Type_alias ->
      []

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Callee -> [ [ Method ] ]
  | Expression
  | Type_position
  | Qualifier
  | Name ->
      [ [ Variable ] ]
