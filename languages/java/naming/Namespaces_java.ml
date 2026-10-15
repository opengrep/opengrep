(* Java keeps variables, methods and types apart: a field, a method and a
   class may have the same name. Where a name could refer to a variable or a
   type, the variable is chosen (JLS 6.4.2, obscuring), and a method
   invocation refers to a method. *)
type t =
  | Variable
  | Method
  | Type
[@@deriving eq]

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t option = function
  | Function
  | Method ->
      Some Method
  (* a single-type import imports a type *)
  | Class _
  | Type_alias
  | Import ->
      Some Type
  | Module -> Some Variable

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Callee -> [ [ Method ] ]
  | Type_position -> [ [ Type ] ]
  | Expression
  | Qualifier ->
      [ [ Variable ]; [ Type ] ]
  (* an annotation or a record pattern refers to a type *)
  | Name -> [ [ Type ]; [ Variable ] ]
