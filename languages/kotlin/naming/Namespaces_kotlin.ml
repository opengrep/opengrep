(* Kotlin keeps variables and properties, functions, and classifiers apart: a
   property, a function and a class may have the same name. Where a name could
   refer to a variable or a classifier, the variable is chosen. *)
type t =
  | Variable
  | Function
  | Classifier
[@@deriving eq]

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t option = function
  | Function
  | Method ->
      Some Function
  | Class _
  | Type_alias
  | Import ->
      Some Classifier
  | Module -> Some Variable

let searched_by : Lang_namespaces.use_position -> t list list = function
  (* a function or a constructor is chosen before the invoke of a variable *)
  | Callee -> [ [ Function; Classifier ]; [ Variable ] ]
  | Type_position -> [ [ Classifier ] ]
  | Expression
  | Qualifier ->
      [ [ Variable ]; [ Classifier ] ]
  (* an annotation refers to a classifier *)
  | Name -> [ [ Classifier ]; [ Variable ] ]
