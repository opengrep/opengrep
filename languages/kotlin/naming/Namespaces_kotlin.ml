(* Kotlin keeps variables and properties, functions, and classifiers apart: a
   property, a function and a class may have the same name. Where a name could
   refer to a variable or a classifier, the variable is chosen. *)
type t =
  | Variable
  | Function
  | Classifier

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Variable -> 0
  | Function -> 1
  | Classifier -> 2

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Function
  | Method
  | Constructor ->
      [ Function ]
  | Class
  | Interface
  | Type_definition
  | Type_alias
  | Type_parameter
  | Import
  | Static_import ->
      [ Classifier ]
  | Module -> [ Variable ]

let searched_by : Lang_namespaces.use_position -> t list list = function
  (* a function or a constructor is chosen before the invoke of a variable *)
  | Callee -> [ [ Function; Classifier ]; [ Variable ] ]
  | Type_position
  | Elaborated_type
  | Base_class ->
      [ [ Classifier ] ]
  | Expression
  | Qualifier ->
      [ [ Variable ]; [ Classifier ] ]
  (* an annotation refers to a classifier *)
  | Name -> [ [ Classifier ]; [ Variable ] ]

let class_rebinds_import = true
let class_in_scope_in_its_definition = true
