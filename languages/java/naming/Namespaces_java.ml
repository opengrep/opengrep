(* Java keeps variables, methods and types apart: a field, a method and a
   class may have the same name. Where a name could refer to a variable or a
   type, the variable is chosen (JLS 6.4.2, obscuring), and a method
   invocation refers to a method. *)
type t =
  | Variable
  | Method
  | Type

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Variable -> 0
  | Method -> 1
  | Type -> 2

let variables = Variable

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Function
  | Method ->
      [ Method ]
  (* a constructor is not a member, and no invocation refers to it by name
     (JLS 8.8): its name refers to its class *)
  | Constructor -> []
  (* a single-type import imports a type *)
  | Class
  | Type_definition
  | Type_alias
  | Type_parameter
  | Import ->
      [ Type ]
  | Module -> [ Variable ]
  (* a single-static-import imports every static member of its name: fields,
     methods and member types (JLS 7.5.3) *)
  | Static_import -> [ Variable; Method; Type ]

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Callee -> [ [ Method ] ]
  | Type_position
  | Elaborated_type ->
      [ [ Type ] ]
  | Expression
  | Qualifier ->
      [ [ Variable ]; [ Type ] ]
  (* an annotation or a record pattern refers to a type *)
  | Name -> [ [ Type ]; [ Variable ] ]

let class_rebinds_import = true
let class_in_scope_in_its_definition = true
