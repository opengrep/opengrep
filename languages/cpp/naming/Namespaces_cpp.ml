(* C++ declares class and enumeration names in the scope of ordinary names,
   but a variable, a data member, a function or an enumerator hides a class or
   enumeration of the same name declared in the same scope, in any order
   ([basic.scope.hiding]). A name with its class key or [enum], as in
   [struct s], refers to a class or an enumeration ([basic.lookup.elab]). *)
type t =
  | Ordinary
  | Class_or_enumeration

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Ordinary -> 0
  | Class_or_enumeration -> 1

let variables = Ordinary

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Class
  | Interface
  | Type_definition ->
      [ Class_or_enumeration ]
  (* a constructor has no name: the name in its declaration is the class's
     ([class.ctor.general]) *)
  | Constructor -> []
  | Function
  | Method
  | Type_alias
  | Type_parameter
  | Module
  | Import
  | Static_import ->
      [ Ordinary ]

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Elaborated_type -> [ [ Class_or_enumeration ] ]
  (* the name before [::] refers to a namespace or a type, not to a variable
     or a function ([basic.lookup.qual.general]): in a scope, a class or an
     enumeration comes before an ordinary name, as a typedef name or a
     namespace alias *)
  | Qualifier -> [ [ Class_or_enumeration; Ordinary ] ]
  | Expression
  | Callee
  | Type_position
  | Name
  | Base_class ->
      [ [ Ordinary; Class_or_enumeration ] ]

let class_rebinds_import = true

(* the class's name is in scope in its own definition, as its
   injected-class-name ([class.pre]) *)
let class_in_scope_in_its_definition = true
