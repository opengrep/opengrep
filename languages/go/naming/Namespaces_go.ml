(* Go has one namespace. A method, declared with a receiver or in an interface,
   belongs to the method set of a type: the package block holds the constants,
   types, variables and functions declared at the top level, but not methods
   (Go spec, Declarations and scope), so a method binds nothing. *)
include Lang_namespaces.Single

let of_definition (kind : Lang_namespaces.definition_kind) : t list =
  match kind with
  | Method -> []
  | Function
  | Constructor
  | Class
  | Interface
  | Type_definition
  | Type_alias
  | Type_parameter
  | Module
  | Import
  | Static_import ->
      Lang_namespaces.Single.of_definition kind
