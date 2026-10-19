(* C keeps the tags of structures, unions and enumerations apart from ordinary
   identifiers: variables, functions, typedef names and enumeration constants
   (C11 6.2.3). A tag is referred to with its keyword, as in [struct s]; a
   name without a keyword is an ordinary identifier. *)
type t =
  | Ordinary
  | Tag

let namespace_hash : t -> Lang_namespaces.namespace_hash = function
  | Ordinary -> 0
  | Tag -> 1

let variables = Ordinary

let of_definition : Lang_namespaces.definition_kind -> t list = function
  | Class
  | Type_definition ->
      [ Tag ]
  | Function
  | Method
  | Constructor
  | Type_alias
  | Type_parameter
  | Module
  | Import
  | Static_import ->
      [ Ordinary ]

let searched_by : Lang_namespaces.use_position -> t list list = function
  | Elaborated_type -> [ [ Tag ] ]
  | Expression
  | Callee
  | Type_position
  | Qualifier
  | Name ->
      [ [ Ordinary ] ]

let class_rebinds_import = true

(* a tag is in scope just after its appearance in its declaration (C11
   6.2.1), as in [struct node { struct node *next; }] *)
let class_in_scope_in_its_definition = true
