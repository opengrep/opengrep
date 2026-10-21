(* The namespaces of a language, for name resolution: a definition binds its
   name, with one identity, in some namespaces of its language, and a use of a
   name searches some of them. *)

(* Identifies a namespace of a language: distinct namespaces of one language
   have distinct hashes. *)
type namespace_hash = int

(* A definition other than a variable, as name resolution tells them apart. *)
type definition_kind =
  | Function (* outside a class *)
  | Method (* in a class body, or whose [fkind] is [Method], as a Go method *)
  | Constructor (* a function the front end marks with [KeywordAttr Ctor] *)
  | Class
  | Interface
  | Type_definition (* a type of its own: an enumeration, a record, a variant *)
  | Type_alias (* another name for a type, as a C typedef *)
  | Type_parameter
  | Module (* a module alias *)
  | Import
  | Static_import (* of static members, as Java's [import static] *)

type use_position =
  | Expression
  | Callee (* the function of a call *)
  | Type_position (* in a type *)
  | Elaborated_type (* in a type with its keyword, as [struct s] in C *)
  | Qualifier (* the first part of a qualified name *)
  | Base_class (* a parent of a class other than an interface *)
  | Name (* any other place: a constructor, an attribute *)

(* The scopes in which a class, a type definition or a type alias rebinds or
   shadows a name that an import binds. Elsewhere the import keeps the name,
   and the definition's name takes the import's resolution. *)
type import_rebinding =
  | Every_scope
  | Inner_scopes (* not the top level, where an import outranks the
                    declarations of the file *)
  | No_scope

module type NAMESPACES = sig
  type t

  val namespace_hash : t -> namespace_hash

  (* The namespace of variables, which also holds parameters and fields. *)
  val variables : t

  (* The namespaces a definition binds its name in; [[]]: it binds nothing. *)
  val of_definition : definition_kind -> t list

  (* The groups of namespaces a use searches, in order. A group is looked up
     in each scope from the innermost outward, and within a scope in the
     order of its namespaces. *)
  val searched_by : use_position -> t list list

  (* The scopes in which a class, a type definition or a type alias rebinds a
     name that an import binds. *)
  val class_rebinds_import : import_rebinding

  (* A class, a type definition or a type alias is in scope in its own
     definition; otherwise it binds once its definition is visited. *)
  val class_in_scope_in_its_definition : bool
end

(* A single namespace for every definition. *)
module Single : NAMESPACES = struct
  type t = Value

  let namespace_hash (Value : t) : namespace_hash = 0
  let variables = Value
  let of_definition (_ : definition_kind) : t list = [ Value ]
  let searched_by (_ : use_position) : t list list = [ [ Value ] ]
  let class_rebinds_import = Every_scope
  let class_in_scope_in_its_definition = true
end

(* A single namespace, in which classes, types and type parameters bind
   nothing. *)
module Single_without_type_bindings : NAMESPACES = struct
  include Single

  let of_definition (kind : definition_kind) : t list =
    match kind with
    | Function
    | Method
    | Constructor
    | Module
    | Import
    | Static_import ->
        Single.of_definition kind
    | Class
    | Interface
    | Type_definition
    | Type_alias
    | Type_parameter ->
        []
end
