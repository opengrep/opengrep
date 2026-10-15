(* The namespaces of a language, for name resolution: a definition binds its
   name, with one identity, in some namespaces of its language, and a use of a
   name searches some of them. *)

(* Identifies a namespace of a language: distinct namespaces of one language
   have distinct hashes. *)
type namespace_hash = int

(* A definition other than a variable, as name resolution tells them apart. *)
type definition_kind =
  | Function (* outside a class *)
  | Method (* in a class body *)
  | Class
  | Type_alias
  | Module (* a module alias *)
  | Import

type use_position =
  | Expression
  | Callee (* the function of a call *)
  | Type_position (* in a type *)
  | Qualifier (* the first part of a qualified name *)
  | Name (* any other place: a constructor, an attribute *)

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
end

(* A single namespace, in which classes and type aliases bind nothing. *)
module Single_without_type_bindings : NAMESPACES = struct
  type t = Value

  let namespace_hash (Value : t) : namespace_hash = 0
  let variables = Value

  let of_definition : definition_kind -> t list = function
    | Function
    | Method
    | Module
    | Import ->
        [ Value ]
    | Class
    | Type_alias ->
        []

  let searched_by (_ : use_position) : t list list = [ [ Value ] ]
end
