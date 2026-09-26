type side =
  | Instance_side
  | Class_side

type t = {
  written : AST_generic.type_;
  relation : Linearisation.relation;
  side : side;
  arguments : AST_generic.arguments option;
  delegate : AST_generic.expr option;
}

val of_definition : Lang.t -> AST_generic.definition_kind -> t list
val definition_body : AST_generic.definition_kind -> AST_generic.stmt list
val member_imports :
  AST_generic.definition_kind -> AST_generic.member_import list
val type_members :
  AST_generic.definition_kind -> (string * AST_generic.type_) list

val declared_members : AST_generic.definition_kind -> string list

type singleton_exposure =
  | No_singleton_exposure
  | Every_method_is_a_singleton
  | Named_singleton_methods of string list

val singleton_exposure :
  Lang.t -> AST_generic.definition_kind -> singleton_exposure

val exposes : singleton_exposure -> string -> bool

val extended_type : AST_generic.definition_kind -> AST_generic.type_ option

val reopens :
  Lang.t -> AST_generic.entity -> AST_generic.definition_kind -> bool

type metatable_fact =
  | Metatable_set of {
      holder : AST_generic.name;
      metatable : AST_generic.expr;
    }
  | Index_assigned of {
      table : AST_generic.name;
      index : AST_generic.expr;
    }

val index_field :
  Lang_config.metatable -> AST_generic.expr list -> AST_generic.expr list

val metatable_facts :
  Lang_config.metatable -> AST_generic.program -> metatable_fact list
