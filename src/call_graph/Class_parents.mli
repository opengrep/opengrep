type side =
  | Instance_side
  | Class_side

type t = {
  written : AST_generic.type_;
  relation : Member_lookup.relation;
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

type module_functions =
  | No_module_functions
  | All_module_functions
  | Module_functions of string list

val module_functions :
  Lang.t -> AST_generic.definition_kind -> module_functions

val is_module_function : module_functions -> string -> bool

val extended_type : AST_generic.definition_kind -> AST_generic.type_ option

val reopens :
  Lang.t -> AST_generic.entity -> AST_generic.definition_kind -> bool

type metatable_fact =
  | Metatable_set of {
      table : AST_generic.name;
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
