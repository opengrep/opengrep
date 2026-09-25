type key = Class_table.cls

type t

val empty : t

val set_module_singleton : t -> Names.Module_qn.t -> key -> t

val get_module_singleton : t -> Names.Module_qn.t -> key option

val set_method_return : t -> key -> string -> key -> t

val method_return : t -> key -> string -> key option

val set_method_return_tuple : t -> key -> string -> key option list -> t

val method_return_tuple : t -> key -> string -> key option list option

val set_field : t -> key -> string -> key -> t

val field : t -> key -> string -> key option

val set_field_element : t -> key -> string -> key -> t

val field_element : t -> key -> string -> key option

val set_function_return : t -> Function_id.t -> key -> t

val function_return : t -> Function_id.t -> key option

val set_function_return_tuple : t -> Function_id.t -> key option list -> t

val function_return_tuple : t -> Function_id.t -> key option list option

val add_value_type_site : t -> AST_generic.SId.t -> t

val is_value_type : t -> AST_generic.type_ -> bool

val equal : t -> t -> bool
