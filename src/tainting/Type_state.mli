type cls = Class_table.cls

type t

val empty : t

val set_module_singleton : t -> Names.Module_qn.t -> cls -> t

val get_module_singleton : t -> Names.Module_qn.t -> cls option

val set_method_return : t -> cls -> string -> cls -> t

val method_return : t -> cls -> string -> cls option

val set_method_return_tuple : t -> cls -> string -> cls option list -> t

val method_return_tuple : t -> cls -> string -> cls option list option

val set_field : t -> cls -> string -> cls -> t

val field : t -> cls -> string -> cls option

val set_field_element : t -> cls -> string -> cls -> t

val field_element : t -> cls -> string -> cls option

val set_function_return : t -> Function_id.t -> cls -> t

val function_return : t -> Function_id.t -> cls option

val set_function_return_tuple : t -> Function_id.t -> cls option list -> t

val function_return_tuple : t -> Function_id.t -> cls option list option

val add_value_type_annotation : t -> AST_generic.SId.t -> t

val is_value_type : t -> AST_generic.type_ -> bool

val equal : t -> t -> bool
