(* Project-wide type augmentation: extends the [Type_state.t] lattice with
   types derived from declarations and function bodies, for callee
   resolution to read. *)

type table_of_file = Fpath.t -> Symbol_table.t option

(* Defining file of a function, preferring the entity token (reshaped defs
   carry a fake [fkind] but a real entity name token). *)
val func_def_file : Graph_from_AST.func_info -> string option

(* Declared return types: free-function, method (with this/self resolved to
   the enclosing class), and tuple returns. *)
val populate_returns_from_decls :
  table_of_file:table_of_file ->
  Type_state.t -> Graph_from_AST.func_info list -> Type_state.t

(* Field types declared on classes; second result maps
   (class, field) -> element type for slice/array fields. *)
val build_fields_by_class_index :
  cfg:Index_lang_rules.t ->
  table_of_file:table_of_file ->
  Type_state.t ->
  Types.file_info list ->
  Type_state.t

(* Group functions by defining file. *)
val build_file_funcs_index :
  Graph_from_AST.func_info list ->
  (string, Graph_from_AST.func_info list) Hashtbl.t

(* Return types inferred from [return EXPR] bodies. *)
val augment_return_types_from_bodies :
  table_of_file:table_of_file ->
  type_state:Type_state.t ->
  Graph_from_AST.func_info list ->
  Type_state.t

(* (class, method, arg index) -> inferred argument type, from call sites. *)
val build_caller_arg_types :
  table_of_file:table_of_file ->
  type_state:Type_state.t ->
  funcs_by_file:(string, Graph_from_AST.func_info list) Hashtbl.t ->
  Types.file_info list ->
  (Function_id.t * int, Class_table.cls) Hashtbl.t

(* Module-level singleton bindings typed from their initialisers. *)
val build_module_singleton_types :
  table_of_file:table_of_file ->
  Type_state.t ->
  Types.file_info list ->
  Type_state.t

(* Field types inferred from [this.X = RHS] assignments in method bodies. *)
val augment_fields_from_self_assignments :
  lang:Lang.t ->
  caller_arg_types:(Function_id.t * int, Class_table.cls) Hashtbl.t ->
  cfg:Index_lang_rules.t ->
  table_of_file:table_of_file ->
  type_state:Type_state.t ->
  Graph_from_AST.func_info list ->
  Type_state.t

(* Stamp inferred variable classes onto [id_instance_type] across an AST. *)
val add_value_type_sites :
  lang:Lang.t ->
  table_of_file:table_of_file ->
  Type_state.t ->
  Graph_from_AST.func_info list ->
  Type_state.t

val stamp_var_types_from_bodies :
  table:Symbol_table.t ->
  type_state:Type_state.t ->
  caller:Function_id.t option ->
  AST_generic.program ->
  unit
