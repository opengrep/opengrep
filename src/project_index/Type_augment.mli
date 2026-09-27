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

type undeclared_function

val undeclared_functions :
  table_of_file:table_of_file ->
  Graph_from_AST.func_info list ->
  undeclared_function list

(* Return types inferred from [return EXPR] bodies; the flag is true when
   the state changed. *)
val augment_return_types_from_bodies :
  undeclared:undeclared_function list ->
  type_state:Type_state.t ->
  Type_state.t * bool

type calls_of_file

val calls_of_file :
  table_of_file:table_of_file ->
  funcs_by_file:(string, Graph_from_AST.func_info list) Hashtbl.t ->
  Types.file_info ->
  calls_of_file option

val caller_arg_types_of_file :
  lang:Lang.t ->
  type_state:Type_state.t ->
  memo:Class_table.memo ->
  calls_of_file ->
  (Function_id.t * int * Class_table.cls) list

(* (class, method, arg index) -> inferred argument type, from call sites. *)
val build_caller_arg_types :
  (Function_id.t * int * Class_table.cls) list list ->
  (Function_id.t * int, Class_table.cls) Hashtbl.t

(* Module-level singleton bindings typed from their initialisers. *)
val build_module_singleton_types :
  table_of_file:table_of_file ->
  Type_state.t ->
  Types.file_info list ->
  Type_state.t

type method_info

val method_infos :
  lang:Lang.t ->
  cfg:Index_lang_rules.t ->
  table_of_file:table_of_file ->
  Graph_from_AST.func_info list ->
  method_info list

(* Field types inferred from [this.X = RHS] assignments in method bodies;
   the flag is true when the state changed. *)
val augment_fields_from_self_assignments :
  caller_arg_types:(Function_id.t * int, Class_table.cls) Hashtbl.t ->
  methods:method_info list ->
  type_state:Type_state.t ->
  Type_state.t * bool

val add_value_type_annotations :
  lang:Lang.t ->
  table_of_file:table_of_file ->
  Type_state.t ->
  Graph_from_AST.func_info list ->
  Type_state.t

(* Stamp inferred variable classes onto [id_instance_type] across an AST. *)
val stamp_var_types_from_bodies :
  lang:Lang.t ->
  table:Symbol_table.t ->
  type_state:Type_state.t ->
  caller:Function_id.t option ->
  AST_generic.program ->
  unit
