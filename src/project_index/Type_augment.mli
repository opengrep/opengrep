(* Project-wide type augmentation: extends the [Type_state.t] lattice with
   types derived from declarations and function bodies, for callee
   resolution to read. *)

(* Defining file of a function, preferring the entity token (reshaped defs
   carry a fake [fkind] but a real entity name token). *)
val func_def_file : Graph_from_AST.func_info -> string option

(* Declared return types: free-function, method (with this/self resolved to
   the enclosing class), and tuple returns. *)
val populate_returns_from_decls :
  Type_state.t -> Graph_from_AST.func_info list -> Type_state.t

(* Field types declared on classes; second result maps
   (class, field) -> element type for slice/array fields. *)
val build_fields_by_class_index :
  cfg:Index_lang_rules.t ->
  Type_state.t ->
  Types.file_info list ->
  Type_state.t * (string * string, AST_generic.name) Hashtbl.t

(* TS default/named export -> class name indexes. *)
val build_export_class_indexes :
  lang:Lang.t ->
  type_state:Type_state.t ->
  Types.file_info list ->
  (string, AST_generic.name) Hashtbl.t
  * (string * string, AST_generic.name) Hashtbl.t

(* Group functions by defining file. *)
val build_file_funcs_index :
  Graph_from_AST.func_info list ->
  (string, Graph_from_AST.func_info list) Hashtbl.t

(* Return types inferred from [return EXPR] bodies. *)
val augment_return_types_from_bodies :
  uses_new_keyword:bool ->
  type_state:Type_state.t ->
  Graph_from_AST.func_info list ->
  Type_state.t

(* (class, method, arg index) -> inferred argument type, from call sites. *)
val build_caller_arg_types :
  uses_new_keyword:bool ->
  type_state:Type_state.t ->
  Types.file_info list ->
  (string * string * int, AST_generic.name) Hashtbl.t

(* Module-level singleton bindings typed from their initialisers. *)
val build_module_singleton_types :
  uses_new_keyword:bool ->
  Type_state.t ->
  Types.file_info list ->
  Type_state.t

(* Field types inferred from [this.X = RHS] assignments in method bodies. *)
val augment_fields_from_self_assignments :
  lang:Lang.t ->
  uses_new_keyword:bool ->
  caller_arg_types:(string * string * int, AST_generic.name) Hashtbl.t ->
  cfg:Index_lang_rules.t ->
  type_state:Type_state.t ->
  Graph_from_AST.func_info list ->
  Type_state.t

(* Stamp inferred variable classes onto [id_type] across an AST. *)
val stamp_var_types_from_bodies :
  uses_new_keyword:bool ->
  type_state:Type_state.t ->
  slice_element_of_field:(string * string, AST_generic.name) Hashtbl.t ->
  AST_generic.program ->
  unit
