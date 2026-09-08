(* The implicit receiver of a method: the definition parameter that no
   call argument fills, and the names a body calls it by. One rule for
   every site that compares a call to a definition or reads a receiver. *)

val is_method : AST_generic.function_definition -> bool
(** The definition sits directly in a class body. A function nested in a
    method is under the class but is not a method. *)

val is_static : AST_generic.entity option -> bool
(** Declared static ([@staticmethod] and the like): no receiver. *)

val implicit_param :
  Lang.t ->
  is_method:bool ->
  is_static:bool ->
  is_first:bool ->
  AST_generic.parameter ->
  bool
(** A Go or Rust [ParamReceiver]; in Python, the first parameter of an
    instance or class method whatever its name. Other languages pass the
    receiver outside the parameter list. *)

val arity :
  Lang.t ->
  is_method:bool ->
  is_static:bool ->
  AST_generic.parameter list ->
  int
(** The parameters a call fills: all of them but the implicit receiver. *)

val self_names : Lang.t -> string list
(** The names a body calls its receiver by; none for Go, whose receiver
    is named by each method. *)

val is_self_name : Lang.t -> string -> bool
