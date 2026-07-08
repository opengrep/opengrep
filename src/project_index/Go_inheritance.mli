(* Go post-pass: an interface embedding another (a [Spread] field in
   [TyRecordAnon]) gains the transitively-embedded interfaces' methods in its
   [Type_state] method set. No-op for non-Go. *)

val lift_embedded_interfaces :
  lang:Lang.t ->
  Types.file_info list -> Type_state.t -> Type_state.t
