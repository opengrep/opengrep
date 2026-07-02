(* Canonical leaf-class extractors for [G.type_]. *)

module G = AST_generic

val leaf_of_name : G.name -> string option

(* The package qualifier of a name: [pkg] in [pkg.Foo]; [None] for bare [Id]. *)
val qualifier_of_name : G.name -> string option

(* Keeps the qualifier; strips pointer/ref but not generic application. *)
val qualified_class_name_of_ty : G.type_ -> G.name option

val class_name_of_ty : G.type_ -> G.name option

(* Like [class_name_of_ty] but also unwraps generics/pointer-ref;
   [~through_funty:true] walks function types to their return. *)
val inner_class_name_of_ty :
  ?through_funty:bool -> G.type_ -> G.name option
