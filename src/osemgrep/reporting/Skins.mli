(* the skin a scan uses when --skin is not given *)
val default : Skin.name

(* the module that renders the report under this name *)
val resolve : Skin.name -> (module Skin.S)
