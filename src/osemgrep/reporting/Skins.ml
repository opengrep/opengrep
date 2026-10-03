(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The skins that --skin selects. The signature and Skin.name are in
 * osemgrep_core, which the drivers and the core runner depend on; the skins
 * are here, with the rendering code.
 *)

let resolve (name : Skin.name) : (module Skin.S) =
  match name with
  | Skin.Legacy -> (module Skin_legacy : Skin.S)
  | Skin.Simple -> (module Skin_simple : Skin.S)
  | Skin.Vivid -> (module Skin_vivid : Skin.S)
