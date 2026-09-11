(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The skins --skin can select.
 *
 * The signature and the name live in osemgrep_core, low enough for the
 * drivers and the core runner to see them; the skins themselves live here,
 * where the rendering machinery is.
 *)

let default : Skin.name = Skin.Legacy

let resolve (name : Skin.name) : (module Skin.S) =
  match name with
  | Skin.Legacy -> (module Skin_legacy : Skin.S)
  | Skin.Simple -> (module Skin_simple : Skin.S)
  | Skin.Vivid -> (module Skin_vivid : Skin.S)
