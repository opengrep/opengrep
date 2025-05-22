(* Engine configuration for opengrep *)

(* Configuration type that can be passed around instead of using global refs *)
type t = {
  custom_ignore_pattern : string option;
  (* Add other configuration fields here as more global refs are migrated *)
}
[@@deriving show]

(* Default configuration with no custom ignore pattern *)
let default = {
  custom_ignore_pattern = None;
}

(* Default patterns for ignoring findings *)
let default_ignore_patterns = ["nosem"; "nosemgrep"]

(* Get the list of patterns to use for ignoring lines *)
let get_ignore_patterns config : string list =
  match config.custom_ignore_pattern with
  | None -> default_ignore_patterns
  | Some pattern -> [pattern]
