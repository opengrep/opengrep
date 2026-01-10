(* Engine configuration for opengrep *)

(* Configuration type that can be passed around instead of using global refs *)
type t = {
  custom_ignore_pattern : string option;
  taint_intrafile : bool option;
  (* Add other configuration fields here as more global refs are migrated *)
}
[@@deriving show]

(* Default configuration with no custom ignore pattern *)
let default = {
  custom_ignore_pattern = None;
  taint_intrafile = None;
}

(* Get the list of patterns to use for ignoring lines *)
let get_ignore_patterns config : string list =
  let default_patterns = ["nosem"; "nosemgrep"; "noopengrep"] in
  match config.custom_ignore_pattern with
  | None -> default_patterns
  | Some pattern -> pattern :: default_patterns
