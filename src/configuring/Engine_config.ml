(* Engine configuration for opengrep *)

(* Configuration type that can be passed around instead of using global refs *)
type t = {
  custom_ignore_pattern : string option;
  (* NOTE: taint_intrafile / taint_interfile[_depth] used to live here too
     but were never read — the engine reads them from Core_scan_config.
     Removed to avoid a second, dead source of truth. *)
}
[@@deriving show]

let default = {
  custom_ignore_pattern = None;
}

(* Get the list of patterns to use for ignoring lines *)
let get_ignore_patterns config : string list =
  let default_patterns = ["nosem"; "nosemgrep"; "noopengrep"] in
  match config.custom_ignore_pattern with
  | None -> default_patterns
  | Some pattern -> pattern :: default_patterns
