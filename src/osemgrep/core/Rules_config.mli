(* ex: "p/python" *)
type config_string = string [@@deriving show]

(* config_string in a parsed form *)
type t =
  (* ex: 'foo.yaml' *)
  | File of Fpath.t
  (* ex: 'myrules/' (will go also recursively in subdirs of myrules) *)
  | Dir of Fpath.t
  (* ex: 'https://raw.githubusercontent.com/r2c/semgrep-rules/template.yaml' *)
  | URL of Uri.t
  (* ex: 'git+https://github.com/org/rules', 'git+ssh://git@host/org/rules#v1'
   * A whole remote git repository of rules, cloned locally then loaded
   * like a Dir. *)
  | Git of git_config
  | R of registry_config_kind

(* the config string after the 'git+' prefix: the clone URL and an optional
 * branch/tag given as a '#'-fragment (e.g. '...#v1.2.0'). *)
and git_config = {
  url : Uri.t;
  ref_ : string option;
}

and registry_config_kind =
  (* r/... *)
  | Registry of string
  (* p/... *)
  | Pack of string
  (* s/... *)
  | Snippet of string
  (* shortcuts *)
  (* "p/default" *)
  | Auto
  (* p/r2c *)
  | R2c
[@@deriving show]

(* the in_docker parameter is useful just for better error reporting *)
val parse_config_string : in_docker:bool -> config_string -> t
