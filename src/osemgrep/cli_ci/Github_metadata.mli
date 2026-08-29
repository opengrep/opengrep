(** Gather metadata from GitHub Actions. *)

type env = {
  _GITHUB_EVENT_JSON : Yojson.Basic.t;
  _GITHUB_REPOSITORY : string option;
  _GITHUB_REPOSITORY_ID : string option;
  _GITHUB_REPOSITORY_OWNER_ID : string option;
  _GITHUB_API_URL : Uri.t option;
  _GITHUB_SERVER_URL : Uri.t;
  _GITHUB_SHA : Digestif.SHA1.t option;
  _GITHUB_REF : string option;
  _GITHUB_HEAD_REF : string option;
  _GITHUB_RUN_ID : string option;
  _GITHUB_EVENT_NAME : string option;
  _GH_TOKEN : string option;
}

(* Read via Opengrep_env (empty values count as unset). Aborts when
 * GITHUB_EVENT_PATH names a missing or invalid JSON file. *)
val env_from_environment : unit -> env

(* network is for the merge-base shortcut through the GitHub API *)
class meta :
  < Cap.exec ; Cap.network > ->
  ?subdir:string ->
  cli_baseline_ref:string option ->
  Git_metadata.env ->
  env ->
  Git_metadata.meta_t
