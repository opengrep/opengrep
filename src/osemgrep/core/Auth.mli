(* A bearer token representing your identity on a code-hosting platform.
 * Currently only the GitHub token used by 'opengrep ci' to query the
 * GitHub API.
 *)
type token

(* to be used in headers *)
val string_of_token : token -> string

(* TODO: should require a semgrep_dev capa or semgrep_settings capa *)
val unsafe_token_of_string : string -> token
