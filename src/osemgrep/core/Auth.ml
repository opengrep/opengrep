(* A bearer token representing your identity on a code-hosting platform.
 * Currently only the GitHub token used by 'opengrep ci' to query the
 * GitHub API.
 *)
type token = Token of string

let string_of_token (Token str) = str

(* TODO: remove at some point and force to get first a semgrep capability *)

let unsafe_token_of_string str = Token str
