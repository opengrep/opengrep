(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Redacting secrets from strings that end up in logs or error messages. *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* the userinfo component of a URL (RFC 3986), e.g.
 * gitlab-ci-token:$CI_JOB_TOKEN spliced into a fetch URL as credentials *)
let url_userinfo_re = Pcre2_.regexp {|([A-Za-z][A-Za-z0-9+.-]*://)[^/@\s]+@|}

let redact_url_userinfo (str : string) : string =
  Pcre2_.replace ~rex:url_userinfo_re ~template:"$1***@" str

(* Add further redactions here. *)
let filters : (string -> string) list = [ redact_url_userinfo ]
let apply (str : string) : string = List.fold_left ( |> ) str filters
