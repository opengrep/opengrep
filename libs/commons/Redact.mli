(* Redacting secrets from strings that end up in logs or error messages. *)

(* Replace the userinfo component of any URL in the string (RFC 3986,
 * e.g. credentials spliced into a fetch URL) with "***". *)
val redact_url_userinfo : string -> string

(* Apply every known redaction to the string. *)
val apply : string -> string
