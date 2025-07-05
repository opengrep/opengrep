type shared_secret = Uuidm.t
(** [shared_secret] is shared secret between the Semgrep App and the CLI.
  We use a UUID (a sufficiently random and therefore exceedingly unlikely for
  both accidental collisions and brute force attacks) as the shared secret.
  Depending on the user flow, the CLI will either generate a new UUID and
  ask the App DB to create and store a new access token keyed by that UUID,
  or the App will generate a UUID and corresponidng access token and ask the CLI
  to retrieve the access token for that UUID. In both cases, the CLI will have
  the responsibility of storing the access token in the user's settings file.
  *)

type login_session = shared_secret * Uri.t
(** [login_session] is a request token and request url tuple.*)

val is_logged_in_weak : unit -> bool
(** this does not really check whether the user is logged in; it just checks
 * whether a token is defined in ~/.semgrep/settings.yml (or in
 * SEMGREP_APP_TOKEN.
 *)
