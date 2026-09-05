(* Opengrep was forked from Semgrep, so many environment variables are named
 * named SEMGREP_* (e.g., SEMGREP_RULES). We now also honor the OPENGREP_*
 * equivalents (e.g., OPENGREP_RULES) for those variables, and give the
 * OPENGREP_* name precedence when both are set.
 *
 * The mapping is a plain textual substitution of the "SEMGREP" substring by
 * "OPENGREP", so "SEMGREP_URL" maps to "OPENGREP_URL" and
 * "PYTEST_SEMGREP_LOG_LEVEL" maps to "PYTEST_OPENGREP_LOG_LEVEL". Variables
 * without a "SEMGREP" substring (e.g., HOME, NO_COLOR) have no alias and are
 * looked up as-is.
 *)

let opengrep_alias (var : string) : string option =
  let alias = Str.global_replace (Str.regexp_string "SEMGREP") "OPENGREP" var in
  if String.equal alias var then None else Some alias

(* Like [Sys.getenv_opt] but treats an empty value as unset, matching the rest
 * of Opengrep's environment handling. *)
let getenv_nonempty (var : string) : string option =
  match USys.getenv_opt var with
  | Some "" -> None
  | x -> x

let getenv_with_name_opt (var : string) : (string * string) option =
  let named (var : string) : (string * string) option =
    getenv_nonempty var |> Option.map (fun (value : string) -> (var, value))
  in
  match opengrep_alias var with
  | Some alias -> (
      match named alias with
      | Some _ as v -> v
      | None -> named var)
  | None -> named var

let getenv_opt (var : string) : string option =
  getenv_with_name_opt var |> Option.map snd
