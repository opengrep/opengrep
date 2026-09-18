open Cohttp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers to perform http GET and POST requests.
 *
 * Below we separate the methods out by async (returns Lwt promise),
 * and sync (runs async method in lwt runtime)
 * This way we can use the async methods in the language server,
 * and other places too.
 *
 * Note that using [@@profiling] with xxx_async function is useless
 * as the actual computation is done in the caller doing the
 * Lwt_main.run
 *)

let src = Logs.Src.create "networking.http"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type body_result = (string, string) result

type server_response = {
  body : body_result;
  response : Cohttp.Response.t;
  code : int;
}

type client_result = (server_response, string) result

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* Create a client reference so we can swap it out with a testing version *)

(* TODO: Check if we need to adapt for OCaml 5. *)
let client_ref : (module Cohttp_lwt.S.Client) option ref = ref None
let in_mock_context = ref false
let set_client_ref v = if not !in_mock_context then client_ref := Some v

let with_client_ref v f x =
  let old = !client_ref in
  set_client_ref v;
  let result = f x in
  (match old with
  | Some old -> set_client_ref old
  | None -> client_ref := None);
  result

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_meth = Cohttp.Code.string_of_method

let server_response_of_response (response, body) meth =
  let code = response |> Response.status |> Code.code_of_status in
  let meth_str = string_of_meth meth in
  match code with
  | _ when Code.is_success code -> { body = Ok body; response; code }
  | _ when Code.is_error code ->
      Log.debug (fun m -> m "HTTP %s failed:\n %s" meth_str body);
      { body = Error body; response; code }
  (* This case is anything that is [Code.is_redirection] or [Code.is_informational]*)
  | _ ->
      Log.debug (fun m -> m "HTTP %s unexpected response:\n %s" meth_str body);
      { body = Error body; response; code }

let default_resp_handler (response, body) =
  let%lwt body_str = Cohttp_lwt.Body.to_string body in
  Lwt.return (response, body_str)

(* Why do we need a response_handler? From the cohttp docs: *)
(*
    [response_body] is not buffered, but stays on the wire until
        consumed. It must therefore be consumed in a timely manner.
        Otherwise the connection would stay open and a file descriptor leak
        may be caused. Following responses would get blocked.
        Functions in the {!Body} module can be used to consume [response_body]. *)
(* So if we don't handle the body, we can leak file descriptors and accidentally keep the connection open *)
(* Let's just handle the body when making the request then, so we don't risk leaving this up*)
(* to a consumer of this library, who may or may not know about this requirement *)
let call_client ?(body = Cohttp_lwt.Body.empty) ?(headers = [])
    ?(chunked = false) ?(resp_handler = default_resp_handler) meth url =
  let module Client =
    (val match !client_ref with
         | Some client -> client
         | None -> failwith "HTTP client not initialized")
  in
  let headers = Header.of_list headers in
  try%lwt
    let%lwt response = Client.call ~headers ~body ~chunked meth url in
    let%lwt result = resp_handler response in
    Lwt.return_ok result
  with
  | Cohttp_lwt.Connection.Retry ->
      Lwt.return_error "Error in request: maybe the server hung up prematurely?"
  | exn ->
      let err = Printexc.to_string exn in
      Log.err (fun m ->
          m "HTTP %s to '%s' failed: %s" (string_of_meth meth)
            (Uri.to_string url) err);
      Lwt.return_error err

(*****************************************************************************)
(* Async *)
(*****************************************************************************)
let rec get ?(headers = []) ?(redirects = 5) caps url =
  Log.info (fun m -> m "GET on %s" (Uri.to_string url));
  (* This checks to make sure a client has been set *)
  (* Instead of defaulting to a client, as that can cause *)
  (* Hard to debug build and runtime issues *)
  let response_result = call_client ~headers `GET url in
  let handle_response (response, body) =
    let server_response = server_response_of_response (response, body) `GET in
    match server_response.code with
    (* Automatically resolve redirects, in this case a 307 Temporary Redirect.
       This is important for installing the Semgrep Pro Engine binary, which
       receives a temporary redirect at the proper endpoint.
    *)
    | 301
    | 302
    | 307
    | 308 -> (
        let location = Header.get (response |> Response.headers) "location" in
        match location with
        | None ->
            let code_str = Code.string_of_status response.status in
            let err = "HTTP GET failed: " ^ code_str ^ ":\n" ^ body in
            Log.err (fun m -> m "%s" err);
            let server_response = { server_response with body = Error err } in
            Lwt.return_ok server_response
        | Some url when redirects > 0 ->
            get ~redirects:(redirects - 1) caps (Uri.of_string url)
        | Some url ->
            let err = "HTTP GET failed: too many redirects, last one to " ^ url in
            Log.err (fun m -> m "%s" err);
            Lwt.return_ok { server_response with body = Error err })
    | _ -> Lwt.return_ok server_response
  in
  Lwt_result.bind response_result handle_response
[@@profiling]

let post ~body ?(headers = [ ("content-type", "application/json") ])
    ?(chunked = false) _caps url =
  Log.info (fun m -> m "POST on %s" (Uri.to_string url));
  let response =
    call_client
      ~body:(Cohttp_lwt.Body.of_string body)
      ~headers ~chunked `POST url
  in
  Lwt_result.bind response (fun (response, body) ->
      Lwt.return_ok (server_response_of_response (response, body) `POST))
[@@profiling]
