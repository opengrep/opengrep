(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* Commentary *)
(* Handles all notifications from the client. There are none that are handled *)
(* when the server is uninitialized. Custom notifications should be setup here. *)
(* Notifications never return a response, but may trigger server to client *)
(* notifications or requests *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
open Lsp
open Lsp_
open Types
open Jsonrpc
open Yojson.Safe.Util
module CN = Client_notification
module Conv = Convert_utils

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* Dispatch to the various custom request handlers. *)
let handle_custom_notification (meth : string) : Reply.t option =
  Logs.warn (fun m -> m "Unhandled custom notification %s" meth);
  None

let on_notification (server : RPC_server.t) notification =
  Logs.debug (fun m ->
      m "Handling notification %s"
        (CN.to_jsonrpc notification |> Notification.yojson_of_t
       |> Yojson.Safe.pretty_to_string));
  let session = server.session in
  let server, reply_opt =
    match notification with
    | _ when server.state = State.Uninitialized ->
        Logs.warn (fun m -> m "Server is uninitialized");
        (server, None)
    | CN.Initialized ->
        let reply = Scan_helpers.refresh_rules session
        in
        let session = Session.load_local_skipped_fingerprints session in
        let server = { server with session } in
        (server, Some reply)
    | CN.DidSaveTextDocument { textDocument = { uri }; _ } ->
        Logs.debug (fun m -> m "Scanning file %s on save" (Uri.to_string uri));
        (server, Some (Scan_helpers.scan_file session uri))
    | CN.TextDocumentDidClose { textDocument = { uri; _ } } ->
        let path = uri |> Uri.to_path |> Fpath.v in
        ( server,
          Some
            (Reply.later (fun _ -> Session.remove_open_document session path))
        )
    | CN.TextDocumentDidChange
        { textDocument = { uri; _ }; contentChanges = first :: _ } ->
        (* TODO: remove diagnostics if edit is in range *)
        ignore first;
        ignore uri;
        (server, None)
    | CN.TextDocumentDidOpen { textDocument = { uri; _ } } ->
        let path = uri |> Uri.to_path |> Fpath.v in
        let reply =
          Reply.later (fun send ->
              let%lwt () =
                Reply.apply send (Scan_helpers.scan_file session uri)
              in
              Session.add_open_document session path)
        in
        (server, Some reply)
    | CN.ChangeWorkspaceFolders { event = { added; removed }; _ } ->
        let session =
          let added = Conv.workspace_folders_to_paths added in
          let removed = Conv.workspace_folders_to_paths removed in
          Session.update_workspace_folders session ~added ~removed
        in
        Session.cache_workspace_targets session;
        let server = { server with session } in
        (server, Some (Scan_helpers.scan_workspace session))
    (* If files are renamed or created, update our targets *)
    | CN.DidRenameFiles _
    | CN.DidCreateFiles _ ->
        Session.cache_workspace_targets session;
        (server, None)
    | CN.DidDeleteFiles { files = paths; _ } ->
        (* This is lame, for whatever reason they chose to type uri as string here, not Uri.t *)
        Session.cache_workspace_targets session;
        let paths =
          paths
          |> List_.map (fun { FileDelete.uri } ->
                 Str.string_after uri (String.length "file://") |> Fpath.v)
          (* Be careful! Because each file that DidDeleteFiles sends us might actually
             be a folder, we cannot just delete findings from those paths.
             We must check all files for which we have results, and check if they may be
             contained in the reported folder.
          *)
          |> List.concat_map (fun path ->
                 List.filter
                   (fun scanned_file -> Fpath.is_prefix path scanned_file)
                   (Session.scanned_files session))
        in
        let diagnostics =
          Diagnostics.diagnostics_of_results ~is_intellij:session.is_intellij []
            paths
        in
        ( server,
          Some
            (Reply.later (fun send ->
                 let%lwt () =
                   Lwt_list.iter_p send (Lsp_.batch_notify diagnostics)
                 in

                 Session.remove_open_documents session paths)) )
    | CN.Exit ->
        Logs.debug (fun m -> m "Server exiting");
        ({ server with state = State.Stopped }, None)
    | CN.UnknownNotification { method_ = "semgrep/refreshRules"; _ } ->
        (server, Some (Scan_helpers.refresh_rules session))
    | CN.UnknownNotification
        { method_ = "semgrep/scanWorkspace"; params = Some json } -> (
        match session.cached_session.initialized with
        | false ->
            ( server,
              Some
                (Reply.now
                   (notify_show_message ~kind:MessageType.Warning
                      "The Semgrep Extension is still loading rules. Please \
                       wait a moment and try again.")) )
        | true ->
            let full =
              Structured.yojson_of_t json
              |> member "full" |> to_bool_option
              |> Option.value ~default:false
            in
            let session =
              {
                session with
                user_settings =
                  { session.user_settings with only_git_dirty = not full };
              }
            in
            let reply =
              Reply.later (fun send ->
                  let%lwt () =
                    if full
                    then
                      send
                        (notify_show_message ~kind:MessageType.Info
                           "Scanning all files regardless of git status. These \
                            diagnostics will persist until a file is edited. \
                            To default to always scanning regardless of git \
                            status, please disable 'Only Git Dirty' in \
                            settings")
                    else Lwt.return_unit
                  in
                  Logs.debug (fun m -> m "Scanning workspace, full: %b" full);
                  let%lwt () =
                    Reply.apply send (Scan_helpers.scan_workspace session)
                  in
                  Logs.debug (fun m -> m "Scanning workspace complete");
                  Lwt.return_unit)
            in
            ({ server with session }, Some reply))
    | CN.UnknownNotification { method_; _ } ->
        (server, handle_custom_notification method_)
    | _ ->
        Logs.debug (fun m ->
            m "Unhandled notification %s"
              (CN.to_jsonrpc notification |> Notification.yojson_of_t
             |> Yojson.Safe.pretty_to_string));
        (* TODO: log this to the client *)
        (server, None)
  in
  (server, reply_opt)
