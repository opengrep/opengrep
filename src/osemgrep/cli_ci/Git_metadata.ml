open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata from the git repo, or from SEMGREP_XXX environment
 * variables if set.
 *
 * TODO? rename to Git_and_semgrep_metadata.ml ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  _SEMGREP_REPO_NAME : string option;
  _SEMGREP_REPO_DISPLAY_NAME : string option;
  _SEMGREP_REPO_URL : Uri.t option;
  _SEMGREP_COMMIT : Digestif.SHA1.t option;
  _SEMGREP_JOB_URL : Uri.t option;
  _SEMGREP_PR_ID : string option;
  _SEMGREP_PR_TITLE : string option;
  _SEMGREP_BRANCH : string option;
}

(*****************************************************************************)
(* Reading the environment *)
(*****************************************************************************)

let env_from_environment () : env =
  let get = Opengrep_env.getenv_opt in
  {
    _SEMGREP_REPO_NAME = get "SEMGREP_REPO_NAME";
    _SEMGREP_REPO_DISPLAY_NAME = get "SEMGREP_REPO_DISPLAY_NAME";
    _SEMGREP_REPO_URL = Option.map Uri.of_string (get "SEMGREP_REPO_URL");
    _SEMGREP_COMMIT =
      (match get "SEMGREP_COMMIT" with
      | None -> None
      | Some str -> (
          match Digestif.SHA1.of_hex_opt str with
          | Some _ as sha -> sha
          (* aborting rather than falling back to HEAD: the fallback would
             silently misattribute the findings to another commit *)
          | None ->
              Error.abort (spf "SEMGREP_COMMIT is not a full commit id: %s" str)));
    _SEMGREP_JOB_URL = Option.map Uri.of_string (get "SEMGREP_JOB_URL");
    _SEMGREP_PR_ID = get "SEMGREP_PR_ID";
    _SEMGREP_PR_TITLE = get "SEMGREP_PR_TITLE";
    _SEMGREP_BRANCH = get "SEMGREP_BRANCH";
  }

(*****************************************************************************)
(* Helpers for the provider subclasses *)
(*****************************************************************************)
(* The OPENGREP_*/SEMGREP_* override always wins over the provider's own
 * variable. The provider variables are read via Opengrep_env so that an
 * empty value counts as unset, like pyopengrep's os.getenv values are
 * falsy when empty. *)

let override_or_getenv (override : string option) (var : string) :
    string option =
  match override with
  | Some _ as v -> v
  | None -> Opengrep_env.getenv_opt var

let uri_override_or_getenv (override : Uri.t option) (var : string) :
    Uri.t option =
  match override with
  | Some _ as v -> v
  | None -> Option.map Uri.of_string (Opengrep_env.getenv_opt var)

let sha_override_or_getenv (override : Digestif.SHA1.t option) (var : string) :
    Digestif.SHA1.t option =
  match override with
  | Some _ as v -> v
  | None -> (
      match Opengrep_env.getenv_opt var with
      | None -> None
      | Some str -> (
          match Digestif.SHA1.of_hex_opt str with
          | Some _ as sha -> sha
          | None ->
              Logs.warn (fun m ->
                  m "%s is not a full commit id, ignoring it: %s" var str);
              None))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* the surface shared by all the provider metadata classes *)
class type meta_t = object
  method project_metadata : Project_metadata.t
  method branch : string option
  method ci_job_url : Uri.t option
  method commit_sha : Digestif.SHA1.t option
  method event_name : string
  method is_full_scan : bool
  method pr_id : string option
  method pr_title : string option
  method repo_name : string
  method repo_display_name : string
  method repo_url : Uri.t option
  method merge_base_ref : Find_targets.baseline_ref option
  method is_pull_request_event : bool
  method head_branch_hash : Digestif.SHA1.t option
end

(* cli_baseline_ref is the raw --baseline-commit rev, any git rev like
 * pyopengrep's cli_baseline_ref (not necessarily a commit id) *)
class meta (caps : < Cap.exec >) ~scan_environment
  ~(cli_baseline_ref : string option) env =
  object (self)
    method project_metadata : Project_metadata.t =
      let commit_title : string =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%B" ]
      in
      let commit_author_email : Emile.mailbox =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%ae" ]
        |> Emile.of_string |> Result.get_ok
      in
      let commit_author_name : string =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%an" ]
      in
      (* Returns strict ISO 8601 time as str of head commit *)
      let commit_timestamp : Timedesc.Timestamp.t =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%cI" ]
        |> Timedesc.Timestamp.of_iso8601 |> Result.get_ok
      in
      {
        semgrep_version = Version.version;
        (* REQUIRED for semgrep backed *)
        repository = self#repo_name;
        (* OPTIONAL for semgrep backed *)
        repo_url = self#repo_url;
        repo_display_name = Some self#repo_display_name;
        branch = self#branch;
        ci_job_url = self#ci_job_url;
        commit = self#commit_sha;
        commit_author_email = Some (Emile.to_string commit_author_email);
        commit_author_name = Some commit_author_name;
        commit_author_username = None;
        commit_author_image_url = None;
        commit_title = Some commit_title;
        commit_timestamp = Some commit_timestamp;
        on = self#event_name;
        pull_request_author_username = None;
        pull_request_author_image_url = None;
        pull_request_id = self#pr_id;
        pull_request_title = self#pr_title;
        scan_environment;
        is_full_scan = self#is_full_scan;
        repo_id = None;
        org_id = None;
        (* TODO ugly: gitlab stuff, should maybe split
         * semgrep_output_v1.metadata and use inherit
         *)
        base_sha = None;
        start_sha = None;
        is_sca_scan = None;
        is_code_scan = None;
        is_secrets_scan = None;
      }

    (* to be overriden in children *)
    method repo_name =
      match env._SEMGREP_REPO_NAME with
      | Some repo_name -> repo_name
      | None ->
          let str =
            Git_wrapper.command caps [ "rev-parse"; "--show-toplevel" ]
          in
          Printf.sprintf "local_scan/%s" (Fpath.basename (Fpath.v str))

    method repo_display_name =
      match env._SEMGREP_REPO_DISPLAY_NAME with
      | Some repo_display_name -> repo_display_name
      | None -> self#repo_name

    method repo_url =
      match env._SEMGREP_REPO_URL with
      | Some repo_url -> Some repo_url
      | None -> (
          let cmd = (Cmd.Name "git", [ "remote"; "get-url"; "origin" ]) in
          match CapExec.string_of_run caps#exec ~trim:true cmd with
          | Ok (str, _status) ->
              Project_metadata.get_url_from_sstp_url (Some str)
          | Error (`Msg _err) ->
              Logs.warn (fun m ->
                  m
                    "Unable to infer repo_url. Set SEMGREP_REPO_URL \
                     environment variable or run in a valid git project with \
                     remote origin defined.");
              None)

    method branch =
      match env._SEMGREP_BRANCH with
      | Some branch -> Some branch
      | None -> (
          let cmd = (Cmd.Name "git", [ "rev-parse"; "--abbrev-ref"; "HEAD" ]) in
          match CapExec.string_of_run caps#exec ~trim:true cmd with
          | Ok (branch, (_, `Exited 0)) -> Some branch
          | Ok _
          | Error (`Msg _) ->
              None)

    method ci_job_url = env._SEMGREP_JOB_URL

    method commit_sha =
      match env._SEMGREP_COMMIT with
      | Some sha1 -> Some sha1
      | None -> (
          let cmd = (Cmd.Name "git", [ "rev-parse"; "HEAD" ]) in
          match CapExec.string_of_run caps#exec ~trim:true cmd with
          | Ok (str, (_, `Exited 0)) -> Digestif.SHA1.of_hex_opt str
          | Ok _
          | Error (`Msg _) ->
              None)

    method event_name =
      match self#pr_id with
      | Some _ -> "pull_request"
      | None -> "unknown"

    method pr_id = env._SEMGREP_PR_ID
    method pr_title = env._SEMGREP_PR_TITLE
    method is_full_scan = self#merge_base_ref =*= None

    (* both are only meaningful on GitHub Actions (overridden there); they
     * let 'opengrep ci' fix up the checked-out head without the isinstance
     * test pyopengrep's fix_head_if_github_action does *)
    method is_pull_request_event : bool = false
    method head_branch_hash : Digestif.SHA1.t option = None

    (* TODO? get rid of? use directly baseline_ref in is_full_scan? *)
    method merge_base_ref : Find_targets.baseline_ref option =
      (* the flag names the base itself, not something to compute a
       * merge-base from (python: ci passes is_mergebase=True) *)
      Option.map (fun rev -> Find_targets.Rev rev) cli_baseline_ref
  end
