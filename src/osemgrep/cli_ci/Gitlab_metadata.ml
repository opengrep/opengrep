(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by GitLab CI (10.0+),
 * or default to Git_metadata.ml if unset.
 *
 * Translated from GitlabMeta in meta.py.
 *
 * The provider CI_* variables are read directly with Opengrep_env.getenv_opt, like
 * pyopengrep reads them with os.getenv; only the user-facing SEMGREP_*
 * overrides go through the cmdliner terms of Git_metadata.env.
 * See https://docs.gitlab.com/ee/ci/variables/predefined_variables.html
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let missing_variable (var : string) ~(branch_name : string) : 'a =
  Error.abort
    (Fmt.str
       "%s is not set: the merge request target branch %s cannot be fetched \
        to find the baseline"
       var branch_name)

(* "Return merge base of current head and head commit in branch_name.
 *  Use Gitlab env vars to fetch target branch.
 *  By default gitlab pipelines do a shallow clone."
 *
 * The fetch authenticates with the job token; on git 2.31 or newer it is
 * passed as a header through the environment, on older git an inline
 * credential helper supplies it. Either way the command line carries the
 * plain url and no token. supports_config_env is the result of the
 * Git_wrapper probe, a parameter so tests can force the old-git path. *)
let fetch_branch_get_merge_base (caps : < Cap.exec >)
    ~(supports_config_env : bool) ~(branch_name : string)
    ~(head_sha : string) : Digestif.SHA1.t option =
  match
    (Opengrep_env.getenv_opt "CI_MERGE_REQUEST_PROJECT_URL", Opengrep_env.getenv_opt "CI_JOB_TOKEN")
  with
  | Some project_url, Some job_token ->
      let _ =
        if supports_config_env then
          (* the credentials go to this one child process as an
           * Authorization header for the project url *)
          let header =
            "Authorization: Basic "
            ^ Base64.encode_string (Fmt.str "gitlab-ci-token:%s" job_token)
          in
          Git_wrapper.command_with_config caps
            ~config:[ (Fmt.str "http.%s.extraHeader" project_url, header) ]
            [ "fetch"; project_url; branch_name ]
        else
          (* older git ignores the GIT_CONFIG_* variables: an inline
           * credential helper supplies the token when git asks for
           * credentials. The command line carries only the variable
           * name; the helper's shell reads the value from the
           * environment *)
          let helper =
            "credential.helper=!f() { echo username=gitlab-ci-token; echo \
             password=$CI_JOB_TOKEN; }; f"
          in
          Git_wrapper.command caps
            [ "-c"; helper; "fetch"; project_url; branch_name ]
      in
      let out =
        Git_wrapper.command caps [ "merge-base"; "--all"; head_sha; "FETCH_HEAD" ]
      in
      (* 'merge-base --all' can print several commits, one per line;
       * pyopengrep passes its raw output along, we take the first *)
      (match String.split_on_char '\n' out with
      | first :: _ -> Digestif.SHA1.consistent_of_hex_opt (String.trim first)
      | [] -> None)
  (* without the url or the token there is no baseline: stop rather than
   * silently scan the whole repository *)
  | None, _ -> missing_variable "CI_MERGE_REQUEST_PROJECT_URL" ~branch_name
  | Some _, None -> missing_variable "CI_JOB_TOKEN" ~branch_name

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ?subdir ~cli_baseline_ref env =
  (* like pyopengrep GitlabMeta.merge_base_ref (a cachedproperty, hence the
     lazy): --baseline-commit wins; in merge-request context the base is
     the merge-base with the target branch *)
  let merge_base_ref =
    lazy
      (match cli_baseline_ref with
      | Some rev -> Some (Find_targets.Rev rev)
      | None -> (
          match Opengrep_env.getenv_opt "CI_MERGE_REQUEST_TARGET_BRANCH_NAME" with
          | None -> None
          | Some target_branch ->
              let head_sha = Git_wrapper.command caps [ "rev-parse"; "HEAD" ] in
              fetch_branch_get_merge_base caps
                ~supports_config_env:(Git_wrapper.supports_config_env caps)
                ~branch_name:target_branch ~head_sha
              |> Option.map (fun sha -> Find_targets.Commit sha)))
  in
  object (self)
    inherit
      Git_metadata.meta
        caps ?subdir ~scan_environment:"gitlab-ci" ~cli_baseline_ref env
        as super

    method private commit_ref = Opengrep_env.getenv_opt "CI_COMMIT_REF_NAME"
    method private start_sha = Opengrep_env.getenv_opt "CI_MERGE_REQUEST_DIFF_BASE_SHA"

    method! project_metadata =
      let base_sha =
        match self#merge_base_ref with
        | Some (Find_targets.Commit sha) -> Some sha
        | Some (Find_targets.Rev rev) ->
            Git_metadata.resolve_commit_ref caps ~origin:"the baseline rev"
              (Git_metadata.Commit_rev rev)
        | Some (Find_targets.Merge_base_of rev) -> (
            let cmd = (Cmd.Name "git", [ "merge-base"; rev; "HEAD" ]) in
            match CapExec.string_of_run caps#exec ~trim:true cmd with
            | Ok (str, (_, `Exited 0)) ->
                Digestif.SHA1.consistent_of_hex_opt str
            | Ok _
            | Error (`Msg _) ->
                Logs.warn (fun m ->
                    m
                      "no merge base with the baseline rev, leaving out \
                       base_sha: %s"
                      rev);
                None)
        | None -> None
      in
      {
        (super#project_metadata) with
        branch = self#commit_ref;
        base_sha;
        start_sha = Option.bind self#start_sha Digestif.SHA1.consistent_of_hex_opt;
      }

    method! repo_name =
      (* super would shell out to git, so it stays the last resort *)
      match
        Git_metadata.override_or_getenv env._SEMGREP_REPO_NAME
          "CI_PROJECT_PATH"
      with
      | Some name -> name
      | None -> super#repo_name

    method! repo_url =
      match
        Git_metadata.uri_override_or_getenv env._SEMGREP_REPO_URL
          "CI_PROJECT_URL"
      with
      | Some _ as url -> url
      | None -> super#repo_url

    method! commit_sha =
      match
        Git_metadata.sha_override_or_getenv caps env._SEMGREP_COMMIT
          "CI_COMMIT_SHA"
      with
      | Some _ as sha -> sha
      | None -> super#commit_sha

    method! ci_job_url =
      match
        Git_metadata.uri_override_or_getenv env._SEMGREP_JOB_URL "CI_JOB_URL"
      with
      | Some _ as url -> url
      | None -> super#ci_job_url

    method! event_name =
      match Opengrep_env.getenv_opt "CI_PIPELINE_SOURCE" with
      | Some ("merge_request_event" | "external_pull_request_event") ->
          "pull_request"
      | Some source -> source
      | None -> "unknown"

    method! pr_id =
      match super#pr_id with
      | Some _ as value -> value
      | None -> Opengrep_env.getenv_opt "CI_MERGE_REQUEST_IID"

    method! pr_title =
      match super#pr_title with
      | Some _ as value -> value
      | None -> Opengrep_env.getenv_opt "CI_MERGE_REQUEST_TITLE"

    method! merge_base_ref = Lazy.force merge_base_ref
  end
