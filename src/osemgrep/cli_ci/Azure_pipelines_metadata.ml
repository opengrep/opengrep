(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by Azure Pipelines,
 * or default to Git_metadata.ml if unset.
 *
 * Translated from AzurePipelinesMeta in meta.py, which "pulled a lot from
 * https://github.com/DataDog/dd-trace-py/blob/f583fec63c4392a0784b4199b0e20931f9aae9b5/ddtrace/ext/ci.py"
 *)

open Git_metadata

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ?subdir ~cli_baseline_ref env =
  object (self)
    inherit
      Git_metadata.meta
        caps ?subdir ~scan_environment:"azure-pipelines" ~cli_baseline_ref env
        as super

    method! repo_name =
      match env._SEMGREP_REPO_NAME with
      | Some name -> name
      | None -> (
          match Project_metadata.get_repo_name_from_repo_url
                  (Option.map Uri.to_string self#repo_url)
          with
          | Some name -> name
          | None -> super#repo_name)

    method! repo_url =
      match env._SEMGREP_REPO_URL with
      | Some _ as url -> url
      | None -> (
          match
            ( Opengrep_env.getenv_opt "SYSTEM_PULLREQUEST_SOURCEREPOSITORYURI",
              Opengrep_env.getenv_opt "BUILD_REPOSITORY_URI" )
          with
          | Some url, _
          | None, Some url ->
              Some (Uri.of_string url)
          | None, None -> super#repo_url)

    method! branch =
      match env._SEMGREP_BRANCH with
      | Some _ as branch -> branch
      | None -> (
          let branch_or_tag =
            match
              ( Opengrep_env.getenv_opt "SYSTEM_PULLREQUEST_SOURCEBRANCH",
                Opengrep_env.getenv_opt "BUILD_SOURCEBRANCH",
                Opengrep_env.getenv_opt "BUILD_SOURCEBRANCHNAME" )
            with
            | Some b, _, _ -> Some b
            | None, Some b, _ -> Some b
            | None, None, b -> b
          in
          match branch_or_tag with
          | Some b when not (String_.contains b ~term:"tags/") -> Some b
          | _else_ -> None)

    method! ci_job_url =
      match env._SEMGREP_JOB_URL with
      | Some _ as url -> url
      | None -> (
          match
            ( Opengrep_env.getenv_opt "SYSTEM_TEAMFOUNDATIONSERVERURI",
              Opengrep_env.getenv_opt "SYSTEM_TEAMPROJECTID",
              Opengrep_env.getenv_opt "BUILD_BUILDID" )
          with
          | Some server_uri, Some project_id, Some build_id ->
              let base_url =
                Fmt.str "%s%s/_build/results?buildId=%s" server_uri project_id
                  build_id
              in
              (* unlike pyopengrep, which interpolates the missing values as
                 "None", the logs view suffix only when its ids are known *)
              let url =
                match
                  ( Opengrep_env.getenv_opt "SYSTEM_JOBID",
                    Opengrep_env.getenv_opt "SYSTEM_TASKINSTANCEID" )
                with
                | Some job_id, Some task_id ->
                    Fmt.str "%s&view=logs&j=%s&t=%s" base_url job_id task_id
                | _else_ -> base_url
              in
              Some (Uri.of_string url)
          | _else_ -> None)

    method! commit_sha =
      match
        sha_override_or_getenv caps env._SEMGREP_COMMIT
          "SYSTEM_PULLREQUEST_SOURCECOMMITID"
      with
      | Some _ as sha -> sha
      | None ->
          Option.bind
            (Opengrep_env.getenv_opt "BUILD_SOURCEVERSION")
            Digestif.SHA1.consistent_of_hex_opt

    method! pr_id =
      override_or_getenv env._SEMGREP_PR_ID "SYSTEM_PULLREQUEST_PULLREQUESTNUMBER"
  end
