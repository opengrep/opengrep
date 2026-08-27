(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by Jenkins,
 * or default to Git_metadata.ml if unset.
 *
 * Translated from JenkinsMeta in meta.py.
 * See https://e.printstacktrace.blog/jenkins-pipeline-environment-variables-the-definitive-guide/
 *)

open Git_metadata

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ?subdir ~cli_baseline_ref env =
  object (_self)
    inherit
      Git_metadata.meta
        caps ?subdir ~scan_environment:"jenkins" ~cli_baseline_ref env
        as super

    method! repo_name =
      (* assumes the git url is in the github format *)
      match env._SEMGREP_REPO_NAME with
      | Some name -> name
      | None -> (
          match
            Project_metadata.get_repo_name_from_repo_url
              (Opengrep_env.getenv_opt "GIT_URL")
          with
          | Some name -> name
          | None -> super#repo_name)

    method! repo_url =
      match env._SEMGREP_REPO_URL with
      | Some _ as url -> url
      | None -> (
          let git_url =
            match Opengrep_env.getenv_opt "GIT_URL" with
            | Some _ as url -> url
            | None -> Opengrep_env.getenv_opt "GIT_URL_1"
          in
          match Project_metadata.get_url_from_sstp_url git_url with
          | Some _ as url -> url
          | None -> super#repo_url)

    method! branch =
      match env._SEMGREP_BRANCH with
      | Some _ as branch -> branch
      | None -> (
          match Opengrep_env.getenv_opt "GIT_BRANCH" with
          | Some branch_or_tag
            when not (String_.contains branch_or_tag ~term:"tags/") ->
              Some branch_or_tag
          | _else_ -> None)

    method! ci_job_url = uri_override_or_getenv env._SEMGREP_JOB_URL "BUILD_URL"
    method! commit_sha =
      sha_override_or_getenv caps env._SEMGREP_COMMIT "GIT_COMMIT"
  end
