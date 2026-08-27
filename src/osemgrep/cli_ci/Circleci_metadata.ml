(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by Circle CI,
 * or default to Git_metadata.ml if unset.
 *
 * Translated from CircleCIMeta in meta.py.
 * See https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables
 *)

open Git_metadata

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ?subdir ~cli_baseline_ref env =
  object (_self)
    inherit
      Git_metadata.meta
        caps ?subdir ~scan_environment:"circleci" ~cli_baseline_ref env
        as super

    method! repo_name =
      match env._SEMGREP_REPO_NAME with
      | Some name -> name
      | None -> (
          match
            ( Opengrep_env.getenv_opt "CIRCLE_PROJECT_USERNAME",
              Opengrep_env.getenv_opt "CIRCLE_PROJECT_REPONAME" )
          with
          | Some project, Some repo -> Fmt.str "%s/%s" project repo
          | _else_ -> (
              match
                Project_metadata.get_repo_name_from_repo_url
                  (Opengrep_env.getenv_opt "CIRCLE_REPOSITORY_URL")
              with
              | Some name -> name
              | None -> super#repo_name))

    method! repo_url =
      match env._SEMGREP_REPO_URL with
      | Some _ as url -> url
      | None -> (
          (* may be in SSH url format *)
          match
            Project_metadata.get_url_from_sstp_url
              (Opengrep_env.getenv_opt "CIRCLE_REPOSITORY_URL")
          with
          | Some _ as url -> url
          | None -> super#repo_url)

    method! branch = override_or_getenv env._SEMGREP_BRANCH "CIRCLE_BRANCH"

    method! ci_job_url =
      uri_override_or_getenv env._SEMGREP_JOB_URL "CIRCLE_BUILD_URL"

    method! commit_sha =
      sha_override_or_getenv caps env._SEMGREP_COMMIT "CIRCLE_SHA1"

    method! pr_id =
      match env._SEMGREP_PR_ID with
      | Some _ as id -> id
      | None ->
          (* the pull request url's last nonempty segment is the id, so
           * the id is found even when the url has a trailing slash *)
          Option.bind
            (Opengrep_env.getenv_opt "CIRCLE_PULL_REQUEST")
            (fun url ->
              String.split_on_char '/' url
              |> List.rev
              |> List.find_opt (fun segment -> not (String.equal segment "")))
  end
