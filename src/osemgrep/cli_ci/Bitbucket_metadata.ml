(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by Bitbucket,
 * or default to Git_metadata.ml if unset.
 *
 * Translated from BitbucketMeta in meta.py.
 * See https://support.atlassian.com/bitbucket-cloud/docs/variables-and-secrets/
 *)

open Git_metadata

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ~cli_baseline_ref env =
  object (_self)
    inherit
      Git_metadata.meta
        caps ~scan_environment:"bitbucket" ~cli_baseline_ref env as super

    method! repo_name =
      match env._SEMGREP_REPO_NAME with
      | Some name -> name
      | None -> (
          match Opengrep_env.getenv_opt "BITBUCKET_REPO_FULL_NAME" with
          | Some name -> Some name
          | None ->
              (* try pulling from url *)
              Project_metadata.get_repo_name_from_repo_url
                (Opengrep_env.getenv_opt "BITBUCKET_GIT_HTTP_ORIGIN")
          )
          |> (function
          | Some name -> name
          | None -> super#repo_name)

    (* Bitbucket Cloud URLs should be in the format:
         http://bitbucket.org/<workspace>/<repo>
       Bitbucket Server URLs should be in the format:
         https://bitbucket<company>.com/projects/<PROJECT>/repos/<REPO_NAME> *)
    method! repo_url =
      match
        uri_override_or_getenv env._SEMGREP_REPO_URL "BITBUCKET_GIT_HTTP_ORIGIN"
      with
      | Some _ as url -> url
      | None -> super#repo_url

    method! branch = override_or_getenv env._SEMGREP_BRANCH "BITBUCKET_BRANCH"

    method! ci_job_url =
      match env._SEMGREP_JOB_URL with
      | Some _ as url -> url
      | None -> (
          (* unlike pyopengrep, which interpolates the missing values as
             "None", no url at all when either variable is unset *)
          match
            ( Opengrep_env.getenv_opt "BITBUCKET_GIT_HTTP_ORIGIN",
              Opengrep_env.getenv_opt "BITBUCKET_PIPELINE_UUID" )
          with
          | Some origin, Some uuid ->
              Some
                (Uri.of_string
                   (Fmt.str "%s/addon/pipelines/home#!/results/%s" origin uuid))
          | _else_ -> None)

    method! commit_sha =
      sha_override_or_getenv env._SEMGREP_COMMIT "BITBUCKET_COMMIT"

    method! pr_id = override_or_getenv env._SEMGREP_PR_ID "BITBUCKET_PR_ID"
  end
