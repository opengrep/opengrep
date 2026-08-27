(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by Buildkite,
 * or default to Git_metadata.ml if unset.
 *
 * Translated from BuildkiteMeta in meta.py.
 * See https://buildkite.com/docs/pipelines/environment-variables
 *)

open Git_metadata

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ?subdir ~cli_baseline_ref env =
  object (_self)
    inherit
      Git_metadata.meta
        caps ?subdir ~scan_environment:"buildkite" ~cli_baseline_ref env
        as super

    method! project_metadata =
      let base = super#project_metadata in
      (* unlike pyopengrep, which overwrites the git-derived values even
         with unset variables, the provider's values only when present *)
      let value_or (fallback : string option) (var : string) : string option =
        match Opengrep_env.getenv_opt var with
        | Some _ as v -> v
        | None -> fallback
      in
      {
        base with
        commit_author_email =
          value_or base.commit_author_email "BUILDKITE_BUILD_AUTHOR_EMAIL";
        commit_author_name =
          value_or base.commit_author_name "BUILDKITE_BUILD_AUTHOR";
        commit_title = value_or base.commit_title "BUILDKITE_MESSAGE";
      }

    method! repo_name =
      match env._SEMGREP_REPO_NAME with
      | Some name -> name
      | None -> (
          match
            Project_metadata.get_repo_name_from_repo_url
              (Opengrep_env.getenv_opt "BUILDKITE_REPO")
          with
          | Some name -> name
          | None -> super#repo_name)

    method! repo_url =
      match env._SEMGREP_REPO_URL with
      | Some _ as url -> url
      | None -> (
          match
            Project_metadata.get_url_from_sstp_url
              (Opengrep_env.getenv_opt "BUILDKITE_REPO")
          with
          | Some _ as url -> url
          | None -> super#repo_url)

    method! branch = override_or_getenv env._SEMGREP_BRANCH "BUILDKITE_BRANCH"

    method! ci_job_url =
      match env._SEMGREP_JOB_URL with
      | Some _ as url -> url
      | None -> (
          match
            ( Opengrep_env.getenv_opt "BUILDKITE_BUILD_URL",
              Opengrep_env.getenv_opt "BUILDKITE_JOB_ID" )
          with
          | Some build_url, Some job_id ->
              Some (Uri.of_string (Fmt.str "%s#%s" build_url job_id))
          | _else_ -> None)

    method! commit_sha =
      sha_override_or_getenv caps env._SEMGREP_COMMIT "BUILDKITE_COMMIT"

    method! pr_id =
      match env._SEMGREP_PR_ID with
      | Some _ as id -> id
      | None -> (
          (* "false" when there is no PR *)
          match Opengrep_env.getenv_opt "BUILDKITE_PULL_REQUEST" with
          | Some "false" -> None
          | id -> id)
  end
