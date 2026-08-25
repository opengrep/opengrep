(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by Travis CI,
 * or default to Git_metadata.ml if unset.
 *
 * Translated from TravisMeta in meta.py.
 * See https://docs.travis-ci.com/user/environment-variables/
 *)

open Git_metadata

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ~cli_baseline_ref env =
  object (self)
    inherit
      Git_metadata.meta
        caps ~scan_environment:"travis-ci" ~cli_baseline_ref env as super

    method! project_metadata =
      let base = super#project_metadata in
      (* unlike pyopengrep, which overwrites the git-derived value even with
         an unset variable, the provider's value only when present *)
      let commit_title =
        match Opengrep_env.getenv_opt "TRAVIS_COMMIT_MESSAGE" with
        | Some _ as title -> title
        | None -> base.commit_title
      in
      { base with commit_title }

    method! repo_name =
      match override_or_getenv env._SEMGREP_REPO_NAME "TRAVIS_REPO_SLUG" with
      | Some name -> name
      | None -> super#repo_name

    method! repo_url =
      match env._SEMGREP_REPO_URL with
      | Some _ as url -> url
      | None ->
          Some (Uri.of_string (Fmt.str "https://github.com/%s" self#repo_name))

    method! branch =
      match
        override_or_getenv env._SEMGREP_BRANCH "TRAVIS_PULL_REQUEST_BRANCH"
      with
      | Some _ as branch -> branch
      | None -> Opengrep_env.getenv_opt "TRAVIS_BRANCH"

    method! ci_job_url =
      uri_override_or_getenv env._SEMGREP_JOB_URL "TRAVIS_JOB_WEB_URL"

    method! commit_sha =
      sha_override_or_getenv caps env._SEMGREP_COMMIT "TRAVIS_COMMIT"

    method! pr_id = override_or_getenv env._SEMGREP_PR_ID "TRAVIS_PULL_REQUEST"
  end
