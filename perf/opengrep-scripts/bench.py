#! /usr/bin/env python3

import os
import subprocess
import sys
import time
import csv
from urllib.parse import urlparse
from datetime import datetime
import statistics
import resource

num_cpus = str(max(1, os.cpu_count() - 1))
changes_while_running = False

# SETUP
# (TODO: make some of these cli options?)

repositories = [ ("https://github.com/jellyfin/jellyfin","a0931baa8eb879898f4bc4049176ed3bdb4d80d1")
               , ("https://github.com/grafana/grafana", "afcb55156260fe1887c4731e6cc4c155cc8281a2")
               , ("https://gitlab.com/gitlab-org/gitlab", "915627de697e2dd71fe8205853de51ad3794f3ac")
               , ("https://github.com/Netflix/lemur", "28b9a73a83d350b1c7ab71fdd739d64eec5d06aa")
               , ("https://github.com/pythongosssss/ComfyUI-Custom-Scripts","943e5cc7526c601600150867a80a02ab008415e7")
               , ("https://github.com/pmd/pmd", "81739da5caff948dbcd2136c17532b65c726c781")
               , ("https://github.com/square/leakcanary", "bf5086da26952e3627f18865bb232963e4d019c5")
               ]

def regular_cmd(repo):
    return ["opengrep", "scan",
            "-c", "rules",
            repo,
            "-j", num_cpus,
            "--timeout", "0",
            "--max-memory", "4000",
            "--max-target-bytes", "200000",
            "--quiet"]

# Not used ATM
def _python_cmd(repo):
    return ["pipenv", "run", "opengrep",
            "scan", "-c", "rules", repo, "-j", num_cpus]

repeat_each_test_n_times = 5

# Implementation

ts = datetime.now().strftime('%Y%m%d-%H%M%S')

def log_to_file(msg):
    with open(f"results/log-{ts}.txt", 'a') as file:
        file.write(msg)

def show_num(n):
    return f"{n:.2f}"

def get_repo_name(repo_url):
    path = urlparse(repo_url).path
    repo_name = os.path.splitext(os.path.basename(path))[0]
    return repo_name

repos = [{"url": url, "sha": sha, "name": get_repo_name(url)}
         for (url, sha) in repositories]

def run(cmd, cwd=None):
    # my_env = os.environ.copy()
    # my_env["PIPENV_PIPFILE"] = "../opengrep/cli/Pipfile"
    print(f"Running: {' '.join(cmd)}")
    sys.stdout.flush()
    subprocess.run(cmd, cwd=cwd, check=True)

def clone_specific_commit(repo_url, commit_hash, name):
    if os.path.exists(name):
        print(f"Repository '{name}' present.")

    else:
        print(f"Cloning {repo_url} into {name} (shallow)...")
        run(["git", "clone", "--no-checkout", "--depth", "1", repo_url, name])

        print(f"Fetching commit {commit_hash}...")
        run(["git", "fetch", "--depth", "1", "origin", commit_hash], cwd=name)

        print(f"Checking out commit {commit_hash}...")
        run(["git", "checkout", commit_hash], cwd=name)

        print(f"Done: {name} at commit {commit_hash}")

def setup():
    os.makedirs("results", exist_ok=True)

    clone_specific_commit("https://github.com/opengrep/opengrep-rules", "f1d2b562b414783763fd02a6ed2736eaed622efa", "rules")

    run(["rm", "-f", "stats/web_frameworks.yml"], cwd="rules")
    run(["rm", "-f", "stats/cwe_to_metacategory.yml"], cwd="rules")
    run(["rm", "-f", "stats/metacategory_to_support_tier.yml"], cwd="rules")
    run(["rm", "-f", ".github/stale.yml"], cwd="rules")
    run(["rm", "-f", ".github/workflows/semgrep-rule-lints.yaml"], cwd="rules")
    run(["rm", "-f", ".github/workflows/validate-registry-metadata.yaml"], cwd="rules")
    run(["rm", "-f", ".github/workflows/semgrep-rules-test.yml"], cwd="rules")
    run(["rm", "-f", ".github/workflows/pre-commit.yml"], cwd="rules")
    run(["rm", "-f", "template.yaml"], cwd="rules")
    run(["rm", "-f", ".pre-commit-config.yaml"], cwd="rules")

    for r in repos:
        clone_specific_commit(r["url"], r["sha"], "repos/" + r["name"])

def single_run(repo):
    #t1 = time.time()
    #run(regular_cmd(repo))
    #t2 = time.time()
    #return t2 - t1
    time.sleep(5)
    usage_start = resource.getrusage(resource.RUSAGE_CHILDREN)
    run(regular_cmd(repo))
    usage_end = resource.getrusage(resource.RUSAGE_CHILDREN)
    time.sleep(5)
    user_time = usage_end.ru_utime - usage_start.ru_utime
    system_time = usage_end.ru_stime - usage_start.ru_stime
    total_cpu_time = user_time + system_time
    log_to_file(f"- run completed: {repo}. user: {show_num(user_time)}, system: {show_num(system_time)}, total: {show_num(total_cpu_time)}\n")
    return total_cpu_time

def run_opengrep(repo):
    durs = [single_run(repo) for x in range(0, repeat_each_test_n_times)]
    #if repeat_each_test_n_times >= 5:
    #    durs.remove(max(durs))
    #    durs.remove(max(durs))
    return statistics.mean(durs)

def has_changes():
    result = subprocess.run(
        ["git", "status", "--porcelain", "--untracked-files", "no"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    return bool(result.stdout.strip())

def checkout_opengrep(sha):
    if has_changes():
        run(["git", "stash", "-m", f"'changes made while perf test {ts} was running'"])
        changes_while_running = True
    run(["git", "checkout", sha], cwd="../..")
    run(["git", "submodule", "update", "--init", "--recursive"], cwd="../..")

def make_opengrep():
    run(["make"], cwd="../..")

def run_bench():
    return [{"name": r["name"], "duration": run_opengrep("repos/" + r["name"])}
     for r in repos]

def combine_results(res1, res2):
    return [{"name": r1["name"], "d1": r1["duration"], "d2": r2["duration"]}
            for (r1, r2) in zip(res1, res2)]

def report_results(sha1, sha2, res1, res2):
    combined = combine_results(res1, res2)
    with_stats = [{"name": r["name"],
                   sha1: f"{r["d1"]:.2f}",
                   sha2: f"{r["d2"]:.2f}",
                   "diff(s)": f"{(r["d2"] - r["d1"]):.2f}",
                   "diff(%)": f"{(100 * (r["d2"] - r["d1"]) / r["d1"]):.2f}"}
                  for r in combined]
    # print to screen
    print("--------- BENCHMARK RUN COMPLETED ---------\n")
    for e in with_stats:
        print(e)
    # save to csv
    print("\nsaving to csv...")
    with open(f"results/results-{ts}.csv", 'w', newline='') as csvfile:
        fieldnames = with_stats[0].keys()
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(with_stats)

def go():
    setup()
    log_to_file(f"PERF TEST {ts}\n\n")

    if len(sys.argv) < 2:
        print("Usage: bench.py <sha1>")
        print("Usage: bench.py <sha1> <sha2>")
        sys.exit(1)

    currentBranch = subprocess.check_output(['git', 'rev-parse', '--abbrev-ref', 'HEAD']).decode('ascii').strip()
    sha1 = sys.argv[1]

    changes = has_changes()

    try:
        if changes:
            run(["git", "stash", "-m", f"'uncommitted changes for perf test {ts}'"])

        # run two arbitrary shas/branches
        if len(sys.argv) == 3:
            sha2 = sys.argv[2]
            print(f"Running {sha1} against {sha2}")
            checkout_opengrep(sha1)
            make_opengrep()
            log_to_file(f"Commit 1: {sha1}\n\n")
            res1 = run_bench()
            checkout_opengrep(sha2)
            make_opengrep()
            log_to_file(f"\nCommit 2: {sha2}\n\n")
            res2 = run_bench()

        # run a sha against the current state of the world
        else:
            run(["git", "checkout", "-b", "bench/test-" + ts])
            if changes:
                run(["git", "stash", "apply"])
                run(["git", "commit", "-am", f"'uncommitted changes for perf test {ts}'"])
            sha2 = subprocess.check_output(['git', 'rev-parse', 'HEAD']).decode('ascii').strip()
            make_opengrep()
            log_to_file(f"Commit 2: {sha2}\n\n")
            res2 = run_bench()
            checkout_opengrep(sha1)
            make_opengrep()
            log_to_file(f"Commit 1: {sha1}\n\n")
            res1 = run_bench()

    # restore the original state of the world
    finally:
        checkout_opengrep(currentBranch)
        if changes:
                run(["git", "stash", "pop"])

    report_results(sha1, sha2, res1, res2)

    if changes_while_running:
        print("WARNING!!! Changes while the test was running have been detected! (stored to stash)")

go()
