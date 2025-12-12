#! /usr/bin/env python3

import argparse
import os
import subprocess
import sys
import time
import csv
from urllib.parse import urlparse
from datetime import datetime
import statistics
import resource
import hashlib


num_cpus = str(max(1, os.cpu_count() - 1))
changes_while_running = False

# SETUP

# git url, git commit, number of repetitions of the test for a given repo
repositories = [ ("https://github.com/jellyfin/jellyfin","a0931baa8eb879898f4bc4049176ed3bdb4d80d1", 5)
               , ("https://github.com/grafana/grafana", "afcb55156260fe1887c4731e6cc4c155cc8281a2", 1)
               , ("https://gitlab.com/gitlab-org/gitlab", "915627de697e2dd71fe8205853de51ad3794f3ac", 1)
               , ("https://github.com/Netflix/lemur", "28b9a73a83d350b1c7ab71fdd739d64eec5d06aa", 5)
               , ("https://github.com/pythongosssss/ComfyUI-Custom-Scripts","943e5cc7526c601600150867a80a02ab008415e7", 5)
               , ("https://github.com/pmd/pmd", "81739da5caff948dbcd2136c17532b65c726c781", 5)
               , ("https://github.com/square/leakcanary", "bf5086da26952e3627f18865bb232963e4d019c5", 5)
               ]

# Global config set by CLI args
config = {
    "rules": "rules",
    "target": None,
    "with_semgrep": False,
    "taint_intrafile": False,
    "reps": None,  # Override default reps if set
}

def regular_cmd(repo):
    return ["pipenv", "run", "opengrep", "scan",
            "-c", config["rules"],
            repo,
            "-j", num_cpus,
            "--timeout", "0",
            "--max-memory", "4000",
            "--max-target-bytes", "200000",
            "--quiet"]

def custom_cmd(rules, target, intrafile=False):
    cmd = ["pipenv", "run", "opengrep", "-f", rules, target, "--timeout", "9"]
    if intrafile:
        cmd.append("--taint-intrafile")
    return cmd

def semgrep_cmd(rules, target, pro_intrafile=False):
    cmd = ["semgrep", "-f", rules, target, "--timeout", "9"]
    if pro_intrafile:
        cmd.append("--pro-intrafile")
    return cmd

def is_semgrep_installed():
    """Check if semgrep is installed and available."""
    try:
        result = subprocess.run(
            ["semgrep", "--version"],
            capture_output=True,
            text=True
        )
        return result.returncode == 0
    except FileNotFoundError:
        return False

def get_opengrep_version():
    """Get opengrep version and git SHA."""
    try:
        result = subprocess.run(
            ["pipenv", "run", "opengrep", "--version"],
            cwd="../../cli",
            capture_output=True,
            text=True
        )
        version = result.stdout.strip() if result.stdout else "unknown"
    except Exception:
        version = "unknown"

    try:
        sha = subprocess.check_output(
            ['git', 'rev-parse', 'HEAD'],
            cwd="../.."
        ).decode('ascii').strip()
    except Exception:
        sha = "unknown"

    return version, sha


def hash_output(s):
    s = s.strip()
    return hashlib.sha256(s.encode("utf-8")).hexdigest()


# Not used ATM
def _python_cmd(repo):
    return ["pipenv", "run", "opengrep",
            "scan", "-c", "rules", repo, "-j", num_cpus]

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

repos = [{"url": url, "sha": sha, "name": get_repo_name(url), "reps": reps}
         for (url, sha, reps) in repositories]

def run(cmd, cwd=None):
    # my_env = os.environ.copy()
    # my_env["PIPENV_PIPFILE"] = "../opengrep/cli/Pipfile"
    print(f"Running: {' '.join(cmd)}")
    sys.stdout.flush()
    result = subprocess.run(cmd, cwd=cwd, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    return hash_output(result.stdout)

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

def extract_stats(output):
    """Extract findings and files scanned from command output."""
    import re
    findings = 0
    files = 0
    for line in output.split('\n'):
        match = re.search(r'(\d+)\s+findings?', line, re.IGNORECASE)
        if match:
            findings = int(match.group(1))
        match = re.search(r'(\d+)\s+files?', line, re.IGNORECASE)
        if match:
            files = int(match.group(1))
    return findings, files

def format_time(seconds):
    """Format seconds as human-readable time."""
    if seconds < 60:
        return f"{seconds:.1f}s"
    elif seconds < 3600:
        mins = int(seconds // 60)
        secs = int(seconds % 60)
        return f"{mins}m {secs}s"
    else:
        hours = int(seconds // 3600)
        mins = int((seconds % 3600) // 60)
        return f"{hours}h {mins}m"

def progress_bar(current, total, elapsed, width=30):
    """Create a progress bar with ETA."""
    if total == 0:
        return ""
    pct = current / total
    filled = int(width * pct)
    bar = "█" * filled + "░" * (width - filled)

    if current > 0 and pct > 0:
        eta = (elapsed / pct) - elapsed
        eta_str = format_time(eta)
    else:
        eta_str = "?"

    return f"[{bar}] {current}/{total} ({pct*100:.0f}%) ETA: {eta_str}"

def single_run_cmd(cmd, label="", run_num=None, total_runs=None, start_time_all=None):
    """Run a single benchmark with given command."""
    import pty
    import select
    import re

    time.sleep(1)

    start_time = time.time()
    output_data = []
    last_pct = -1

    # Use pty to get real-time output with progress
    master_fd, slave_fd = pty.openpty()

    process = subprocess.Popen(
        cmd,
        stdout=slave_fd,
        stderr=slave_fd,
        close_fds=True
    )
    os.close(slave_fd)

    # Read output and show our own progress bar
    while True:
        try:
            ready, _, _ = select.select([master_fd], [], [], 0.1)
            if ready:
                chunk = os.read(master_fd, 4096).decode('utf-8', errors='replace')
                if chunk:
                    output_data.append(chunk)
                    # Look for percentage like "28%" or "100%"
                    matches = re.findall(r'(\d+)%', chunk)
                    if matches:
                        pct = int(matches[-1])
                        if pct != last_pct:
                            last_pct = pct
                            elapsed = time.time() - start_time
                            bar_width = 30
                            filled = int(bar_width * pct / 100)
                            bar = '█' * filled + '░' * (bar_width - filled)
                            sys.stdout.write(f"\r  {label}: [{bar}] {pct}% {elapsed:.1f}s   ")
                            sys.stdout.flush()
        except OSError:
            break

        if process.poll() is not None:
            # Process done, read remaining
            try:
                while True:
                    ready, _, _ = select.select([master_fd], [], [], 0)
                    if not ready:
                        break
                    chunk = os.read(master_fd, 4096).decode('utf-8', errors='replace')
                    if not chunk:
                        break
                    output_data.append(chunk)
            except:
                pass
            break

    os.close(master_fd)
    process.wait()

    elapsed_time = time.time() - start_time
    output_str = ''.join(output_data)

    output_hash = hash_output(output_str)
    findings, files = extract_stats(output_str)

    # Final result line
    sys.stdout.write(f"\r  {label}: {elapsed_time:.2f}s, {findings} findings, {files} files                    \n")
    sys.stdout.flush()

    log_to_file(f"- run completed: {label}. elapsed: {show_num(elapsed_time)}, findings: {findings}, files: {files}, result_sha: {output_hash}\n")
    return (elapsed_time, output_hash, findings, files)

def single_run(repo):
    time, hash, findings, files = single_run_cmd(regular_cmd(repo), repo)
    return (time, hash)

def run_opengrep(repo, reps):
    durs = [single_run(repo) for x in range(0, reps)]
    hash = set([y for (x,y) in durs])
    return (statistics.mean([x for (x, _) in durs]), hash)

def run_with_cmd(cmd, reps, label="", run_offset=0, total_runs=None, start_time_all=None):
    """Run multiple iterations with given command."""
    results = []
    for i in range(reps):
        run_num = run_offset + i + 1 if total_runs else None
        r = single_run_cmd(cmd, f"{label} #{i+1}", run_num, total_runs, start_time_all)
        results.append(r)
    times = [r[0] for r in results]
    findings_list = [r[2] for r in results]
    files_list = [r[3] for r in results]
    avg_findings = statistics.mean(findings_list) if findings_list else 0
    avg_files = statistics.mean(files_list) if files_list else 0
    # Check if findings count is consistent across runs
    consistent = len(set(findings_list)) == 1
    return (statistics.mean(times), consistent, avg_findings, avg_files)

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
    run(["make", "core"], cwd="../..")
    run(["pipenv", "run", "make", "install"], cwd="../..")
    run(["pipenv", "run", "pip", "install", "-e", "."], cwd="../../cli")

def run_bench():
    return [{"name": r["name"], "duration": duration, "sha": sha }
            for r in repos for (duration, sha) in [run_opengrep("repos/" + r["name"], r["reps"])]]

def combine_results(res1, res2):
    return [{"name": r1["name"], "d1": r1["duration"], "d2": r2["duration"], "same-results": r1["sha"]==r2["sha"]}
            for (r1, r2) in zip(res1, res2)]

def report_results(sha1, sha2, res1, res2):
    combined = combine_results(res1, res2)
    with_stats = [{"name": r["name"],
                   sha1: f"{r["d1"]:.2f}",
                   sha2: f"{r["d2"]:.2f}",
                   "diff(s)": f"{(r["d2"] - r["d1"]):.2f}",
                   "diff(%)": f"{(100 * (r["d2"] - r["d1"]) / r["d1"]):.2f}",
                   "same-results": r["same-results"]}
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

def run_custom_benchmark(rules, target, reps, with_semgrep, taint_intrafile, label_prefix="", run_offset=0, total_runs=None, start_time_all=None):
    """Run a single custom benchmark configuration and return results."""
    results = []

    # Run semgrep if requested
    if with_semgrep:
        duration, consistent, findings, files = run_with_cmd(
            semgrep_cmd(rules, target), reps, f"{label_prefix}semgrep",
            run_offset, total_runs, start_time_all)
        results.append({"name": f"{label_prefix}Semgrep", "duration": duration, "consistent": consistent, "findings": findings, "files": files})
        run_offset += reps

        duration, consistent, findings, files = run_with_cmd(
            semgrep_cmd(rules, target, pro_intrafile=True), reps, f"{label_prefix}semgrep-pro",
            run_offset, total_runs, start_time_all)
        results.append({"name": f"{label_prefix}Semgrep (pro-intrafile)", "duration": duration, "consistent": consistent, "findings": findings, "files": files})
        run_offset += reps

    # Run opengrep
    duration, consistent, findings, files = run_with_cmd(
        custom_cmd(rules, target), reps, f"{label_prefix}opengrep",
        run_offset, total_runs, start_time_all)
    results.append({"name": f"{label_prefix}OpenGrep", "duration": duration, "consistent": consistent, "findings": findings, "files": files})
    run_offset += reps

    if taint_intrafile:
        duration, consistent, findings, files = run_with_cmd(
            custom_cmd(rules, target, intrafile=True), reps, f"{label_prefix}opengrep-intra",
            run_offset, total_runs, start_time_all)
        results.append({"name": f"{label_prefix}OpenGrep (taint-intrafile)", "duration": duration, "consistent": consistent, "findings": findings, "files": files})
        run_offset += reps

    return results, run_offset

def go_custom(args):
    """Run custom benchmark mode with user-specified rules and target."""
    os.makedirs("results", exist_ok=True)
    log_to_file(f"CUSTOM PERF TEST {ts}\n\n")

    rules = args.rules
    target = args.target
    reps = args.reps
    shas = args.sha if args.sha else []

    # Calculate total runs for progress bar
    opengrep_configs = 1  # opengrep
    if args.taint_intrafile:
        opengrep_configs += 1  # opengrep-intra
    semgrep_configs = 2 if args.with_semgrep else 0  # semgrep + semgrep-pro (run once)

    num_builds = 1 + len(shas)
    total_runs = reps * (semgrep_configs + opengrep_configs * num_builds)

    # Get and display opengrep version
    version, git_sha = get_opengrep_version()
    print(f"\nOpengrep version: {version}")
    print(f"Git SHA: {git_sha[:12]}")
    print(f"\nBenchmarking: {rules} on {target}")
    print(f"Runs per config: {reps}, Total runs: {total_runs}")
    if shas:
        print(f"Comparing: current vs {', '.join(shas)}")
    print()

    start_time_all = time.time()
    all_results = {}
    run_offset = 0

    # Run semgrep once (doesn't depend on opengrep build)
    if args.with_semgrep:
        if not is_semgrep_installed():
            print("Warning: semgrep is not installed, skipping semgrep benchmarks")
            print("See: https://semgrep.dev/docs/getting-started/quickstart")
        else:
            print(f"\n=== Running Semgrep ===")
            duration, consistent, findings, files = run_with_cmd(
                semgrep_cmd(rules, target), reps, "semgrep",
                run_offset, total_runs, start_time_all)
            all_results["semgrep"] = [{"name": "Semgrep", "duration": duration, "consistent": consistent, "findings": findings, "files": files}]
            run_offset += reps

            duration, consistent, findings, files = run_with_cmd(
                semgrep_cmd(rules, target, pro_intrafile=True), reps, "semgrep-pro",
                run_offset, total_runs, start_time_all)
            all_results["semgrep"].append({"name": "Semgrep (pro-intrafile)", "duration": duration, "consistent": consistent, "findings": findings, "files": files})
            run_offset += reps

    # Run opengrep with current build
    print(f"\n=== Building current ===")
    make_opengrep()
    version, git_sha = get_opengrep_version()
    print(f"    Version: {version}, SHA: {git_sha[:12]}")
    results, run_offset = run_custom_benchmark(
        rules, target, reps, False, args.taint_intrafile,
        "[current] " if shas else "", run_offset, total_runs, start_time_all)
    all_results["current"] = results

    # Run additional SHAs if specified
    if shas:
        current_branch = subprocess.check_output(['git', 'rev-parse', '--abbrev-ref', 'HEAD'], cwd="../..").decode('ascii').strip()

        try:
            for sha in shas:
                print(f"\n=== Building {sha} ===")
                checkout_opengrep(sha)
                make_opengrep()
                version, built_sha = get_opengrep_version()
                print(f"    Version: {version}, SHA: {built_sha[:12]}")
                log_to_file(f"\nSHA: {sha}\n")

                results, run_offset = run_custom_benchmark(
                    rules, target, reps, False, args.taint_intrafile,
                    f"[{sha[:8]}] ", run_offset, total_runs, start_time_all)
                all_results[sha] = results
        finally:
            # Restore original branch
            print(f"\n=== Restoring {current_branch} ===")
            checkout_opengrep(current_branch)

    # Print summary
    elapsed_total = time.time() - start_time_all
    print(f"\n{'='*60}")
    print(f"SUMMARY (total time: {format_time(elapsed_total)})")
    print(f"{'='*60}")

    # Collect all results for display
    all_rows = []

    # Add semgrep results if present
    if "semgrep" in all_results:
        for r in all_results["semgrep"]:
            all_rows.append({"name": r["name"], "duration": r["duration"], "findings": r["findings"], "files": r["files"], "consistent": r["consistent"]})

    # Get opengrep build keys (exclude "semgrep")
    build_keys = [k for k in all_results.keys() if k != "semgrep"]

    # Add opengrep results with build prefix
    for key in build_keys:
        prefix = f"[{key[:8]}] " if len(build_keys) > 1 else ""
        if key == "current" and len(build_keys) > 1:
            prefix = "[current] "
        for r in all_results[key]:
            name = r['name'].split('] ')[1] if '] ' in r['name'] else r['name']
            all_rows.append({"name": prefix + name, "duration": r["duration"], "findings": r["findings"], "files": r["files"], "consistent": r["consistent"]})

    name_width = max(len("Configuration"), max(len(r['name']) for r in all_rows))
    print(f"{'Configuration':<{name_width}}  Files     Findings  Time (s)  Consistent")
    print("-" * (name_width + 45))
    for r in all_rows:
        consistent_str = "yes" if r['consistent'] else "no"
        print(f"{r['name']:<{name_width}}  {r['files']:<8.0f}  {r['findings']:<8.0f}  {r['duration']:<8.2f}  {consistent_str}")

def go():
    parser = argparse.ArgumentParser(description='Benchmark opengrep performance')
    parser.add_argument('sha1', nargs='?', help='First SHA/branch to compare')
    parser.add_argument('sha2', nargs='?', help='Second SHA/branch to compare (optional)')
    parser.add_argument('-f', '--rules', help='Custom rules file/directory')
    parser.add_argument('-t', '--target', help='Custom target file/directory')
    parser.add_argument('-r', '--reps', type=int, default=3, help='Number of repetitions (default: 3)')
    parser.add_argument('-s', '--sha', action='append', help='SHA/branch to benchmark (can use multiple times to compare)')
    parser.add_argument('--with-semgrep', action='store_true', help='Include semgrep benchmarks')
    parser.add_argument('--taint-intrafile', action='store_true', help='Include taint-intrafile benchmarks')

    args = parser.parse_args()

    # Check for partial custom mode args
    if args.rules and not args.target:
        print("Error: -f/--rules requires -t/--target")
        sys.exit(1)
    if args.target and not args.rules:
        print("Error: -t/--target requires -f/--rules")
        sys.exit(1)

    # Custom mode: -f and -t specified
    if args.rules and args.target:
        go_custom(args)
        return

    # Original mode: SHA comparison
    if not args.sha1:
        parser.print_help()
        print("\nExamples:")
        print("  bench.py <sha1>                              # Compare sha1 vs current")
        print("  bench.py <sha1> <sha2>                       # Compare sha1 vs sha2")
        print("  bench.py -f rules.yaml -t target/            # Custom benchmark (current build)")
        print("  bench.py -f rules.yaml -t target/ -s main    # Custom benchmark comparing main vs current")
        print("  bench.py -f rules.yaml -t target/ -s sha1 -s sha2  # Compare two SHAs")
        print("  bench.py -f rules.yaml -t target/ --with-semgrep --taint-intrafile")
        sys.exit(1)

    setup()
    log_to_file(f"PERF TEST {ts}\n\n")

    currentBranch = subprocess.check_output(['git', 'rev-parse', '--abbrev-ref', 'HEAD']).decode('ascii').strip()
    sha1 = args.sha1

    changes = has_changes()

    try:
        if changes:
            run(["git", "stash", "-m", f"'uncommitted changes for perf test {ts}'"])

        # run two arbitrary shas/branches
        if args.sha2:
            sha2 = args.sha2
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

if __name__ == "__main__":
    go()
