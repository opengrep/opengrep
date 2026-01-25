#!/usr/bin/env python3
"""
Benchmark script for opengrep.

Examples:
  bench.py                          # Run opengrep + semgrep on default targets
  bench.py -t <target>              # Run on specific target(s)
  bench.py -s <branch>              # Compare current vs branch(es)
  bench.py -s <branch> --no-semgrep # Compare branches without semgrep
"""

import argparse
import csv
import os
import pty
import re
import select
import shutil
import subprocess
import sys
import tempfile
import time
from datetime import datetime
from urllib.parse import urlparse

DEFAULT_REPOS = [
    ("https://github.com/jellyfin/jellyfin", "a0931baa8eb879898f4bc4049176ed3bdb4d80d1"),
    ("https://github.com/grafana/grafana", "afcb55156260fe1887c4731e6cc4c155cc8281a2"),
    ("https://gitlab.com/gitlab-org/gitlab", "915627de697e2dd71fe8205853de51ad3794f3ac"),
    ("https://github.com/Netflix/lemur", "28b9a73a83d350b1c7ab71fdd739d64eec5d06aa"),
    ("https://github.com/pythongosssss/ComfyUI-Custom-Scripts", "943e5cc7526c601600150867a80a02ab008415e7"),
    ("https://github.com/pmd/pmd", "81739da5caff948dbcd2136c17532b65c726c781"),
    ("https://github.com/square/leakcanary", "bf5086da26952e3627f18865bb232963e4d019c5"),
    ("https://github.com/OWASP-Benchmark/BenchmarkJava", "2f279f902366829522d5e5a08f6aa74b1baf5653"),
]

NUM_CPUS = str(max(1, os.cpu_count() - 1))
TIMESTAMP = datetime.now().strftime('%Y%m%d-%H%M%S')
ROOT_DIR = "../.."
BOOL = argparse.BooleanOptionalAction
METRICS = [
    ('time', 'Time (s)', lambda r: f"{r['time']:.1f}"),
    ('findings', 'Findings', lambda r: str(r['findings'])),
    ('timeouts', 'Timeouts', lambda r: str(r['timeouts'])),
]
ARGS = [
    (('-f', '--rules'), {'default': '../../tests/semgrep-rules', 'help': 'Rules file/directory'}),
    (('-t', '--target'), {'action': 'append', 'help': 'Target(s) to scan (can specify multiple)'}),
    (('-s', '--sha'), {'action': 'append', 'help': 'Branch(es) to compare against (can specify multiple)'}),
    (('-r', '--reps'), {'type': int, 'default': 1, 'help': 'Repetitions per config'}),
    (('--semgrep',), {'action': BOOL, 'default': True, 'help': 'Include semgrep benchmarks (default: on)'}),
    (('--taint-only',), {'action': BOOL, 'default': False, 'help': 'Only run taint rules (default: off)'}),
    (('--timeouts',), {'action': BOOL, 'default': True, 'help': 'Count fixpoint timeouts via --debug (default: on)'}),
]


def get_repo_name(url):
    return os.path.splitext(os.path.basename(urlparse(url).path))[0]


def get_dir_size_mb(path):
    def safe_size(p):
        try: return os.path.getsize(p)
        except OSError: return 0
    return sum(safe_size(os.path.join(d, f)) for d, _, files in os.walk(path) for f in files) / (1024 * 1024)


def count_rules(rules_dir):
    return sum(1 for _, _, files in os.walk(rules_dir) for f in files if f.endswith(('.yaml', '.yml')))


def filter_taint_rules(rules_dir):
    filtered_dir = tempfile.mkdtemp(prefix="taint_rules_")
    count = 0
    for root, _, files in os.walk(rules_dir):
        for f in files:
            if not f.endswith(('.yaml', '.yml')):
                continue
            src = os.path.join(root, f)
            try:
                with open(src) as fh:
                    if re.search(r'mode:\s*taint', fh.read()):
                        dst_dir = os.path.join(filtered_dir, os.path.relpath(root, rules_dir))
                        os.makedirs(dst_dir, exist_ok=True)
                        shutil.copy2(src, os.path.join(dst_dir, f))
                        count += 1
            except Exception:
                pass
    print(f"  Filtered to {count} taint rule files")
    return filtered_dir


def run_cmd(cmd, cwd=None):
    print(f"  $ {' '.join(cmd)}")
    result = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"    ERROR: {result.stderr[:200]}")
    return result


def clone_repo(url, sha, dest):
    if os.path.exists(dest):
        return print(f"  Repo exists: {dest}")
    print(f"  Cloning {url}...")
    for cmd, cwd in [(["git", "clone", "--no-checkout", "--depth", "1", url, dest], None),
                     (["git", "fetch", "--depth", "1", "origin", sha], dest),
                     (["git", "checkout", sha], dest)]:
        run_cmd(cmd, cwd=cwd)


def checkout_and_build(ref):
    print(f"\n=== Building {ref} ===")
    for cmd in [["git", "checkout", ref],
                ["git", "submodule", "update", "--init", "--recursive"],
                ["rm", "-rf", "cli/build", "cli/src/opengrep.egg-info"],
                ["make", "core"],
                ["pipenv", "run", "make", "install"]]:
        run_cmd(cmd, cwd=ROOT_DIR)


def git_info(*args):
    return subprocess.run(["git", "rev-parse"] + list(args), cwd=ROOT_DIR, capture_output=True, text=True).stdout.strip()


def is_semgrep_installed():
    try:
        return subprocess.run(["semgrep", "--version"], capture_output=True).returncode == 0
    except FileNotFoundError:
        return False


def run_with_progress(cmd, label, target, tool="opengrep", rules_dir=None, show_timeouts=True):
    print(f"\n  CMD: {' '.join(cmd)}")
    start = time.time()
    output_data = []

    # Estimate: (30 + size) * sqrt(rules/2001) * tool_mult
    size_mb = get_dir_size_mb(target)
    rules_count = count_rules(rules_dir) if rules_dir else 2001
    rules_factor = (rules_count / 2001) ** 0.5
    multiplier = 1.5 if tool == "semgrep" else 1.0
    est_time = (30 + size_mb) * rules_factor * multiplier

    master_fd, slave_fd = pty.openpty()
    process = subprocess.Popen(cmd, stdout=slave_fd, stderr=slave_fd, close_fds=True)
    os.close(slave_fd)

    while True:
        try:
            ready, _, _ = select.select([master_fd], [], [], 0.1)
            if ready:
                chunk = os.read(master_fd, 4096).decode('utf-8', errors='replace')
                if chunk:
                    output_data.append(chunk)

            elapsed = time.time() - start
            pct = min(99, int(100 * elapsed / est_time))
            bar = '█' * (pct * 30 // 100) + '░' * (30 - pct * 30 // 100)
            sys.stdout.write(f"\r  {label}: [{bar}] {pct}% {elapsed:.0f}s   ")
            sys.stdout.flush()
        except OSError:
            break

        if process.poll() is not None:
            try:
                while select.select([master_fd], [], [], 0)[0]:
                    chunk = os.read(master_fd, 4096).decode('utf-8', errors='replace')
                    if not chunk:
                        break
                    output_data.append(chunk)
            except OSError:
                pass
            break

    os.close(master_fd)
    process.wait()

    elapsed = time.time() - start
    output = ''.join(output_data)

    findings = rules = files = 0
    match = re.search(r'Ran (\d+) rules on (\d+) files: (\d+) findings', output)
    if match:
        rules, files, findings = int(match.group(1)), int(match.group(2)), int(match.group(3))
    timeouts = len(re.findall(r'fixpoint timeout', output, re.IGNORECASE))

    timeout_str = f", {timeouts} timeouts" if show_timeouts else ""
    sys.stdout.write(f"\r  {label}: {elapsed:.1f}s, {findings} findings, {rules} rules, {files} files{timeout_str}\n")
    sys.stdout.flush()

    return elapsed, findings, timeouts if show_timeouts else 0


def scan(tool, rules, target, extra_args=None, label="", show_timeouts=False):
    cmd = (["semgrep"] if tool == "semgrep" else ["pipenv", "run", "opengrep"]) + \
          ["-f", rules, target, "-j", NUM_CPUS, "--timeout", "9"] + \
          (extra_args or []) + (["--debug"] if show_timeouts else [])
    return run_with_progress(cmd, label or tool, target, tool=tool, rules_dir=rules, show_timeouts=show_timeouts)


def run_scans(tool, rules, target, extra_args, branch_label, reps, show_timeouts):
    tool_name = f"{tool}{'-intra' if extra_args else ''}"
    runs = [scan(tool, rules, target, extra_args, f"{tool_name} [{branch_label}] #{i+1}/{reps}", show_timeouts)
            for i in range(reps)]
    return {
        "target": os.path.basename(target),
        "tool": tool_name,
        "branch": branch_label,
        "time": sum(t for t, _, _ in runs) / len(runs),
        "findings": runs[-1][1],
        "timeouts": sum(to for _, _, to in runs) // len(runs)
    }


def run_benchmark(rules, targets, branches, with_semgrep, reps, show_timeouts):
    os.makedirs("results", exist_ok=True)
    current_branch = git_info("--abbrev-ref", "HEAD")
    results = []
    builds = ["current"] + (branches or [])

    if with_semgrep and not is_semgrep_installed():
        sys.exit("ERROR: semgrep not installed. Run with --no-semgrep to skip semgrep benchmarks.")

    print(f"\nConfiguration: rules={rules}, targets={len(targets)}, builds={builds}, semgrep={with_semgrep}")

    try:
        for target in targets:
            print(f"\n{'='*60}\nTarget: {target}\n{'='*60}")

            if with_semgrep:
                results += [run_scans("semgrep", rules, target, args, "-", reps, show_timeouts)
                            for args in [None, ["--pro-intrafile"]]]

            for build in builds:
                checkout_and_build(current_branch if build == "current" else build)
                label = f"current ({git_info('HEAD')[:12]})" if build == "current" else build[:12]
                results += [run_scans("opengrep", rules, target, args, label, reps, show_timeouts)
                            for args in [None, ["--taint-intrafile"]]]
    finally:
        print(f"\n=== Restoring {current_branch} ===")
        checkout_and_build(current_branch)

    return results


def run_label(r):
    return f"{r['tool']} [{r['branch']}]" if r['branch'] != '-' else r['tool']


def build_lookup(results):
    targets = sorted(set(r["target"] for r in results))
    runs = list(dict.fromkeys(run_label(r) for r in results))  # unique, preserves order
    return targets, runs, {(run_label(r), r['target']): r for r in results}


def print_summary(results, show_timeouts=True):
    targets, runs, lookup = build_lookup(results)
    current = next((r for r in runs if "current" in r), None)
    bold = lambda run: "**" if current and "current" in run else ""
    metrics = [m for m in METRICS if show_timeouts or m[0] != 'timeouts']

    def table(fmt):
        header = "| Run |" + "".join(f" {t} |" for t in targets)
        sep = "|-----|" + "------|" * len(targets)
        rows = [f"| {bold(run)}{run}{bold(run)} |" + "".join(
            f" {bold(run)}{fmt(lookup[(run, t)]) if (run, t) in lookup else ''}{bold(run)} |" for t in targets
        ) for run in runs]
        return [header, sep] + rows + [""]

    lines = [f"## Benchmark Results ({datetime.now().strftime('%Y-%m-%d')})\n"] + [
        line for _, title, fmt in metrics for line in [f"### {title}\n"] + table(fmt)
    ]
    output = "\n".join(lines)
    print(output)
    with open(f"results/bench-{TIMESTAMP}.md", 'w') as f:
        f.write(output)


def save_results(results, show_timeouts=True):
    targets, runs, lookup = build_lookup(results)
    metrics = [m for m in METRICS if show_timeouts or m[0] != 'timeouts']
    with open(f"results/bench-{TIMESTAMP}.csv", 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['metric', 'run'] + targets)
        [writer.writerow([m, run] + [fmt(lookup[(run, t)]) if (run, t) in lookup else '' for t in targets])
         for m, _, fmt in metrics for run in runs]


def main():
    parser = argparse.ArgumentParser(description='Benchmark opengrep', epilog=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    for names, opts in ARGS:
        parser.add_argument(*names, **opts)
    args = parser.parse_args()

    rules = filter_taint_rules(args.rules) if args.taint_only else args.rules

    if not args.target:
        print("Using default repos...")
        [clone_repo(url, sha, f"repos/{get_repo_name(url)}") for url, sha in DEFAULT_REPOS]
    targets = args.target or [f"repos/{get_repo_name(url)}" for url, _ in DEFAULT_REPOS]

    results = run_benchmark(rules, targets, args.sha, args.semgrep, args.reps, args.timeouts)
    print_summary(results, args.timeouts)
    save_results(results, args.timeouts)


if __name__ == "__main__":
    main()
