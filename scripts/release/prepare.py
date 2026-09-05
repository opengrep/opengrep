#!/usr/bin/env python3
"""Prepare an Opengrep release: branch, version bump, changelog, pull request.

This covers steps 1-4 of the release process.  Steps 5 and 6 stay manual: after
the release PR is merged you dispatch the 'rolling-release' workflow from main
with the new tag, then publish the draft release it creates.

The changelog entry is written by Claude Code when it is on PATH, from a dossier
of the pull requests merged since the previous release.  Without Claude Code the
script falls back to reformatting GitHub's own generated notes, which is faster
but leaves the wording and the categorisation for you to fix by hand.

Standard library only; git, gh and dune must be on PATH.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import contextlib
import datetime
import difflib
import itertools
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# The version string lives in six files.  Five are edited here; opam/semgrep.opam
# is generated from dune-project by dune.  Each pattern must match exactly once.
VERSION_FILES: list[tuple[str, str]] = [
    ("cli/src/semgrep/__init__.py", r'^(__VERSION__ = ")[^"]*(")$'),
    ("src/core/Version.ml", r'^(let version = ")[^"]*(")$'),
    ("dune-project", r"^(\(version )[^)]*(\))$"),
    ("setup.py", r'^(    version=")[^"]*(",)$'),
    ("cli/setup.py", r'^(    version=")[^"]*(",)$'),
]

OPAM_FILE = "opam/semgrep.opam"
CHANGELOG = "CHANGELOG.md"

BUMP_COMMIT_MSG = "bump version"
CHANGELOG_COMMIT_MSG = "add changelog"

# The dossier embeds merged pull request titles and descriptions verbatim, and a
# pull request's description stays editable by its author after the merge, so this
# is untrusted input.  Claude therefore gets read-only tools only, and no `gh api`:
# `gh api` writes whenever it is given --method or -f, which would put authenticated
# GitHub calls behind attacker-controlled text.  Everything the script itself needs
# from the API it fetches before the model runs.
CLAUDE_ALLOWED_TOOLS = (
    "Read Grep Glob "
    "Bash(gh pr view:*) Bash(gh pr diff:*) "
    "Bash(git log:*) Bash(git show:*) Bash(git diff:*)"
)

# Section headings the changelog uses; anything else is a sign the output drifted.
KNOWN_SECTIONS = {"New features", "Language support", "Improvements", "Bug fixes",
                  "New Contributors"}

CLAUDE_TIMEOUT_S = 900

CHANGELOG_SYSTEM_PROMPT = """\
You write the changelog for Opengrep, a fast syntax-aware static analysis engine
for many languages (a fork of Semgrep). You are given a dossier of the pull
requests merged since the previous release, and you produce the body of one
CHANGELOG entry.

AUDIENCE

Advanced Opengrep users: people who write and run rules, wire Opengrep into CI,
and read its JSON/SARIF output. Not Opengrep's own developers, but not beginners
either. They can read a sentence about the taint engine or the parser.

WHAT TO INCLUDE

Include anything that changes what Opengrep does: engine and matching behaviour,
taint analysis, parsers and language support, rule syntax and rule options, CLI
flags, output formats, performance, and bug fixes.

Include changes that are strictly internal to the engine -- a different algorithm
for something, a large restructuring of one part of Opengrep. Describe them in
whatever technical terms the change actually requires. Do NOT water such an entry
down into something vague, and do NOT drop it because it looks like "internals".

Drop pull requests that are only *project* infrastructure and have no bearing on
what a user gets: CI configuration, GitHub Actions workflows, the build system,
Dockerfiles used for building, benchmark and perf harnesses, test-only changes,
routine dependency bumps, formatting. Keep one of these only when it is genuinely
significant to users: a new platform or architecture, a change to the released
artifacts, a change to how Opengrep is installed.

When in doubt, include it. A reader who wants less detail can skim; a reader whose
bug was fixed needs to find the line.

HOW TO WRITE EACH LINE

LENGTH IS A HARD CONSTRAINT. One bullet is one sentence on one line. Existing
entries average about 75 characters before the " by @author in #NNN" suffix, and
the longest justified ones reach roughly 140. Never write a paragraph, never a
semicolon-chained list of everything a branch touched, never a commit-by-commit
summary.

When one pull request really does contain several independent user-visible
changes, emit several bullets that repeat the same " by @author in #NNN" suffix.
That is normal here and far better than one long bullet. Otherwise, describe the
pull request as a whole and let the reader open it for the detail.

Prefer one bullet. Split only where the extra changes are independent and each is
worth knowing on its own. Past about four bullets from a single pull request you
are enumerating commits: group them back into the change they add up to. One
pull request must never dominate the entry.

Start from the pull request title, but do not treat it as fixed. When a title or
description is cryptic, abbreviated, or written as a note to another maintainer,
replace it with a description of what actually changed and what it affects. You
have read-only access to the repository and to gh -- read the diff before you
guess. `gh pr diff <number>` and `gh pr view <number>` are available.

Err toward too technical rather than too vague -- but that governs word choice,
not length. Name the language, the analysis, the flag or the syntax involved
instead of retreating to "fix a bug" or "improve support". Spending the budget on
a precise noun beats spending it on a second clause.

  bad   PHP: various parser fixes
  bad   Fix bug in the parser
  good  PHP: parse PHP 7.3 flexible heredocs, with an indented closing marker
  good  taint: deliver propagator taints when `to` is visited before `from`
  bad   PHP: heredocs and nowdocs are constant expressions, so they can
        initialise a const, a class constant, a property, a parameter default,
        a static variable and an enum case value; flexible heredocs parse,
        meaning ...                        (a paragraph -- split it into bullets)

Prefix language-specific entries with the language, as in "PHP: ..." or "Rust: ...".
Use the imperative or a plain noun phrase, no trailing period.

OUTPUT FORMAT

Emit raw markdown and nothing else: no code fences, no preamble, no closing
remarks, no version heading, no "Full Changelog" line. Those are added for you.

Use only these section names, in this order, omitting any that would be empty:

### New features
### Language support
### Improvements
### Bug fixes
### New Contributors

Under each, one bullet per pull request:

* <description> by @<author> in #<number>

Keep the " by @author in #number" suffix exactly as given in the dossier: the
author's login unchanged, the pull request as #1234 and never as a URL. New
Contributors bullets keep their own form:

* @<author> made their first contribution in #<number>

Separate every heading from its bullets, and every section from the next, with a
single blank line. Bullets inside a section are consecutive lines with no blank
line between them.
"""


# ---------------------------------------------------------------------------
# Terminal styling
# ---------------------------------------------------------------------------


class Style:
    """ANSI styling with graceful degradation to 256-colour and to plain text."""

    ACCENT = (86, 214, 199)
    ACCENT2 = (167, 139, 250)
    OK = (74, 222, 128)
    WARN = (250, 204, 21)
    ERR = (248, 113, 113)
    DIM = (125, 137, 152)

    def __init__(self, enabled: bool) -> None:
        self.enabled = enabled
        colorterm = os.environ.get("COLORTERM", "")
        self.truecolor = colorterm in ("truecolor", "24bit")

    def fg(self, rgb: tuple[int, int, int]) -> str:
        if not self.enabled:
            return ""
        r, g, b = rgb
        if self.truecolor:
            return f"\033[38;2;{r};{g};{b}m"
        return f"\033[38;5;{self._cube(r, g, b)}m"

    @staticmethod
    def _cube(r: int, g: int, b: int) -> int:
        def q(v: int) -> int:
            return 0 if v < 48 else 1 if v < 115 else (v - 35) // 40

        return 16 + 36 * q(r) + 6 * q(g) + q(b)

    @property
    def reset(self) -> str:
        return "\033[0m" if self.enabled else ""

    @property
    def bold(self) -> str:
        return "\033[1m" if self.enabled else ""

    def paint(self, text: str, rgb: tuple[int, int, int], bold: bool = False) -> str:
        if not self.enabled:
            return text
        return f"{self.bold if bold else ''}{self.fg(rgb)}{text}{self.reset}"

    def accent(self, t: str, bold: bool = False) -> str:
        return self.paint(t, self.ACCENT, bold)

    def accent2(self, t: str, bold: bool = False) -> str:
        return self.paint(t, self.ACCENT2, bold)

    def ok(self, t: str) -> str:
        return self.paint(t, self.OK)

    def warn(self, t: str) -> str:
        return self.paint(t, self.WARN)

    def err(self, t: str) -> str:
        return self.paint(t, self.ERR)

    def dim(self, t: str) -> str:
        return self.paint(t, self.DIM)

    def gradient(self, text: str,
                 start: tuple[int, int, int],
                 end: tuple[int, int, int]) -> str:
        """Fade `text` from `start` to `end`, one step per character."""
        if not self.enabled:
            return text
        n = max(len(text) - 1, 1)
        out = []
        for i, ch in enumerate(text):
            rgb = tuple(int(s + (e - s) * i / n) for s, e in zip(start, end))
            out.append(f"{self.fg(rgb)}{ch}")  # type: ignore[arg-type]
        return self.bold + "".join(out) + self.reset


S = Style(False)  # replaced in main()


def width() -> int:
    return min(shutil.get_terminal_size((80, 24)).columns, 96)


def banner(subtitle: str) -> None:
    w = width()
    title = "OPENGREP  RELEASE"
    pad = max((w - 2 - len(title)) // 2, 0)
    print()
    print(S.accent("╭" + "─" * (w - 2) + "╮"))
    print(S.accent("│") + " " * pad
          + S.gradient(title, Style.ACCENT, Style.ACCENT2)
          + " " * max(w - 2 - pad - len(title), 0) + S.accent("│"))
    sub_pad = max((w - 2 - len(subtitle)) // 2, 0)
    print(S.accent("│") + " " * sub_pad + S.dim(subtitle)
          + " " * max(w - 2 - sub_pad - len(subtitle), 0) + S.accent("│"))
    print(S.accent("╰" + "─" * (w - 2) + "╯"))
    print()


def step(n: int, total: int, title: str) -> None:
    label = f" {n}/{total} · {title} "
    w = width()
    lead = S.accent2("━━━")
    tail = S.accent2("━" * max(w - len(label) - 3, 3))
    print()
    print(f"{lead}{S.accent(label, bold=True)}{tail}")
    print()


def info(msg: str) -> None:
    print(f"  {S.dim('·')} {msg}")


def good(msg: str) -> None:
    print(f"  {S.ok('✓')} {msg}")


def bad(msg: str) -> None:
    print(f"  {S.err('✗')} {msg}")


def warning(msg: str) -> None:
    print(f"  {S.warn('!')} {msg}")


def field(name: str, value: str) -> None:
    print(f"  {S.dim(name.rjust(12))}  {value}")


class Abort(Exception):
    """Anything that should stop the run with a message but no traceback."""


@contextlib.contextmanager
def spinner(label: str):
    """Braille spinner for the slow steps; a plain line when not on a tty."""
    if not (S.enabled and sys.stdout.isatty()):
        print(f"  {S.dim('·')} {label} ...")
        yield
        return

    stop = threading.Event()

    def spin() -> None:
        frames = itertools.cycle("⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")
        while not stop.is_set():
            sys.stdout.write(f"\r  {S.accent(next(frames))} {label} ")
            sys.stdout.flush()
            time.sleep(0.08)

    thread = threading.Thread(target=spin, daemon=True)
    thread.start()
    try:
        yield
    except BaseException:
        stop.set()
        thread.join()
        sys.stdout.write("\r\033[2K")
        bad(label)
        raise
    else:
        stop.set()
        thread.join()
        sys.stdout.write("\r\033[2K")
        good(label)


def ask(prompt: str, choices: str, default: str | None = None) -> str:
    """Prompt for one of the single-letter `choices`; returns a lowercase letter."""
    rendered = "/".join(
        S.accent(c.upper()) if c == default else c for c in choices
    )
    while True:
        try:
            raw = input(f"  {S.accent2('?')} {prompt} [{rendered}] ").strip().lower()
        except EOFError:
            print()
            raise Abort("no input available: this script is interactive by design")
        except KeyboardInterrupt:
            print()
            raise Abort("cancelled")
        if not raw and default:
            return default
        if len(raw) == 1 and raw in choices:
            return raw


def ask_line(prompt: str, default: str = "") -> str:
    suffix = f" [{S.accent(default)}]" if default else ""
    try:
        raw = input(f"  {S.accent2('?')} {prompt}{suffix} ").strip()
    except EOFError:
        print()
        raise Abort("no input available: this script is interactive by design")
    except KeyboardInterrupt:
        print()
        raise Abort("cancelled")
    return raw or default


# ---------------------------------------------------------------------------
# Subprocess helpers
# ---------------------------------------------------------------------------


DRY_RUN = False


def run(cmd: list[str], *, check: bool = True,
        stdin: str | None = None) -> subprocess.CompletedProcess:
    proc = subprocess.run(cmd, input=stdin, text=True, capture_output=True)
    if check and proc.returncode != 0:
        detail = (proc.stderr or proc.stdout or "").strip()
        raise Abort(f"{' '.join(cmd)} failed:\n{detail}")
    return proc


def git(*args: str) -> str:
    return run(["git", *args]).stdout.strip()


def gh(*args: str) -> str:
    return run(["gh", *args]).stdout


def have(tool: str) -> bool:
    return shutil.which(tool) is not None


# ---------------------------------------------------------------------------
# Versions
# ---------------------------------------------------------------------------


VERSION_RE = re.compile(r"^(\d+)\.(\d+)\.(\d+)(?:[-+].*)?$")


def read_current_version() -> str:
    text = open("src/core/Version.ml", encoding="utf-8").read()
    m = re.search(r'^let version = "([^"]*)"', text, re.M)
    if not m:
        raise Abort("could not find the version in src/core/Version.ml")
    return m.group(1)


def check_version_consistency(expected: str) -> None:
    for path, pattern in VERSION_FILES:
        text = open(path, encoding="utf-8").read()
        m = re.search(pattern, text, re.M)
        if not m:
            warning(f"{path}: version line not found")
            continue
        found = re.search(r"[\d][\w.\-+]*", m.group(0))
        if found and found.group(0) != expected:
            warning(f"{path}: has {found.group(0)}, expected {expected}")
    opam = open(OPAM_FILE, encoding="utf-8").read()
    m = re.search(r'^version: "([^"]*)"', opam, re.M)
    if m and m.group(1) != expected:
        warning(f"{OPAM_FILE}: has {m.group(1)}, expected {expected}")


def bump_version(current: str, part: str) -> str:
    m = VERSION_RE.match(current)
    if not m:
        raise Abort(f"cannot bump non-semver current version {current!r}")
    major, minor, patch = (int(g) for g in m.groups())
    if part == "major":
        return f"{major + 1}.0.0"
    if part == "minor":
        return f"{major}.{minor + 1}.0"
    return f"{major}.{minor}.{patch + 1}"


def latest_stable_tag() -> str | None:
    tags = git("tag", "--list", "v*", "--sort=-v:refname").splitlines()
    for tag in tags:
        if re.fullmatch(r"v\d+\.\d+\.\d+", tag.strip()):
            return tag.strip()
    return None


def choose_version(current: str, prev_tag: str | None, pr_count: int,
                   args: argparse.Namespace) -> str:
    field("current", S.accent(current, bold=True))
    field("latest tag", prev_tag or S.dim("none"))
    field("merged PRs", f"{pr_count} since {prev_tag}" if prev_tag else str(pr_count))
    print()

    if args.version:
        return args.version
    if args.part:
        return bump_version(current, args.part)

    options = [(p, bump_version(current, p)) for p in ("major", "minor", "patch")]
    for i, (part, nxt) in enumerate(options, start=1):
        marker = S.dim("   <- typical") if part == "minor" else ""
        print(f"    {S.accent2(str(i))}) {part.ljust(7)} "
              f"{S.dim(current)} {S.dim('->')} {S.accent(nxt, bold=True)}{marker}")
    print(f"    {S.accent2('4')}) custom  {S.dim('(e.g. 1.30.0-rc.1)')}")
    print()

    while True:
        raw = ask_line("select", "2")
        if raw in ("1", "2", "3"):
            return options[int(raw) - 1][1]
        if raw == "4":
            custom = ask_line("version")
            if re.fullmatch(r"\d+\.\d+\.\d+([-+][\w.\-]+)?", custom):
                return custom
            bad(f"{custom!r} is not a version like 1.30.0 or 1.30.0-rc.1")


# ---------------------------------------------------------------------------
# Preflight
# ---------------------------------------------------------------------------


def preflight(args: argparse.Namespace) -> str:
    for tool in ("git", "gh", "dune"):
        if not have(tool):
            raise Abort(f"{tool} is not on PATH")

    root = git("rev-parse", "--show-toplevel")
    os.chdir(root)
    info(f"repository {S.accent(root)}")

    if run(["gh", "auth", "status"], check=False).returncode != 0:
        raise Abort("gh is not authenticated; run 'gh auth login'")

    # A dry run commits nothing, so these two are advisory there: it stays
    # usable from any branch and from a worktree with work in progress.
    def require(message: str) -> None:
        if DRY_RUN:
            warning(message)
        else:
            raise Abort(message)

    branch = git("rev-parse", "--abbrev-ref", "HEAD")
    if branch != args.base:
        require(f"on branch {branch!r}, expected {args.base!r}")

    # Untracked files are fine and must not block: a maintainer's worktree is
    # allowed to carry scratch notes.  Modified tracked files are not.
    dirty = git("status", "--porcelain", "--untracked-files=no")
    if dirty:
        require("working tree has modified tracked files:\n" + dirty)

    # No --tags: git already follows tags reachable from the fetched branch,
    # and an explicit --tags aborts the whole fetch over any locally diverged
    # tag ("would clobber existing tag").
    with spinner(f"fetching {args.remote}/{args.base}"):
        run(["git", "fetch", "--quiet", args.remote, args.base])
    # The branch is cut from local HEAD, but GitHub generates the notes from the
    # remote branch, so any divergence puts commits in the release PR that the
    # changelog never saw.  Being ahead is as wrong as being behind.
    remote_ref = f"{args.remote}/{args.base}"
    if branch == args.base:
        behind, ahead = git("rev-list", "--left-right", "--count",
                            f"{remote_ref}...HEAD").split()
        if (behind, ahead) != ("0", "0"):
            remedy = ("push or reset them first" if ahead != "0" else "pull first")
            require(f"local {args.base} differs from {remote_ref} "
                    f"({behind} behind, {ahead} ahead); {remedy}")

    repo = gh("repo", "view", "--json", "nameWithOwner",
              "-q", ".nameWithOwner").strip()
    info(f"target {S.accent(repo)} {S.dim('base')} {args.base}")
    return repo


# ---------------------------------------------------------------------------
# Step 2: version bump
# ---------------------------------------------------------------------------


def rewrite_version_files(version: str) -> dict[str, tuple[str, str]]:
    """Apply the version substitutions.  Returns {path: (before, after)}."""
    edits: dict[str, tuple[str, str]] = {}
    for path, pattern in VERSION_FILES:
        before = open(path, encoding="utf-8").read()
        after, count = re.subn(pattern, r"\g<1>" + version + r"\g<2>",
                               before, flags=re.M)
        if count != 1:
            raise Abort(f"{path}: expected 1 version line, matched {count} "
                        f"(pattern {pattern!r})")
        edits[path] = (before, after)

    # setup.py's install_requires pin is commented out because we are not on
    # PyPI; the old bash script rewrote it. Make sure it stayed commented.
    setup = edits["setup.py"][1]
    for line in setup.splitlines():
        if "install_requires" in line and not line.lstrip().startswith("#"):
            warning("setup.py: install_requires is no longer commented out; "
                    "it may need the version too")
    return edits


def show_diff(edits: dict[str, tuple[str, str]]) -> None:
    for path, (before, after) in edits.items():
        for line in difflib.unified_diff(
            before.splitlines(), after.splitlines(),
            fromfile=path, tofile=path, n=0, lineterm="",
        ):
            if line.startswith("+++") or line.startswith("---"):
                print("  " + S.dim(line))
            elif line.startswith("@@"):
                print("  " + S.accent2(line))
            elif line.startswith("+"):
                print("  " + S.ok(line))
            elif line.startswith("-"):
                print("  " + S.err(line))


def do_bump(version: str, args: argparse.Namespace) -> None:
    edits = rewrite_version_files(version)

    if DRY_RUN:
        show_diff(edits)
        info(S.dim(f"would run: dune build {OPAM_FILE}"))
        info(S.dim(f"would commit: {BUMP_COMMIT_MSG}"))
        return

    for path, (_, after) in edits.items():
        open(path, "w", encoding="utf-8").write(after)

    with spinner(f"regenerating {OPAM_FILE}"):
        run(["dune", "build", OPAM_FILE])

    changed = set(git("diff", "--name-only").splitlines())
    expected = {p for p, _ in VERSION_FILES} | {OPAM_FILE}
    if changed != expected:
        raise Abort("unexpected set of modified files after the bump:\n"
                    f"  changed:  {sorted(changed)}\n"
                    f"  expected: {sorted(expected)}")

    numstat = git("diff", "--numstat", "--", OPAM_FILE).split()
    if numstat[:2] != ["1", "1"]:
        raise Abort(
            f"{OPAM_FILE} changed by more than the version line "
            f"(+{numstat[0]}/-{numstat[1]}): dune-project has drifted from the "
            f"committed opam file. Regenerate and commit that separately first.")

    show_diff(edits)
    print()

    commit = ["git", "commit", "-m", BUMP_COMMIT_MSG, "--"] + sorted(expected)
    if args.no_verify:
        commit.insert(2, "--no-verify")
    run(["git", "add", "--"] + sorted(expected))
    run(commit)
    good(f"committed {S.accent(BUMP_COMMIT_MSG)}")


# ---------------------------------------------------------------------------
# Step 3: changelog
# ---------------------------------------------------------------------------


def github_notes(repo: str, tag: str, prev_tag: str | None, target: str) -> str:
    """GitHub's own generated release notes: the same source as the manual step."""
    cmd = ["gh", "api", "--method", "POST",
           f"repos/{repo}/releases/generate-notes",
           "-f", f"tag_name={tag}",
           "-f", f"target_commitish={target}"]
    if prev_tag:
        cmd += ["-f", f"previous_tag_name={prev_tag}"]
    return json.loads(run(cmd).stdout)["body"]


def pr_numbers(notes: str) -> list[int]:
    # GitHub's generated notes always reference pull requests by full URL; the
    # '#123' form only appears inside a pull request's own title, so it is used
    # as a fallback and never mixed in.
    found = {int(n) for n in re.findall(r"/pull/(\d+)", notes)}
    if not found:
        found = {int(n) for n in re.findall(r"(?<![\w/])#(\d+)", notes)}
    return sorted(found)


def fetch_pr(number: int) -> dict:
    proc = run(["gh", "pr", "view", str(number), "--json",
                "number,title,body,author,url,files"], check=False)
    if proc.returncode != 0:
        raise Abort(f"gh pr view {number} failed:\n"
                    + (proc.stderr or proc.stdout).strip())
    return json.loads(proc.stdout)


def fetch_dossier(numbers: list[int]) -> list[dict]:
    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
        return sorted(pool.map(fetch_pr, numbers), key=lambda d: d["number"])


def format_dossier(prs: list[dict], notes: str) -> str:
    chunks = []
    for pr in prs:
        author = (pr.get("author") or {}).get("login", "unknown")
        body = (pr.get("body") or "").strip()
        if len(body) > 4000:
            body = body[:4000] + "\n[...truncated...]"
        files = [f["path"] for f in (pr.get("files") or [])][:40]
        chunks.append(
            f"### PR #{pr['number']} — {pr['title']}\n"
            f"author: @{author}\n"
            f"files: {', '.join(files) if files else '(unknown)'}\n"
            f"description:\n{body or '(empty)'}\n"
        )
    return (
        "GitHub's own generated notes, for the exact set of pull requests and "
        "for the New Contributors list:\n\n"
        f"{notes}\n\n"
        "---\n\nDossier:\n\n" + "\n".join(chunks)
    )


def sanitize_sections(text: str) -> str:
    """Strip anything the model may have added around the sections themselves."""
    text = text.strip()
    if text.startswith("```"):
        lines = text.splitlines()
        lines = lines[1:]
        while lines and not lines[-1].startswith("```"):
            lines.pop()
        text = "\n".join(lines[:-1]) if lines else ""
    keep = [
        line for line in text.splitlines()
        if not line.startswith("## [")
        and not line.startswith("**Full Changelog**")
        and line.strip() != "# Changelog"
    ]
    # Collapse runs of blank lines, and keep the bullets of a section
    # contiguous: the model sometimes puts a blank line between every bullet.
    lines = [line.rstrip() for line in keep]
    out: list[str] = []
    for i, line in enumerate(lines):
        if not line.strip():
            if not out or not out[-1].strip():
                continue
            following = next((l for l in lines[i + 1:] if l.strip()), "")
            if out[-1].startswith("* ") and following.startswith("* "):
                continue
        out.append(line)
    return "\n".join(out).strip()


# Existing bullets average ~75 characters and top out around 140; well past that
# the model has written a paragraph rather than an entry.
LONG_BULLET = 220


def warn_long_bullets(sections: str) -> None:
    long = [line for line in sections.splitlines()
            if line.startswith("* ") and len(line) > LONG_BULLET]
    if long:
        warning(f"{len(long)} bullet(s) run past {LONG_BULLET} characters; "
                f"consider regenerating or shortening them")


def validate_sections(sections: str, authors: set[str]) -> None:
    """Confine the model's output to the shape of a changelog entry.

    Claude has no write tools, but what it returns is committed and pushed, so the
    output is checked rather than trusted: with untrusted pull request text in the
    prompt, this bounds what injected instructions could get as far as a public PR.
    It is a backstop for the review prompt, not a replacement for it.
    """
    stray = [line for line in sections.splitlines()
             if line.strip() and not line.startswith(("### ", "* "))]
    if stray:
        raise Abort("the generated changelog has lines that are neither a section "
                    "heading nor a bullet, so it was discarded:\n  "
                    + "\n  ".join(stray[:5]))

    unknown = {line[4:].strip() for line in sections.splitlines()
               if line.startswith("### ")} - KNOWN_SECTIONS
    if unknown:
        warning("unexpected section(s): " + ", ".join(sorted(unknown)))

    cited = set(re.findall(r"@([A-Za-z0-9][\w-]*)", sections)) - authors
    if cited:
        warning("credits an author not among the merged pull requests: "
                + ", ".join(sorted(cited)))


def sections_with_claude(version: str, prev_tag: str | None, dossier: str,
                         model: str | None, authors: set[str]) -> str:
    cmd = ["claude", "-p", "--output-format", "text",
           "--permission-prompts", "none",
           "--allowed-tools", CLAUDE_ALLOWED_TOOLS,
           "--append-system-prompt", CHANGELOG_SYSTEM_PROMPT]
    if model:
        cmd += ["--model", model]

    prompt = (
        f"Write the CHANGELOG sections for Opengrep {version}"
        + (f", covering everything merged since {prev_tag}." if prev_tag else ".")
        + "\n\nUse the dossier below. Read diffs with `gh pr diff <number>` for any "
          "pull request whose description does not tell you what actually changed."
          "\n\n" + dossier
    )

    with spinner("writing the changelog with Claude Code"):
        proc = subprocess.run(cmd, input=prompt, text=True,
                              capture_output=True, timeout=CLAUDE_TIMEOUT_S)
    if proc.returncode != 0:
        raise Abort("claude failed:\n" + (proc.stderr or proc.stdout).strip())
    sections = sanitize_sections(proc.stdout)
    if not sections:
        raise Abort("claude returned no changelog text")
    validate_sections(sections, authors)
    warn_long_bullets(sections)
    return sections


def sections_from_github(notes: str) -> str:
    """Fallback: reshape GitHub's flat list into the house sections."""
    improvements: list[str] = []
    fixes: list[str] = []
    contributors: list[str] = []
    in_contributors = False

    for line in notes.splitlines():
        if line.startswith("##"):
            in_contributors = "new contributor" in line.lower()
            continue
        # "* " only: '**Full Changelog**' is bold markup, not a bullet.
        if not line.startswith("* "):
            continue
        bullet = re.sub(r"https://github\.com/[^/\s]+/[^/\s]+/pull/(\d+)",
                        r"#\1", line).rstrip()
        if in_contributors:
            contributors.append(bullet)
        elif re.search(r"^\*\s*(fix|bugfix)\b", bullet, re.I) or " fix" in bullet.lower():
            fixes.append(bullet)
        else:
            improvements.append(bullet)

    blocks = []
    for title, bullets in (("Improvements", improvements),
                           ("Bug fixes", fixes),
                           ("New Contributors", contributors)):
        if bullets:
            blocks.append(f"### {title}\n\n" + "\n".join(bullets))
    if not blocks:
        raise Abort("no pull requests found in GitHub's generated notes")
    return "\n\n".join(blocks)


def build_entry(repo: str, version: str, sections: str,
                prev_tag: str | None) -> str:
    tag = f"v{version}"
    date = datetime.date.today().strftime("%d-%m-%Y")
    heading = (f"## [{version}](https://github.com/{repo}/releases/tag/{tag})"
               f" - {date}")
    if prev_tag:
        full = (f"**Full Changelog**: "
                f"https://github.com/{repo}/compare/{prev_tag}...{tag}")
    else:
        full = f"**Full Changelog**: https://github.com/{repo}/commits/{tag}"
    return f"{heading}\n\n{sections}\n\n{full}"


def insert_entry(changelog: str, entry: str) -> str:
    """Insert below '# Changelog', with two blank lines before the old entry."""
    marker = "# Changelog\n"
    if not changelog.startswith(marker):
        raise Abort(f"{CHANGELOG} does not start with '# Changelog'")
    rest = changelog[len(marker):].lstrip("\n")
    return f"{marker}\n{entry}\n\n\n{rest}"


def extract_entry(changelog: str) -> str:
    """Read back the newest entry, so hand edits flow into the PR body."""
    starts = [m.start() for m in re.finditer(r"^## \[", changelog, re.M)]
    if not starts:
        raise Abort(f"no '## [version]' entry found in {CHANGELOG}")
    end = starts[1] if len(starts) > 1 else len(changelog)
    return changelog[starts[0]:end].strip()


def render_entry(entry: str) -> None:
    for line in entry.splitlines():
        if line.startswith("## "):
            print("  " + S.accent(line, bold=True))
            continue
        if line.startswith("### "):
            print("  " + S.accent2(line, bold=True))
            continue
        if line.startswith("**Full Changelog**"):
            print("  " + S.dim(line))
            continue
        out = re.sub(r"(#\d+)", lambda m: S.warn(m.group(1)), line)
        out = re.sub(r"(@[\w][\w-]*)", lambda m: S.accent2(m.group(1)), out)
        out = re.sub(r"(`[^`]+`)", lambda m: S.accent(m.group(1)), out)
        if out.startswith("*"):
            out = S.dim("•") + out[1:]
        print("  " + out)


def open_editor(path: str) -> None:
    editor = os.environ.get("VISUAL") or os.environ.get("EDITOR") or "vi"
    if subprocess.run([*editor.split(), path]).returncode != 0:
        raise Abort(f"{editor} exited non-zero; leaving {path} as it stands")


def split_existing_entry(changelog: str, version: str) -> tuple[str, str | None]:
    """Detach an entry this run already wrote, so a resumed run revises it
    instead of inserting a second one.  Returns (changelog without it, entry)."""
    if not re.match(rf"^# Changelog\n\n## \[{re.escape(version)}\]", changelog):
        return changelog, None
    entry = extract_entry(changelog)
    rest = changelog.split(entry, 1)[1].lstrip("\n")
    return f"# Changelog\n\n{rest}", entry


def do_changelog(repo: str, version: str, prev_tag: str | None,
                 notes: str, args: argparse.Namespace) -> str:
    use_claude = have("claude") and not args.no_claude
    dossier, authors = "", set()
    if use_claude:
        numbers = pr_numbers(notes)
        with spinner(f"fetching {len(numbers)} pull requests"):
            prs = fetch_dossier(numbers)
        dossier = format_dossier(prs, notes)
        # Logins the entry may credit: the merged authors, plus anyone GitHub's
        # own notes name (a New Contributors line, a co-author).
        authors = {(pr.get("author") or {}).get("login", "") for pr in prs}
        authors |= set(re.findall(r"@([A-Za-z0-9][\w-]*)", notes))
        authors.discard("")
    elif args.no_claude:
        info("skipping Claude Code (--no-claude)")
    else:
        warning("claude is not on PATH; falling back to GitHub's generated notes")

    original, carried = "", None
    was_clean = False
    if not DRY_RUN:
        was_clean = not git("status", "--porcelain", "--", CHANGELOG)
        original, carried = split_existing_entry(
            open(CHANGELOG, encoding="utf-8").read(), version)
        if carried:
            info(f"{CHANGELOG} already carries a {version} entry; reviewing it")

    def restore() -> None:
        """Put CHANGELOG.md back exactly as this run found it."""
        if was_clean:
            run(["git", "checkout", "--", CHANGELOG], check=False)
        else:
            open(CHANGELOG, "w", encoding="utf-8").write(original)

    while True:
        if carried is not None:
            entry, carried = carried, None
        else:
            if use_claude:
                sections = sections_with_claude(version, prev_tag, dossier,
                                                args.claude_model, authors)
            else:
                sections = sections_from_github(notes)
                warning("wording and categorisation are unreviewed: "
                        "edit before confirming")
            entry = build_entry(repo, version, sections, prev_tag)

        if DRY_RUN:
            tmp = os.path.join(tempfile.gettempdir(),
                               f"opengrep-changelog-{version}.md")
            open(tmp, "w", encoding="utf-8").write(entry + "\n")
            print()
            render_entry(entry)
            print()
            info(S.dim(f"would insert into {CHANGELOG}; preview at {tmp}"))
            return entry

        open(CHANGELOG, "w", encoding="utf-8").write(
            insert_entry(original, entry))

        while True:
            print()
            render_entry(entry)
            print()
            choice = ask("confirm, edit, regenerate or abort?", "cera", "c")

            if choice == "c":
                run(["git", "add", "--", CHANGELOG])
                if not git("diff", "--cached", "--name-only"):
                    info(f"{CHANGELOG} is already committed unchanged")
                    return entry
                commit = ["git", "commit", "-m", CHANGELOG_COMMIT_MSG, "--",
                          CHANGELOG]
                if args.no_verify:
                    commit.insert(2, "--no-verify")
                run(commit)
                good(f"committed {S.accent(CHANGELOG_COMMIT_MSG)}")
                return entry

            if choice == "e":
                open_editor(CHANGELOG)
                entry = extract_entry(open(CHANGELOG, encoding="utf-8").read())
                continue

            if choice == "r":
                restore()
                if not use_claude:
                    raise Abort("nothing to regenerate without Claude Code; "
                                "edit by hand instead")
                break

            restore()
            raise Abort("cancelled at the changelog")


# ---------------------------------------------------------------------------
# Step 4: push and pull request
# ---------------------------------------------------------------------------


def pr_body(entry: str) -> str:
    """The entry without its version heading, at the heading level used by the
    release body, so it can be pasted straight into the GitHub release."""
    lines = entry.splitlines()
    if lines and lines[0].startswith("## ["):
        lines = lines[1:]
    body = "\n".join(lines).strip()
    return re.sub(r"^### ", "## ", body, flags=re.M)


def do_pr(version: str, entry: str, args: argparse.Namespace) -> None:
    branch = f"release-v{version}"
    title = f"Release v{version}"

    if DRY_RUN:
        info(S.dim(f"would run: git push -u {args.remote} {branch}"))
        info(S.dim(f"would run: gh pr create --base {args.base} "
                   f"--title {title!r} --body <notes>"))
        return

    with spinner(f"pushing {branch} to {args.remote}"):
        run(["git", "push", "--quiet", "-u", args.remote, branch])

    with spinner("opening the release pull request"):
        url = run(["gh", "pr", "create",
                   "--base", args.base,
                   "--head", branch,
                   "--title", title,
                   "--body", pr_body(entry)]).stdout.strip()
    print()
    field("pull request", S.accent(url.splitlines()[-1], bold=True))


# ---------------------------------------------------------------------------
# Orchestration
# ---------------------------------------------------------------------------


def ensure_branch(version: str, args: argparse.Namespace) -> bool:
    """Create or reuse release-vX.Y.Z.  Returns True if the bump is already done."""
    branch = f"release-v{version}"
    exists = run(["git", "rev-parse", "--verify", "--quiet",
                  f"refs/heads/{branch}"], check=False).returncode == 0

    if DRY_RUN:
        info(S.dim(f"would run: git switch -c {branch}"))
        return False

    if exists:
        warning(f"branch {branch} already exists")
        if ask("reuse it and continue, or abort?", "ra", "r") == "a":
            raise Abort("cancelled")
        run(["git", "switch", branch])
    else:
        run(["git", "switch", "-c", branch])
    good(f"on branch {S.accent(branch)}")

    return read_current_version() == version


def rollback(version: str, args: argparse.Namespace) -> None:
    """Offer to drop a half-finished release branch after an abort."""
    branch = f"release-v{version}"
    if DRY_RUN or git("rev-parse", "--abbrev-ref", "HEAD") != branch:
        return
    print()
    if ask(f"discard {branch} and return to {args.base}?", "yn", "y") != "y":
        return
    run(["git", "checkout", "--", "."], check=False)
    run(["git", "switch", args.base])
    run(["git", "branch", "-D", branch])
    good(f"removed {branch}")


def next_steps(version: str, args: argparse.Namespace) -> None:
    tag = f"v{version}"
    print()
    print("  " + S.accent("Remaining manual steps", bold=True))
    print()
    print(f"    {S.accent2('5')}  merge the PR, then dispatch "
          f"{S.accent('rolling-release.yml')} from {args.base} with tag "
          f"{S.accent(tag)}")
    print(f"       {S.dim(f'gh workflow run rolling-release.yml --ref {args.base} -f tag={tag}')}")
    print(f"       {S.dim('takes roughly 50 minutes')}")
    print()
    print(f"    {S.accent2('6')}  publish the draft release it creates:")
    print(f"       {S.dim(f'rename to')} {S.accent(f'Opengrep {version}')}"
          f"{S.dim(', body = the notes above,')}")
    print(f"       {S.dim('uncheck pre-release, check latest release')}")
    print()


def main() -> int:
    global S, DRY_RUN

    parser = argparse.ArgumentParser(
        description="Prepare an Opengrep release: branch, bump, changelog, PR.")
    parser.add_argument("--version", help="exact next version, e.g. 1.30.0")
    parser.add_argument("--part", choices=("major", "minor", "patch"),
                        help="bump this part of the current version")
    parser.add_argument("--base", default="main", help="base branch (default: main)")
    parser.add_argument("--remote", default="origin", help="git remote (default: origin)")
    parser.add_argument("--no-claude", action="store_true",
                        help="use GitHub's generated notes instead of Claude Code")
    parser.add_argument("--claude-model", help="model for the changelog, e.g. opus")
    parser.add_argument("--no-verify", action="store_true",
                        help="pass --no-verify to git commit")
    parser.add_argument("--dry-run", action="store_true",
                        help="show what would happen; no commits, push or PR")
    parser.add_argument("--no-color", action="store_true", help="disable colour")
    args = parser.parse_args()

    DRY_RUN = args.dry_run
    S = Style(
        not args.no_color
        and not os.environ.get("NO_COLOR")
        and sys.stdout.isatty()
    )

    banner("steps 1-4: branch · bump · changelog · pull request"
           + ("   [dry run]" if DRY_RUN else ""))

    version = ""
    try:
        step(1, 5, "Preflight")
        repo = preflight(args)

        step(2, 5, "Version")
        current = read_current_version()
        check_version_consistency(current)
        prev_tag = latest_stable_tag()

        # tag_name only feeds the compare link in the returned body, which we
        # rebuild ourselves, so one call serves both the count and the dossier.
        with spinner("reading merged pull requests"):
            notes = github_notes(repo, f"v{current}-next", prev_tag, args.base)
        count = len(pr_numbers(notes))

        version = choose_version(current, prev_tag, count, args)
        if version == current:
            raise Abort(f"{version} is already the current version")
        print()
        field("releasing", S.accent(f"v{version}", bold=True))
        if not DRY_RUN and ask("proceed?", "yn", "y") != "y":
            raise Abort("cancelled")

        already_bumped = ensure_branch(version, args)

        step(3, 5, "Bump")
        if already_bumped:
            info(f"version files already at {version}, skipping")
        else:
            do_bump(version, args)

        step(4, 5, "Changelog")
        entry = do_changelog(repo, version, prev_tag, notes, args)

        step(5, 5, "Pull request")
        do_pr(version, entry, args)

        print()
        good(S.accent(f"release v{version} prepared", bold=True))
        next_steps(version, args)
        return 0

    except Abort as exc:
        print()
        bad(str(exc))
        if version:
            with contextlib.suppress(Exception):
                rollback(version, args)
        return 1
    except KeyboardInterrupt:
        print()
        bad("interrupted")
        return 130


if __name__ == "__main__":
    sys.exit(main())
