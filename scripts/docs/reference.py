#!/usr/bin/env python3
"""Maintain the opengrep reference documentation in docs/reference.

  reference.py inventory [--undocumented]
      Print the user-visible surface found in the binary and the sources.
  reference.py index [--stamp]
      Regenerate the generated blocks of every page. --stamp also records the
      version of bin/opengrep and the current commit as the documented state.
  reference.py check [--only ID] [--actual] [--no-examples] [--strict]
      Check coverage, cross-references, links, generated blocks, the version
      stamp, and run the examples against bin/opengrep.

The documentation is the only store: each entry page starts with a metadata
comment (described in docs/reference/README.md) and everything else is derived
from those comments, the binary and the sources.
"""

import argparse
import difflib
import fnmatch
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
DOCS = REPO / "docs" / "reference"
BIN = REPO / "bin" / "opengrep"

# kind -> (directory of the entry pages, id prefix, index page)
KINDS = {
    "command": ("commands", "cmd-", "commands.md"),
    "flag": ("flags", "flag-", "flags.md"),
    "rule-key": ("rule-syntax", "key-", "rule-syntax.md"),
    "option": ("rule-options", "opt-", "rule-options.md"),
    "env": ("env", "env-", "environment.md"),
}
PREFIX_KIND = {prefix: kind for kind, (_, prefix, _) in KINDS.items()}

# The inventory of these kinds is found heuristically, so an entry that the
# inventory does not know is a warning, not an error.
LOOSE_KINDS = {"rule-key", "env"}

CONSOLE_LANGS = {"console", "shell-session"}


def kind_of_id(id_):
    return next((k for p, k in PREFIX_KIND.items() if id_.startswith(p)), None)


def name_of_id(id_):
    kind = kind_of_id(id_)
    rest = id_[len(KINDS[kind][1]):] if kind else id_
    return "--" + rest if kind == "flag" else rest


# ---------------------------------------------------------------------------
# Inventory
# ---------------------------------------------------------------------------


def run_bin(*args):
    return subprocess.run(
        [str(BIN), *args], capture_output=True, text=True, check=False
    ).stdout


def inventory_commands():
    src = (REPO / "src/osemgrep/cli/CLI.ml").read_text()
    block = re.search(r"let known_subcommands =\s*\[(.*?)\]", src, re.S).group(1)
    return re.findall(r'"([a-z-]+)"', block)


def parse_help_options(text):
    """(names, first doc line) of each option on a --help=plain page."""
    lines = text.splitlines()
    sections = {"OPTIONS", "COMMON OPTIONS"}
    section = None
    items = []
    for i, line in enumerate(lines):
        if re.fullmatch(r"[A-Z][A-Z ]+", line):
            section = line
        elif section in sections and re.match(r" {7}-", line):
            spec = re.sub(r"\s+\((absent|default)[^)]*\)$", "", line.strip())
            names = [
                tok.split("=")[0].split("[")[0].split(" ")[0]
                for tok in spec.split(", ")
            ]
            doc = next((l.strip() for l in lines[i + 1:] if l.strip()), "")
            items.append((names, doc))
    return items


def inventory_flags():
    """flag id -> {"names": set of spellings, "commands": set of commands}"""
    flags = {}
    for cmd in inventory_commands():
        for names, doc in parse_help_options(run_bin(cmd, "--help=plain")):
            longs = [n for n in names if n.startswith("--")]
            if not longs or longs[0] == "--help":
                continue
            negated = re.match(r"negates (\S+)", doc)
            main = (
                next(n for n in negated.group(1).split("/") if n.startswith("--"))
                if negated
                else longs[0]
            )
            entry = flags.setdefault(
                "flag-" + main[2:], {"names": set(), "commands": set()}
            )
            entry["names"].update(names)
            entry["commands"].add(cmd)
    return flags


def inventory_options():
    atd = (REPO / "interfaces/Rule_options.atd").read_text()
    atd = re.sub(r"\(\*.*?\*\)", "", atd, flags=re.S)
    body = re.search(r"type t = \{(.*?)\n\}", atd, re.S).group(1)
    options = []
    for m in re.finditer(r"[~?](\w+)((?:\s*<[^>]*>)*)\s*:", body):
        json_name = re.search(r'<json name="([^"]+)">', m.group(2))
        options.append("opt-" + (json_name.group(1) if json_name else m.group(1)))
    return options


def strip_ocaml_comments(src):
    innermost = re.compile(r"\(\*(?:(?!\(\*|\*\)).)*\*\)", re.S)
    while True:
        stripped = innermost.sub(" ", src)
        if stripped == src:
            return src
        src = stripped


RULE_PARSERS = ["src/parsing/Parse_rule.ml", "src/parsing/Parse_rule_formula.ml"]


def inventory_rule_keys():
    keys = set()
    for f in RULE_PARSERS:
        src = strip_ocaml_comments((REPO / f).read_text())
        keys.update(re.findall(
            r'\b(?:take_key|take_opt|dict_take_opt)\b[^"]{0,300}?"([a-z][a-z0-9-]*)"', src
        ))
        keys.update(re.findall(r'^\s*\|\s*"([a-z][a-z0-9-]*)"', src, re.M))
    return ["key-" + k for k in sorted(keys)]


ENV_READ = re.compile(
    r'(?:getenv\w*|env_opt|in_env|env_or|Env\.info|is_true|\bget)\b[^"\n]*?"([A-Z][A-Z0-9_]+)"'
    r'|~env:\s*"([A-Z][A-Z0-9_]+)"'
    r'|"([A-Z]+_PROXY)"'
)
ENV_LIST = re.compile(r"(?:~envs|_from_env_vars)\s*:\s*\[([^\]]*)\]")
# opengrep-core is internal; tests only read their own variables.
ENV_SKIP = re.compile(r"(^|/)(tests?|core_cli|testo[^/]*)/|(^|/)(Unit|Test)_[^/]*$")


def canonical_env(name):
    # every SEMGREP_* variable is also read as OPENGREP_*, which wins
    return name.replace("SEMGREP", "OPENGREP")


def inventory_env():
    files = subprocess.run(
        ["git", "ls-files", "src/*.ml", "libs/*.ml"],
        cwd=REPO, capture_output=True, text=True, check=True,
    ).stdout.split()
    names = set()
    for f in files:
        if ENV_SKIP.search(f):
            continue
        src = strip_ocaml_comments((REPO / f).read_text(errors="replace"))
        names.update(n for m in ENV_READ.finditer(src) for n in m.groups() if n)
        for m in ENV_LIST.finditer(src):
            names.update(re.findall(r'"([A-Z][A-Z0-9_]+)"', m.group(1)))
    return sorted({"env-" + canonical_env(n) for n in names})


def inventory():
    """id -> {"kind", "name", "commands"?, "names"?}"""
    items = {}
    for cmd in inventory_commands():
        items["cmd-" + cmd] = {"kind": "command"}
    for id_, info in inventory_flags().items():
        items[id_] = {"kind": "flag", **info}
    for id_ in inventory_options():
        items[id_] = {"kind": "option"}
    for id_ in inventory_rule_keys():
        items[id_] = {"kind": "rule-key"}
    for id_ in inventory_env():
        items[id_] = {"kind": "env"}
    for id_, info in items.items():
        info["name"] = name_of_id(id_)
    return items


# ---------------------------------------------------------------------------
# Pages
# ---------------------------------------------------------------------------

META = re.compile(r"\A<!-- reference\n(.*?)\n-->\n", re.S)


def parse_meta(text):
    m = META.match(text)
    if not m:
        return None
    meta = {}
    for line in m.group(1).splitlines():
        key, _, value = line.partition(":")
        value = value.strip()
        if value.startswith("[") and value.endswith("]"):
            meta[key.strip()] = [v.strip() for v in value[1:-1].split(",") if v.strip()]
        else:
            meta[key.strip()] = value
    return meta


class Page:
    def __init__(self, path):
        self.path = path
        self.text = path.read_text()
        self.meta = parse_meta(self.text) or {}

    @property
    def id(self):
        return self.meta.get("id")

    def covers(self):
        return [self.id, *self.meta.get("covers", [])] if self.id else []


def load_pages(errors):
    entries = {}
    for kind, (directory, prefix, _) in KINDS.items():
        for path in sorted((DOCS / directory).glob("*.md")):
            page = Page(path)
            rel = path.relative_to(DOCS)
            missing = [k for k in ("id", "kind", "name", "summary") if not page.meta.get(k)]
            if missing:
                errors.append(f"{rel}: metadata lacks {', '.join(missing)}")
                continue
            if page.meta["kind"] != kind or not page.id.startswith(prefix):
                errors.append(f"{rel}: kind/id do not fit the directory {directory}/")
            if page.id in entries:
                errors.append(f"{rel}: duplicate id {page.id}")
            entries[page.id] = page
    internal = Page(DOCS / "internal.md")
    return entries, internal


def all_doc_files():
    return sorted(DOCS.rglob("*.md"))


# ---------------------------------------------------------------------------
# Generated blocks
# ---------------------------------------------------------------------------


def block_re(name):
    return re.compile(
        rf"(<!-- BEGIN GENERATED: {name} -->\n).*?(<!-- END GENERATED: {name} -->)", re.S
    )


def replace_block(text, name, content):
    return block_re(name).sub(lambda m: m.group(1) + content + m.group(2), text)


def rel_link(from_path, to_path):
    return os.path.relpath(to_path, from_path.parent)


def ref(from_path, id_, entries, inv):
    """A link to the entry of [id_], or its plain name if it has no page."""
    if id_ in entries:
        page = entries[id_]
        return f"[`{page.meta['name']}`]({rel_link(from_path, page.path)})"
    for page in entries.values():
        if id_ in page.meta.get("covers", []):
            return f"[`{name_of_id(id_)}`]({rel_link(from_path, page.path)})"
    return f"`{inv[id_]['name'] if id_ in inv else name_of_id(id_)}`"


def related_ids(page, entries):
    """[page]'s related entries, in both directions."""
    back = [p.id for p in entries.values() if page.id in p.meta.get("related", [])]
    seen = []
    for id_ in [*page.meta.get("related", []), *back]:
        if id_ not in seen and id_ != page.id:
            seen.append(id_)
    return seen


def facts_block(page, entries, inv):
    meta = page.meta
    link = lambda id_: ref(page.path, id_, entries, inv)
    code = lambda xs: ", ".join(f"`{x}`" for x in xs)
    lines = []
    if meta.get("commands"):
        lines.append(("Accepted by", ", ".join(link("cmd-" + c) for c in meta["commands"])))
    if meta.get("aliases"):
        lines.append(("Also spelled", code(meta["aliases"])))
    if meta.get("value"):
        lines.append(("Value", meta["value"]))
    if meta.get("default"):
        lines.append(("Default", f"`{meta['default']}`"))
    if meta.get("env"):
        lines.append(("Environment", ", ".join(link(e) for e in meta["env"])))
    sets = [p.id for p in entries.values() if page.id in p.meta.get("env", [])]
    if sets:
        lines.append(("Equivalent to", ", ".join(link(f) for f in sets)))
    shown = set(meta.get("env", [])) | set(sets)
    see_also = [i for i in related_ids(page, entries) if i not in shown]
    if see_also:
        lines.append(("See also", ", ".join(link(i) for i in see_also)))
    return "".join(f"- **{k}:** {v}\n" for k, v in lines)


def command_flags_block(page, entries, inv):
    cmd = page.meta["name"].split()[-1]
    flag_ids = sorted(
        (i for i, info in inv.items() if info["kind"] == "flag" and cmd in info["commands"]),
        key=lambda i: i[len("flag-"):],
    )
    rows = [
        f"| {ref(page.path, i, entries, inv)} | "
        f"{entries[i].meta['summary'] if i in entries else '*not yet documented*'} |"
        for i in flag_ids
    ]
    return "| Flag | Summary |\n|---|---|\n" + "".join(r + "\n" for r in rows)


def index_block(kind, index_path, entries, internal, inv):
    pages = sorted(
        (p for p in entries.values() if p.meta["kind"] == kind),
        key=lambda p: p.meta["name"].lstrip("-").lower(),
    )
    link = lambda id_: ref(index_path, id_, entries, inv)
    codes = lambda names: ", ".join(f"`{n}`" for n in names)
    if kind == "flag":
        header = "| Flag | Accepted by | Environment | Summary |\n|---|---|---|---|\n"
        row = lambda p: (
            f"| {link(p.id)} | {', '.join(p.meta.get('commands', []))} | "
            f"{codes(name_of_id(e) for e in p.meta.get('env', []))} | {p.meta['summary']} |"
        )
    elif kind == "env":
        header = "| Variable | Equivalent flag | Summary |\n|---|---|---|\n"
        flags_of = lambda p: [
            q.meta["name"] for q in entries.values() if p.id in q.meta.get("env", [])
        ]
        row = lambda p: f"| {link(p.id)} | {codes(flags_of(p))} | {p.meta['summary']} |"
    else:
        header = "| Name | Summary |\n|---|---|\n"
        row = lambda p: f"| {link(p.id)} | {p.meta['summary']} |"
    text = header + "".join(row(p) + "\n" for p in pages)
    missing = undocumented(inv, entries, internal, kind)
    if missing:
        names = ", ".join(f"`{inv[i]['name']}`" for i in missing)
        text += f"\nNot yet documented ({len(missing)}): {names}\n"
    return text


def read_stamp():
    m = re.search(
        r"opengrep (\S+)\*\* \(commit `(\w+)`\)", (DOCS / "README.md").read_text()
    )
    return (m.group(1), m.group(2)) if m else (None, None)


def stamp_block(version, commit):
    return f"> Reference for **opengrep {version}** (commit `{commit}`).\n"


def generate(entries, internal, inv, stamp):
    """path -> regenerated text, for every page with generated blocks."""
    out = {}
    for page in entries.values():
        text = replace_block(page.text, "facts", facts_block(page, entries, inv))
        if page.meta["kind"] == "command":
            text = replace_block(text, "flags", command_flags_block(page, entries, inv))
        out[page.path] = text
    for kind, (_, _, index) in KINDS.items():
        path = DOCS / index
        text = replace_block(path.read_text(), "index", index_block(kind, path, entries, internal, inv))
        out[path] = replace_block(text, "stamp", stamp_block(*stamp))
    readme = DOCS / "README.md"
    out[readme] = replace_block(readme.read_text(), "stamp", stamp_block(*stamp))
    return out


# ---------------------------------------------------------------------------
# Coverage
# ---------------------------------------------------------------------------


def covered_patterns(entries, internal):
    return [c for p in entries.values() for c in p.covers()] + internal.meta.get("covers", [])


def undocumented(inv, entries, internal, kind=None):
    patterns = covered_patterns(entries, internal)
    return [
        i for i, info in sorted(inv.items())
        if (kind is None or info["kind"] == kind)
        and not any(fnmatch.fnmatchcase(i, pat) for pat in patterns)
    ]


# ---------------------------------------------------------------------------
# Examples
# ---------------------------------------------------------------------------


def parse_info(info):
    attrs = dict(re.findall(r'(\w+)="([^"]*)"', info))
    words = re.sub(r'\w+="[^"]*"', "", info).split()
    return (words[0] if words else ""), attrs, set(words[1:])


def examples(page):
    """(title, blocks) for each ### section under '## Examples'."""
    result = []
    in_examples, fence, current = False, None, None
    for line in page.text.splitlines():
        if fence is not None:
            if line.startswith("```"):
                current[1].append(fence)
                fence = None
            else:
                fence[1].append(line)
        elif line.startswith("```"):
            if in_examples and current:
                fence = (line[3:].strip(), [])
        elif line.startswith("## "):
            in_examples = line.strip() == "## Examples"
        elif line.startswith("### ") and in_examples:
            current = (line[4:].strip(), [])
            result.append(current)
    return result


def parse_console(lines):
    """[command, expected output lines] pairs of a console block."""
    cmds = []
    for line in lines:
        if line.startswith("$ "):
            cmds.append([line[2:], []])
        elif cmds and cmds[-1][0].endswith("\\") and not cmds[-1][1]:
            cmds[-1][0] += "\n" + line
        elif cmds:
            cmds[-1][1].append(line)
    return cmds


def binary_version():
    if not hasattr(binary_version, "cached"):
        binary_version.cached = run_bin("--version").strip()
    return binary_version.cached


def normalize(text, work):
    """Output lines with what differs between runs and releases replaced:
    the example's directory by <tmp>, the version by X.Y.Z, log times by
    [00.00]. Trailing spaces and surrounding blank lines are dropped."""
    for path in sorted({str(work), str(work.resolve())}, key=len, reverse=True):
        text = text.replace(path, "<tmp>")
    text = text.replace(binary_version(), "X.Y.Z")
    text = re.sub(r"\[\d{2}\.\d{2}\]", "[00.00]", text)
    lines = [l.rstrip() for l in text.splitlines()]
    while lines and not lines[0]:
        lines.pop(0)
    while lines and not lines[-1]:
        lines.pop()
    return lines


ENV_DROP = re.compile(
    r"^(OPENGREP_|SEMGREP_|PYTEST_|LOG_|GIT_|GITHUB_|GITLAB_|CI_|CIRCLE|BUILDKITE|"
    r"TRAVIS|JENKINS|BITBUCKET_|NO_COLOR$|COLUMNS$|XDG_CONFIG_HOME$|CI$)"
)


def example_env(home):
    env = {k: v for k, v in os.environ.items() if not ENV_DROP.match(k)}
    env.update(
        HOME=str(home),
        PATH=f"{BIN.parent}{os.pathsep}{os.environ.get('PATH', '')}",
        GIT_CONFIG_GLOBAL=os.devnull,
        GIT_CONFIG_NOSYSTEM="1",
        GIT_AUTHOR_NAME="Example",
        GIT_AUTHOR_EMAIL="example@example.com",
        GIT_COMMITTER_NAME="Example",
        GIT_COMMITTER_EMAIL="example@example.com",
    )
    return env


def run_example(page, title, blocks, show_actual):
    """Failure messages of one example; empty when it passes."""
    failures = []
    with tempfile.TemporaryDirectory() as tmp:
        work, home = Path(tmp) / "work", Path(tmp) / "home"
        work.mkdir()
        home.mkdir()
        env = example_env(home)
        for info, body in blocks:
            lang, attrs, flags = parse_info(info)
            if lang in CONSOLE_LANGS:
                if "no-check" in flags:
                    continue
                for cmd, expected in parse_console(body):
                    r = subprocess.run(
                        ["bash", "-c", cmd], cwd=work, env=env,
                        capture_output=True, text=True, timeout=300,
                    )
                    actual = normalize(r.stdout, work)
                    if show_actual:
                        print(f"--- {page.id} / {title}\n$ {cmd}")
                        print("\n".join(actual))
                    if actual != normalize("\n".join(expected), work):
                        diff = "\n".join(difflib.unified_diff(
                            normalize("\n".join(expected), work), actual,
                            "expected", "actual", lineterm="",
                        ))
                        stderr = "\n".join(r.stderr.splitlines()[-5:])
                        failures.append(f"$ {cmd}\n{diff}\n(stderr tail)\n{stderr}")
            elif "title" in attrs:
                f = work / attrs["title"]
                f.parent.mkdir(parents=True, exist_ok=True)
                f.write_text("\n".join(body) + "\n")
    return failures


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------


def cmd_inventory(args):
    inv = inventory()
    shown = inv
    if args.undocumented:
        entries, internal = load_pages([])
        shown = {i: inv[i] for i in undocumented(inv, entries, internal)}
    for id_, info in sorted(shown.items(), key=lambda kv: (kv[1]["kind"], kv[0])):
        cmds = f"  [{','.join(sorted(info['commands']))}]" if "commands" in info else ""
        print(f"{info['kind']:9} {info['name']}{cmds}")
    return 0


def cmd_index(args):
    errors = []
    entries, internal = load_pages(errors)
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    stamp = read_stamp()
    if args.stamp:
        commit = subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            cwd=REPO, capture_output=True, text=True, check=True,
        ).stdout.strip()
        stamp = (run_bin("--version").strip(), commit)
    for path, text in generate(entries, internal, inventory(), stamp).items():
        if path.read_text() != text:
            path.write_text(text)
            print(f"updated {path.relative_to(REPO)}")
    return 0


def cmd_check(args):
    errors, warnings = [], []
    entries, internal = load_pages(errors)
    inv = inventory()
    known = set(inv) | {c for p in entries.values() for c in p.covers()}

    # coverage, in both directions
    for id_ in undocumented(inv, entries, internal):
        (errors if args.strict else warnings).append(f"undocumented: {inv[id_]['kind']} {inv[id_]['name']}")
    for page in entries.values():
        for id_ in page.covers():
            if id_ not in inv:
                msg = f"{page.path.relative_to(DOCS)}: {id_} is not in the inventory"
                (warnings if kind_of_id(id_) in LOOSE_KINDS else errors).append(msg)

    # metadata against the inventory, and cross-references
    for page in entries.values():
        rel = page.path.relative_to(DOCS)
        if page.meta["kind"] == "flag" and page.id in inv:
            documented = set(page.meta.get("commands", []))
            if documented != inv[page.id]["commands"]:
                errors.append(f"{rel}: commands {sorted(documented)} but the binary says {sorted(inv[page.id]['commands'])}")
        for field in ("related", "env"):
            for id_ in page.meta.get(field, []):
                if id_ not in known:
                    errors.append(f"{rel}: {field} names unknown id {id_}")
                elif id_ not in entries and not any(id_ in p.meta.get("covers", []) for p in entries.values()):
                    warnings.append(f"{rel}: {field} names undocumented {id_}")
        for marker in ["facts"] + (["flags"] if page.meta["kind"] == "command" else []):
            if not block_re(marker).search(page.text):
                errors.append(f"{rel}: no generated block '{marker}'")

    # links
    for path in all_doc_files():
        for target in re.findall(r"\]\(([^)#\s]+)(?:#[^)]*)?\)", path.read_text()):
            if not re.match(r"[a-z]+:", target) and not (path.parent / target).exists():
                errors.append(f"{path.relative_to(DOCS)}: broken link {target}")

    # generated blocks and stamp
    stamp = read_stamp()
    if not errors:
        stale = [p for p, t in generate(entries, internal, inv, stamp).items() if p.read_text() != t]
        if stale:
            errors.append("generated blocks are out of date, run: scripts/docs/reference.py index")
    version = run_bin("--version").strip()
    if stamp[0] != version:
        errors.append(f"docs are stamped for opengrep {stamp[0]} but bin/opengrep is {version}")
    source_version = re.search(r'let version = "([^"]+)"', (REPO / "src/core/Version.ml").read_text()).group(1)
    if source_version != version:
        warnings.append(f"bin/opengrep is {version} but src/core/Version.ml says {source_version}: rebuild with make")

    # examples
    n_examples = 0
    if not args.no_examples:
        for page in entries.values():
            if args.only and page.id != args.only:
                continue
            for title, blocks in examples(page):
                n_examples += 1
                for failure in run_example(page, title, blocks, args.actual):
                    errors.append(f"{page.path.relative_to(DOCS)}: example '{title}' failed\n{failure}")

    for w in warnings:
        print(f"warning: {w}")
    for e in errors:
        print(f"error: {e}")
    print(f"{len(entries)} entries, {n_examples} examples run, "
          f"{len(errors)} errors, {len(warnings)} warnings")
    return 1 if errors else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    p = sub.add_parser("inventory")
    p.add_argument("--undocumented", action="store_true")
    p.set_defaults(func=cmd_inventory)
    p = sub.add_parser("index")
    p.add_argument("--stamp", action="store_true")
    p.set_defaults(func=cmd_index)
    p = sub.add_parser("check")
    p.add_argument("--only", metavar="ID", help="run the examples of this entry only")
    p.add_argument("--actual", action="store_true", help="print the output of every example command")
    p.add_argument("--no-examples", action="store_true")
    p.add_argument("--strict", action="store_true", help="undocumented items are errors")
    p.set_defaults(func=cmd_check)
    args = parser.parse_args()
    if not BIN.exists():
        sys.exit(f"{BIN} not found: build opengrep with make first")
    sys.exit(args.func(args))


if __name__ == "__main__":
    main()
