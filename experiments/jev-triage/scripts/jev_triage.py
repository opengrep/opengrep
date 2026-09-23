"""Ask Jev to triage opengrep findings on OWASP Benchmark and score it against the labels.

usage: jev_triage.py <per_cell> <out.jsonl>
  per_cell: max findings sampled per (category, label) cell; 0 = all
"""
import asyncio, csv, json, random, re, sys, time, collections

from typesafe_sdk import AsyncTypeSafeClient, Noul

random.seed(7)
PER_CELL = int(sys.argv[1]); OUT = sys.argv[2]
CONC = 8

bench = json.load(open("bench.json"))
labels = {r[0]: (r[1], r[2] == "true", r[3])
          for r in csv.reader(open("benchmark/expectedresults-1.2.csv"))
          if r and not r[0].startswith("#")}

def cwes_of(f):
    c = f["extra"].get("metadata", {}).get("cwe", [])
    c = [c] if isinstance(c, str) else c
    return [m.group(1) for m in (re.match(r"CWE-(\d+)", x) for x in c) if m]

cands = []
for f in bench["results"]:
    m = re.search(r"(BenchmarkTest\d+)\.java", f["path"])
    if not m:
        continue
    cat, real, cwe = labels[m.group(1)]
    if cwe in cwes_of(f):
        cands.append((m.group(1), cat, real, f))

cells = collections.defaultdict(list)
for c in cands:
    cells[(c[1], c[2])].append(c)
sample = []
for k, v in sorted(cells.items()):
    random.shuffle(v)
    sample += v if PER_CELL == 0 else v[:PER_CELL]
print(f"candidates {len(cands)}, sampled {len(sample)}", file=sys.stderr)

_file_cache = {}
def source_text(path):
    if path not in _file_cache:
        lines = open(path, encoding="utf-8", errors="replace").read().split("\n")
        # drop the license header and imports; keep line numbers stable
        out = []
        for i, l in enumerate(lines, 1):
            s = l.strip()
            if s.startswith(("import ", "package ", "/*", "*", "*/")) or not s:
                continue
            out.append(f"{i:4d}  {l}")
        _file_cache[path] = "\n".join(out)
    return _file_cache[path]

def loc_text(entry):
    # ["CliLoc", [loc, content]] or ["CliCall", [[loc, callee], intermediates, inner]]
    kind, payload = entry
    if kind == "CliLoc":
        loc, content = payload
        return {"line": loc["start"]["line"], "code": content}
    (loc, callee), inters, inner = payload
    return {"call": {"line": loc["start"]["line"], "code": callee},
            "intermediate_variables": [
                {"line": v["location"]["start"]["line"], "code": v["content"]} for v in inters],
            "then": loc_text(inner)}

def state_of(test, cat, f):
    x = f["extra"]
    st = {
        "rule": {"id": f["check_id"].split(".")[-1],
                 "message": x["message"],
                 "cwe": x.get("metadata", {}).get("cwe")},
        "finding": {"line": f["start"]["line"], "code": x["lines"].strip()},
        "source_file": source_text(f["path"]),
    }
    dt = x.get("dataflow_trace")
    if dt:
        st["dataflow"] = {
            "source": loc_text(dt["taint_source"]) if dt.get("taint_source") else None,
            "intermediate_variables": [
                {"line": v["location"]["start"]["line"], "code": v["content"]}
                for v in dt.get("intermediate_vars", [])],
            "sink": loc_text(dt["taint_sink"]) if dt.get("taint_sink") else None,
        }
    return st

QUESTIONS = {
    "exploitable": Noul(
        instructions="A static analysis rule flagged the code at `finding` in `source_file` "
                     "(see `rule.message`). Is this a real, exploitable vulnerability of the "
                     "kind the rule describes, as the code is written?",
        criteria={"true": "An attacker who controls request input can trigger the weakness the rule describes at this location.",
                  "false": "The flagged code is safe here: the input is not attacker-controlled, it is neutralized before use, or the API is used safely."}),
    "untrusted_source": Noul(
        instructions="Does the value that reaches the flagged operation at `finding` originate from "
                     "attacker-controlled input such as HTTP parameters, headers, cookies, or request body?",
        criteria={"true": "The value derives from request data an external client can set.",
                  "false": "The value is a constant, hard-coded, generated internally, or otherwise not settable by a client."}),
    "neutralized": Noul(
        instructions="Before the value reaches the flagged operation at `finding`, is it neutralized for this "
                     "vulnerability class: validated against an allow-list, correctly encoded or escaped for the sink, "
                     "passed through parameterization, or replaced by a safe constant?",
        criteria={"true": "A step on the path to the sink makes the attack the rule describes impossible.",
                  "false": "The value reaches the sink without an effective defence for this vulnerability class; unrelated transformations do not count."}),
    "dangerous_use": Noul(
        instructions="Does the flagged operation at `finding` use the value in a way that is dangerous for this "
                     "vulnerability class, for example concatenating it into a query, command, path, or response body, "
                     "rather than through a safe API such as a prepared statement with bound parameters?",
        criteria={"true": "The sink API usage would let an attacker-controlled value change the meaning of the operation.",
                  "false": "The sink API usage is safe by construction regardless of the value."}),
    "security_sensitive": Noul(
        instructions="Is the flagged API call at `finding` used for a security-sensitive purpose, such as generating "
                     "tokens, session identifiers, keys, passwords, or making a security decision, rather than a "
                     "cosmetic or non-security purpose?",
        criteria={"true": "The result influences authentication, authorization, secrecy, or integrity.",
                  "false": "The result is used for something where predictability or weakness does not matter."}),
}

async def main():
    client = AsyncTypeSafeClient()
    sem = asyncio.Semaphore(CONC)
    usage = collections.Counter()
    out = open(OUT, "w")
    t0 = time.time()

    async def one(i, test, cat, real, f):
        st = state_of(test, cat, f)
        async with sem:
            try:
                r = await client.system_one(st, QUESTIONS, model="jev-latest")
            except Exception as e:
                print(f"{i} {test} ERROR {e}", file=sys.stderr)
                return
        usage["input"] += r.usage.input_tokens if hasattr(r.usage, "input_tokens") else 0
        usage["output"] += r.usage.output_tokens if hasattr(r.usage, "output_tokens") else 0
        rec = {"test": test, "category": cat, "real": real, "rule": st["rule"]["id"],
               "line": st["finding"]["line"], "has_trace": "dataflow" in st,
               "answers": {k: v.noul for k, v in r.nouls.items()}, "model": r.model}
        out.write(json.dumps(rec) + "\n"); out.flush()
        if i % 25 == 0:
            print(f"{i}/{len(sample)} {time.time()-t0:.0f}s usage={dict(usage)}", file=sys.stderr)

    await asyncio.gather(*(one(i, *c) for i, c in enumerate(sample)))
    print(f"done in {time.time()-t0:.0f}s usage={dict(usage)}", file=sys.stderr)

asyncio.run(main())
