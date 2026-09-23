"""Score a Jev run with a generic isolated list resolver, on the tuning sample and on held-out findings.

usage: v4_eval.py <jev results.jsonl covering all candidates> <tuning sample.jsonl>
"""
import json, re, asyncio, sys
from sklearn.metrics import roc_auc_score
from typesafe_sdk import AsyncTypeSafeClient, Choice
import jev_triage_lib as lib

TAINT = {"sqli", "xss", "cmdi", "pathtraver", "ldapi", "xpathi", "trustbound"}
key = lambda r: (r["test"], r["rule"], r["line"])
jev = {key(r): r for r in map(json.loads, open(sys.argv[1]))}
tuning = {key(r) for r in map(json.loads, open(sys.argv[2]))}
v1 = {key(r): r for r in map(json.loads, open("full.jsonl"))}
cands = {(t, f["check_id"].split(".")[-1], f["start"]["line"]): (f, real) for t, c, real, f in lib.candidates()}

# ---- generic list resolver: any java.util.List variable whose element operations appear in the file
LIST_DECL = re.compile(r"List<\w+>\s+(\w+)\s*=\s*new\s+[\w.]*(?:ArrayList|LinkedList)")
def list_program(f):
    src = open(f["path"]).read()
    for var in LIST_DECL.findall(src):
        ops = re.findall(rf"\b{re.escape(var)}\.(add|remove|get|set|clear)\(([^)]*)\)", src)
        if any(op == "get" for op, _ in ops):
            steps = [f"{var}.{op}({arg.strip()})" for op, arg in ops]
            elems = [arg.strip() for op, arg in ops if op == "add"]
            return var, steps, elems
    return None

def taint_names(f):
    dt = f["extra"].get("dataflow_trace") or {}
    names = {v["content"] for v in dt.get("intermediate_vars", [])}
    def walk(e):
        kind, payload = e
        if kind == "CliCall":
            (_l, _c), inters, inner = payload
            names.update(v["content"] for v in inters); walk(inner)
    if dt.get("taint_sink"): walk(dt["taint_sink"])
    # the variable the source expression is assigned to, read from the source line itself
    src_entry = dt.get("taint_source")
    if src_entry and src_entry[0] == "CliLoc":
        loc, _code = src_entry[1]
        line = open(f["path"]).read().split("\n")[loc["start"]["line"] - 1]
        m = re.search(r"\b(\w+)\s*=[^=]", line)
        if m: names.add(m.group(1))
    return {n for n in names if re.fullmatch(r"\w+", n)}

programs = {}
for k, (f, real) in cands.items():
    if k in jev and jev[k]["category"] in TAINT:
        p = list_program(f)
        if p: programs[k] = (p, taint_names(f))
no_tainted = sum(1 for (var, steps, elems), t in programs.values() if not any(e in t for e in elems if not e.startswith('"')))
print(f"list programs: {len(programs)}, with no recognised tainted element: {no_tainted}")

def describe(e):
    return f'the literal {e}' if e.startswith('"') else f"the value of the variable {e}"

async def resolve():
    client = AsyncTypeSafeClient(); cache = {}
    for k, ((var, steps, elems), _) in programs.items():
        sig = tuple(steps)
        if sig in cache: continue
        crit = {f"e{i}": describe(e) for i, e in enumerate(dict.fromkeys(elems))}
        crit["other"] = "something else, or the read is out of bounds"
        q = Choice(instructions=f"`operations` is a sequence of Java List operations run in order on `{var}`, an empty "
                                f"ArrayList. Indices are 0-based and remove(i) shifts later elements down. Which element "
                                f"does the final get(...) return?", criteria=crit)
        r = await client.system_one({"operations": steps}, {"returned": q}, model="jev-latest")
        elems_u = list(dict.fromkeys(elems))
        cache[sig] = {elems_u[int(name[1:])]: p for name, p in r.choices["returned"].probabilities.items() if name != "other"}
    return cache
resolved = asyncio.run(resolve())

def p_const_micro(k):
    if k not in programs: return None
    (var, steps, elems), tainted = programs[k]
    probs = resolved[tuple(steps)]
    if not any(e in tainted for e in elems if not e.startswith('"')):
        return None  # no element is known to be tainted: nothing to resolve, abstain
    p_tainted = sum(p for e, p in probs.items() if not e.startswith('"') and e in tainted)
    return 1 - p_tainted

def s_v1(k):
    a = v1[k]["answers"]; return a["untrusted_source"] * (1 - a["neutralized"]) * a["dangerous_use"]
def s_jev(k):
    a = jev[k]["answers"]
    return 1 - max(a["constant_reaches_sink"], a["neutralized_for_class"], a.get("sink_safe_by_construction", 0))
def s_jev_micro(k):
    a = jev[k]["answers"]; pc = p_const_micro(k)
    if pc is None: pc = a["constant_reaches_sink"]
    return 1 - max(pc, a["neutralized_for_class"], a.get("sink_safe_by_construction", 0))

def report(name, f, ks):
    y = [int(cands[k][1]) for k in ks]; s = [f(k) for k in ks]; fp = len(y) - sum(y)
    rem = sum(1 for yy, ss in zip(y, s) if not yy and ss < 0.5); lost = sum(1 for yy, ss in zip(y, s) if yy and ss < 0.5)
    return f"  {name:30s} AUC={roc_auc_score(y, s):.3f}  @0.5 FP removed {rem}/{fp}  TP lost {lost}/{sum(y)}"

taint = [k for k in jev if jev[k]["category"] in TAINT]
for split, ks in (("TUNING sample", [k for k in taint if k in tuning]), ("HELD-OUT", [k for k in taint if k not in tuning])):
    print(f"== {split}: n={len(ks)}, list programs found: {sum(1 for k in ks if k in programs)}")
    print(report("Jev v1", s_v1, ks)); print(report("Jev v4 policy only", s_jev, ks)); print(report("Jev v4 + generic list resolver", s_jev_micro, ks))
    print("  per category AUC (v1 / v4 / v4+resolver):")
    for c in sorted({jev[k]["category"] for k in ks}):
        kk = [k for k in ks if jev[k]["category"] == c]; y = [int(cands[k][1]) for k in kk]
        if len(set(y)) < 2: continue
        print(f"    {c:11s} n={len(kk):3d}  {roc_auc_score(y,[s_v1(k) for k in kk]):.3f} / {roc_auc_score(y,[s_jev(k) for k in kk]):.3f} / {roc_auc_score(y,[s_jev_micro(k) for k in kk]):.3f}")
