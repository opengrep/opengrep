"""Micro-test: can Jev evaluate an isolated list add/remove/get sequence?"""
import asyncio, json, re, sys, collections
from typesafe_sdk import AsyncTypeSafeClient, Choice
import jev_triage_lib as lib

wanted = {(r["test"], r["rule"], r["line"]) for r in map(json.loads, open("mid.jsonl"))}
cases = []
for test, cat, real, f in lib.candidates():
    if (test, f["check_id"].split(".")[-1], f["start"]["line"]) not in wanted: continue
    src = open(f["path"]).read()
    ops = re.findall(r"valuesList\.(add|remove|get|set)\(([^)]*)\)", src)
    if not ops: continue
    steps = [f"valuesList.{op}({arg})" for op, arg in ops]
    cases.append((test, cat, real, steps))
print(len(cases), "findings use the list template;", collections.Counter(tuple(s) for *_, s in cases).most_common(6), file=sys.stderr)

Q = {"returned": Choice(
    instructions="`operations` is a sequence of Java List operations run in order on an empty ArrayList named "
                 "valuesList. Indices are 0-based and remove(i) shifts later elements down. Which element does the "
                 "final get(...) return?",
    criteria={"safe": "the literal \"safe\"", "param": "the variable param", "moresafe": "the literal \"moresafe\"",
              "other": "something else or out of bounds"})}

async def main():
    client = AsyncTypeSafeClient(); sem = asyncio.Semaphore(8); out = []
    async def one(c):
        test, cat, real, steps = c
        async with sem:
            r = await client.system_one({"operations": steps}, Q, model="jev-latest")
        a = r.choices["returned"]; out.append((test, cat, real, tuple(steps), a.choice, dict(a.probabilities)))
    await asyncio.gather(*(one(c) for c in cases))
    return out

res = asyncio.run(main())
# ground truth by actually simulating
def simulate(steps):
    lst = []
    for s in steps:
        m = re.match(r"valuesList\.(\w+)\((.*)\)", s); op, arg = m.groups()
        if op == "add": lst.append(arg.strip('"'))
        elif op == "remove": lst.pop(int(arg))
        elif op == "get": return lst[int(arg)]
    return None
right = 0; by_seq = collections.defaultdict(lambda: [0, 0, None])
for test, cat, real, steps, choice, p in res:
    truth = simulate(steps); ok = choice == truth; right += ok
    by_seq[steps][0] += ok; by_seq[steps][1] += 1; by_seq[steps][2] = (truth, choice, {k: round(v, 2) for k, v in p.items()})
print(f"isolated list simulation: {right}/{len(res)} correct")
for seq, (ok, n, info) in by_seq.items():
    print(f"  {ok}/{n} truth={info[0]:9s} jev={info[1]:9s} {info[2]}  ops={' ; '.join(seq)}")
# how does the truth relate to the label?
agree = sum(1 for test, cat, real, steps, *_ in res if (simulate(steps) == "param") == real)
print(f"label agrees with 'returned value is param' in {agree}/{len(res)} cases")
