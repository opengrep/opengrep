import json, re, asyncio, collections, sys
JEVFILE = sys.argv[1] if len(sys.argv) > 1 else "jev2.jsonl"
from sklearn.metrics import roc_auc_score
from typesafe_sdk import AsyncTypeSafeClient, Choice
import jev_triage_lib as lib
TAINT={"sqli","xss","cmdi","pathtraver","ldapi","xpathi","trustbound"}
key=lambda r:(r["test"],r["rule"],r["line"])
v1={key(r):r for r in map(json.loads,open("mid.jsonl"))}
v2={key(r):r for r in map(json.loads,open(JEVFILE))}
haiku={key(r):r for r in map(json.loads,open("haiku.jsonl"))}
opus={key(r):r for r in map(json.loads,open("opus.jsonl"))}
fable={key(r):r for r in map(json.loads,open("fable.jsonl"))}
cands={(t,f["check_id"].split(".")[-1],f["start"]["line"]):f for t,c,real,f in lib.candidates()}
# isolated list resolution (same question as list_micro.py), cached per op sequence
Q={"returned":Choice(instructions="`operations` is a sequence of Java List operations run in order on an empty ArrayList named valuesList. Indices are 0-based and remove(i) shifts later elements down. Which element does the final get(...) return?",
   criteria={"safe":"the literal \"safe\"","param":"the variable param","moresafe":"the literal \"moresafe\"","other":"something else or out of bounds"})}
seqs={}
for k,f in cands.items():
    if k in v2:
        ops=re.findall(r"valuesList\.(add|remove|get|set)\(([^)]*)\)",open(f["path"]).read())
        if ops: seqs[k]=tuple(f"valuesList.{o}({a})" for o,a in ops)
async def resolve():
    client=AsyncTypeSafeClient(); out={}
    for s in set(seqs.values()):
        r=await client.system_one({"operations":list(s)},Q,model="jev-latest"); out[s]=r.choices["returned"].probabilities
    return out
p_by_seq=asyncio.run(resolve())
def p_const_micro(k):
    s=seqs.get(k); return None if s is None else 1-p_by_seq[s].get("param",0)
def s_v1(k):
    a=v1[k]["answers"]; return a["untrusted_source"]*(1-a["neutralized"])*a["dangerous_use"]
def s_v2(k):
    a=v2[k]["answers"]; return (1-max(a["constant_reaches_sink"],a["neutralized_for_class"],a.get("sink_safe_by_construction", 0)))*a["sink_receives_p"].get("request_value",0)
def s_v3(k):
    a=v2[k]["answers"]; pc=p_const_micro(k)
    if pc is None: pc=a["constant_reaches_sink"]
    return 1-max(pc,a["neutralized_for_class"],a.get("sink_safe_by_construction", 0))
def s_llm(run): return lambda k: run[k]["answers"]["untrusted_source"]*(1-run[k]["answers"]["neutralized"])*run[k]["answers"]["dangerous_use"]
keys=[k for k in v2 if v2[k]["category"] in TAINT]
def report(name,f,ks):
    y=[int(v2[k]["real"]) for k in ks]; s=[f(k) for k in ks]; fp=len(y)-sum(y)
    rem=sum(1 for yy,ss in zip(y,s) if not yy and ss<0.5); lost=sum(1 for yy,ss in zip(y,s) if yy and ss<0.5)
    return f"{name:34s} n={len(ks)}  AUC={roc_auc_score(y,s):.3f}  @0.5 FP removed {rem}/{fp}  TP lost {lost}/{sum(y)}"
print("== taint findings, same sample")
print(report("Jev v1 (first questions)",s_v1,keys))
print(report("Jev v2 (class policy + value path)",s_v2,keys))
print(report("Jev v3 (v2 + isolated list resolver)",s_v3,keys))
hk=[k for k in keys if k in haiku]; print(report("Haiku 4.5",s_llm(haiku),hk))
print(report("Opus 5 medium",s_llm(opus),keys))
fk=[k for k in keys if k in fable]; print(report("Fable 5.1 medium (partial)",s_llm(fable),fk)); print(report("Jev v3 on Fable's subset",s_v3,fk)); print(report("Opus 5 on Fable's subset",s_llm(opus),fk))
print("\n== per category AUC")
print(f"{'category':11s} {'v1':>6s} {'v2':>6s} {'v3':>6s} {'Haiku':>6s} {'Opus':>6s}")
for c in sorted({v2[k]["category"] for k in keys}):
    ks=[k for k in keys if v2[k]["category"]==c]; y=[int(v2[k]["real"]) for k in ks]
    if len(set(y))<2: continue
    row=f"{c:11s}"
    for f in (s_v1,s_v2,s_v3,s_llm(haiku),s_llm(opus)):
        kk=[k for k in ks if f is not s_llm or True]
        try: row+=f"{roc_auc_score(y,[f(k) for k in ks]):6.3f}"
        except KeyError: row+=f"{'-':>6s}"
    print(row)
