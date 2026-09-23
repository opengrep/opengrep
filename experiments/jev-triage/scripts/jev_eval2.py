import json, sys
from sklearn.metrics import roc_auc_score
TAINT={"sqli","xss","cmdi","pathtraver","ldapi","xpathi","trustbound"}
v1={(r["test"],r["rule"],r["line"]):r for r in map(json.loads,open("mid.jsonl"))}
v2=[json.loads(l) for l in open(sys.argv[1]) if l.strip()]
v2=[r for r in v2 if r["category"] in TAINT]
def s_v1(r):
    a=v1[(r["test"],r["rule"],r["line"])]["answers"]; return a["untrusted_source"]*(1-a["neutralized"])*a["dangerous_use"]
def s_fpmax(r):
    a=r["answers"]; return 1-max(a["constant_reaches_sink"],a["neutralized_for_class"],a["sink_safe_by_construction"])
def s_choice(r): return r["answers"]["sink_receives_p"].get("request_value",0)
def s_combo(r): return s_fpmax(r)*s_choice(r)
def s_all(r): return s_combo(r)*r["answers"]["exploitable"]
scorers={"v1 composite":s_v1,"v2 exploitable":lambda r:r["answers"]["exploitable"],"v2 choice P(request_value)":s_choice,
         "v2 1-max(constant,neutralized,safe sink)":s_fpmax,"v2 choice*fpmax":s_combo,"v2 choice*fpmax*exploitable":s_all,
         "v2 constant alone":lambda r:1-r["answers"]["constant_reaches_sink"],"v2 neutralized alone":lambda r:1-r["answers"]["neutralized_for_class"],
         "v2 sink_safe alone":lambda r:1-r["answers"]["sink_safe_by_construction"]}
y=[int(r["real"]) for r in v2]; fp=len(y)-sum(y)
print(f"taint findings n={len(v2)} TP={sum(y)} FP={fp}")
for name,f in scorers.items():
    s=[f(r) for r in v2]
    rem=sum(1 for yy,ss in zip(y,s) if not yy and ss<0.5); lost=sum(1 for yy,ss in zip(y,s) if yy and ss<0.5)
    print(f"{name:42s} AUC={roc_auc_score(y,s):.3f}  @0.5 FP removed {rem}/{fp} TP lost {lost}/{sum(y)}")
print("\nper category AUC: v1 composite vs v2 choice*fpmax")
for c in sorted({r["category"] for r in v2}):
    rs=[r for r in v2 if r["category"]==c]; yy=[int(r["real"]) for r in rs]
    if len(set(yy))<2: continue
    print(f"  {c:11s} n={len(rs):3d}  v1={roc_auc_score(yy,[s_v1(r) for r in rs]):.3f}  v2={roc_auc_score(yy,[s_combo(r) for r in rs]):.3f}")
import collections
print("\nchoice by label:"); 
for real in (True,False): print(" real" if real else " fp  ", collections.Counter(r["answers"]["sink_receives"] for r in v2 if r["real"]==real))
