import json,sys
exec(open('v4_eval.py').read().split('taint = [k for k in jev')[0])
runs={"Haiku 4.5":"haiku.jsonl","Opus 5 medium":"opus.jsonl","Fable 5.1 medium":"fable.jsonl"}
runs={n:{key(r):r for r in map(json.loads,open(p))} for n,p in runs.items()}
def s_llm(run): return lambda k: run[k]["answers"]["untrusted_source"]*(1-run[k]["answers"]["neutralized"])*run[k]["answers"]["dangerous_use"]
taint=[k for k in jev if jev[k]["category"] in TAINT and k in tuning and all(k in r for r in runs.values())]
print(f"== {len(taint)} taint findings judged by every model")
print(report("Jev v4 + resolver",s_jev_micro,taint))
for n,r in runs.items(): print(report(n,s_llm(r),taint))
print("per category AUC: Jev v4 / Haiku / Opus / Fable")
for c in sorted({jev[k]["category"] for k in taint}):
    kk=[k for k in taint if jev[k]["category"]==c]; y=[int(cands[k][1]) for k in kk]
    if len(set(y))<2: continue
    vals=[roc_auc_score(y,[s_jev_micro(k) for k in kk])]+[roc_auc_score(y,[s_llm(r)(k) for k in kk]) for r in runs.values()]
    print(f"  {c:11s} n={len(kk):3d}  "+"  ".join(f"{v:.3f}" for v in vals))
