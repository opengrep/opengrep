import json,re,collections,sys
exec(open('v4_eval.py').read().split('taint = [k for k in jev')[0])
bad=[k for k,(p,t) in programs.items() if not any(e in t for e in p[2] if not e.startswith('"'))]
print("unrecognised list programs:",len(bad),"without any dataflow trace:",sum(1 for k in bad if not cands[k][0]["extra"].get("dataflow_trace")), "rules:",dict(collections.Counter(k[1] for k in bad)))
taint=[k for k in jev if jev[k]["category"] in TAINT]; held=[k for k in taint if k not in tuning]
lost=[k for k in held if cands[k][1] and s_jev_micro(k)<0.5]
print("held-out TPs lost:",len(lost))
c=collections.Counter()
for k in lost:
    a=jev[k]["answers"]; pc=p_const_micro(k)
    why="resolver" if pc is not None and pc>=0.5 else "constant_q" if a["constant_reaches_sink"]>=0.5 else "neutralized_q" if a["neutralized_for_class"]>=0.5 else "sink_safe_q"
    c[(jev[k]["category"],k[1],why, "trace" if cands[k][0]["extra"].get("dataflow_trace") else "no-trace")]+=1
for kk,v in sorted(c.items(),key=lambda x:-x[1]): print(" ",v,kk)
