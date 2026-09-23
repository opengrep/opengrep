"""Score Jev triage answers against Benchmark labels. usage: jev_eval.py <results.jsonl>"""
import json, sys, collections
from sklearn.metrics import roc_auc_score, average_precision_score

rows = [json.loads(l) for l in open(sys.argv[1])]
TAINT = {"sqli", "xss", "cmdi", "pathtraver", "ldapi", "xpathi", "trustbound"}

def composite(r):
    a = r["answers"]
    if r["category"] in TAINT:
        return a["untrusted_source"] * (1 - a["neutralized"]) * a["dangerous_use"]
    return a["security_sensitive"]

def report(name, rs, key):
    y = [int(r["real"]) for r in rs]
    s = [key(r) for r in rs]
    if len(set(y)) < 2:
        return f"{name:12s} n={len(rs):4d} TP={sum(y):4d}  (single class)"
    auc = roc_auc_score(y, s); ap = average_precision_score(y, s)
    # if we drop everything below 0.5: how many real TPs lost, how many FPs removed
    lost = sum(1 for yy, ss in zip(y, s) if yy and ss < 0.5)
    removed = sum(1 for yy, ss in zip(y, s) if not yy and ss < 0.5)
    fps = len(y) - sum(y)
    return (f"{name:12s} n={len(rs):4d} TP={sum(y):4d} FP={fps:4d}  AUC={auc:.3f} AP={ap:.3f}  "
            f"@0.5: FP removed {removed}/{fps}, TP lost {lost}/{sum(y)}")

print("== single 'exploitable' question")
print(report("all", rows, lambda r: r["answers"]["exploitable"]))
for cat in sorted({r["category"] for r in rows}):
    print(report(cat, [r for r in rows if r["category"] == cat], lambda r: r["answers"]["exploitable"]))
print("\n== composite of atomic questions")
print(report("all", rows, composite))
for cat in sorted({r["category"] for r in rows}):
    print(report(cat, [r for r in rows if r["category"] == cat], composite))
print("\n== each atomic question alone (taint categories)")
tr = [r for r in rows if r["category"] in TAINT]
for q, sign in (("untrusted_source", 1), ("neutralized", -1), ("dangerous_use", 1), ("exploitable", 1)):
    print(report(q, tr, lambda r, q=q, sign=sign: sign * r["answers"][q]))
print("\n== mean answer by label (taint categories)")
for real in (True, False):
    rs = [r for r in tr if r["real"] == real]
    means = {q: sum(r["answers"][q] for r in rs) / len(rs) for q in rs[0]["answers"]}
    print("real" if real else "fp  ", {k: round(v, 2) for k, v in means.items()})
