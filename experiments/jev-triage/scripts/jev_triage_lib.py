import csv, json, re, collections

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


def candidates():
    return cands

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

