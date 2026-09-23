"""Second Jev setup: focused evidence in the state, class-specific policy, mechanism-level questions.

usage: jev_triage2.py <sample.jsonl> <out.jsonl>
"""
import asyncio, json, re, sys, time, collections
from typesafe_sdk import AsyncTypeSafeClient, Noul, Choice
import jev_triage_lib as lib
IDENTITY = "an encoding immediately followed by the matching decoding, such as Base64 encode then decode, which leaves the value unchanged"

SAMPLE, OUT = sys.argv[1:3]
CONC = 8

wanted = {(r["test"], r["rule"], r["line"]) for r in map(json.loads, open(SAMPLE))}
sample = [c for c in lib.candidates()
          if (c[0], c[3]["check_id"].split(".")[-1], c[3]["start"]["line"]) in wanted]
print(f"judging {len(sample)} findings", file=sys.stderr)

# What the class means, what makes a value harmless for it, what a safe sink looks like.
CLASS = {
    "sqli": dict(
        name="SQL injection",
        neutralizers=["binding the value to a ? placeholder with setString/setInt/setParameter",
                      "SQL-specific escaping such as ESAPI encodeForSQL with the matching codec",
                      "allow-list validation or conversion to a number"],
        not_neutralizers=["HTML or URL encoding", "URL decoding", "case changes", "trimming", IDENTITY],
        safe_sink=["the request value is bound as a parameter of a prepared statement; the SQL text passed to "
                   "prepareStatement does not contain the value"],
        unsafe_sink=["the value is concatenated into the SQL string, even if that string is later given to "
                     "prepareStatement or a template method"]),
    "cmdi": dict(
        name="OS command injection",
        neutralizers=["allow-list validation of the value", "an encoding whose output alphabet has no shell "
                      "metacharacters and is not later decoded"],
        not_neutralizers=["HTML or URL encoding that keeps quotes, semicolons or spaces", "URL decoding", IDENTITY],
        safe_sink=["the value is one element of the argument array or list of a non-shell program, and no element "
                   "is a shell such as sh -c or cmd.exe /c that re-parses a string containing the value"],
        unsafe_sink=["the value is concatenated into a command string", "the value is concatenated into an "
                     "argument given to sh -c or cmd.exe /c",
                     "the value is placed in the environment passed to the process: the envp array of "
                     "Runtime.exec or ProcessBuilder.environment(); environment control is command injection"]),
    "xss": dict(
        name="cross-site scripting",
        neutralizers=["HTML entity encoding for HTML body context, JavaScript encoding for script context, "
                      "attribute encoding for attributes, applied right before output",
                      "the response content type is text/plain so the browser will not render markup"],
        not_neutralizers=["URL decoding", "Base64 decoding", "SQL escaping", "case changes", IDENTITY],
        safe_sink=["the encoded value is written to the response"],
        unsafe_sink=["the raw value is written to the HTML response body via a writer, print or format"]),
    "pathtraver": dict(
        name="path traversal",
        neutralizers=["reducing the value to a bare file name such as FilenameUtils.getName",
                      "allow-list validation", "canonical-path prefix check"],
        not_neutralizers=["HTML encoding", "URL decoding", "prefixing a base directory", IDENTITY],
        safe_sink=[],
        unsafe_sink=["any file API whose path contains the value; no file API shape is safe by itself"]),
    "ldapi": dict(
        name="LDAP injection",
        neutralizers=["ESAPI encodeForLDAP or encodeForDN", "allow-list validation"],
        not_neutralizers=["HTML or URL encoding", "URL decoding", IDENTITY],
        safe_sink=["the filter is built with the value as a bound argument in a filter template"],
        unsafe_sink=["the value is concatenated into the LDAP filter or DN string"]),
    "xpathi": dict(
        name="XPath injection",
        neutralizers=["ESAPI encodeForXPath", "allow-list validation", "XPath variable resolver"],
        not_neutralizers=["HTML or URL encoding", "URL decoding", IDENTITY],
        safe_sink=["the value is supplied through a variable resolver"],
        unsafe_sink=["the value is concatenated into the XPath expression"]),
    "trustbound": dict(
        name="trust boundary violation (untrusted value stored in the server session)",
        neutralizers=["validation against an allow-list before storing"],
        not_neutralizers=["encoding of any kind", IDENTITY],
        safe_sink=["only constants are stored in the session"],
        unsafe_sink=["a request-derived value is stored in the session"]),
}

def value_path(f, st):
    """Lines of the source file that mention a variable on the taint path, in file order."""
    dt = f["extra"].get("dataflow_trace")
    if not dt:
        return None
    names = set()
    def walk(entry):
        kind, payload = entry
        if kind == "CliLoc":
            return
        (_loc, _callee), inters, inner = payload
        for v in inters:
            names.add(v["content"])
        walk(inner)
    for v in dt.get("intermediate_vars", []):
        names.add(v["content"])
    if dt.get("taint_sink"):
        walk(dt["taint_sink"])
    names = {n for n in names if re.fullmatch(r"[A-Za-z_]\w*", n)}
    if not names:
        return None
    pat = re.compile(r"\b(" + "|".join(map(re.escape, names)) + r")\b")
    lines = open(f["path"], encoding="utf-8", errors="replace").read().split("\n")
    picked = [f"{i:4d}  {l.strip()}" for i, l in enumerate(lines, 1)
              if pat.search(l) and not l.strip().startswith(("//", "*", "import"))]
    return {"variables": sorted(names), "lines": picked[:60]}

def state_of(test, cat, f):
    st = lib.state_of(test, cat, f)
    st["vulnerability_class"] = CLASS.get(cat, {"name": cat})
    vp = value_path(f, st)
    if vp:
        st["value_path"] = vp
    return st

def questions_for(cat):
    q = {
        "exploitable": Noul(
            instructions="A static analysis rule flagged the operation at `finding` in `source_file`. Judging the "
                         "code as written, is this a real, exploitable instance of `vulnerability_class.name`?",
            criteria={"true": "A client controlling request input can trigger the weakness at this location.",
                      "false": "The flagged code is safe here."}),
    }
    if cat in CLASS:
        q.update({
            "sink_receives": Choice(
                instructions="Follow the value used by the flagged operation at `finding` backwards through "
                             "`value_path` and `dataflow`. Take collection indices, removals, map keys, switch "
                             "cases and conditions literally, step by step. What does the operation actually receive?",
                criteria={
                    "request_value": "The request-derived value itself, possibly decoded, trimmed, re-cased or "
                                     "encoded in a way that keeps attacker-chosen characters.",
                    "constant": "A hard-coded literal, because the step that would carry the request value is not "
                                "the one used: a fixed index after inserts or removals, a lookup with a fixed key, "
                                "a switch or condition whose outcome is fixed, or a literal assigned afterwards.",
                    "transformed_safe": "The request value after a transformation that removes or escapes every "
                                        "character that could change the meaning of the operation for this class.",
                    "unclear": "The code does not show enough to tell."}),
            "constant_reaches_sink": Noul(
                instructions="Does the flagged operation at `finding` receive a hard-coded constant rather than the "
                             "request value, because of how indices, removals, keys, branches or reassignments in "
                             "`value_path` are arranged? Work through each collection or branch operation literally.",
                criteria={"true": "The request value is stored or computed but the element actually used at the sink is a literal.",
                          "false": "The request value, or something derived from it, is what the sink uses."}),
            "neutralized_for_class": Noul(
                instructions="Before the flagged operation at `finding`, is the request value made harmless for "
                             "`vulnerability_class.name` by a step listed in `vulnerability_class.neutralizers`? "
                             "Steps in `vulnerability_class.not_neutralizers` do not count.",
                criteria={"true": "An effective neutralizer for this class is applied on the path to the sink.",
                          "false": "No effective neutralizer for this class is applied; unrelated encodings or decodings do not count."}),
        })
        if CLASS[cat]["safe_sink"]:
            q["sink_safe_by_construction"] = Noul(
                instructions="Is the flagged operation at `finding` itself safe for `vulnerability_class.name` "
                             "regardless of the value, matching `vulnerability_class.safe_sink` rather than "
                             "`vulnerability_class.unsafe_sink`?",
                criteria={"true": "The sink API is used in the safe shape for this class.",
                          "false": "The sink API is used in an unsafe shape, or the shape does not matter for this class."})
    return q

async def main():
    client = AsyncTypeSafeClient()
    sem = asyncio.Semaphore(CONC)
    usage = collections.Counter()
    out = open(OUT, "w"); t0 = time.time()

    async def one(i, test, cat, real, f):
        st = state_of(test, cat, f)
        async with sem:
            try:
                r = await client.system_one(st, questions_for(cat), model="jev-latest")
            except Exception as e:
                print(f"{i} {test} ERROR {e}", file=sys.stderr); return
        usage["input"] += r.usage.input_tokens; usage["output"] += r.usage.output_tokens
        answers = {k: v.noul for k, v in r.nouls.items()}
        for k, v in r.choices.items():
            answers[k] = v.choice
            answers[k + "_p"] = dict(v.probabilities)
        rec = {"test": test, "category": cat, "real": real, "rule": st["rule"]["id"],
               "line": st["finding"]["line"], "has_trace": "dataflow" in st,
               "answers": answers, "model": r.model}
        out.write(json.dumps(rec) + "\n"); out.flush()

    await asyncio.gather(*(one(i, *c) for i, c in enumerate(sample)))
    print(f"done in {time.time()-t0:.0f}s usage={dict(usage)}", file=sys.stderr)

asyncio.run(main())
