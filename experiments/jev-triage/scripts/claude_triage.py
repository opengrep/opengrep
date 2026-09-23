"""Same triage questions as jev_triage.py, answered by a Claude model with structured output.

usage: claude_triage.py <sample.jsonl from jev run> <model> <effort> <out.jsonl>
The sample file fixes which findings are judged so the two runs are comparable.
"""
import asyncio, json, sys, time, collections
from pydantic import BaseModel, Field
import anthropic

import jev_triage_lib as lib  # state construction shared with the Jev run

SAMPLE, MODEL, EFFORT, OUT = sys.argv[1:5]
CONC = 8

class Triage(BaseModel):
    exploitable: float = Field(ge=0, le=1, description="Probability this is a real, exploitable vulnerability of the kind the rule describes, as written.")
    untrusted_source: float = Field(ge=0, le=1, description="Probability the value reaching the flagged operation originates from attacker-controlled request input.")
    neutralized: float = Field(ge=0, le=1, description="Probability the value is neutralized for this vulnerability class before the flagged operation.")
    dangerous_use: float = Field(ge=0, le=1, description="Probability the flagged operation uses the value in a way that is dangerous for this class rather than via a safe API.")
    security_sensitive: float = Field(ge=0, le=1, description="Probability the flagged API call serves a security-sensitive purpose.")

SYSTEM = ("You triage static-analysis findings. You are given a rule, a flagged location, a dataflow trace when "
          "available, and the source file. Answer each field with a calibrated probability between 0 and 1. "
          "Judge the code as written; do not assume defenses that are not visible.")

wanted = {(r["test"], r["rule"], r["line"]) for r in map(json.loads, open(SAMPLE))}
import os
done = set()
if os.path.exists(OUT):
    done = {(r["test"], r["rule"], r["line"]) for r in map(json.loads, open(OUT)) if r}
sample = [c for c in lib.candidates() if (c[0], c[3]["check_id"].split(".")[-1], c[3]["start"]["line"]) in wanted - done]
print(f"judging {len(sample)} findings with {MODEL} effort={EFFORT}", file=sys.stderr)

async def main():
    client = anthropic.AsyncAnthropic()
    sem = asyncio.Semaphore(CONC)
    usage = collections.Counter()
    out = open(OUT, "a"); t0 = time.time()

    async def one(i, test, cat, real, f):
        st = lib.state_of(test, cat, f)
        async with sem:
            try:
                r = await client.messages.parse(
                    model=MODEL, max_tokens=4000,
                    system=SYSTEM,
                    **({"output_config": {"effort": EFFORT}} if EFFORT != "none" else {}),
                    messages=[{"role": "user", "content": json.dumps(st, indent=1)}],
                    output_format=Triage)
            except Exception as e:
                print(f"{i} {test} ERROR {e}", file=sys.stderr); return
        usage["input"] += r.usage.input_tokens; usage["output"] += r.usage.output_tokens
        a = r.parsed_output
        rec = {"test": test, "category": cat, "real": real, "rule": st["rule"]["id"],
               "line": st["finding"]["line"], "has_trace": "dataflow" in st,
               "answers": a.model_dump(), "model": r.model}
        out.write(json.dumps(rec) + "\n"); out.flush()
        if i % 25 == 0:
            print(f"{i}/{len(sample)} {time.time()-t0:.0f}s usage={dict(usage)}", file=sys.stderr)

    await asyncio.gather(*(one(i, *c) for i, c in enumerate(sample)))
    print(f"done in {time.time()-t0:.0f}s usage={dict(usage)}", file=sys.stderr)

asyncio.run(main())
