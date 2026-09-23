# Finding triage with TypeSafe Jev: experiment and design notes

Status: experiment, September 2026. Nothing here is wired into opengrep. This directory holds the
scripts, the raw model answers and the write-up so the work can be picked up later.

## Question

Can a cheap System One model (TypeSafe's Jev) usefully rank opengrep findings into true and false
positives, and how does it compare with Claude models doing the same job?

A second question, whether Jev could supply "types" for dynamically typed languages, was assessed
but not run. See the last section.

## Setup

- **Corpus**: OWASP Benchmark Java 1.2 (2740 servlet test cases, each labelled true or false for one
  CWE in `expectedresults-1.2.csv`).
- **Scanner**: opengrep 1.30.0 with the Java rules from `opengrep-rules/java`, run with
  `--json --dataflow-traces --taint-intrafile`. Output in `results/bench.json.gz`.
- **Candidate findings**: the 1785 findings whose rule CWE equals the test case's CWE. Benchmark labels
  1408 of them true and 377 false, so **opengrep alone is at 79% precision on this set**. Findings for a
  different CWE than the test's were excluded as unlabelled.
- **Judges**: every judge saw the same package per finding (rule, flagged line, dataflow trace, source
  file) and returned probabilities. All numbers below measure how well a judge *re-orders opengrep's
  output*; no judge scanned code itself.
- **Metric**: AUC (probability a random true finding outranks a random false one), plus what a 0.5 cut
  would do: false positives removed and true positives lost.
- **Splits**: a stratified sample of 371 findings (`results/mid.jsonl`, 25 per category and label) was
  used for all model comparisons and for tuning the Jev questions. The remaining 1414 findings are
  held out and were only ever scored, never looked at while designing questions.

## Results

### Models on the same 295 taint findings (the tuning sample)

| judge | AUC | FP removed at 0.5 | TP lost at 0.5 | wall time, 371 findings | cost, 371 findings |
|---|---|---|---|---|---|
| Jev 1.13, first questions (v1) | 0.741 | 62/132 | 11/164 | 26 s | $0.03 |
| Jev 1.13, v4 questions + list resolver | 0.956 | 107/132 | 0/163 | ~20 s + resolver | $0.04 |
| Claude Haiku 4.5, v1 questions | 0.771 | 90/132 | 33/163 | 78 s | $0.79 |
| Claude Opus 5, medium effort, v1 questions | 0.948 | 111/132 | 2/163 | 191 s | $5.43 |
| Claude Fable 5.1, medium effort, v1 questions | 0.950 | 115/132 | 0/163 | 299 s | $11.15 |

Per-category AUC, same findings (Jev v4 / Haiku / Opus / Fable):

| category | n | Jev v4 | Haiku | Opus | Fable |
|---|---|---|---|---|---|
| cmdi | 50 | 0.811 | 0.726 | 0.842 | 0.890 |
| ldapi | 39 | 0.841 | 0.714 | 0.936 | 0.936 |
| pathtraver | 50 | 0.931 | 0.871 | 0.953 | 0.938 |
| sqli | 49 | 0.984 | 0.760 | 1.000 | 1.000 |
| trustbound | 40 | 1.000 | 0.711 | 1.000 | 1.000 |
| xpathi | 17 | 1.000 | 0.810 | 1.000 | 1.000 |
| xss | 50 | 1.000 | 0.929 | 1.000 | 0.990 |

### Jev on the 1122 held-out taint findings (never used for tuning)

| setup | AUC | FP removed at 0.5 | TP lost at 0.5 |
|---|---|---|---|
| v1, first questions | 0.780 | 108/245 | 50/877 |
| v4, class policy + value path | 0.831 | 91/245 | 5/877 |
| v4 + generic list resolver | 0.918 | 164/245 | 5/877 |

The gap between 0.956 on the tuning sample and 0.918 held out is the overfitting cost of editing
the policy text against observed misses. **0.918 is the honest Jev number.**

### Reading the numbers

- Fable and Opus are the best pure judges and got there from the untuned first questions, by reasoning
  over the raw file. Haiku is not usable as a suppressor (33 true positives dropped).
- Jev with the v4 setup lands a few points below Opus and Fable at about 1/140 and 1/280 of their cost,
  and 10 to 15 times faster. Its accuracy came from question design and from code isolating the hard
  sub-problem, not from the model reasoning.
- Whole benchmark cost: Jev $0.18, Haiku about $3.80, Opus about $26, Fable about $54.
- Some Benchmark labels are debatable (a Base64 encode followed by decode is labelled safe, so is a
  map put then get of the same key), which caps every judge.
- Opus and Fable ran at medium effort with the v1 questions; giving them the v4 state would likely raise
  them further. Not run for cost reasons.

## What the Jev questions look like

`results/request_00200.json` is a literal request body. The API key travels in a header and is not
in the file.

### v1 (`scripts/jev_triage.py`)

State: rule, finding, source file, dataflow trace. Five Nouls: exploitable, untrusted_source,
neutralized, dangerous_use, security_sensitive. Composite score for taint classes:
`untrusted * (1 - neutralized) * dangerous`.

Findings: the single "exploitable" question is weak (AUC 0.67 on all 1785). Decomposition helps
(0.79). `dangerous_use` was badly worded for file APIs and hurt path traversal.

### v2/v4 (`scripts/jev_triage2.py`, `scripts/jev_triage4.py`)

Three changes:

1. **Class policy in the state.** Per CWE class: what neutralizes it, what does not, what a safe sink
   shape is and what an unsafe one is. The TypeSafe docs put policy in state, not in the question. v4
   adds two general facts that v2 lacked: an encode immediately followed by the matching decode is the
   identity, and placing a value in `exec`'s environment array is command injection. Classes with no safe
   sink shape (path traversal) do not get the safe-sink question.
2. **Value path.** Code extracts the source lines that mention any variable on opengrep's trace, so the
   relevant indirection is on one screen instead of buried in the file.
3. **Mechanism questions.** A Choice on what the sink actually receives (request value, constant,
   transformed-safe, unclear), and Nouls for constant-reaches-sink, neutralized-for-class and
   sink-safe-by-construction. Score: `1 - max(constant, neutralized, sink_safe)`.

The Choice was the useful addition: it labelled 59/132 false positives "constant" against 5/164 true
positives on the sample.

### The isolated list resolver (`scripts/list_micro.py`, `scripts/v4_eval.py`)

Every remaining miss after v2 was one Benchmark template: add three elements to a list, `remove(0)`,
read index 1. Inside a whole file Jev gets this wrong. Given the five operations alone and asked which
element the read returns, it was right in 80 of 80 cases. So code isolates the operation sequence and
asks one narrow question; the answer replaces the whole-file "constant" probability.

The resolver in `v4_eval.py` is generic over any Java `List` variable declared in the file: it extracts
that variable's `add/remove/get/set` sequence, offers the elements actually added as the Choice options,
and reads which of them are tainted from opengrep's trace plus the variable the source expression is
assigned to. When no added element is known to be tainted it abstains. An earlier version matched the
literal Benchmark variable name and literals; that was removed, and the held-out numbers above are from
the generic version. It is still Java-regex based and would be replaced by an IL-based extraction in a
real implementation.

## Design for an OCaml implementation

Assessment only; not started.

**Existing pieces.** `Http_helpers.post` (libs/networking) takes a body, headers and `Cap.Network.t`
and returns an Lwt result; it is what registry fetching uses. Findings are ATD records
(`semgrep_output_v1`) with JSON serializers, so `cli_match`, `dataflow_trace` and rule metadata are
available typed. Jev's request is plain JSON over HTTPS with one bearer header.

**Plug-in point.** In `Scan_subcommand`, after `Core_runner.mk_result` builds the `cli_output` and
before `Output.output_result` renders it. One pass over `cli_output.results`: build a state per
finding, issue requests with bounded Lwt concurrency, write the probabilities and the model id into
`extra.metadata` under a `triage` key. Every output format then carries them; text skins can sort or
dim by them. Suppression stays a separate explicit option; the default use is ranking.

**State builder.** This is where an engine implementation beats the prototype:

- `value_path` from the IL rather than a regex over lines: the taint engine knows the lvals on the path,
  so emitting them is a small extension of the dataflow trace.
- Collection resolver from the IL: `add/remove/get` on a tainted variable are explicit instructions, so
  "isolate the operation sequence on this collection" is a fold over the CFG, language-independent, no
  Java regex.
- Class policy as a table keyed by CWE, or better a rule-metadata field so rule authors state what
  neutralizes their sink. That keeps policy in rules, where opengrep already keeps such knowledge.

**Constraints.** Needs `Cap.network` and a `TYPESAFE_API_KEY`, so opt-in behind a flag. Results vary
across model versions, so record the model id in the output. Latency is a few hundred milliseconds per
request; the whole Benchmark took under two minutes at eight in flight. Show it as a phase in the
status bar.

**Effort.** First version (state from the existing trace, HTTP, metadata write-back, flag): a few
hundred lines in `src/osemgrep`. IL-based value path and collection resolver: engine work, best done as
an extension of the dataflow trace. Measure the first version against this harness before touching the
IL.

## The typing question (assessed, not run)

Could Jev give types to untyped code? Opengrep consumes types in three places, and all three return
`NoType` for un-annotated dynamic code: typed metavariables in `Generic_vs_generic.m_compatible_type`
(which already has an external-lookup fallback designed for an LSP oracle), taint's bool/number
dropping via `Typing.type_of_expr`, and interfile receiver resolution. Jev cannot generate a type name
but can pick one from candidates code supplies (project classes, imports, framework types, builtins,
"unknown"). Its niche would be what static inference cannot reach: duck-typed parameters and receivers
whose type comes from naming and context. Concerns: probabilistic findings, network calls inside the
engine, unknown calibration on code. A cheap first experiment: dump `$OBJ.$M(...)` call sites with
unresolved receivers from a Python repo, ask Jev a Choice over candidates, score against Pyright with
annotations stripped. Not started because the triage question was the better fit and was tried first.

## Reproducing

```
git clone --depth 1 https://github.com/OWASP-Benchmark/BenchmarkJava.git benchmark
opengrep scan --config <opengrep-rules>/java --json --dataflow-traces --taint-intrafile \
  -o bench.json benchmark/src/main/java/org/owasp/benchmark/testcode
python3 -m venv venv && ./venv/bin/pip install typesafe_sdk anthropic scikit-learn
export TYPESAFE_API_KEY=...            # and ANTHROPIC_API_KEY for the Claude runs
./venv/bin/python scripts/jev_triage.py 25 mid.jsonl          # v1, stratified sample
./venv/bin/python scripts/jev_triage4.py mid.jsonl jev4.jsonl # v4 on the same sample
./venv/bin/python scripts/v4_eval.py jev4.jsonl mid.jsonl     # scores + isolated list resolver
./venv/bin/python scripts/claude_triage.py mid.jsonl claude-opus-5 medium opus.jsonl
./venv/bin/python scripts/cmp_all.py jev4.jsonl mid.jsonl
```

The scripts expect to run from a directory containing `benchmark/` and `bench.json`
(`gunzip results/bench.json.gz`). `scripts/jev_triage_lib.py` holds the shared state builder.

## Files

- `scripts/jev_triage.py`, `jev_triage_lib.py`: v1 questions and shared state construction.
- `scripts/jev_triage2.py`, `jev_triage4.py`: v2 and v4 questions with class policy and value path.
- `scripts/list_micro.py`: the isolated list-operation test (80/80).
- `scripts/v3_eval.py`, `v4_eval.py`: scoring, tuning vs held-out split, generic list resolver.
- `scripts/claude_triage.py`: same questions answered by a Claude model via structured output.
- `scripts/jev_eval.py`, `jev_eval2.py`, `cmp_all.py`, `lost_heldout.py`: evaluation helpers.
- `results/*.jsonl`: raw answers per model (`full` = Jev v1 on all 1785, `mid` = Jev v1 on the sample,
  `jev2`, `jev4_full`, `haiku`, `opus`, `fable`).
- `results/request_00200.json`: one literal Jev request body.
- `results/bench.json.gz`: the opengrep scan the experiment started from.
