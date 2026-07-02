# Intrafile → interfile split divergences

The intra-file inter-procedural taint corpus under
`tests/rules/cross_function_tainting/` can be mechanically converted into
interfile tests: split each top-level function into its own file and set
`taint_interfile: true`. Because interfile is a superset of intra-file
inter-procedural analysis, the finding *should* reproduce across the file
boundary.

Cases that fail to reproduce are either **split artifacts** (the test
depends on something that cannot survive file separation) or **candidate
interfile gaps** (a real cross-file resolution limitation).

## Cross-language results

| Language | Split | Pass | Diverge |
|----------|------:|-----:|--------:|
| python | 37 | 30 | 7 |
| javascript | 17 | 15 | 2 |
| go | 11 | 11 | 0 |
| ruby | 10 | 6 | 4 |
| clojure | 11 | 10 | 1 |
| rust | 7 | 6 | 1 |
| php | 3 | 3 | 0 |
| typescript | 4 | 3 | 1 |
| kotlin | 4 | 3 | 1 |
| c | 2 | 2 | 0 |
| cpp | 2 | 1 | 1 |
| java | 4 | 4 | 0 |
| **total** | **112** | **95** | **18** |

(java is class-level split — only multi-class tests qualify; csharp produced 0
because its tests are single-class. elixir not yet converted.)

**Headline finding — cross-file HOF callbacks.** `hof_comprehensive` (callbacks
passed via dict/alias/lambda whose HOF body lives in another file) diverges in
**python, javascript, ruby, clojure, typescript** but PASSES in **go, rust, php**
(typed/named function values rather than dynamic dict/lambda callbacks). This is
the highest-confidence interfile resolution gap. `constructor_taint_bugs`
diverges in python + ruby.

Not yet converted: java/csharp/kotlin (methods live inside classes — no top-level
split), elixir (needs `defmodule` wrapping), swift/crystal/dart/ocaml (blocked by
the call-graph extraction gap).

## Python (first pass)

37 tests with ≥2 top-level functions were split. 30 reproduce the
intra-file finding interfile; 7 diverge.

### Split artifacts — not interfile gaps

Both depend on a shared mutable **module-level global**. After splitting,
each file is its own module and `global x` no longer names the same
variable, so no cross-file flow exists. Interfile reporting nothing is
correct.

| Test | Construct |
|------|-----------|
| `taint_global_lval` | `global x` written in `foo_lval`, read in `do_sink` |
| `test_guards_global_lval_python` | guard on a `global gvar` ToLval |

### Candidate interfile gaps — need a trace to confirm

The construct is separable, yet the finding disappears once the functions
are in different files. Each needs the source→sink path traced to confirm
it is a resolution gap (vs. a finer split artifact, e.g. a comprehensive
test whose top-level module code or one sub-flow is what actually broke).

| Test | Suspected cross-file limitation |
|------|----------------------------------|
| `test_hof_destructure_python` | HOF callback via dict literal/alias; HOF body in another file |
| `test_callback_paths_python` | branch-dependent dict-callback dispatch across files |
| `test_hof_comprehensive_python` | custom + builtin HOF callbacks (named + lambda) across files |
| `test_constructor_taint_bugs_python` | object built in one file, method resolved to a class in another (internal-source FN) |
| `test_guards_forwarding_chain_python` | 31-deep guard-forwarding chain, one function per file |

## Method (reproduce)

A throwaway splitter walked each `*.py` with a matching `*.yaml`, cut it
at top-level `def`/`class` boundaries into per-function files (module-level
statements → `zz_main.py`), and re-emitted the rule with
`options.taint_interfile: true`. Tests with <2 functions were skipped.
