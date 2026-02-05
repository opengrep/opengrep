# Opengrep perf scripts

## Quickstart

**Benchmark your changes against a release:**
```bash
./bench-against-sha.sh v1.15.0 --repo jellyfin:3
```

**Compare opengrep vs semgrep:**
```bash
./run-benchmarks.sh --reference-binary ~/.local/bin/opengrep --binary semgrep --repo jellyfin:3
```

**Re-generate reports from existing results:**
```bash
./generate-reports.sh                    # latest results
./generate-reports.sh bench_results/...  # specific directory
```

Results are saved to `bench_results/<timestamp>/` with timing, findings, and stability reports.

## Scripts

### `bench-against-sha.sh`

Benchmark current HEAD against a reference commit (typically a release tag). The reference is built once and cached; HEAD is always rebuilt.

```bash
./bench-against-sha.sh <reference-sha> [bench.sh options]
```

Examples:
```bash
./bench-against-sha.sh v1.15.0 --repo jellyfin:1
./bench-against-sha.sh v1.14.0 --repo jellyfin:3 --taint-rules-only
./bench-against-sha.sh v1.15.0 --dry-run --repo jellyfin:1
```

Workflow:
1. First run: checks out reference, builds it, returns to HEAD, builds HEAD, benchmarks
2. Subsequent runs: reference is cached, just builds HEAD in place (no checkout needed)

Binaries are cached in `binaries/<label>/venv/bin/opengrep`.

Unstaged changes are allowed when the reference is cached (no checkout required).

### `bench.sh`

Core benchmarking script using [hyperfine](https://github.com/sharkdp/hyperfine). Compares two binaries (reference vs comparison).

```bash
./bench.sh --reference-binary <path> --binary <path> [options]
```

Options:
- `--reference-binary PATH` - Reference opengrep binary (required; `~/.local/bin/opengrep` is a common location)
- `--binary PATH` - Binary to compare (required)
- `--binary-variant TYPE` - Type of binary: `opengrep` or `semgrep` (auto-detected if binary is `semgrep` or ends with `/semgrep`)
- `--repo NAME[:RUNS]` - Benchmark specific repo (can be repeated)
- `--taint-rules-only` - Use only taint rules
- `--no-taint-rules` - Use only non-taint (search) rules
- `--intrafile` - Enable intrafile taint analysis
- `--max-time MINS` - Hard timeout per command (default: 15)
- `--dry-run` - Print commands without executing
- `-- ARGS` - Pass additional arguments to both tools (e.g., `-- --time`)

Examples:
```bash
# Compare two opengrep binaries
./bench.sh --reference-binary ~/.local/bin/opengrep --binary /path/to/other/opengrep --repo jellyfin:3

# Compare opengrep vs semgrep (variant auto-detected from binary name)
./bench.sh --reference-binary ~/.local/bin/opengrep --binary semgrep --repo jellyfin:1

# Pass extra arguments to both tools (e.g., --time for detailed timing)
./bench.sh --reference-binary ~/.local/bin/opengrep --binary semgrep --repo jellyfin:1 -- --time
```

Results are saved to timestamped directories under `bench_results/`.

### `run-benchmarks.sh`

Comprehensive benchmark suite. Prepares repos and rules, then runs benchmarks for multiple configurations:
- `no_intrafile_taint/` - taint rules without intrafile
- `no_intrafile_search/` - search (non-taint) rules
- `intrafile_taint/` - taint rules with intrafile analysis

```bash
./run-benchmarks.sh --binary <path> [--binary-variant semgrep] [bench.sh options]
```

Examples:
```bash
# Compare two opengrep binaries
./run-benchmarks.sh --reference-binary ~/.local/bin/opengrep --binary /path/to/other/opengrep --repo jellyfin:1

# Compare opengrep vs semgrep (variant auto-detected from binary name)
./run-benchmarks.sh --reference-binary ~/.local/bin/opengrep --binary semgrep --repo jellyfin:1
```

### `compare-outputs.sh`

Compare benchmark output files and produce a summary table. Uses `opengrep-diff` to compare findings between the reference and comparison binaries.

```bash
./compare-outputs.sh [options]
```

Options:
- `--output-dir DIR` - Directory containing output files (default: `.`)
- `--variant TYPE` - Comparison variant: `opengrep` or `semgrep` (auto-detected if not specified)
- `--intrafile` - Match files with `_intrafile` suffix
- `--experimental` - Match files with `_experimental` suffix

Environment variables:
- `TOLERANCE` - Line tolerance for proximity matching (default: 8)

Examples:
```bash
# Compare results in a benchmark output directory (auto-detects variant)
./compare-outputs.sh --output-dir bench_results/20260128_143052

# Compare intrafile results
./compare-outputs.sh --output-dir bench_results/20260128_143052/intrafile_taint --intrafile

# Explicitly specify comparison variant
./compare-outputs.sh --output-dir bench_results/20260128_143052 --variant semgrep
```

Output is a table showing findings comparison per repository:
```
Repo                    Reference  Semgrep  Exact  Proximity  Only Ref  Only Semgrep
-----                   ---------- -------- ------ ---------- --------- ------------
ComfyUI-Custom-Scripts  5          5        5      0          0         0
gitlab                  268        266      259    6          3         1
grafana                 240        237      219    17         4         1
jellyfin                5          5        5      0          0         0
leakcanary              0          0        0      0          0         0
lemur                   4          4        4      0          0         0
pmd                     12         12       12     0          0         0
```

### `calculate-stability.sh`

Analyze determinism/stability of benchmark runs. Compares multiple runs of the same tool on the same repo to detect non-deterministic behavior.

```bash
./calculate-stability.sh [options] <directory>
```

Options:
- `--intrafile` - Match files with `_intrafile` suffix
- `--experimental` - Match files with `_experimental` suffix

Examples:
```bash
# Analyze stability in a benchmark output directory
./calculate-stability.sh bench_results/20260128_143052

# Analyze intrafile results
./calculate-stability.sh --intrafile bench_results/20260128_143052/intrafile_taint
```

Output is a statistical table showing finding counts across runs:
```
Stability Analysis
==================
Directory: bench_results/20260128_213555
Suffix filter: _experimental

Tool            Repo                    Runs  Mean  Std  Min  Max  CV%
----            ----                    ----  ----  ---  ---  ---  ---
opengrep (ref)  ComfyUI-Custom-Scripts  3     5.0   0.0  5    5    0.00%
opengrep (ref)  jellyfin                3     5.0   0.0  5    5    0.00%
opengrep (ref)  leakcanary              3     0.0   0.0  0    0    0.00%
opengrep (ref)  lemur                   3     4.0   0.0  4    4    0.00%
opengrep (ref)  pmd                     3     12.0  0.0  12   12   0.00%
semgrep         ComfyUI-Custom-Scripts  3     5.0   0.0  5    5    0.00%
semgrep         jellyfin                3     5.0   0.0  5    5    0.00%
semgrep         leakcanary              3     0.0   0.0  0    0    0.00%
semgrep         lemur                   3     4.0   0.0  4    4    0.00%
semgrep         pmd                     3     12.0  0.0  12   12   0.00%

Summary: 10/10 stable (std=0), 0/10 unstable
```

A perfectly deterministic tool has `Std=0` and `CV=0%`.

### `summarise-benchmark.sh`

Summarise timing and memory results from hyperfine JSON output.

```bash
./summarise-benchmark.sh [options] <directory>
```

Options:
- `--intrafile` - Match files with `_intrafile` suffix
- `--experimental` - Match files with `_experimental` suffix

Mode is auto-detected from `metadata.json` if not specified.

Examples:
```bash
./summarise-benchmark.sh bench_results/20260128_213555
```

Output:
```
Benchmark Summary
=================
Directory: bench_results/20260128_213555
Mode: experimental

Repo                    Command          Mean(s)  ±      Min      Max      Relative  Mem(MB)  Mem±
-----                   --------         -------- --     ----     ----     --------- -------- ----
ComfyUI-Custom-Scripts  opengrep-1.15.1  1.244    0.045  1.208    1.294    1         273      3.6
ComfyUI-Custom-Scripts  semgrep-1.148.0  1.367    0.07   1.301    1.441    1.1       278      0
jellyfin                opengrep-1.15.1  3.831    0.037  3.79     3.863    1         399      5.3
jellyfin                semgrep-1.148.0  4.2      0.099  4.1      4.298    1.1       475      1.6

Aggregate by Command
--------------------
Command          TotalTime(s)  AvgMem(MB)  Repos
--------         ------------- ----------- -----
opengrep-1.15.1  172.23        613         7
semgrep-1.148.0  365.59        913         7
```

### `benchmark-report.sh`

Generate a comprehensive benchmark report combining timing, findings, and stability analysis.

```bash
./benchmark-report.sh <directory>
```

Produces a complete report including:
- Metadata (directory, timestamp, mode, binaries, rules)
- Timing and memory summary
- Findings comparison between tools
- Stability analysis across runs

Example:
```bash
./benchmark-report.sh bench_results/20260128_213555
```

### `generate-reports.sh`

Generate benchmark reports for all configurations in a results directory. Calls `benchmark-report.sh` for each configuration subdirectory.

```bash
./generate-reports.sh [directory]
```

If no directory is specified, uses the most recent `bench_results/` directory.

Examples:
```bash
# Generate reports for the most recent benchmark run
./generate-reports.sh

# Generate reports for a specific run
./generate-reports.sh bench_results/20260203_222652
```

This script is automatically called by `run-benchmarks.sh` after benchmarks complete.

## Output

Results are saved in timestamped directories:
```
bench_results/
  20260128_143052/
    metadata.json           # git state, versions, flags
    results_jellyfin.json   # hyperfine results
    results_jellyfin.md     # markdown table
    opengrep_ref_jellyfin_143052.json  # opengrep output
    semgrep_jellyfin_143055.json       # comparison output
```

## Dependencies

- [hyperfine](https://github.com/sharkdp/hyperfine) - benchmarking tool
- `timeout` (coreutils) - for `--max-time` support
- `jq` - for JSON formatting