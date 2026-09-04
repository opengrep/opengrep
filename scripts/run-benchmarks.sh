#! /usr/bin/env bash

# Coupling: If you update this script, you likely also want
# to update run-pro-benchmarks.sh in the pro repo.

# You may also need to update the comment in perf/README.md

set -e

# the benchmarks run the 'opengrep' of the PATH
export PATH=$PWD/bin:$PATH

config_path=perf/configs/ci_small_repos.yaml
echo $config_path

# Run timing benchmark
python3 perf/run-benchmarks --config $config_path --std-only --save-to baseline_timing1.json --no-time
jq . baseline_timing1.json
python3 perf/run-benchmarks --config $config_path --std-only --save-to baseline_timing2.json --no-time
jq . baseline_timing2.json

# Run latest timing benchmark
opengrep --version
opengrep-core -version
python3 perf/run-benchmarks --config $config_path --std-only --save-to timing1.json
jq . timing1.json
python3 perf/run-benchmarks --config $config_path --std-only --trace --save-to timing2.json --save-findings-to ci_small_repos_findings.json
jq . timing2.json
jq . ci_small_repos_findings.json

# Compare timing infos
perf/compare-perf baseline_timing1.json baseline_timing2.json timing1.json timing2.json "$1" "$2"
perf/compare-bench-findings ci_small_repos_findings.json

# Remove generated files
rm baseline_timing1.json
rm baseline_timing2.json
rm ci_small_repos_findings.json
rm timing1.json
rm timing2.json
