# Exercises dedup_sig_effects + recursive_cache in Sig_inst.ml.
#
# [helper] holds a signature with three symbolic taint paths
# (one per argument).  Under the current exact-match dedup these paths
# stay separate and each argument's tainted-or-not status is preserved
# at instantiation time.  A regression to merging-based dedup would
# collapse them and lose per-argument granularity.
#
# The nested [wrapper] / [wrapper2] chain exercises recursive_cache:
# without memoisation the inner [helper] signature would be
# re-instantiated for every outer effect.


# === Exact-match dedup: each arg independently tracked ===


# === Recursive cache: same callback through nested HOFs ===


