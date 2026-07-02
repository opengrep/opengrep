# Guard on a callback's return decided through higher-order dispatch.
# [apply] returns its callback's result; [handler] returns taint only
# when its guard parameter [flag] holds. The guard must be decided at the
# top-level caller: evaluated against a concrete literal, or rebound
# across a forwarding wrapper. (On main, with no guards, every call below
# false-positives.)

# ---------- Guard value supplied as a literal at the call site ----------

# ---------- Guard forwarded through a wrapper (rebinding) ----------

