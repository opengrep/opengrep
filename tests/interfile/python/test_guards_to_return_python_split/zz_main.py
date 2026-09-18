# ToReturn guard propagation across a forwarding call chain. [inner]
# returns a source under a parameter-anchored guard. [outer] forwards
# its parameter into [inner] and returns the result. A top-level
# caller that supplies a value satisfying the guard must fire; one
# that supplies a value violating the guard must not.

# ---------- No finding: top-level dict has len != 2 ----------

# ---------- Finding expected: top-level dict has len == 2 ----------

# ---------- Three-level chain with shifted forwarded parameter
# positions to surface any indexing bug in the rebinding ----------

