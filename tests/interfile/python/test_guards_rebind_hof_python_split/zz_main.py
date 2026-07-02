# HOF guard rebinding across a call chain: the inner function's
# [ToSinkInCall] effect (the callback invocation) is guarded by a
# branch condition on one of its parameters. Outer forwards both
# the callback and the guard-relevant parameter to inner without
# repackaging. At top-level instantiation the guard should drop
# effects whose top-level-caller argument fails the condition.
#
# Each intermediate frame rebinds the guard to its own parameter
# positions, which are intentionally shifted from the callee's to
# surface any index-arithmetic bug in the rebinding.


# ---------- No finding: top-level list has len != 2 ----------

# ---------- Finding expected: top-level list has len == 2 ----------

