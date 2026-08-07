# Guard rebinding across a call chain: [outer] forwards its parameter
# to [inner] without re-wrapping. [inner]'s sig has a guard on its own
# [opts]. At instantiation of [inner] inside [outer]'s body, the
# guard's free [Fetch opts] substitutes to outer's argument for that
# position. Without rebinding, outer's sig emits the effect with
# guards stripped; a top-level caller then sees the sink fire
# regardless of whether outer's argument satisfies the inner guard.
#
# With rebinding, the substituted cond — now outer-frame-anchored —
# is carried as a new guard in outer's sig with [param_refs] pointing
# to outer's own parameter by its position in outer's signature.
#
# [outer] is intentionally declared with additional parameters on
# either side of the forwarded one so that any indexing bug in the
# rebinding would surface: a rebound [param_refs] with the wrong
# outer-frame index would look up the wrong argument at the
# top-level call and either fail to drop or drop spuriously.


# ---------- No finding: top-level caller's dict has len != 2 ----------

# ---------- Finding expected: top-level caller's dict has len == 2 ----------

# ---------- Three-level chain with distinct param positions ----------
# At every level the forwarded parameter sits at a different index. If
# the rebinding uses the wrong index at any step, the top-level call's
# concrete dict lookup will resolve to the wrong argument and the guard
# will fail to drop (or drop spuriously).


