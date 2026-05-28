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


def source():
    return "taint"


def sink(_x):
    pass


# ---------- No finding: top-level caller's dict has len != 2 ----------

def inner_no(opts, x):
    if len(opts["data"]) == 2:
        sink(x)


def outer_no(a, p, b, x):
    inner_no(p, x)


def call_chain_no():
    outer_no("dummy", {"data": [1]}, "dummy", source())


# ---------- Finding expected: top-level caller's dict has len == 2 ----------

def inner_yes(opts, x):
    if len(opts["data"]) == 2:
        # ruleid: test-guards-rebind
        sink(x)


def outer_yes(a, p, b, x):
    inner_yes(p, x)


def call_chain_yes():
    outer_yes("dummy", {"data": [1, 2]}, "dummy", source())


# ---------- Three-level chain with distinct param positions ----------
# At every level the forwarded parameter sits at a different index. If
# the rebinding uses the wrong index at any step, the top-level call's
# concrete dict lookup will resolve to the wrong argument and the guard
# will fail to drop (or drop spuriously).


def innermost_no(a, opts, b, x):          # opts=1, x=3
    if len(opts["data"]) == 2:
        sink(x)


def middle_no(c, d, m, e, x):             # m=2, x=4
    innermost_no("dummy_a", m, "dummy_b", x)


def outer3_no(o, f, x):                   # o=0, x=2
    middle_no("c", "d", o, "e", x)


def call_chain3_no():
    outer3_no({"data": [1]}, "f", source())


def innermost_yes(a, opts, b, x):
    if len(opts["data"]) == 2:
        # ruleid: test-guards-rebind
        sink(x)


def middle_yes(c, d, m, e, x):
    innermost_yes("dummy_a", m, "dummy_b", x)


def outer3_yes(o, f, x):
    middle_yes("c", "d", o, "e", x)


def call_chain3_yes():
    outer3_yes({"data": [1, 2]}, "f", source())
