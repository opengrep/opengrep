# ToReturn guard propagation across a forwarding call chain. [inner]
# returns a source under a parameter-anchored guard. [outer] forwards
# its parameter into [inner] and returns the result. A top-level
# caller that supplies a value satisfying the guard must fire; one
# that supplies a value violating the guard must not.

def source():
    return "taint"


def sink(_x):
    pass


# ---------- No finding: top-level dict has len != 2 ----------

def inner_no(opts):
    if len(opts["data"]) == 2:
        return source()
    return ""


def outer_no(p):
    return inner_no(p)


def call_chain_no():
    # ok: test-guards-to-return
    sink(outer_no({"data": [1]}))


# ---------- Finding expected: top-level dict has len == 2 ----------

def inner_yes(opts):
    if len(opts["data"]) == 2:
        return source()
    return ""


def outer_yes(p):
    return inner_yes(p)


def call_chain_yes():
    # ruleid: test-guards-to-return
    sink(outer_yes({"data": [1, 2]}))


# ---------- Three-level chain with shifted forwarded parameter
# positions to surface any indexing bug in the rebinding ----------

def innermost_no(a, opts, b):
    if len(opts["data"]) == 2:
        return source()
    return ""


def middle_no(c, d, m, e):
    return innermost_no("dummy_a", m, "dummy_b")


def outer3_no(o, f):
    return middle_no("c", "d", o, "e")


def call_chain3_no():
    # ok: test-guards-to-return
    sink(outer3_no({"data": [1]}, "f"))


def innermost_yes(a, opts, b):
    if len(opts["data"]) == 2:
        return source()
    return ""


def middle_yes(c, d, m, e):
    return innermost_yes("dummy_a", m, "dummy_b")


def outer3_yes(o, f):
    return middle_yes("c", "d", o, "e")


def call_chain3_yes():
    # ruleid: test-guards-to-return
    sink(outer3_yes({"data": [1, 2]}, "f"))
