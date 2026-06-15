# Two return statements with identical taints but different path guards have
# one effect identity: [Effects] fuses their guards disjunctively on insertion
# ([Effect.compare] ignores guards), so the signature carries a single
# ToReturn effect guarded by [(a == 1) || (!(a == 1) && (a == 2))]. A call
# refuting both disjuncts drops the effect; a call satisfying one keeps it.


def source():
    return "taint"


def sink(_x):
    pass


def f(a, x):
    if a == 1:
        return x
    if a == 2:
        return x
    return ""


def calls():
    # ok: test-guard-effect-fuse
    sink(f(0, source()))
    # ruleid: test-guard-effect-fuse
    sink(f(1, source()))
