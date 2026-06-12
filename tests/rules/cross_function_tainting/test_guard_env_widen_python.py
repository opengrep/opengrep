# A taint whose guard widens across fixpoint iterations with an otherwise
# identical environment: [g] is tainted under [a == 2] in the first pass;
# the loop-carried copy [g = g2] (with [g2] tainted at the end of the
# previous iteration) fuses in the disjunct [b == 5 && a == 3] one pass
# later. The taint's identity is unchanged -- only its guard widens -- so
# the fixpoint's stability test must be guard-aware
# ([Lval_env.equal] via [equal_with_guards]): a guard-blind test stops
# before the widened guard reaches the sink recording, only [a == 2] is
# stored, and the call below -- which refutes [a == 2] but satisfies the
# fused disjunct -- loses its finding.


def source():
    return "taint"


def sink(_x):
    pass


def f(a, b, x):
    g = ""
    g2 = ""
    i = 0
    while i < 3:
        # ruleid: test-guard-env-widen
        sink(g)
        if a == 2:
            g = x
        if b == 5:
            g = g2
        if a == 3:
            g2 = x
        i = i + 1


def call():
    f(3, 5, source())
