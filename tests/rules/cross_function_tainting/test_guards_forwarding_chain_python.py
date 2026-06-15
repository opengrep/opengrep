# A guard forwarded through a deep chain of wrappers that each pass [a + a] for
# the guarded parameter. The condition [code == 0] reads the parameter once;
# every hop substitutes it with an actual that reads the next parameter twice,
# so the rebound condition doubles per hop -- 2^depth nodes if the conds are not
# shared. It stays symbolic until a concrete value reaches the parameter, so it
# is rebound (never folded) at every hop and only folds at the final call.
# Without hash-consing the guard conds this is exponential in the chain depth:
# the analysis times out and the finding is dropped. This fixture pins both the
# bound (it must run well within the timeout) and that folding stays correct at
# the end of the chain.


def source():
    return "taint"


def sink(_x):
    pass


def c0(code, x):
    if code == 0:
        return x
    return ""


def c1(a, x): return c0(a + a, x)
def c2(a, x): return c1(a + a, x)
def c3(a, x): return c2(a + a, x)
def c4(a, x): return c3(a + a, x)
def c5(a, x): return c4(a + a, x)
def c6(a, x): return c5(a + a, x)
def c7(a, x): return c6(a + a, x)
def c8(a, x): return c7(a + a, x)
def c9(a, x): return c8(a + a, x)
def c10(a, x): return c9(a + a, x)
def c11(a, x): return c10(a + a, x)
def c12(a, x): return c11(a + a, x)
def c13(a, x): return c12(a + a, x)
def c14(a, x): return c13(a + a, x)
def c15(a, x): return c14(a + a, x)
def c16(a, x): return c15(a + a, x)
def c17(a, x): return c16(a + a, x)
def c18(a, x): return c17(a + a, x)
def c19(a, x): return c18(a + a, x)
def c20(a, x): return c19(a + a, x)
def c21(a, x): return c20(a + a, x)
def c22(a, x): return c21(a + a, x)
def c23(a, x): return c22(a + a, x)
def c24(a, x): return c23(a + a, x)
def c25(a, x): return c24(a + a, x)
def c26(a, x): return c25(a + a, x)
def c27(a, x): return c26(a + a, x)
def c28(a, x): return c27(a + a, x)
def c29(a, x): return c28(a + a, x)
def c30(a, x): return c29(a + a, x)


def call_yes():
    # ruleid: test-guards-forwarding-chain
    sink(c30(0, source()))


def call_no():
    # ok: test-guards-forwarding-chain
    sink(c30(5, source()))
