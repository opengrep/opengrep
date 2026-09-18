# More A-labelled taints reach the sink than the per-label bound on the items
# of a sink effect, but the single B-labelled taint must still be recorded for
# `requires: A and B` to be satisfied.
def foo():
    a1 = srcA()
    a2 = srcA()
    a3 = srcA()
    a4 = srcA()
    a5 = srcA()
    a6 = srcA()
    a7 = srcA()
    a8 = srcA()
    a9 = srcA()
    a10 = srcA()
    a11 = srcA()
    a12 = srcA()
    a13 = srcA()
    a14 = srcA()
    a15 = srcA()
    a16 = srcA()
    a17 = srcA()
    a18 = srcA()
    a19 = srcA()
    a20 = srcA()
    a21 = srcA()
    a22 = srcA()
    a23 = srcA()
    a24 = srcA()
    a25 = srcA()
    a26 = srcA()
    a27 = srcA()
    a28 = srcA()
    a29 = srcA()
    a30 = srcA()
    b = srcB()
    # ruleid: test
    sink(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15,
         a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28,
         a29, a30, b)

def bar():
    a1 = srcA()
    a2 = srcA()
    a3 = srcA()
    # ok: test
    sink(a1, a2, a3)
