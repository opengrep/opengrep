def p(x): return q(x)
def q(x): return r(source())
def r(x):
    if cond():
        return p(x)
    return x

def test_3cycle():
    # ruleid: test-recursion-fixpoint
    sink(p(0))
