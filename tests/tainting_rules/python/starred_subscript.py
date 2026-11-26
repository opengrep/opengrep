def f(a):
    idx = source()
    # ruleid: taint
    sink(a[*idx])

def g(a):
    idx = safe()
    # ok: taint
    sink(a[*idx])
