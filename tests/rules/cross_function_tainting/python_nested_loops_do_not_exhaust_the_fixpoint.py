# Each loop has its rounds however deep it is nested: a value that one pass
# of the outer loop taints reaches the sink in the next.
def f(x, y, z, w, q):
    a = b = c = d = e = f = g = ''
    for i0 in x:
        # ruleid: python_nested_loops_do_not_exhaust_the_fixpoint
        sink(e)
        for i1 in x:
            if q:
                g = source()
            else:
                while y:
                    for i4 in x:
                        c = source()
                        f = c + b
                if q:
                    for i4 in x:
                        e = e + g
                    for i4 in x:
                        e = source()
                else:
                    c = e + c
    # ok: python_nested_loops_do_not_exhaust_the_fixpoint
    sink(a)
