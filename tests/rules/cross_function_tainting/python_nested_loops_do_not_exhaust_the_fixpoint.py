def f(x):
    a = b = c = d = z = ''
    for i0 in x:
        # ruleid: python_nested_loops_do_not_exhaust_the_fixpoint
        sink(d)
        # ok: python_nested_loops_do_not_exhaust_the_fixpoint
        sink(z)
        d = c
        for i1 in x:
            c = b
            b = a
            z = "safe"
        a = source()
