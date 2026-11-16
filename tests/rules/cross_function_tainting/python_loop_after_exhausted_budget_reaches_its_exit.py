def f(c):
    v0 = v1 = v2 = v3 = v4 = v5 = ''
    while c():
        v0 = v1
        v1 = v2
        v2 = v3
        v3 = v4
        v4 = v5
        v5 = source()
    b = ''
    z = ''
    while c():
        b = source()
        z = "safe"
    # ruleid: python_loop_after_exhausted_budget_reaches_its_exit
    sink(b)
    # ok: python_loop_after_exhausted_budget_reaches_its_exit
    sink(z)
