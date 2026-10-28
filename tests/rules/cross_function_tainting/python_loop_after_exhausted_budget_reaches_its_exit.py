# The nested loops use up the visit budget; the loop after them still
# passes what its body writes to the code after it.
def f(x, y, q):
    while y:
        for i1 in x:
            if q:
                for i3 in x:
                    while y:
                        pass
                    d = ''
                    while y:
                        pass
                c = source()
            else:
                e = a + d
                d = d + c
        c = source()
    z = "safe"
    while y:
        b = source()
    # ruleid: python_loop_after_exhausted_budget_reaches_its_exit
    sink(b)
    # ok: python_loop_after_exhausted_budget_reaches_its_exit
    sink(z)
