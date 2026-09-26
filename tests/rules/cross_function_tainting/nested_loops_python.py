def f(n):
    a = ""
    b = ""
    for i in range(n):
        for j in range(n):
            for k in range(n):
                a = a + b
            b = b + str(j)
        b = source()
    # ruleid: nested_loops_python
    sink(a)


def g(n):
    a = ""
    b = ""
    c = ""
    d = ""
    for i in range(n):
        for j in range(n):
            a = a + b
        b = c
        c = d
        d = source()
    # ruleid: nested_loops_python
    sink(a)
