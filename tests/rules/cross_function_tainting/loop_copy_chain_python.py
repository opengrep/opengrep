def f(n):
    a = ""
    b = ""
    c = ""
    d = ""
    e = ""
    for i in range(n):
        a = b
        b = c
        c = d
        d = e
        e = source()
    # ruleid: loop_copy_chain_python
    sink(a)
