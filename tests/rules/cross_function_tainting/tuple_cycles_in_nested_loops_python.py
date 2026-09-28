def f():
    v0 = source()
    while c1():
        while c2():
            v1 = (v0, v3)
            v2 = (v1, v6)
            v3 = (v2, v2)
            v4 = (v3, v5)
            v5 = (v4, v1)
            v6 = (v5, v4)
            v7 = (v6, v0)
            v0 = v7
    # ruleid: tuple_cycles_in_nested_loops_python
    sink(v7)
