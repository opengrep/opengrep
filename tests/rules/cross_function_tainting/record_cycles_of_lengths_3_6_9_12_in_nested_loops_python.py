def f():
    v0 = source()
    while c1():
        while c2():
            v1 = {"a1": v0, "b": [v3]}
            v2 = {"a2": v1, "b": [v6]}
            v3 = {"a3": v2, "b": [v9]}
            v4 = {"a4": v3, "b": [v0]}
            v5 = {"a5": v4, "b": [v3]}
            v6 = {"a6": v5, "b": [v6]}
            v7 = {"a0": v6, "b": [v9]}
            v8 = {"a1": v7, "b": [v0]}
            v9 = {"a2": v8, "b": [v3]}
            v10 = {"a3": v9, "b": [v6]}
            v11 = {"a4": v10, "b": [v9]}
            v12 = {"a5": v11, "b": [v0]}
            v0 = v12
    # ruleid: record_cycles_of_lengths_3_6_9_12_in_nested_loops_python
    sink(v12)
