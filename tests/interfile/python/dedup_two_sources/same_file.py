def same_file(cond):
    a = source_a()
    b = source_b()
    x = a if cond else b
    # ruleid: dedup-two-sources
    sink(x)
