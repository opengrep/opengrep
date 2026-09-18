def first(cond):
    a = source_a()
    b = source_b()
    x = a if cond else b
    # ruleid: dedup-two-sinks
    sink(x)


def second(cond):
    a = source_a()
    b = source_b()
    y = a if cond else b
    # ruleid: dedup-two-sinks
    sink(y)
