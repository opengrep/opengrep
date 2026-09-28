def f(p, r, c):
    x = r + ""
    while c:
        x = {"n": x}
    if c:
        x = p
    return x["n"]


def caller_deep():
    q = {"n": source()}
    # ruleid: parameter_read_below_leafless_cycle_python
    sink(f(q, "", True))


def caller_other_field():
    q = {"m": source()}
    # The read of p["n"] below the cycle entry is cut at p, which stands for
    # every field below it.
    # todook: parameter_read_below_leafless_cycle_python
    sink(f(q, "", True))
