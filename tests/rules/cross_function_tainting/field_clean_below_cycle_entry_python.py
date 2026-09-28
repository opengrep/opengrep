def direct(p):
    return p["n"]["b"]


def below_cycle(p, r, items, c):
    if c:
        x = {"b": r + "", "n": None}
        for i in items:
            x = {"b": r + "", "n": x}
    else:
        x = p
    return x["n"]["b"]


def caller_control():
    q = {}
    q["n"] = source()
    q["n"]["b"] = "safe"
    # ok: field_clean_below_cycle_entry_python
    sink(direct(q))


def caller_below_cycle():
    q = {}
    q["n"] = source()
    q["n"]["b"] = "safe"
    # The read of p["n"]["b"] below the cycle entry is cut at p, which stands
    # for every field below it.
    # todook: field_clean_below_cycle_entry_python
    sink(below_cycle(q, "", [], False))
