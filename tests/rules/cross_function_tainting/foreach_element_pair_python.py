def local_pairs():
    for a, b in [(source(), "x"), ("y", "z")]:
        # ruleid: foreach_element_pair_python
        sink(a)
        # ok: foreach_element_pair_python
        sink(b)


def each_second(pairs, cb):
    for a, b in pairs:
        cb(b)


def sink_tainted(v):
    # ruleid: foreach_element_pair_python
    sink(v)


def sink_clean(v):
    # ok: foreach_element_pair_python
    sink(v)


def passed_pairs():
    each_second([("x", source())], sink_tainted)
    each_second([(source(), "x")], sink_clean)
