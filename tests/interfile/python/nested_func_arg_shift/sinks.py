def leak_tainted(v):
    # ruleid: nested-func-arg-shift
    sink(v)


def leak_clean(v):
    # ok: nested-func-arg-shift
    sink(v)
