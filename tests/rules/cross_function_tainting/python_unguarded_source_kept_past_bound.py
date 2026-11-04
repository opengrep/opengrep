# A source without a guard is kept when the sources under a guard fill the
# bound of their label: the caller's arguments may make the guard false.
def inner(opts):
    if len(opts["data"]) == 2:
        return source(1) + source(2) + source(3) + source(4) + source(5) + source(6) + source(7) + source(8)
    return source(9)


def outer(p):
    return inner(p)


def main():
    # ruleid: python_unguarded_source_kept_past_bound
    sink(outer({"data": [1]}))
    # ok: python_unguarded_source_kept_past_bound
    sink("safe")
