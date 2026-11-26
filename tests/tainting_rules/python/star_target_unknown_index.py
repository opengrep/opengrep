def tainted_at_unknown_index(i):
    xs = list()
    xs[i] = source()
    a, *b = xs
    # ruleid: star_target_unknown_index
    sink(b)


def clean_at_unknown_index(i):
    xs = list()
    xs[i] = "safe"
    a, *b = xs
    # ok: star_target_unknown_index
    sink(b)
