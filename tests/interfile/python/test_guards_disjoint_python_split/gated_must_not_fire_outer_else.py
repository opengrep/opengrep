def gated_must_not_fire_outer_else():
    # data[0] = 1 -> outer takes else, returns "" with no taint.
    # ok: test-guards-disjoint
    sink(outer_gated({"data": [1]}))


