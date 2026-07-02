def gated_must_fire_inner_true():
    # data[0] = 2 != 1 -> outer if-true. len == 2 -> inner True branch.
    # ruleid: test-guards-disjoint
    sink(outer_gated({"data": [2, 3]}))


