def gated_must_fire_inner_false():
    # data[0] = 3 != 1 -> outer if-true. len == 1 -> inner False branch.
    # ruleid: test-guards-disjoint
    sink(outer_gated({"data": [3]}))


