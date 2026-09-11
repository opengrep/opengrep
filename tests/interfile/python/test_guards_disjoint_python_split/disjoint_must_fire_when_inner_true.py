def disjoint_must_fire_when_inner_true():
    # len == 2 -> inner takes the True branch.
    # ruleid: test-guards-disjoint
    sink(outer_disjoint({"data": [1, 2]}))


