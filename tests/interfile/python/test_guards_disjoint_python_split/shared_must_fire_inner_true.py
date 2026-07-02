def shared_must_fire_inner_true():
    # len == 2 -> inner True branch, but both branches return the same x.
    # ruleid: test-guards-disjoint
    sink(outer_shared({"data": [1, 2]}))


