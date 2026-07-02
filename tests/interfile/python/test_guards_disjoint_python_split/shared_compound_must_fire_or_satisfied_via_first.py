def shared_compound_must_fire_or_satisfied_via_first():
    # data[0] == 1 satisfies the OR -> inner True branch.
    # ruleid: test-guards-disjoint
    sink(outer_shared_compound({"data": [1, 2, 3]}))


