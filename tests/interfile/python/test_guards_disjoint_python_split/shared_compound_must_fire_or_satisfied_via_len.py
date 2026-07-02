def shared_compound_must_fire_or_satisfied_via_len():
    # len == 2 satisfies the OR -> inner True branch.
    # ruleid: test-guards-disjoint
    sink(outer_shared_compound({"data": [3, 4]}))


