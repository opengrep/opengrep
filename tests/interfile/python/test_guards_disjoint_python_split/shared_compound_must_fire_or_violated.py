def shared_compound_must_fire_or_violated():
    # Neither disjunct satisfied -> inner False branch.
    # ruleid: test-guards-disjoint
    sink(outer_shared_compound({"data": [3, 4, 5]}))
