def shared_must_fire_inner_false():
    # len != 2 -> inner False branch. Same x is returned.
    # ruleid: test-guards-disjoint
    sink(outer_shared({"data": [1]}))


