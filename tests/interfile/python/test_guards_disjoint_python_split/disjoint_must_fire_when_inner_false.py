def disjoint_must_fire_when_inner_false():
    # len != 2 -> inner takes the False branch.
    # ruleid: test-guards-disjoint
    sink(outer_disjoint({"data": [1]}))


