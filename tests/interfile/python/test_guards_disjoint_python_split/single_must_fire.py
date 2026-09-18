from outer_single import outer_single
from sink import sink

def single_must_fire():
    # ruleid: test-guards-disjoint
    sink(outer_single({"data": [1, 2]}))


