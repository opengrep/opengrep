from outer_single import outer_single
from sink import sink

def single_must_not_fire():
    # ok: test-guards-disjoint
    sink(outer_single({"data": [1]}))


