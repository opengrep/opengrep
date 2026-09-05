def sink(x):
    pass


# Four call hops separate the source from the sink, one more than the
# default interfile depth follows; the rule lifts the bound.
def step4(v):
    # ruleid: depth-unbounded
    sink(v)
