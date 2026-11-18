def sink(x):
    pass


# Four call hops separate the source from the sink; an interfile depth of
# three stops one hop short, so nothing is reported.
def step4(v):
    # ok: depth-cutoff
    sink(v)
