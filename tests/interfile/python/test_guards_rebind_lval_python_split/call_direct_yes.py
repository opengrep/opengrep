def call_direct_yes():
    c = C()
    tagger(c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)


