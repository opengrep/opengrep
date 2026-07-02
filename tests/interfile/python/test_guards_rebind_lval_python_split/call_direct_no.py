def call_direct_no():
    c = C()
    tagger(c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


