def call_shift_no():
    c = C()
    forwarder_shift("d", c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


