def call_shift_yes():
    c = C()
    forwarder_shift("d", c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)
