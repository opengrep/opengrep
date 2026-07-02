def call_chain_no():
    c = C()
    forwarder(c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


