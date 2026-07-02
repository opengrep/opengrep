def call_chain_yes():
    c = C()
    forwarder(c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)


