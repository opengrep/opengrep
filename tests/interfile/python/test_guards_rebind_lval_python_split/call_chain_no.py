from C import C
from forwarder import forwarder
from sink import sink
from source import source

def call_chain_no():
    c = C()
    forwarder(c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


