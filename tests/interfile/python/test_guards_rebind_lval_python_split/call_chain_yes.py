from C import C
from forwarder import forwarder
from sink import sink
from source import source

def call_chain_yes():
    c = C()
    forwarder(c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)


